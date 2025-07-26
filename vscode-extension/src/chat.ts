import * as vscode from 'vscode';
import { LantaeProvider } from './provider';

export class LantaeChat implements vscode.WebviewViewProvider {
    public static readonly viewType = 'lantaeChat';

    private _view?: vscode.WebviewView;
    private _conversationHistory: Array<{role: string, content: string, timestamp: Date}> = [];

    constructor(
        private readonly _extensionContext: vscode.ExtensionContext,
        private readonly _provider: LantaeProvider
    ) {}

    public resolveWebviewView(
        webviewView: vscode.WebviewView,
        context: vscode.WebviewViewResolveContext,
        _token: vscode.CancellationToken,
    ) {
        this._view = webviewView;

        webviewView.webview.options = {
            enableScripts: true,
            localResourceRoots: [
                this._extensionContext.extensionUri
            ]
        };

        webviewView.webview.html = this._getHtmlForWebview(webviewView.webview);

        // Handle messages from the webview
        webviewView.webview.onDidReceiveMessage(
            async (data) => {
                switch (data.type) {
                    case 'send':
                        await this.sendMessage(data.message);
                        break;
                    case 'clear':
                        this.clearConversation();
                        break;
                    case 'export':
                        this.exportConversation();
                        break;
                    case 'switchProvider':
                        await this._provider.switchProvider(data.provider);
                        this.updateProviderInfo();
                        break;
                    case 'switchModel':
                        await this._provider.switchModel(data.model);
                        this.updateProviderInfo();
                        break;
                }
            },
            undefined,
            this._extensionContext.subscriptions
        );

        // Initialize with current provider info
        this.updateProviderInfo();
    }

    public async sendMessage(message: string): Promise<void> {
        if (!this._view) {
            return;
        }

        // Add user message to history
        this._conversationHistory.push({
            role: 'user',
            content: message,
            timestamp: new Date()
        });

        // Update UI with user message
        this._view.webview.postMessage({
            type: 'userMessage',
            message: message,
            timestamp: new Date().toLocaleTimeString()
        });

        // Show typing indicator
        this._view.webview.postMessage({
            type: 'typing',
            isTyping: true
        });

        try {
            // Get response from provider
            const response = await this._provider.sendMessage(message, {
                conversationHistory: this._conversationHistory.slice(-10) // Last 10 messages for context
            });

            // Add AI response to history
            this._conversationHistory.push({
                role: 'assistant',
                content: response,
                timestamp: new Date()
            });

            // Update UI with AI response
            this._view.webview.postMessage({
                type: 'assistantMessage',
                message: response,
                timestamp: new Date().toLocaleTimeString()
            });

        } catch (error) {
            console.error('Error sending message:', error);
            
            this._view.webview.postMessage({
                type: 'error',
                message: `Error: ${error instanceof Error ? error.message : 'Unknown error'}`
            });
        } finally {
            // Hide typing indicator
            this._view.webview.postMessage({
                type: 'typing',
                isTyping: false
            });
        }
    }

    public clearConversation(): void {
        this._conversationHistory = [];
        
        if (this._view) {
            this._view.webview.postMessage({
                type: 'clear'
            });
        }
    }

    public async exportConversation(): Promise<void> {
        if (this._conversationHistory.length === 0) {
            vscode.window.showInformationMessage('No conversation to export');
            return;
        }

        const content = this._conversationHistory.map(msg => {
            const timestamp = msg.timestamp.toLocaleString();
            return `[${timestamp}] ${msg.role.toUpperCase()}: ${msg.content}`;
        }).join('\n\n');

        const doc = await vscode.workspace.openTextDocument({
            content: content,
            language: 'markdown'
        });

        await vscode.window.showTextDocument(doc);
    }

    private updateProviderInfo(): void {
        if (!this._view) {
            return;
        }

        const status = this._provider.getStatus();
        
        this._view.webview.postMessage({
            type: 'providerInfo',
            provider: status.provider,
            model: status.model,
            connected: status.connected
        });
    }

    private _getHtmlForWebview(webview: vscode.Webview): string {
        return `<!DOCTYPE html>
        <html lang="en">
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <title>Lantae Chat</title>
            <style>
                body {
                    font-family: var(--vscode-font-family);
                    font-size: var(--vscode-font-size);
                    color: var(--vscode-foreground);
                    background-color: var(--vscode-editor-background);
                    margin: 0;
                    padding: 10px;
                    height: 100vh;
                    display: flex;
                    flex-direction: column;
                }

                .header {
                    border-bottom: 1px solid var(--vscode-panel-border);
                    padding-bottom: 10px;
                    margin-bottom: 10px;
                }

                .provider-info {
                    font-size: 0.9em;
                    color: var(--vscode-descriptionForeground);
                }

                .status-indicator {
                    width: 8px;
                    height: 8px;
                    border-radius: 50%;
                    display: inline-block;
                    margin-right: 5px;
                }

                .status-connected {
                    background-color: var(--vscode-charts-green);
                }

                .status-disconnected {
                    background-color: var(--vscode-charts-red);
                }

                .chat-container {
                    flex: 1;
                    overflow-y: auto;
                    margin-bottom: 10px;
                    padding: 10px;
                    border: 1px solid var(--vscode-panel-border);
                    border-radius: 4px;
                }

                .message {
                    margin-bottom: 15px;
                    padding: 10px;
                    border-radius: 8px;
                    max-width: 100%;
                    word-wrap: break-word;
                }

                .user-message {
                    background-color: var(--vscode-button-background);
                    color: var(--vscode-button-foreground);
                    margin-left: 20px;
                }

                .assistant-message {
                    background-color: var(--vscode-editor-selectionBackground);
                    margin-right: 20px;
                }

                .message-header {
                    font-size: 0.8em;
                    opacity: 0.7;
                    margin-bottom: 5px;
                }

                .typing-indicator {
                    display: none;
                    padding: 10px;
                    font-style: italic;
                    color: var(--vscode-descriptionForeground);
                }

                .input-container {
                    display: flex;
                    gap: 5px;
                }

                .message-input {
                    flex: 1;
                    padding: 8px;
                    border: 1px solid var(--vscode-input-border);
                    border-radius: 4px;
                    background-color: var(--vscode-input-background);
                    color: var(--vscode-input-foreground);
                    font-family: inherit;
                    font-size: inherit;
                }

                .send-button, .action-button {
                    padding: 8px 12px;
                    background-color: var(--vscode-button-background);
                    color: var(--vscode-button-foreground);
                    border: none;
                    border-radius: 4px;
                    cursor: pointer;
                    font-family: inherit;
                }

                .send-button:hover, .action-button:hover {
                    background-color: var(--vscode-button-hoverBackground);
                }

                .action-buttons {
                    display: flex;
                    gap: 5px;
                    margin-top: 10px;
                }

                .error-message {
                    color: var(--vscode-errorForeground);
                    background-color: var(--vscode-inputValidation-errorBackground);
                    border: 1px solid var(--vscode-inputValidation-errorBorder);
                    padding: 10px;
                    border-radius: 4px;
                    margin-bottom: 10px;
                }

                pre {
                    background-color: var(--vscode-textCodeBlock-background);
                    border: 1px solid var(--vscode-panel-border);
                    border-radius: 4px;
                    padding: 10px;
                    overflow-x: auto;
                    font-family: var(--vscode-editor-font-family);
                }

                code {
                    background-color: var(--vscode-textCodeBlock-background);
                    border-radius: 2px;
                    padding: 2px 4px;
                    font-family: var(--vscode-editor-font-family);
                }
            </style>
        </head>
        <body>
            <div class="header">
                <div class="provider-info">
                    <span class="status-indicator" id="statusIndicator"></span>
                    <span id="providerInfo">Connecting...</span>
                </div>
            </div>

            <div class="chat-container" id="chatContainer">
                <div class="message assistant-message">
                    <div class="message-header">Lantae AI</div>
                    <div>Hello! I'm Lantae, your AI assistant. How can I help you today?</div>
                </div>
            </div>

            <div class="typing-indicator" id="typingIndicator">
                Lantae is typing...
            </div>

            <div class="input-container">
                <input type="text" id="messageInput" class="message-input" placeholder="Type your message..." />
                <button id="sendButton" class="send-button">Send</button>
            </div>

            <div class="action-buttons">
                <button id="clearButton" class="action-button">Clear</button>
                <button id="exportButton" class="action-button">Export</button>
            </div>

            <script>
                const vscode = acquireVsCodeApi();
                const chatContainer = document.getElementById('chatContainer');
                const messageInput = document.getElementById('messageInput');
                const sendButton = document.getElementById('sendButton');
                const clearButton = document.getElementById('clearButton');
                const exportButton = document.getElementById('exportButton');
                const typingIndicator = document.getElementById('typingIndicator');
                const statusIndicator = document.getElementById('statusIndicator');
                const providerInfo = document.getElementById('providerInfo');

                // Send message
                function sendMessage() {
                    const message = messageInput.value.trim();
                    if (message) {
                        vscode.postMessage({
                            type: 'send',
                            message: message
                        });
                        messageInput.value = '';
                    }
                }

                sendButton.addEventListener('click', sendMessage);
                
                messageInput.addEventListener('keypress', (e) => {
                    if (e.key === 'Enter' && !e.shiftKey) {
                        e.preventDefault();
                        sendMessage();
                    }
                });

                clearButton.addEventListener('click', () => {
                    vscode.postMessage({ type: 'clear' });
                });

                exportButton.addEventListener('click', () => {
                    vscode.postMessage({ type: 'export' });
                });

                // Handle messages from extension
                window.addEventListener('message', event => {
                    const message = event.data;

                    switch (message.type) {
                        case 'userMessage':
                            addMessage('user', message.message, message.timestamp);
                            break;
                        case 'assistantMessage':
                            addMessage('assistant', message.message, message.timestamp);
                            break;
                        case 'error':
                            addErrorMessage(message.message);
                            break;
                        case 'typing':
                            typingIndicator.style.display = message.isTyping ? 'block' : 'none';
                            break;
                        case 'clear':
                            chatContainer.innerHTML = '';
                            break;
                        case 'providerInfo':
                            updateProviderInfo(message.provider, message.model, message.connected);
                            break;
                    }
                });

                function addMessage(role, content, timestamp) {
                    const messageDiv = document.createElement('div');
                    messageDiv.className = \`message \${role}-message\`;
                    
                    const headerDiv = document.createElement('div');
                    headerDiv.className = 'message-header';
                    headerDiv.textContent = \`\${role === 'user' ? 'You' : 'Lantae'} - \${timestamp}\`;
                    
                    const contentDiv = document.createElement('div');
                    
                    // Simple markdown-like formatting
                    content = content
                        .replace(/\`\`\`([\\s\\S]*?)\`\`\`/g, '<pre><code>$1</code></pre>')
                        .replace(/\`([^\`]+)\`/g, '<code>$1</code>')
                        .replace(/\\n/g, '<br>');
                    
                    contentDiv.innerHTML = content;
                    
                    messageDiv.appendChild(headerDiv);
                    messageDiv.appendChild(contentDiv);
                    chatContainer.appendChild(messageDiv);
                    
                    chatContainer.scrollTop = chatContainer.scrollHeight;
                }

                function addErrorMessage(message) {
                    const errorDiv = document.createElement('div');
                    errorDiv.className = 'error-message';
                    errorDiv.textContent = message;
                    chatContainer.appendChild(errorDiv);
                    chatContainer.scrollTop = chatContainer.scrollHeight;
                }

                function updateProviderInfo(provider, model, connected) {
                    statusIndicator.className = \`status-indicator \${connected ? 'status-connected' : 'status-disconnected'}\`;
                    providerInfo.textContent = \`\${provider} - \${model}\`;
                }

                // Focus input on load
                messageInput.focus();
            </script>
        </body>
        </html>`;
    }
}