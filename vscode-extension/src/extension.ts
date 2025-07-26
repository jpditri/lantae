import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient/node';
import { LantaeChat } from './chat';
import { LantaeProvider } from './provider';
import { StatusBarManager } from './statusBar';
import { LantaeDiagnostics } from './diagnostics';

let client: LanguageClient;
let chatProvider: LantaeChat;
let providerManager: LantaeProvider;
let statusBar: StatusBarManager;
let diagnostics: LantaeDiagnostics;

export function activate(context: vscode.ExtensionContext) {
    console.log('Lantae extension is now active');

    // Initialize components
    providerManager = new LantaeProvider(context);
    statusBar = new StatusBarManager(context);
    diagnostics = new LantaeDiagnostics(context);
    chatProvider = new LantaeChat(context, providerManager);

    // Register commands
    registerCommands(context);

    // Start LSP if enabled
    if (vscode.workspace.getConfiguration('lantae').get('enableLSP')) {
        startLanguageServer(context);
    }

    // Register webview providers
    context.subscriptions.push(
        vscode.window.registerWebviewViewProvider('lantaeChat', chatProvider)
    );

    // Update status bar
    statusBar.update(providerManager.getCurrentProvider(), providerManager.getCurrentModel());

    // Auto-start LSP if configured
    if (vscode.workspace.getConfiguration('lantae').get('autoStartLSP')) {
        vscode.commands.executeCommand('lantae.startLSP');
    }
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

function registerCommands(context: vscode.ExtensionContext) {
    // Chat command
    const chatCommand = vscode.commands.registerCommand('lantae.chat', async () => {
        const input = await vscode.window.showInputBox({
            prompt: 'Enter your message for Lantae',
            placeHolder: 'Ask me anything...'
        });

        if (input) {
            await chatProvider.sendMessage(input);
        }
    });

    // Explain code command
    const explainCommand = vscode.commands.registerCommand('lantae.explainCode', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showErrorMessage('No active editor');
            return;
        }

        const selection = editor.selection;
        const selectedText = editor.document.getText(selection);
        
        if (!selectedText) {
            vscode.window.showErrorMessage('No code selected');
            return;
        }

        const language = editor.document.languageId;
        const prompt = `Explain this ${language} code:\n\n${selectedText}`;
        
        await chatProvider.sendMessage(prompt);
    });

    // Optimize code command
    const optimizeCommand = vscode.commands.registerCommand('lantae.optimizeCode', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showErrorMessage('No active editor');
            return;
        }

        const selection = editor.selection;
        const selectedText = editor.document.getText(selection);
        
        if (!selectedText) {
            vscode.window.showErrorMessage('No code selected');
            return;
        }

        const language = editor.document.languageId;
        const prompt = `Optimize this ${language} code and explain the improvements:\n\n${selectedText}`;
        
        const response = await providerManager.sendMessage(prompt);
        
        // Show optimization in a new editor
        const doc = await vscode.workspace.openTextDocument({
            content: response,
            language: 'markdown'
        });
        
        await vscode.window.showTextDocument(doc);
    });

    // Generate tests command
    const generateTestsCommand = vscode.commands.registerCommand('lantae.generateTests', async () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showErrorMessage('No active editor');
            return;
        }

        const selection = editor.selection;
        const selectedText = editor.document.getText(selection);
        
        if (!selectedText) {
            vscode.window.showErrorMessage('No code selected');
            return;
        }

        const language = editor.document.languageId;
        const prompt = `Generate comprehensive unit tests for this ${language} code:\n\n${selectedText}`;
        
        const response = await providerManager.sendMessage(prompt);
        
        // Create new test file
        const testFileName = `test_${Date.now()}.${getTestFileExtension(language)}`;
        const doc = await vscode.workspace.openTextDocument({
            content: response,
            language: language
        });
        
        await vscode.window.showTextDocument(doc);
    });

    // Switch provider command
    const switchProviderCommand = vscode.commands.registerCommand('lantae.switchProvider', async () => {
        const providers = ['ollama', 'openai', 'anthropic', 'bedrock', 'gemini', 'mistral'];
        const selected = await vscode.window.showQuickPick(providers, {
            placeHolder: 'Select AI provider'
        });

        if (selected) {
            await providerManager.switchProvider(selected);
            statusBar.update(selected, providerManager.getCurrentModel());
            vscode.window.showInformationMessage(`Switched to ${selected}`);
        }
    });

    // Switch model command
    const switchModelCommand = vscode.commands.registerCommand('lantae.switchModel', async () => {
        const models = await providerManager.getAvailableModels();
        const selected = await vscode.window.showQuickPick(models, {
            placeHolder: 'Select AI model'
        });

        if (selected) {
            await providerManager.switchModel(selected);
            statusBar.update(providerManager.getCurrentProvider(), selected);
            vscode.window.showInformationMessage(`Switched to model ${selected}`);
        }
    });

    // Show status command
    const showStatusCommand = vscode.commands.registerCommand('lantae.showStatus', async () => {
        const status = await providerManager.getStatus();
        const message = `Provider: ${status.provider}\nModel: ${status.model}\nStatus: ${status.connected ? 'Connected' : 'Disconnected'}`;
        
        vscode.window.showInformationMessage(message);
    });

    // LSP commands
    const startLSPCommand = vscode.commands.registerCommand('lantae.startLSP', () => {
        if (!client) {
            startLanguageServer(context);
        }
    });

    const stopLSPCommand = vscode.commands.registerCommand('lantae.stopLSP', () => {
        if (client) {
            client.stop();
            client = undefined as any;
        }
    });

    const restartLSPCommand = vscode.commands.registerCommand('lantae.restartLSP', () => {
        if (client) {
            client.restart();
        }
    });

    // Register all commands
    context.subscriptions.push(
        chatCommand,
        explainCommand,
        optimizeCommand,
        generateTestsCommand,
        switchProviderCommand,
        switchModelCommand,
        showStatusCommand,
        startLSPCommand,
        stopLSPCommand,
        restartLSPCommand
    );
}

function startLanguageServer(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration('lantae');
    const port = config.get<number>('lspPort', 7777);

    // Server options for Lantae LSP
    const serverOptions: ServerOptions = {
        command: 'lantae',
        args: ['--lsp', '--port', port.toString()],
        options: {
            cwd: vscode.workspace.rootPath
        }
    };

    // Language client options
    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: 'file', language: 'typescript' },
            { scheme: 'file', language: 'javascript' },
            { scheme: 'file', language: 'python' },
            { scheme: 'file', language: 'ruby' },
            { scheme: 'file', language: 'go' },
            { scheme: 'file', language: 'rust' },
            { scheme: 'file', language: 'java' },
            { scheme: 'file', language: 'cpp' },
            { scheme: 'file', language: 'c' },
            { scheme: 'file', language: 'lantae-prompt' }
        ],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/.lantae')
        },
        initializationOptions: {
            provider: config.get('provider'),
            model: config.get('model'),
            temperature: config.get('temperature')
        }
    };

    // Create language client
    client = new LanguageClient(
        'lantae-lsp',
        'Lantae Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client
    client.start().then(() => {
        console.log('Lantae LSP client started');
        vscode.window.showInformationMessage('Lantae LSP server started');
    }).catch((error) => {
        console.error('Failed to start Lantae LSP client:', error);
        vscode.window.showErrorMessage('Failed to start Lantae LSP server');
    });

    context.subscriptions.push(client);
}

function getTestFileExtension(language: string): string {
    switch (language) {
        case 'typescript':
            return 'test.ts';
        case 'javascript':
            return 'test.js';
        case 'python':
            return 'test.py';
        case 'ruby':
            return 'test.rb';
        case 'go':
            return 'test.go';
        case 'rust':
            return 'test.rs';
        case 'java':
            return 'Test.java';
        default:
            return 'test.txt';
    }
}