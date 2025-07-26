import * as vscode from 'vscode';
import axios from 'axios';

interface LantaeStatus {
    provider: string;
    model: string;
    connected: boolean;
    lastError?: string;
}

interface MessageOptions {
    conversationHistory?: Array<{role: string, content: string}>;
    temperature?: number;
    maxTokens?: number;
}

export class LantaeProvider {
    private _currentProvider: string;
    private _currentModel: string;
    private _status: LantaeStatus;
    private _baseUrl: string = 'http://localhost:11434'; // Default Ollama URL

    constructor(private readonly _context: vscode.ExtensionContext) {
        const config = vscode.workspace.getConfiguration('lantae');
        this._currentProvider = config.get('provider', 'ollama');
        this._currentModel = config.get('model', 'cogito:latest');
        
        this._status = {
            provider: this._currentProvider,
            model: this._currentModel,
            connected: false
        };

        // Initialize connection
        this.testConnection();
    }

    public async sendMessage(message: string, options: MessageOptions = {}): Promise<string> {
        const config = vscode.workspace.getConfiguration('lantae');
        const temperature = options.temperature || config.get('temperature', 0.1);

        try {
            // Build messages array for chat
            const messages = [];
            
            // Add conversation history if provided
            if (options.conversationHistory) {
                messages.push(...options.conversationHistory.slice(-10)); // Last 10 for context
            }
            
            // Add current message
            messages.push({ role: 'user', content: message });

            // Send to appropriate provider
            const response = await this.callProvider(messages, temperature);
            
            this._status.connected = true;
            this._status.lastError = undefined;
            
            return response;

        } catch (error) {
            this._status.connected = false;
            this._status.lastError = error instanceof Error ? error.message : 'Unknown error';
            
            console.error('Provider error:', error);
            throw new Error(`Failed to get response from ${this._currentProvider}: ${this._status.lastError}`);
        }
    }

    public async switchProvider(provider: string): Promise<void> {
        this._currentProvider = provider;
        this._status.provider = provider;
        
        // Update configuration
        const config = vscode.workspace.getConfiguration('lantae');
        await config.update('provider', provider, vscode.ConfigurationTarget.Global);
        
        // Test new provider
        await this.testConnection();
    }

    public async switchModel(model: string): Promise<void> {
        this._currentModel = model;
        this._status.model = model;
        
        // Update configuration
        const config = vscode.workspace.getConfiguration('lantae');
        await config.update('model', model, vscode.ConfigurationTarget.Global);
    }

    public async getAvailableModels(): Promise<string[]> {
        try {
            switch (this._currentProvider) {
                case 'ollama':
                    return await this.getOllamaModels();
                case 'openai':
                    return ['gpt-4', 'gpt-3.5-turbo', 'gpt-4-turbo'];
                case 'anthropic':
                    return ['claude-3-opus', 'claude-3-sonnet', 'claude-3-haiku'];
                case 'bedrock':
                    return ['claude-v2', 'claude-instant-v1'];
                case 'gemini':
                    return ['gemini-pro', 'gemini-pro-vision'];
                case 'mistral':
                    return ['mistral-large', 'mistral-medium', 'mistral-small'];
                default:
                    return [this._currentModel];
            }
        } catch (error) {
            console.error('Error getting models:', error);
            return [this._currentModel];
        }
    }

    public getStatus(): LantaeStatus {
        return { ...this._status };
    }

    public getCurrentProvider(): string {
        return this._currentProvider;
    }

    public getCurrentModel(): string {
        return this._currentModel;
    }

    private async testConnection(): Promise<void> {
        try {
            switch (this._currentProvider) {
                case 'ollama':
                    await this.testOllamaConnection();
                    break;
                case 'openai':
                    await this.testOpenAIConnection();
                    break;
                case 'anthropic':
                    await this.testAnthropicConnection();
                    break;
                default:
                    // For other providers, assume connected if no error
                    this._status.connected = true;
            }
        } catch (error) {
            this._status.connected = false;
            this._status.lastError = error instanceof Error ? error.message : 'Connection failed';
            console.error('Connection test failed:', error);
        }
    }

    private async callProvider(messages: Array<{role: string, content: string}>, temperature: number): Promise<string> {
        switch (this._currentProvider) {
            case 'ollama':
                return await this.callOllama(messages, temperature);
            case 'openai':
                return await this.callOpenAI(messages, temperature);
            case 'anthropic':
                return await this.callAnthropic(messages, temperature);
            default:
                throw new Error(`Provider ${this._currentProvider} not implemented`);
        }
    }

    // Ollama implementation
    private async callOllama(messages: Array<{role: string, content: string}>, temperature: number): Promise<string> {
        const response = await axios.post(`${this._baseUrl}/api/chat`, {
            model: this._currentModel,
            messages: messages,
            stream: false,
            options: {
                temperature: temperature
            }
        });

        return response.data.message?.content || 'No response';
    }

    private async testOllamaConnection(): Promise<void> {
        await axios.get(`${this._baseUrl}/api/tags`);
        this._status.connected = true;
    }

    private async getOllamaModels(): Promise<string[]> {
        const response = await axios.get(`${this._baseUrl}/api/tags`);
        return response.data.models?.map((model: any) => model.name) || [];
    }

    // OpenAI implementation
    private async callOpenAI(messages: Array<{role: string, content: string}>, temperature: number): Promise<string> {
        const apiKey = process.env.OPENAI_API_KEY || await this.getSecretValue('OPENAI_API_KEY');
        
        if (!apiKey) {
            throw new Error('OpenAI API key not found. Set OPENAI_API_KEY environment variable.');
        }

        const response = await axios.post('https://api.openai.com/v1/chat/completions', {
            model: this._currentModel,
            messages: messages,
            temperature: temperature,
            max_tokens: 4000
        }, {
            headers: {
                'Authorization': `Bearer ${apiKey}`,
                'Content-Type': 'application/json'
            }
        });

        return response.data.choices[0]?.message?.content || 'No response';
    }

    private async testOpenAIConnection(): Promise<void> {
        const apiKey = process.env.OPENAI_API_KEY || await this.getSecretValue('OPENAI_API_KEY');
        
        if (!apiKey) {
            throw new Error('OpenAI API key not found');
        }

        await axios.get('https://api.openai.com/v1/models', {
            headers: {
                'Authorization': `Bearer ${apiKey}`
            }
        });
        
        this._status.connected = true;
    }

    // Anthropic implementation
    private async callAnthropic(messages: Array<{role: string, content: string}>, temperature: number): Promise<string> {
        const apiKey = process.env.ANTHROPIC_API_KEY || await this.getSecretValue('ANTHROPIC_API_KEY');
        
        if (!apiKey) {
            throw new Error('Anthropic API key not found. Set ANTHROPIC_API_KEY environment variable.');
        }

        // Convert messages to Anthropic format
        const prompt = messages.map(msg => `${msg.role}: ${msg.content}`).join('\n\n');

        const response = await axios.post('https://api.anthropic.com/v1/messages', {
            model: this._currentModel,
            max_tokens: 4000,
            temperature: temperature,
            messages: [{ role: 'user', content: prompt }]
        }, {
            headers: {
                'x-api-key': apiKey,
                'anthropic-version': '2023-06-01',
                'Content-Type': 'application/json'
            }
        });

        return response.data.content[0]?.text || 'No response';
    }

    private async testAnthropicConnection(): Promise<void> {
        const apiKey = process.env.ANTHROPIC_API_KEY || await this.getSecretValue('ANTHROPIC_API_KEY');
        
        if (!apiKey) {
            throw new Error('Anthropic API key not found');
        }

        // Test with a minimal request
        await axios.post('https://api.anthropic.com/v1/messages', {
            model: 'claude-3-haiku-20240307',
            max_tokens: 10,
            messages: [{ role: 'user', content: 'test' }]
        }, {
            headers: {
                'x-api-key': apiKey,
                'anthropic-version': '2023-06-01',
                'Content-Type': 'application/json'
            }
        });
        
        this._status.connected = true;
    }

    private async getSecretValue(key: string): Promise<string | undefined> {
        // Try to get from VS Code secrets storage
        try {
            return await this._context.secrets.get(key);
        } catch (error) {
            console.error(`Failed to get secret ${key}:`, error);
            return undefined;
        }
    }

    public async setSecretValue(key: string, value: string): Promise<void> {
        await this._context.secrets.store(key, value);
    }
}