/**
 * Lantae Node.js Implementation
 * Multi-Provider LLM Interface
 */

export interface LantaeConfig {
  provider: string;
  model?: string;
  apiKey?: string;
  temperature?: number;
  timeout?: number;
}

export interface ChatResponse {
  message: string;
  provider: string;
  model: string;
  usage?: {
    tokens: number;
    cost?: number;
  };
}

export class Lantae {
  private config: LantaeConfig;

  constructor(config: LantaeConfig = { provider: 'ollama' }) {
    this.config = config;
  }

  async chat(message: string): Promise<ChatResponse> {
    // TODO: Implement actual chat functionality
    return {
      message: `Echo from ${this.config.provider}: ${message}`,
      provider: this.config.provider,
      model: this.config.model || 'default',
      usage: {
        tokens: message.length,
      },
    };
  }

  setProvider(provider: string): void {
    this.config.provider = provider;
  }

  setModel(model: string): void {
    this.config.model = model;
  }

  getConfig(): LantaeConfig {
    return { ...this.config };
  }
}

// Export everything for easy importing
export * from './providers/base';
export * from './config/types';

export default Lantae;