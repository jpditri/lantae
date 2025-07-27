/**
 * Configuration types for Lantae Node.js implementation
 */

export interface ProviderSettings {
  apiKeyEnv?: string;
  apiKey?: string;
  baseUrl?: string;
  timeout?: number;
  defaultModel?: string;
}

export interface UISettings {
  colors?: boolean;
  progressBars?: boolean;
  streaming?: boolean;
  banner?: boolean;
}

export interface LoggingSettings {
  level?: 'debug' | 'info' | 'warn' | 'error';
  file?: string;
  console?: boolean;
}

export interface LantaeConfiguration {
  default?: {
    provider?: string;
    model?: string;
    temperature?: number;
  };
  providers?: {
    ollama?: ProviderSettings;
    openai?: ProviderSettings;
    anthropic?: ProviderSettings;
    gemini?: ProviderSettings;
    mistral?: ProviderSettings;
    perplexity?: ProviderSettings;
  };
  ui?: UISettings;
  logging?: LoggingSettings;
}