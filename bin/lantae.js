#!/usr/bin/env node

const { Command } = require('commander');
const axios = require('axios');
const readline = require('readline');
const { exec } = require('child_process');
const { promisify } = require('util');
const fs = require('fs-extra');
const path = require('path');
const os = require('os');
const packageJson = require('../package.json');

const execAsync = promisify(exec);

const program = new Command();

program
  .name('lantae')
  .description('Multi-provider AI CLI/REPL with local tool support')
  .version(packageJson.version);

program
  .option('-m, --model <model>', 'specify the model to use', 'cogito:latest')
  .option('-p, --provider <provider>', 'specify the provider (ollama, openai, anthropic, bedrock, gemini, mistral, perplexity)', 'ollama')
  .option('-u, --url <url>', 'Ollama server URL', 'http://localhost:11434')
  .option('-r, --region <region>', 'AWS region for Bedrock and Secrets', 'us-east-1')
  .option('-s, --secret <secret>', 'AWS Secrets Manager secret name', 'lantae/api-keys')
  .option('-t, --temperature <temp>', 'set temperature for responses', '0.1')
  .option('-y, --auto-accept', 'auto-accept all prompts and confirmations')
  .option('--planning-mode', 'enable planning mode for complex tasks')
  .option('--no-banner', 'disable the startup banner');

program
  .command('chat')
  .description('Start interactive chat session')
  .action(async (options) => {
    await startREPL(program.opts());
  });

program
  .argument('[prompt]', 'single prompt to send')
  .action(async (prompt, options) => {
    if (prompt) {
      await sendSinglePrompt(prompt, program.opts());
    } else {
      await startREPL(program.opts());
    }
  });

class SecretManager {
  constructor(region = 'us-east-1', secretName = 'lantae/api-keys') {
    this.region = region;
    this.secretName = secretName;
    this.client = null;
    this.cachedSecrets = {};
  }

  async initClient() {
    if (this.client) return;
    
    const { SecretsManagerClient } = require('@aws-sdk/client-secrets-manager');
    const { fromNodeProviderChain } = require('@aws-sdk/credential-providers');
    
    this.client = new SecretsManagerClient({
      region: this.region,
      credentials: fromNodeProviderChain()
    });
  }

  async getApiKey(provider) {
    // First check environment variable
    const envKey = `${provider.toUpperCase()}_API_KEY`;
    if (process.env[envKey]) {
      return process.env[envKey];
    }

    // Check cached secrets
    if (this.cachedSecrets[provider]) {
      process.env[envKey] = this.cachedSecrets[provider];
      return this.cachedSecrets[provider];
    }

    // Fetch from AWS Secrets Manager
    try {
      await this.initClient();
      const { GetSecretValueCommand } = require('@aws-sdk/client-secrets-manager');
      
      const command = new GetSecretValueCommand({
        SecretId: this.secretName
      });
      
      const response = await this.client.send(command);
      const secrets = JSON.parse(response.SecretString);
      
      // Cache all secrets and set environment variables
      for (const [key, value] of Object.entries(secrets)) {
        this.cachedSecrets[key] = value;
        process.env[`${key.toUpperCase()}_API_KEY`] = value;
      }
      
      return this.cachedSecrets[provider];
    } catch (error) {
      if (error.name === 'ResourceNotFoundException') {
        throw new Error(`AWS Secret '${this.secretName}' not found. Create it with your API keys.`);
      }
      throw new Error(`Failed to retrieve API keys from AWS Secrets Manager: ${error.message}`);
    }
  }
}

class ProviderManager {
  constructor(secretManager, toolManager = null) {
    this.secretManager = secretManager;
    this.toolManager = toolManager;
    this.providers = {
      ollama: new OllamaProvider(),
      openai: new OpenAIProvider(secretManager),
      anthropic: new AnthropicProvider(secretManager),
      bedrock: new BedrockProvider(),
      gemini: new GeminiProvider(secretManager),
      mistral: new MistralProvider(secretManager),
      perplexity: new PerplexityProvider(secretManager)
    };
    this.currentProvider = 'ollama';
    this.currentModel = 'cogito:latest';
    
    // Set tool manager for Ollama provider
    if (this.toolManager) {
      this.providers.ollama.setToolManager(this.toolManager);
    }
  }

  async switchProvider(provider, model = null) {
    if (!this.providers[provider]) {
      throw new Error(`Provider '${provider}' not supported. Available: ${Object.keys(this.providers).join(', ')}`);
    }
    this.currentProvider = provider;
    if (model) {
      this.currentModel = model;
    } else {
      // Set default model for provider
      const defaults = {
        ollama: 'cogito:latest',
        openai: 'gpt-4o',
        anthropic: 'claude-3-5-sonnet-20241022',
        bedrock: 'claude-3-sonnet',
        gemini: 'gemini-1.5-pro',
        mistral: 'mistral-large-latest',
        perplexity: 'llama-3.1-sonar-large-128k-online'
      };
      this.currentModel = defaults[provider];
    }
  }

  async chat(messages, options = {}) {
    const provider = this.providers[this.currentProvider];
    return await provider.chat(this.currentModel, messages, options);
  }

  async listModels() {
    const provider = this.providers[this.currentProvider];
    return await provider.listModels();
  }

  getProviderInfo() {
    return {
      provider: this.currentProvider,
      model: this.currentModel
    };
  }
}

class OllamaProvider {
  constructor(baseURL = 'http://localhost:11434') {
    this.baseURL = baseURL;
    this.client = axios.create({
      baseURL,
      timeout: 120000,
    });
    this.toolManager = null;
  }

  setToolManager(toolManager) {
    this.toolManager = toolManager;
  }

  async chat(model, messages, options = {}) {
    try {
      // First ensure the model is available
      await this.ensureModel(model);
      
      // Add tool context to the system message
      const enhancedMessages = [...messages];
      if (this.toolManager && enhancedMessages.length > 0) {
        const toolsContext = this.toolManager.getToolsContext();
        const systemMessage = {
          role: 'system',
          content: `You are an AI assistant with access to various tools for file operations and system commands. ${toolsContext}

When you want to use a tool, include a TOOL_CALL in your response. You can make multiple tool calls in a single response. After each tool call, I will provide the result, and you can continue your response or make additional tool calls as needed.

Always explain what you're doing and why before using tools. Be helpful and thorough in your responses.`
        };
        
        // Insert system message at the beginning if it doesn't exist, or merge with existing system message
        if (enhancedMessages[0]?.role === 'system') {
          enhancedMessages[0].content = systemMessage.content + '\n\n' + enhancedMessages[0].content;
        } else {
          enhancedMessages.unshift(systemMessage);
        }
      }
      
      const response = await this.client.post('/api/chat', {
        model,
        messages: enhancedMessages,
        stream: false,
        options: {
          temperature: parseFloat(options.temperature || 0.1),
        }
      });
      
      let content = response.data.message.content;
      
      // Process tool calls in the response
      if (this.toolManager) {
        content = await this.processToolCalls(content);
      }
      
      return content;
    } catch (error) {
      if (error.code === 'ECONNREFUSED') {
        throw new Error('Cannot connect to Ollama server. Make sure Ollama is running.');
      }
      throw error;
    }
  }

  async processToolCalls(content) {
    const toolCallRegex = /TOOL_CALL:\s*([^\n]+)/g;
    let match;
    let processedContent = content;
    
    while ((match = toolCallRegex.exec(content)) !== null) {
      const toolCall = match[1].trim();
      const [toolName, ...args] = toolCall.split(' ');
      
      try {
        console.log(`\n🔧 Executing tool: ${toolName} ${args.join(' ')}`);
        const result = await this.toolManager.executeTool(toolName, args.join(' '));
        
        // Replace the tool call with the result
        const toolCallLine = match[0];
        const replacement = `${toolCallLine}\n\nTool Result:\n\`\`\`\n${result}\n\`\`\`\n`;
        processedContent = processedContent.replace(toolCallLine, replacement);
        
        console.log(`✅ Tool result: ${result.substring(0, 100)}${result.length > 100 ? '...' : ''}`);
      } catch (error) {
        const errorMsg = `Error executing ${toolName}: ${error.message}`;
        console.log(`❌ ${errorMsg}`);
        
        const toolCallLine = match[0];
        const replacement = `${toolCallLine}\n\nTool Error:\n\`\`\`\n${errorMsg}\n\`\`\`\n`;
        processedContent = processedContent.replace(toolCallLine, replacement);
      }
    }
    
    return processedContent;
  }

  async ensureModel(model) {
    try {
      // Check if model exists
      const models = await this.listModels();
      if (models.includes(model)) {
        return; // Model already exists
      }
      
      console.log(`Model "${model}" not found locally. Pulling model...`);
      
      // Pull the model
      const response = await this.client.post('/api/pull', {
        name: model,
        stream: false
      });
      
      if (response.status === 200) {
        console.log(`Successfully pulled model "${model}"`);
      }
    } catch (error) {
      if (error.response?.status === 404) {
        throw new Error(`Model "${model}" not found in Ollama registry. Check model name.`);
      }
      throw error;
    }
  }

  async listModels() {
    try {
      const response = await this.client.get('/api/tags');
      return (response.data.models || []).map(m => m.name);
    } catch (error) {
      if (error.code === 'ECONNREFUSED') {
        throw new Error('Cannot connect to Ollama server. Make sure Ollama is running.');
      }
      throw error;
    }
  }
}

class OpenAIProvider {
  constructor(secretManager) {
    this.secretManager = secretManager;
    this.client = null;
  }

  async initClient() {
    if (this.client) return;
    
    const OpenAI = require('openai');
    const apiKey = await this.secretManager.getApiKey('openai');
    
    if (!apiKey) {
      throw new Error('OpenAI API key not found in environment or AWS Secrets Manager');
    }
    
    this.client = new OpenAI({ apiKey });
  }

  async chat(model, messages, options = {}) {
    await this.initClient();
    
    try {
      const response = await this.client.chat.completions.create({
        model,
        messages,
        temperature: parseFloat(options.temperature || 0.1),
        max_tokens: 4096
      });
      
      return response.choices[0].message.content;
    } catch (error) {
      if (error.status === 401) {
        throw new Error('Invalid OpenAI API key. Check your credentials.');
      }
      throw error;
    }
  }

  async listModels() {
    return [
      'gpt-4o',
      'gpt-4o-mini',
      'gpt-4-turbo',
      'gpt-4',
      'gpt-3.5-turbo',
      'o1-preview',
      'o1-mini'
    ];
  }
}

class AnthropicProvider {
  constructor(secretManager) {
    this.secretManager = secretManager;
    this.client = null;
  }

  async initClient() {
    if (this.client) return;
    
    const Anthropic = require('@anthropic-ai/sdk');
    const apiKey = await this.secretManager.getApiKey('anthropic');
    
    if (!apiKey) {
      throw new Error('Anthropic API key not found in environment or AWS Secrets Manager');
    }
    
    this.client = new Anthropic({ apiKey });
  }

  async chat(model, messages, options = {}) {
    await this.initClient();
    
    try {
      const response = await this.client.messages.create({
        model,
        max_tokens: 4096,
        temperature: parseFloat(options.temperature || 0.1),
        messages
      });
      
      return response.content[0].text;
    } catch (error) {
      if (error.status === 401) {
        throw new Error('Invalid Anthropic API key. Check your credentials.');
      }
      throw error;
    }
  }

  async listModels() {
    return [
      'claude-3-5-sonnet-20241022',
      'claude-3-5-haiku-20241022',
      'claude-3-opus-20240229',
      'claude-3-sonnet-20240229',
      'claude-3-haiku-20240307'
    ];
  }
}

class GeminiProvider {
  constructor(secretManager) {
    this.secretManager = secretManager;
    this.client = null;
  }

  async initClient() {
    if (this.client) return;
    
    const { GoogleGenerativeAI } = require('@google/generative-ai');
    const apiKey = await this.secretManager.getApiKey('gemini');
    
    if (!apiKey) {
      throw new Error('Gemini API key not found in environment or AWS Secrets Manager');
    }
    
    this.client = new GoogleGenerativeAI(apiKey);
  }

  async chat(model, messages, options = {}) {
    await this.initClient();
    
    try {
      const geminiModel = this.client.getGenerativeModel({ 
        model,
        generationConfig: {
          temperature: parseFloat(options.temperature || 0.1),
          maxOutputTokens: 4096
        }
      });
      
      // Convert messages to Gemini format
      const history = messages.slice(0, -1).map(msg => ({
        role: msg.role === 'assistant' ? 'model' : 'user',
        parts: [{ text: msg.content }]
      }));
      
      const chat = geminiModel.startChat({ history });
      const result = await chat.sendMessage(messages[messages.length - 1].content);
      
      return result.response.text();
    } catch (error) {
      if (error.message.includes('API_KEY_INVALID')) {
        throw new Error('Invalid Gemini API key. Check your credentials.');
      }
      throw error;
    }
  }

  async listModels() {
    return [
      'gemini-1.5-pro',
      'gemini-1.5-flash',
      'gemini-1.0-pro'
    ];
  }
}

class MistralProvider {
  constructor(secretManager) {
    this.secretManager = secretManager;
    this.client = null;
  }

  async initClient() {
    if (this.client) return;
    
    const apiKey = await this.secretManager.getApiKey('mistral');
    
    if (!apiKey) {
      throw new Error('Mistral API key not found in environment or AWS Secrets Manager');
    }
    
    this.client = axios.create({
      baseURL: 'https://api.mistral.ai/v1',
      headers: {
        'Authorization': `Bearer ${apiKey}`,
        'Content-Type': 'application/json'
      },
      timeout: 30000
    });
  }

  async chat(model, messages, options = {}) {
    await this.initClient();
    
    try {
      const response = await this.client.post('/chat/completions', {
        model,
        messages,
        temperature: parseFloat(options.temperature || 0.1),
        max_tokens: 4096
      });
      
      return response.data.choices[0].message.content;
    } catch (error) {
      if (error.response?.status === 401) {
        throw new Error('Invalid Mistral API key. Check your credentials.');
      }
      throw error;
    }
  }

  async listModels() {
    return [
      'mistral-large-latest',
      'mistral-medium-latest',
      'mistral-small-latest',
      'open-mistral-7b',
      'open-mixtral-8x7b',
      'open-mixtral-8x22b'
    ];
  }
}

class PerplexityProvider {
  constructor(secretManager) {
    this.secretManager = secretManager;
    this.client = null;
  }

  async initClient() {
    if (this.client) return;
    
    const apiKey = await this.secretManager.getApiKey('perplexity');
    
    if (!apiKey) {
      throw new Error('Perplexity API key not found in environment or AWS Secrets Manager');
    }
    
    this.client = axios.create({
      baseURL: 'https://api.perplexity.ai',
      headers: {
        'Authorization': `Bearer ${apiKey}`,
        'Content-Type': 'application/json'
      },
      timeout: 30000
    });
  }

  async chat(model, messages, options = {}) {
    await this.initClient();
    
    try {
      const response = await this.client.post('/chat/completions', {
        model,
        messages,
        temperature: parseFloat(options.temperature || 0.1),
        max_tokens: 4096
      });
      
      return response.data.choices[0].message.content;
    } catch (error) {
      if (error.response?.status === 401) {
        throw new Error('Invalid Perplexity API key. Check your credentials.');
      }
      throw error;
    }
  }

  async listModels() {
    return [
      'llama-3.1-sonar-large-128k-online',
      'llama-3.1-sonar-small-128k-online',
      'llama-3.1-sonar-large-128k-chat',
      'llama-3.1-sonar-small-128k-chat',
      'llama-3.1-8b-instruct',
      'llama-3.1-70b-instruct'
    ];
  }
}

class BedrockProvider {
  constructor(region = 'us-east-1') {
    this.region = region;
    this.client = null;
  }

  async initClient() {
    if (this.client) return;
    
    const { BedrockRuntimeClient } = require('@aws-sdk/client-bedrock-runtime');
    const { fromNodeProviderChain } = require('@aws-sdk/credential-providers');
    
    this.client = new BedrockRuntimeClient({
      region: this.region,
      credentials: fromNodeProviderChain()
    });
  }

  async chat(model, messages, options = {}) {
    await this.initClient();
    
    const { InvokeModelCommand } = require('@aws-sdk/client-bedrock-runtime');
    
    const modelMap = {
      // Claude models
      'claude-3-sonnet': 'anthropic.claude-3-sonnet-20240229-v1:0',
      'claude-3-haiku': 'anthropic.claude-3-haiku-20240307-v1:0',
      'claude-3-opus': 'anthropic.claude-3-opus-20240229-v1:0',
      'claude-3-5-sonnet': 'anthropic.claude-3-5-sonnet-20240620-v1:0',
      'claude-3-5-haiku': 'anthropic.claude-3-5-haiku-20241022-v1:0',
      
      // Titan models
      'titan-text-g1-large': 'amazon.titan-text-lite-v1',
      'titan-text-g1-express': 'amazon.titan-text-express-v1',
      'titan-embed-text-v1': 'amazon.titan-embed-text-v1',
      'titan-embed-text-v2': 'amazon.titan-embed-text-v2:0',
      
      // Llama models
      'llama2-13b': 'meta.llama2-13b-chat-v1',
      'llama2-70b': 'meta.llama2-70b-chat-v1',
      'llama3-8b': 'meta.llama3-8b-instruct-v1:0',
      'llama3-70b': 'meta.llama3-70b-instruct-v1:0',
      
      // Cohere models
      'command-text': 'cohere.command-text-v14',
      'command-light': 'cohere.command-light-text-v14'
    };
    
    const bedrockModelId = modelMap[model] || model;
    
    try {
      let body;
      
      // Format request based on model family
      if (bedrockModelId.includes('anthropic.claude')) {
        body = JSON.stringify({
          anthropic_version: "bedrock-2023-05-31",
          max_tokens: 4096,
          temperature: parseFloat(options.temperature || 0.1),
          messages: messages
        });
      } else if (bedrockModelId.includes('amazon.titan')) {
        // Convert messages to text prompt for Titan
        const prompt = messages.map(m => `${m.role}: ${m.content}`).join('\n');
        body = JSON.stringify({
          inputText: prompt,
          textGenerationConfig: {
            temperature: parseFloat(options.temperature || 0.1),
            maxTokenCount: 4096
          }
        });
      } else if (bedrockModelId.includes('meta.llama')) {
        const prompt = messages.map(m => `${m.role}: ${m.content}`).join('\n');
        body = JSON.stringify({
          prompt: prompt,
          temperature: parseFloat(options.temperature || 0.1),
          max_gen_len: 4096
        });
      } else if (bedrockModelId.includes('cohere.command')) {
        const prompt = messages.map(m => `${m.role}: ${m.content}`).join('\n');
        body = JSON.stringify({
          prompt: prompt,
          temperature: parseFloat(options.temperature || 0.1),
          max_tokens: 4096
        });
      } else {
        throw new Error(`Unsupported model format: ${bedrockModelId}`);
      }
      
      const command = new InvokeModelCommand({
        modelId: bedrockModelId,
        body
      });
      
      const response = await this.client.send(command);
      const responseBody = JSON.parse(Buffer.from(response.body).toString());
      
      // Parse response based on model family
      if (bedrockModelId.includes('anthropic.claude')) {
        return responseBody.content[0].text;
      } else if (bedrockModelId.includes('amazon.titan')) {
        return responseBody.results[0].outputText;
      } else if (bedrockModelId.includes('meta.llama')) {
        return responseBody.generation;
      } else if (bedrockModelId.includes('cohere.command')) {
        return responseBody.generations[0].text;
      }
      
    } catch (error) {
      if (error.name === 'CredentialsProviderError') {
        throw new Error('AWS credentials not found. Configure AWS CLI or environment variables.');
      }
      throw error;
    }
  }

  async listModels() {
    return [
      // Claude models
      'claude-3-5-sonnet', 'claude-3-5-haiku', 'claude-3-sonnet', 'claude-3-haiku', 'claude-3-opus',
      
      // Titan models
      'titan-text-g1-large', 'titan-text-g1-express', 'titan-embed-text-v1', 'titan-embed-text-v2',
      
      // Llama models
      'llama2-13b', 'llama2-70b', 'llama3-8b', 'llama3-70b',
      
      // Cohere models
      'command-text', 'command-light'
    ];
  }
}

class LantaeCodeTracker {
  constructor() {
    this.generatedFiles = new Set();
    this.codeSnippets = new Map(); // file path -> snippets metadata
    this.lantaeSignature = this.generateSignature();
  }

  generateSignature() {
    return {
      version: packageJson.version,
      timestamp: new Date().toISOString(),
      generator: 'lantae-ai'
    };
  }

  // Add Lantae metadata header to generated code
  addLantaeHeader(content, language = 'javascript', context = {}) {
    const headers = {
      javascript: `/* Generated by Lantae AI v${packageJson.version} - ${new Date().toISOString()} */\n/* Context: ${JSON.stringify(context)} */\n`,
      python: `# Generated by Lantae AI v${packageJson.version} - ${new Date().toISOString()}\n# Context: ${JSON.stringify(context)}\n`,
      go: `// Generated by Lantae AI v${packageJson.version} - ${new Date().toISOString()}\n// Context: ${JSON.stringify(context)}\n`,
      rust: `// Generated by Lantae AI v${packageJson.version} - ${new Date().toISOString()}\n// Context: ${JSON.stringify(context)}\n`,
      html: `<!-- Generated by Lantae AI v${packageJson.version} - ${new Date().toISOString()} -->\n<!-- Context: ${JSON.stringify(context)} -->\n`,
      css: `/* Generated by Lantae AI v${packageJson.version} - ${new Date().toISOString()} */\n/* Context: ${JSON.stringify(context)} */\n`,
      sql: `-- Generated by Lantae AI v${packageJson.version} - ${new Date().toISOString()}\n-- Context: ${JSON.stringify(context)}\n`,
      yaml: `# Generated by Lantae AI v${packageJson.version} - ${new Date().toISOString()}\n# Context: ${JSON.stringify(context)}\n`,
      json: `{\n  "_lantae_metadata": {\n    "version": "${packageJson.version}",\n    "timestamp": "${new Date().toISOString()}",\n    "context": ${JSON.stringify(context)}\n  },\n`,
    };

    const header = headers[language] || headers.javascript;
    
    // For JSON, we need to handle it specially
    if (language === 'json') {
      try {
        const parsed = JSON.parse(content);
        parsed._lantae_metadata = {
          version: packageJson.version,
          timestamp: new Date().toISOString(),
          context
        };
        return JSON.stringify(parsed, null, 2);
      } catch (e) {
        return header + content;
      }
    }
    
    return header + content;
  }

  // Detect if content was generated by Lantae
  isLantaeGenerated(content) {
    const patterns = [
      /Generated by Lantae AI/,
      /_lantae_metadata/,
      /Context:.*lantae/i,
      /🤖.*Generated with.*Lantae/i
    ];
    
    return patterns.some(pattern => pattern.test(content));
  }

  // Register a file as Lantae-generated
  registerFile(filePath, metadata = {}) {
    this.generatedFiles.add(filePath);
    this.codeSnippets.set(filePath, {
      ...metadata,
      timestamp: new Date().toISOString(),
      version: packageJson.version
    });
    
    // Write metadata file
    this.saveMetadata();
  }

  // Save tracking metadata
  async saveMetadata() {
    const metadataPath = path.join(process.cwd(), '.lantae-generated.json');
    const metadata = {
      files: Array.from(this.generatedFiles),
      snippets: Object.fromEntries(this.codeSnippets),
      lastUpdate: new Date().toISOString(),
      version: packageJson.version
    };
    
    try {
      await fs.writeFile(metadataPath, JSON.stringify(metadata, null, 2));
    } catch (error) {
      console.error('Failed to save Lantae metadata:', error.message);
    }
  }

  // Load existing metadata
  async loadMetadata() {
    const metadataPath = path.join(process.cwd(), '.lantae-generated.json');
    
    try {
      if (await fs.pathExists(metadataPath)) {
        const content = await fs.readFile(metadataPath, 'utf8');
        const metadata = JSON.parse(content);
        
        this.generatedFiles = new Set(metadata.files || []);
        this.codeSnippets = new Map(Object.entries(metadata.snippets || {}));
      }
    } catch (error) {
      console.error('Failed to load Lantae metadata:', error.message);
    }
  }

  // Get language from file extension
  getLanguageFromExtension(filePath) {
    const ext = path.extname(filePath).toLowerCase();
    const languageMap = {
      '.js': 'javascript',
      '.ts': 'typescript', 
      '.py': 'python',
      '.go': 'go',
      '.rs': 'rust',
      '.html': 'html',
      '.css': 'css',
      '.sql': 'sql',
      '.yaml': 'yaml',
      '.yml': 'yaml',
      '.json': 'json',
      '.rb': 'ruby',
      '.php': 'php',
      '.java': 'java',
      '.cpp': 'cpp',
      '.c': 'c',
      '.sh': 'shell'
    };
    
    return languageMap[ext] || 'text';
  }
}

class ToolManager {
  constructor() {
    this.tools = {
      bash: this.executeBash.bind(this),
      node: this.executeNode.bind(this),
      python: this.executePython.bind(this),
      ls: this.listFiles.bind(this),
      cat: this.readFile.bind(this),
      pwd: this.getCurrentDir.bind(this),
      git: this.executeGit.bind(this),
      npm: this.executeNpm.bind(this),
      write_file: this.writeFile.bind(this),
      edit_file: this.editFile.bind(this),
      create_file: this.createFile.bind(this),
      delete_file: this.deleteFile.bind(this),
      mkdir: this.makeDirectory.bind(this),
      find: this.findFiles.bind(this)
    };
    
    this.codeTracker = new LantaeCodeTracker();
    this.codeTracker.loadMetadata();
  }

  async executeTool(toolName, args) {
    if (!this.tools[toolName]) {
      throw new Error(`Tool '${toolName}' not found. Available tools: ${Object.keys(this.tools).join(', ')}`);
    }
    return await this.tools[toolName](args);
  }

  async executeBash(command) {
    try {
      const { stdout, stderr } = await execAsync(command);
      return stdout + (stderr ? `\nSTDERR: ${stderr}` : '');
    } catch (error) {
      return `Error: ${error.message}`;
    }
  }

  async executeNode(code) {
    try {
      const result = eval(code);
      return String(result);
    } catch (error) {
      return `Error: ${error.message}`;
    }
  }

  async executePython(code) {
    try {
      const tempFile = path.join(os.tmpdir(), `lantae_${Date.now()}.py`);
      await fs.writeFile(tempFile, code);
      const { stdout, stderr } = await execAsync(`python3 ${tempFile}`);
      await fs.remove(tempFile);
      return stdout + (stderr ? `\nSTDERR: ${stderr}` : '');
    } catch (error) {
      return `Error: ${error.message}`;
    }
  }

  async executeGit(command) {
    return this.executeBash(`git ${command}`);
  }

  async executeNpm(command) {
    return this.executeBash(`npm ${command}`);
  }

  async listFiles(dir = '.') {
    try {
      const files = await fs.readdir(dir);
      return files.join('\n');
    } catch (error) {
      return `Error: ${error.message}`;
    }
  }

  async readFile(filePath) {
    try {
      const content = await fs.readFile(filePath, 'utf8');
      return content;
    } catch (error) {
      return `Error: ${error.message}`;
    }
  }

  async getCurrentDir() {
    return process.cwd();
  }

  async writeFile(args) {
    try {
      const [filePath, ...contentParts] = args.split(' ');
      let content = contentParts.join(' ');
      
      // Auto-tag with Lantae header
      const language = this.codeTracker.getLanguageFromExtension(filePath);
      const context = { tool: 'write_file', timestamp: new Date().toISOString() };
      content = this.codeTracker.addLantaeHeader(content, language, context);
      
      await fs.writeFile(filePath, content, 'utf8');
      
      // Register the file as Lantae-generated
      this.codeTracker.registerFile(filePath, {
        tool: 'write_file',
        language,
        size: content.length
      });
      
      return `File ${filePath} written successfully (tagged as Lantae-generated)`;
    } catch (error) {
      return `Error: ${error.message}`;
    }
  }

  async editFile(args) {
    try {
      const parts = args.split(' ');
      const filePath = parts[0];
      const lineNumber = parseInt(parts[1]);
      const newContent = parts.slice(2).join(' ');
      
      const content = await fs.readFile(filePath, 'utf8');
      const lines = content.split('\n');
      
      if (lineNumber > 0 && lineNumber <= lines.length) {
        lines[lineNumber - 1] = newContent;
        await fs.writeFile(filePath, lines.join('\n'), 'utf8');
        return `Line ${lineNumber} in ${filePath} edited successfully`;
      } else {
        return `Error: Line number ${lineNumber} out of range`;
      }
    } catch (error) {
      return `Error: ${error.message}`;
    }
  }

  async createFile(args) {
    try {
      const [filePath, ...contentParts] = args.split(' ');
      let content = contentParts.join(' ') || '';
      
      // Check if file already exists
      const exists = await fs.pathExists(filePath);
      if (exists) {
        return `Error: File ${filePath} already exists`;
      }
      
      // Auto-tag with Lantae header
      const language = this.codeTracker.getLanguageFromExtension(filePath);
      const context = { tool: 'create_file', timestamp: new Date().toISOString() };
      content = this.codeTracker.addLantaeHeader(content, language, context);
      
      await fs.writeFile(filePath, content, 'utf8');
      
      // Register the file as Lantae-generated
      this.codeTracker.registerFile(filePath, {
        tool: 'create_file',
        language,
        size: content.length
      });
      
      return `File ${filePath} created successfully (tagged as Lantae-generated)`;
    } catch (error) {
      return `Error: ${error.message}`;
    }
  }

  async deleteFile(filePath) {
    try {
      await fs.remove(filePath);
      return `File ${filePath} deleted successfully`;
    } catch (error) {
      return `Error: ${error.message}`;
    }
  }

  async makeDirectory(dirPath) {
    try {
      await fs.ensureDir(dirPath);
      return `Directory ${dirPath} created successfully`;
    } catch (error) {
      return `Error: ${error.message}`;
    }
  }

  async findFiles(pattern) {
    try {
      const { stdout } = await execAsync(`find . -name "${pattern}"`);
      return stdout || 'No files found';
    } catch (error) {
      return `Error: ${error.message}`;
    }
  }

  getToolsContext() {
    return `
Available tools you can use:
- bash <command>: Execute bash commands
- cat <file>: Read file contents
- write_file <file> <content>: Write content to a file
- edit_file <file> <line_number> <new_content>: Edit a specific line in a file
- create_file <file> [content]: Create a new file with optional content
- delete_file <file>: Delete a file
- mkdir <directory>: Create a directory
- ls [directory]: List files in directory
- find <pattern>: Find files matching pattern
- pwd: Get current directory
- git <command>: Execute git commands
- npm <command>: Execute npm commands
- node <code>: Execute Node.js code
- python <code>: Execute Python code

To use a tool, format your response like this:
TOOL_CALL: tool_name arguments
For example:
TOOL_CALL: cat package.json
TOOL_CALL: write_file hello.txt Hello World!
TOOL_CALL: edit_file main.js 5 console.log("Updated line");
`;
  }

  listAvailableTools() {
    return Object.keys(this.tools);
  }
}

async function sendSinglePrompt(prompt, options) {
  const secretManager = new SecretManager(options.region, options.secret);
  const toolManager = new ToolManager();
  const providerManager = new ProviderManager(secretManager, toolManager);
  
  if (options.provider !== 'ollama') {
    await providerManager.switchProvider(options.provider, options.model);
  } else {
    providerManager.currentModel = options.model;
  }
  
  try {
    console.log('Sending request...');
    const response = await providerManager.chat([
      { role: 'user', content: prompt }
    ], options);
    
    console.log('\n' + response);
  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  }
}

function printBanner() {
  console.log(`\x1b[96m
╔══════════════════════════════════════════════════════════════╗
║  \x1b[95m██╗      █████╗ ███╗   ██╗████████╗ █████╗ ███████╗\x1b[96m  ║
║  \x1b[95m██║     ██╔══██╗████╗  ██║╚══██╔══╝██╔══██╗██╔════╝\x1b[96m  ║
║  \x1b[95m██║     ███████║██╔██╗ ██║   ██║   ███████║█████╗\x1b[96m    ║
║  \x1b[95m██║     ██╔══██║██║╚██╗██║   ██║   ██╔══██║██╔══╝\x1b[96m    ║
║  \x1b[95m███████╗██║  ██║██║ ╚████║   ██║   ██║  ██║███████╗\x1b[96m  ║
║  \x1b[95m╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝   ╚═╝   ╚═╝  ╚═╝╚══════╝\x1b[96m  ║
║                                                              ║
║  \x1b[93m🚀 Multi-Provider LLM Interface v${packageJson.version}\x1b[96m                    ║
║  \x1b[92m⚡ Powered by Cogito Reasoning Model\x1b[96m                      ║
║  \x1b[94m🔗 Ollama • OpenAI • Anthropic • Bedrock & More\x1b[96m          ║
╚══════════════════════════════════════════════════════════════╝
\x1b[0m`);
}

function createCompleter(providerManager, toolManager) {
  // Define slash commands
  const slashCommands = ['help', 'model', 'provider', 'models', 'tool', 'tools', 'clear', 'info', 'env'];
  
  // Define providers
  const providers = ['ollama', 'openai', 'anthropic', 'bedrock', 'gemini', 'mistral', 'perplexity'];
  
  // Get available models (cached for performance)
  let models = [];
  providerManager.listModels().then(m => models = m).catch(() => {});
  
  // Get available tools
  const tools = toolManager.listAvailableTools();
  
  return function(line) {
    const completions = [];
    
    // Handle slash commands
    if (line.startsWith('/')) {
      const parts = line.slice(1).split(' ');
      const command = parts[0] || '';
      const args = parts.slice(1).join(' ');
      
      if (parts.length === 1) {
        // Complete slash command itself
        slashCommands.forEach(cmd => {
          if (cmd.startsWith(command)) {
            completions.push('/' + cmd);
          }
        });
      } else {
        // Complete command arguments
        switch (command) {
          case 'provider':
            if (parts.length === 2) {
              providers.forEach(p => {
                if (p.startsWith(args)) {
                  completions.push(`/provider ${p}`);
                }
              });
            } else if (parts.length === 3) {
              const providerName = parts[1];
              const modelStart = parts[2];
              models.forEach(m => {
                if (m.startsWith(modelStart)) {
                  completions.push(`/provider ${providerName} ${m}`);
                }
              });
            }
            break;
          case 'model':
            models.forEach(m => {
              if (m.startsWith(args)) {
                completions.push(`/model ${m}`);
              }
            });
            break;
          case 'tool':
            const toolParts = args.split(' ', 2);
            if (toolParts.length === 1) {
              tools.forEach(t => {
                if (t.startsWith(args)) {
                  completions.push(`/tool ${t}`);
                }
              });
            } else {
              const toolName = toolParts[0];
              const fileArg = toolParts[1];
              // For file-based tools, complete file paths
              if (['cat', 'write_file', 'edit_file', 'create_file', 'delete_file'].includes(toolName)) {
                try {
                  const files = fs.readdirSync(path.dirname(fileArg || '.'));
                  files.forEach(file => {
                    const fullPath = path.join(path.dirname(fileArg || '.'), file);
                    if (fullPath.startsWith(fileArg || '')) {
                      completions.push(`/tool ${toolName} ${fullPath}`);
                    }
                  });
                } catch (e) {
                  // Ignore errors
                }
              }
            }
            break;
        }
      }
    } else {
      // Non-slash command completions (file paths)
      try {
        const dir = path.dirname(line || '.');
        const basename = path.basename(line || '');
        const files = fs.readdirSync(dir);
        files.forEach(file => {
          if (file.startsWith(basename)) {
            completions.push(path.join(dir, file));
          }
        });
      } catch (e) {
        // Ignore errors
      }
    }
    
    return [completions, line];
  };
}

async function startREPL(options) {
  const secretManager = new SecretManager(options.region, options.secret);
  const toolManager = new ToolManager();
  const providerManager = new ProviderManager(secretManager, toolManager);
  const conversation = [];
  
  if (options.provider !== 'ollama') {
    await providerManager.switchProvider(options.provider, options.model);
  } else {
    providerManager.currentModel = options.model;
  }
  
  if (!options.noBanner) {
    printBanner();
  }
  
  const info = providerManager.getProviderInfo();
  console.log(`\x1b[96mProvider: \x1b[93m${info.provider}\x1b[96m | Model: \x1b[92m${info.model}\x1b[0m`);
  
  // Display active modes
  const modes = [];
  if (options.autoAccept) modes.push('\x1b[93mAuto-Accept\x1b[0m');
  if (options.planningMode) modes.push('\x1b[94mPlanning Mode\x1b[0m');
  if (modes.length > 0) {
    console.log(`\x1b[96mActive Modes: ${modes.join(', ')}\x1b[0m`);
  }
  
  console.log('\x1b[90mType "/help" for commands, "exit" or "quit" to end\x1b[0m\n');

  try {
    const models = await providerManager.listModels();
    if (models.length === 0) {
      console.log('⚠️  No models found.');
    } else if (!models.includes(providerManager.currentModel)) {
      console.log(`⚠️  Model "${providerManager.currentModel}" not found. Available models:`);
      models.slice(0, 10).forEach(m => console.log(`  - ${m}`));
      if (models.length > 10) console.log(`  ... and ${models.length - 10} more`);
      console.log('');
    }
  } catch (error) {
    console.error('Error checking models:', error.message);
  }

  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: '> ',
    completer: createCompleter(providerManager, toolManager)
  });

  rl.prompt();

  rl.on('line', async (line) => {
    const input = line.trim();
    
    if (input === 'exit' || input === 'quit') {
      rl.close();
      return;
    }
    
    if (input.startsWith('/')) {
      await handleSlashCommand(input, providerManager, toolManager, conversation);
      rl.prompt();
      return;
    }
    
    if (!input) {
      rl.prompt();
      return;
    }

    // Handle planning mode
    let processedInput = input;
    if (options.planningMode && !input.toLowerCase().includes('execute') && !input.toLowerCase().includes('proceed')) {
      processedInput = `Please create a detailed plan for: ${input}. Break it down into clear steps and ask for confirmation before proceeding.`;
    }

    conversation.push({ role: 'user', content: processedInput });
    
    try {
      console.log('🤖 Thinking...');
      const response = await providerManager.chat(conversation, options);
      conversation.push({ role: 'assistant', content: response });
      console.log('\n' + response + '\n');
      
      // Auto-accept mode handling
      if (options.autoAccept && (response.toLowerCase().includes('would you like') || response.toLowerCase().includes('shall i') || response.toLowerCase().includes('proceed'))) {
        console.log('\x1b[93m[AUTO-ACCEPT] Automatically confirming action...\x1b[0m\n');
        conversation.push({ role: 'user', content: 'Yes, please proceed.' });
        
        console.log('🤖 Executing...');
        const autoResponse = await providerManager.chat(conversation, options);
        conversation.push({ role: 'assistant', content: autoResponse });
        console.log('\n' + autoResponse + '\n');
      }
    } catch (error) {
      console.error('Error:', error.message);
    }
    
    rl.prompt();
  });

  rl.on('close', () => {
    console.log('\nGoodbye!');
    process.exit(0);
  });
}

async function handleSlashCommand(input, providerManager, toolManager, conversation) {
  const parts = input.slice(1).split(' ');
  const command = parts[0];
  const args = parts.slice(1).join(' ');

  switch (command) {
    case 'help':
      console.log(`
Available commands:
  /model <name>       - Switch to a different model
  /provider <name>    - Switch provider (ollama, openai, anthropic, bedrock, gemini, mistral, perplexity)
  /models             - List available models for current provider
  /tool <name> <args> - Execute a local tool
  /tools              - List available tools
  /clear              - Clear conversation history
  /info               - Show current provider and model info
  /env                - Show environment variables status
  /help               - Show this help message
`);
      break;

    case 'model':
      if (!args) {
        console.log('Usage: /model <model-name>');
        break;
      }
      try {
        providerManager.currentModel = args;
        console.log(`Switched to model: ${args}`);
      } catch (error) {
        console.error('Error:', error.message);
      }
      break;

    case 'provider':
      if (!args) {
        console.log('Usage: /provider <provider-name> [model]');
        break;
      }
      try {
        const [provider, model] = args.split(' ');
        await providerManager.switchProvider(provider, model);
        const info = providerManager.getProviderInfo();
        console.log(`Switched to provider: ${info.provider}, model: ${info.model}`);
      } catch (error) {
        console.error('Error:', error.message);
      }
      break;

    case 'models':
      try {
        const models = await providerManager.listModels();
        console.log('Available models:');
        models.forEach(m => console.log(`  - ${m}`));
      } catch (error) {
        console.error('Error:', error.message);
      }
      break;

    case 'tool':
      if (!args) {
        console.log('Usage: /tool <tool-name> <arguments>');
        break;
      }
      try {
        const spaceIndex = args.indexOf(' ');
        const toolName = spaceIndex === -1 ? args : args.substring(0, spaceIndex);
        const toolArgs = spaceIndex === -1 ? '' : args.substring(spaceIndex + 1);
        
        const result = await toolManager.executeTool(toolName, toolArgs);
        console.log(result);
      } catch (error) {
        console.error('Error:', error.message);
      }
      break;

    case 'tools':
      const tools = toolManager.listAvailableTools();
      console.log('Available tools:');
      tools.forEach(t => console.log(`  - ${t}`));
      break;

    case 'clear':
      conversation.length = 0;
      console.log('Conversation cleared.');
      break;

    case 'info':
      const info = providerManager.getProviderInfo();
      console.log(`Provider: ${info.provider}`);
      console.log(`Model: ${info.model}`);
      break;

    case 'env':
      console.log('Environment Variables Status:');
      const providers = ['openai', 'anthropic', 'gemini', 'mistral', 'perplexity'];
      providers.forEach(provider => {
        const key = `${provider.toUpperCase()}_API_KEY`;
        console.log(`${key}: ${process.env[key] ? '✓ Set' : '✗ Not set'}`);
      });
      console.log(`AWS_PROFILE: ${process.env.AWS_PROFILE || 'default'}`);
      console.log(`AWS_REGION: ${process.env.AWS_REGION || 'not set'}`);
      break;

    default:
      console.log(`Unknown command: /${command}. Type /help for available commands.`);
  }
}

if (require.main === module) {
  program.parse();
}

module.exports = { ProviderManager, ToolManager, SecretManager };