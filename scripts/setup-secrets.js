#!/usr/bin/env node

const { SecretsManagerClient, GetSecretValueCommand } = require('@aws-sdk/client-secrets-manager');
const { fromNodeProviderChain } = require('@aws-sdk/credential-providers');
const fs = require('fs-extra');
const path = require('path');

async function fetchAndCreateSecretsFile() {
  try {
    console.log('🔐 Fetching API keys from AWS Secrets Manager...');
    
    const region = process.env.AWS_REGION || 'us-east-1';
    const secretName = process.env.LANTAE_SECRET_NAME || 'lantae/api-keys';
    
    const client = new SecretsManagerClient({
      region,
      credentials: fromNodeProviderChain()
    });
    
    const command = new GetSecretValueCommand({
      SecretId: secretName
    });
    
    const response = await client.send(command);
    const secrets = JSON.parse(response.SecretString);
    
    // Create a formatted secrets file
    const secretsFilePath = path.join(__dirname, 'lantae-secrets.json');
    await fs.writeJSON(secretsFilePath, secrets, { spaces: 2 });
    
    console.log('✅ Successfully created lantae-secrets.json with API keys');
    console.log(`📍 File location: ${secretsFilePath}`);
    console.log('🔒 This file is already in .gitignore for security');
    
    // Set environment variables for immediate use
    Object.entries(secrets).forEach(([provider, key]) => {
      const envKey = `${provider.toUpperCase()}_API_KEY`;
      process.env[envKey] = key;
      console.log(`✓ Set ${envKey}`);
    });
    
    console.log('\n🚀 You can now use lantae with any provider:');
    console.log('   lantae -p openai "Hello"');
    console.log('   lantae -p anthropic "Hello"');
    console.log('   lantae -p gemini "Hello"');
    console.log('   lantae -p mistral "Hello"');
    console.log('   lantae -p perplexity "Hello"');
    
  } catch (error) {
    if (error.name === 'ResourceNotFoundException') {
      console.error(`❌ AWS Secret '${secretName}' not found.`);
      console.log('\n📝 To create the secret, run:');
      console.log(`aws secretsmanager create-secret --name "${secretName}" --description "Lantae API Keys" --secret-string '{
  "openai": "sk-your-openai-key",
  "anthropic": "sk-ant-your-anthropic-key", 
  "gemini": "your-gemini-api-key",
  "mistral": "your-mistral-api-key",
  "perplexity": "pplx-your-perplexity-key"
}'`);
    } else if (error.name === 'CredentialsProviderError') {
      console.error('❌ AWS credentials not found.');
      console.log('\n🔧 Configure AWS credentials:');
      console.log('   aws configure');
      console.log('   # OR set environment variables:');
      console.log('   export AWS_ACCESS_KEY_ID=your-key');
      console.log('   export AWS_SECRET_ACCESS_KEY=your-secret');
      console.log('   export AWS_REGION=us-east-1');
    } else {
      console.error('❌ Error:', error.message);
    }
    process.exit(1);
  }
}

// Also create a template file showing the expected format
async function createTemplate() {
  const templatePath = path.join(__dirname, 'secrets-template.json');
  const template = {
    "openai": "sk-your-openai-api-key-here",
    "anthropic": "sk-ant-your-anthropic-api-key-here",
    "gemini": "your-google-gemini-api-key-here",
    "mistral": "your-mistral-api-key-here", 
    "perplexity": "pplx-your-perplexity-api-key-here"
  };
  
  await fs.writeJSON(templatePath, template, { spaces: 2 });
  console.log('📋 Created secrets-template.json as reference');
}

if (require.main === module) {
  fetchAndCreateSecretsFile().then(() => createTemplate());
}

module.exports = { fetchAndCreateSecretsFile };