#!/usr/bin/env ruby

require 'aws-sdk-secretsmanager'
require 'json'

def fetch_and_create_secrets_file
  puts '🔐 Fetching API keys from AWS Secrets Manager...'
  
  region = ENV['AWS_REGION'] || 'us-east-1'
  secret_name = ENV['LANTAE_SECRET_NAME'] || 'lantae/api-keys'
  
  begin
    client = Aws::SecretsManager::Client.new(region: region)
    
    response = client.get_secret_value(secret_id: secret_name)
    secrets = JSON.parse(response.secret_string)
    
    # Create a formatted secrets file
    secrets_file_path = File.join(__dir__, 'lantae-secrets.json')
    File.write(secrets_file_path, JSON.pretty_generate(secrets))
    
    puts '✅ Successfully created lantae-secrets.json with API keys'
    puts "📍 File location: #{secrets_file_path}"
    puts '🔒 This file is already in .gitignore for security'
    
    # Set environment variables for immediate use
    secrets.each do |provider, key|
      env_key = "#{provider.upcase}_API_KEY"
      ENV[env_key] = key
      puts "✓ Set #{env_key}"
    end
    
    puts "\n🚀 You can now use lantae with any provider:"
    puts '   ./lantae.rb -p openai "Hello"'
    puts '   ./lantae.rb -p anthropic "Hello"'
    puts '   ./lantae.rb -p gemini "Hello"'
    puts '   ./lantae.rb -p mistral "Hello"'
    puts '   ./lantae.rb -p perplexity "Hello"'
    
  rescue Aws::SecretsManager::Errors::ResourceNotFoundException
    puts "❌ AWS Secret '#{secret_name}' not found."
    puts "\n📝 To create the secret, run:"
    puts <<~CMD
      aws secretsmanager create-secret --name "#{secret_name}" --description "Lantae API Keys" --secret-string '{
        "openai": "sk-your-openai-key",
        "anthropic": "sk-ant-your-anthropic-key", 
        "gemini": "your-gemini-api-key",
        "mistral": "your-mistral-api-key",
        "perplexity": "pplx-your-perplexity-key"
      }'
    CMD
    exit 1
  rescue Aws::Errors::MissingCredentialsError
    puts '❌ AWS credentials not found.'
    puts "\n🔧 Configure AWS credentials:"
    puts '   aws configure'
    puts '   # OR set environment variables:'
    puts '   export AWS_ACCESS_KEY_ID=your-key'
    puts '   export AWS_SECRET_ACCESS_KEY=your-secret'
    puts '   export AWS_REGION=us-east-1'
    exit 1
  rescue => e
    puts "❌ Error: #{e.message}"
    exit 1
  end
end

def create_template
  template_path = File.join(__dir__, 'secrets-template.json')
  template = {
    'openai' => 'sk-your-openai-api-key-here',
    'anthropic' => 'sk-ant-your-anthropic-api-key-here',
    'gemini' => 'your-google-gemini-api-key-here',
    'mistral' => 'your-mistral-api-key-here',
    'perplexity' => 'pplx-your-perplexity-api-key-here'
  }
  
  File.write(template_path, JSON.pretty_generate(template))
  puts '📋 Created secrets-template.json as reference'
end

if __FILE__ == $0
  fetch_and_create_secrets_file
  create_template
end