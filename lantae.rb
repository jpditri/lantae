#!/usr/bin/env ruby

require 'json'
require 'net/http'
require 'uri'
require 'optparse'
require 'readline'
require 'tempfile'
require 'aws-sdk-secretsmanager'
require 'aws-sdk-bedrockruntime'

VERSION = '1.0.0'

class SecretManager
  def initialize(region = 'us-east-1', secret_name = 'lantae/api-keys')
    @region = region
    @secret_name = secret_name
    @client = nil
    @cached_secrets = {}
  end

  def get_api_key(provider)
    # First check environment variable
    env_key = "#{provider.upcase}_API_KEY"
    return ENV[env_key] if ENV[env_key]

    # Check cached secrets
    if @cached_secrets[provider]
      ENV[env_key] = @cached_secrets[provider]
      return @cached_secrets[provider]
    end

    # Fetch from AWS Secrets Manager
    begin
      init_client
      response = @client.get_secret_value(secret_id: @secret_name)
      secrets = JSON.parse(response.secret_string)
      
      # Cache all secrets and set environment variables
      secrets.each do |key, value|
        @cached_secrets[key] = value
        ENV["#{key.upcase}_API_KEY"] = value
      end
      
      @cached_secrets[provider]
    rescue Aws::SecretsManager::Errors::ResourceNotFoundException
      raise "AWS Secret '#{@secret_name}' not found. Create it with your API keys."
    rescue => e
      raise "Failed to retrieve API keys from AWS Secrets Manager: #{e.message}"
    end
  end

  private

  def init_client
    return if @client
    @client = Aws::SecretsManager::Client.new(region: @region)
  end
end

class ProviderManager
  def initialize(secret_manager, tool_manager = nil)
    @secret_manager = secret_manager
    @tool_manager = tool_manager
    @providers = {
      'ollama' => OllamaProvider.new,
      'openai' => OpenAIProvider.new(secret_manager),
      'anthropic' => AnthropicProvider.new(secret_manager),
      'bedrock' => BedrockProvider.new,
      'gemini' => GeminiProvider.new(secret_manager),
      'mistral' => MistralProvider.new(secret_manager),
      'perplexity' => PerplexityProvider.new(secret_manager)
    }
    @current_provider = 'ollama'
    @current_model = 'cogito'
    
    # Set tool manager for Ollama provider
    @providers['ollama'].set_tool_manager(@tool_manager) if @tool_manager
  end

  attr_accessor :current_model, :current_provider

  def switch_provider(provider, model = nil)
    raise "Provider '#{provider}' not supported. Available: #{@providers.keys.join(', ')}" unless @providers[provider]
    
    @current_provider = provider
    if model
      @current_model = model
    else
      # Set default model for provider
      defaults = {
        'ollama' => 'cogito',
        'openai' => 'gpt-4o',
        'anthropic' => 'claude-3-5-sonnet-20241022',
        'bedrock' => 'claude-3-sonnet',
        'gemini' => 'gemini-1.5-pro',
        'mistral' => 'mistral-large-latest',
        'perplexity' => 'llama-3.1-sonar-large-128k-online'
      }
      @current_model = defaults[provider]
    end
  end

  def chat(messages, options = {})
    provider = @providers[@current_provider]
    provider.chat(@current_model, messages, options)
  end

  def list_models
    provider = @providers[@current_provider]
    provider.list_models
  end

  def get_provider_info
    { provider: @current_provider, model: @current_model }
  end
end

class OllamaProvider
  def initialize(base_url = 'http://localhost:11434')
    @base_url = base_url
    @tool_manager = nil
  end

  def set_tool_manager(tool_manager)
    @tool_manager = tool_manager
  end

  def chat(model, messages, options = {})
    uri = URI("#{@base_url}/api/chat")
    http = Net::HTTP.new(uri.host, uri.port)
    http.read_timeout = 300
    http.open_timeout = 30
    
    # Add tool context to the system message
    enhanced_messages = messages.dup
    if @tool_manager && !enhanced_messages.empty?
      tools_context = @tool_manager.get_tools_context
      system_message = {
        role: 'system',
        content: "You are an AI assistant with access to various tools for file operations and system commands. #{tools_context}\n\nWhen you want to use a tool, include a TOOL_CALL in your response. You can make multiple tool calls in a single response. After each tool call, I will provide the result, and you can continue your response or make additional tool calls as needed.\n\nAlways explain what you're doing and why before using tools. Be helpful and thorough in your responses."
      }
      
      # Insert system message at the beginning if it doesn't exist, or merge with existing system message
      if enhanced_messages[0] && enhanced_messages[0][:role] == 'system'
        enhanced_messages[0][:content] = system_message[:content] + "\n\n" + enhanced_messages[0][:content]
      else
        enhanced_messages.unshift(system_message)
      end
    end
    
    request = Net::HTTP::Post.new(uri)
    request['Content-Type'] = 'application/json'
    request.body = {
      model: model,
      messages: enhanced_messages,
      stream: false,
      options: {
        temperature: (options[:temperature] || 0.1).to_f
      }
    }.to_json

    begin
      # Start spinner in a separate thread if not showing status already
      spinner_thread = nil
      unless options[:no_spinner]
        spinner_thread = start_spinner
      end
      
      response = http.request(request)
      
      # Stop spinner
      if spinner_thread
        spinner_thread.kill
        print "\r" + " " * 20 + "\r"  # Clear spinner line
      end
      
      data = JSON.parse(response.body)
      content = data['message']['content']
      
      # Process tool calls in the response
      content = process_tool_calls(content) if @tool_manager
      
      content
    rescue Errno::ECONNREFUSED
      if spinner_thread
        spinner_thread.kill
        print "\r" + " " * 20 + "\r"
      end
      raise 'Cannot connect to Ollama server. Make sure Ollama is running.'
    end
  end

  def process_tool_calls(content)
    processed_content = content.dup
    
    content.scan(/TOOL_CALL:\s*([^\n]+)/) do |tool_call_match|
      tool_call = tool_call_match[0].strip
      tool_name, *args = tool_call.split(' ')
      
      begin
        puts "\n🔧 Executing tool: #{tool_name} #{args.join(' ')}"
        result = @tool_manager.execute_tool(tool_name, args.join(' '))
        
        # Replace the tool call with the result
        tool_call_line = "TOOL_CALL: #{tool_call}"
        replacement = "#{tool_call_line}\n\nTool Result:\n```\n#{result}\n```\n"
        processed_content = processed_content.gsub(tool_call_line, replacement)
        
        puts "✅ Tool result: #{result[0..100]}#{result.length > 100 ? '...' : ''}"
      rescue => error
        error_msg = "Error executing #{tool_name}: #{error.message}"
        puts "❌ #{error_msg}"
        
        tool_call_line = "TOOL_CALL: #{tool_call}"
        replacement = "#{tool_call_line}\n\nTool Error:\n```\n#{error_msg}\n```\n"
        processed_content = processed_content.gsub(tool_call_line, replacement)
      end
    end
    
    processed_content
  end

  def list_models
    uri = URI("#{@base_url}/api/tags")
    http = Net::HTTP.new(uri.host, uri.port)
    http.read_timeout = 30
    http.open_timeout = 10
    
    begin
      response = http.get(uri)
      data = JSON.parse(response.body)
      (data['models'] || []).map { |m| m['name'] }
    rescue Errno::ECONNREFUSED
      raise 'Cannot connect to Ollama server. Make sure Ollama is running.'
    end
  end

  private

  def start_spinner
    Thread.new do
      spinner_chars = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏']
      i = 0
      while true
        print "\r🤖 #{spinner_chars[i % spinner_chars.length]} Thinking..."
        sleep 0.1
        i += 1
      end
    end
  end
end

class OpenAIProvider
  def initialize(secret_manager)
    @secret_manager = secret_manager
  end

  def chat(model, messages, options = {})
    api_key = @secret_manager.get_api_key('openai')
    raise 'OpenAI API key not found in environment or AWS Secrets Manager' unless api_key

    uri = URI('https://api.openai.com/v1/chat/completions')
    http = Net::HTTP.new(uri.host, uri.port)
    http.use_ssl = true

    request = Net::HTTP::Post.new(uri)
    request['Authorization'] = "Bearer #{api_key}"
    request['Content-Type'] = 'application/json'
    request.body = {
      model: model,
      messages: messages,
      temperature: (options[:temperature] || 0.1).to_f,
      max_tokens: 4096
    }.to_json

    response = http.request(request)
    
    if response.code == '401'
      raise 'Invalid OpenAI API key. Check your credentials.'
    end

    data = JSON.parse(response.body)
    data['choices'][0]['message']['content']
  end

  def list_models
    %w[o1-preview o1-mini]
  end
end

class AnthropicProvider
  def initialize(secret_manager)
    @secret_manager = secret_manager
  end

  def chat(model, messages, options = {})
    api_key = @secret_manager.get_api_key('anthropic')
    raise 'Anthropic API key not found in environment or AWS Secrets Manager' unless api_key

    uri = URI('https://api.anthropic.com/v1/messages')
    http = Net::HTTP.new(uri.host, uri.port)
    http.use_ssl = true

    request = Net::HTTP::Post.new(uri)
    request['x-api-key'] = api_key
    request['anthropic-version'] = '2023-06-01'
    request['Content-Type'] = 'application/json'
    request.body = {
      model: model,
      max_tokens: 4096,
      temperature: (options[:temperature] || 0.1).to_f,
      messages: messages
    }.to_json

    response = http.request(request)
    
    if response.code == '401'
      raise 'Invalid Anthropic API key. Check your credentials.'
    end

    data = JSON.parse(response.body)
    data['content'][0]['text']
  end

  def list_models
    %w[claude-3-5-sonnet-20241022 claude-3-5-haiku-20241022 claude-3-opus-20240229 claude-3-sonnet-20240229 claude-3-haiku-20240307]
  end
end

class GeminiProvider
  def initialize(secret_manager)
    @secret_manager = secret_manager
  end

  def chat(model, messages, options = {})
    api_key = @secret_manager.get_api_key('gemini')
    raise 'Gemini API key not found in environment or AWS Secrets Manager' unless api_key

    # Convert messages to Gemini format
    contents = messages.map do |msg|
      {
        role: msg[:role] == 'assistant' ? 'model' : 'user',
        parts: [{ text: msg[:content] }]
      }
    end

    uri = URI("https://generativelanguage.googleapis.com/v1beta/models/#{model}:generateContent?key=#{api_key}")
    http = Net::HTTP.new(uri.host, uri.port)
    http.use_ssl = true

    request = Net::HTTP::Post.new(uri)
    request['Content-Type'] = 'application/json'
    request.body = {
      contents: contents,
      generationConfig: {
        temperature: (options[:temperature] || 0.1).to_f,
        maxOutputTokens: 4096
      }
    }.to_json

    response = http.request(request)
    
    if response.body.include?('API_KEY_INVALID')
      raise 'Invalid Gemini API key. Check your credentials.'
    end

    data = JSON.parse(response.body)
    data['candidates'][0]['content']['parts'][0]['text']
  end

  def list_models
    %w[gemini-1.5-pro gemini-1.5-flash gemini-1.0-pro]
  end
end

class MistralProvider
  def initialize(secret_manager)
    @secret_manager = secret_manager
  end

  def chat(model, messages, options = {})
    api_key = @secret_manager.get_api_key('mistral')
    raise 'Mistral API key not found in environment or AWS Secrets Manager' unless api_key

    uri = URI('https://api.mistral.ai/v1/chat/completions')
    http = Net::HTTP.new(uri.host, uri.port)
    http.use_ssl = true

    request = Net::HTTP::Post.new(uri)
    request['Authorization'] = "Bearer #{api_key}"
    request['Content-Type'] = 'application/json'
    request.body = {
      model: model,
      messages: messages,
      temperature: (options[:temperature] || 0.1).to_f,
      max_tokens: 4096
    }.to_json

    response = http.request(request)
    
    if response.code == '401'
      raise 'Invalid Mistral API key. Check your credentials.'
    end

    data = JSON.parse(response.body)
    data['choices'][0]['message']['content']
  end

  def list_models
    %w[mistral-large-latest mistral-medium-latest mistral-small-latest open-mistral-7b open-mixtral-8x7b open-mixtral-8x22b]
  end
end

class PerplexityProvider
  def initialize(secret_manager)
    @secret_manager = secret_manager
  end

  def chat(model, messages, options = {})
    api_key = @secret_manager.get_api_key('perplexity')
    raise 'Perplexity API key not found in environment or AWS Secrets Manager' unless api_key

    uri = URI('https://api.perplexity.ai/chat/completions')
    http = Net::HTTP.new(uri.host, uri.port)
    http.use_ssl = true

    request = Net::HTTP::Post.new(uri)
    request['Authorization'] = "Bearer #{api_key}"
    request['Content-Type'] = 'application/json'
    request.body = {
      model: model,
      messages: messages,
      temperature: (options[:temperature] || 0.1).to_f,
      max_tokens: 4096
    }.to_json

    response = http.request(request)
    
    if response.code == '401'
      raise 'Invalid Perplexity API key. Check your credentials.'
    end

    data = JSON.parse(response.body)
    data['choices'][0]['message']['content']
  end

  def list_models
    %w[llama-3.1-sonar-large-128k-online llama-3.1-sonar-small-128k-online llama-3.1-sonar-large-128k-chat llama-3.1-sonar-small-128k-chat llama-3.1-8b-instruct llama-3.1-70b-instruct]
  end
end

class BedrockProvider
  def initialize(region = 'us-east-1')
    @region = region
    @client = nil
  end

  def chat(model, messages, options = {})
    init_client

    model_map = {
      'claude-3-sonnet' => 'anthropic.claude-3-sonnet-20240229-v1:0',
      'claude-3-haiku' => 'anthropic.claude-3-haiku-20240307-v1:0',
      'claude-3-opus' => 'anthropic.claude-3-opus-20240229-v1:0',
      'claude-3-5-sonnet' => 'anthropic.claude-3-5-sonnet-20240620-v1:0',
      'claude-3-5-haiku' => 'anthropic.claude-3-5-haiku-20241022-v1:0',
      'titan-text-g1-large' => 'amazon.titan-text-lite-v1',
      'titan-text-g1-express' => 'amazon.titan-text-express-v1'
    }

    bedrock_model_id = model_map[model] || model

    if bedrock_model_id.include?('anthropic.claude')
      body = {
        anthropic_version: 'bedrock-2023-05-31',
        max_tokens: 4096,
        temperature: (options[:temperature] || 0.1).to_f,
        messages: messages
      }.to_json
    elsif bedrock_model_id.include?('amazon.titan')
      prompt = messages.map { |m| "#{m[:role]}: #{m[:content]}" }.join("\n")
      body = {
        inputText: prompt,
        textGenerationConfig: {
          temperature: (options[:temperature] || 0.1).to_f,
          maxTokenCount: 4096
        }
      }.to_json
    else
      raise "Unsupported model format: #{bedrock_model_id}"
    end

    response = @client.invoke_model({
      model_id: bedrock_model_id,
      body: body
    })

    response_body = JSON.parse(response.body.read)

    if bedrock_model_id.include?('anthropic.claude')
      response_body['content'][0]['text']
    elsif bedrock_model_id.include?('amazon.titan')
      response_body['results'][0]['outputText']
    end
  rescue Aws::Errors::MissingCredentialsError
    raise 'AWS credentials not found. Configure AWS CLI or environment variables.'
  end

  def list_models
    %w[claude-3-5-sonnet claude-3-5-haiku claude-3-sonnet claude-3-haiku claude-3-opus titan-text-g1-large titan-text-g1-express]
  end

  private

  def init_client
    return if @client
    @client = Aws::BedrockRuntime::Client.new(region: @region)
  end
end

class ToolManager
  def execute_tool(tool_name, args)
    case tool_name
    when 'bash'
      execute_bash(args)
    when 'ruby'
      execute_ruby(args)
    when 'python'
      execute_python(args)
    when 'ls'
      list_files(args.empty? ? '.' : args)
    when 'cat'
      read_file(args)
    when 'pwd'
      Dir.pwd
    when 'git'
      execute_bash("git #{args}")
    when 'bundle'
      execute_bash("bundle #{args}")
    when 'write_file'
      write_file(args)
    when 'edit_file'
      edit_file(args)
    when 'create_file'
      create_file(args)
    when 'delete_file'
      delete_file(args)
    when 'mkdir'
      make_directory(args)
    when 'find'
      find_files(args)
    else
      raise "Tool '#{tool_name}' not found. Available tools: #{list_available_tools.join(', ')}"
    end
  end

  def get_tools_context
    <<~CONTEXT
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
      - bundle <command>: Execute bundle commands
      - ruby <code>: Execute Ruby code
      - python <code>: Execute Python code

      To use a tool, format your response like this:
      TOOL_CALL: tool_name arguments
      For example:
      TOOL_CALL: cat Gemfile
      TOOL_CALL: write_file hello.txt Hello World!
      TOOL_CALL: edit_file main.rb 5 puts "Updated line"
    CONTEXT
  end

  def list_available_tools
    %w[bash ruby python ls cat pwd git bundle write_file edit_file create_file delete_file mkdir find]
  end

  private

  def execute_bash(command)
    result = `#{command} 2>&1`
    $?.success? ? result : "Error: #{result}"
  rescue => e
    "Error: #{e.message}"
  end

  def execute_ruby(code)
    eval(code).to_s
  rescue => e
    "Error: #{e.message}"
  end

  def execute_python(code)
    temp_file = Tempfile.new(['lantae', '.py'])
    temp_file.write(code)
    temp_file.close
    
    result = `python3 #{temp_file.path} 2>&1`
    temp_file.unlink
    result
  rescue => e
    "Error: #{e.message}"
  end

  def list_files(dir)
    Dir.entries(dir).join("\n")
  rescue => e
    "Error: #{e.message}"
  end

  def read_file(file_path)
    File.read(file_path)
  rescue => e
    "Error: #{e.message}"
  end

  def write_file(args)
    parts = args.split(' ', 2)
    file_path = parts[0]
    content = parts[1] || ''
    
    File.write(file_path, content)
    "File #{file_path} written successfully"
  rescue => e
    "Error: #{e.message}"
  end

  def edit_file(args)
    parts = args.split(' ', 3)
    file_path = parts[0]
    line_number = parts[1].to_i
    new_content = parts[2] || ''
    
    lines = File.readlines(file_path)
    
    if line_number > 0 && line_number <= lines.length
      lines[line_number - 1] = new_content + "\n"
      File.write(file_path, lines.join)
      "Line #{line_number} in #{file_path} edited successfully"
    else
      "Error: Line number #{line_number} out of range"
    end
  rescue => e
    "Error: #{e.message}"
  end

  def create_file(args)
    parts = args.split(' ', 2)
    file_path = parts[0]
    content = parts[1] || ''
    
    if File.exist?(file_path)
      "Error: File #{file_path} already exists"
    else
      File.write(file_path, content)
      "File #{file_path} created successfully"
    end
  rescue => e
    "Error: #{e.message}"
  end

  def delete_file(file_path)
    File.delete(file_path)
    "File #{file_path} deleted successfully"
  rescue => e
    "Error: #{e.message}"
  end

  def make_directory(dir_path)
    require 'fileutils'
    FileUtils.mkdir_p(dir_path)
    "Directory #{dir_path} created successfully"
  rescue => e
    "Error: #{e.message}"
  end

  def find_files(pattern)
    result = `find . -name "#{pattern}" 2>&1`
    result.empty? ? 'No files found' : result
  rescue => e
    "Error: #{e.message}"
  end
end

def send_single_prompt(prompt, options)
  secret_manager = SecretManager.new(options[:region], options[:secret])
  tool_manager = ToolManager.new
  provider_manager = ProviderManager.new(secret_manager, tool_manager)
  
  if options[:provider] != 'ollama'
    provider_manager.switch_provider(options[:provider], options[:model])
  else
    provider_manager.current_model = options[:model]
  end
  
  response = provider_manager.chat([{ role: 'user', content: prompt }], options)
  puts "#{response}"
rescue => e
  puts "Error: #{e.message}"
  exit 1
end

def start_repl(options)
  secret_manager = SecretManager.new(options[:region], options[:secret])
  tool_manager = ToolManager.new
  provider_manager = ProviderManager.new(secret_manager, tool_manager)
  conversation = []
  
  if options[:provider] != 'ollama'
    provider_manager.switch_provider(options[:provider], options[:model])
  else
    provider_manager.current_model = options[:model]
  end
  
  puts "🔮 Lantae v#{VERSION}"
  info = provider_manager.get_provider_info
  puts "Provider: #{info[:provider]} | Model: #{info[:model]}"
  puts 'Type "/help" for commands, "exit" or "quit" to end'
  puts

  begin
    models = provider_manager.list_models
    if models.empty?
      puts '⚠️  No models found.'
    elsif !models.include?(provider_manager.current_model)
      puts "⚠️  Model \"#{provider_manager.current_model}\" not found. Available models:"
      models.first(10).each { |m| puts "  - #{m}" }
      puts "  ... and #{models.length - 10} more" if models.length > 10
      puts
    end
  rescue => e
    puts "Error checking models: #{e.message}"
  end

  loop do
    begin
      input = Readline.readline('> ', true)
      break if input.nil? || input.strip == 'exit' || input.strip == 'quit'
      
      input = input.strip
      next if input.empty?
      
      if input.start_with?('/')
        handle_slash_command(input, provider_manager, tool_manager, conversation)
        next
      end
      
      conversation << { role: 'user', content: input }
      
      response = provider_manager.chat(conversation, options)
      conversation << { role: 'assistant', content: response }
      puts "#{response}\n\n"
      
    rescue Interrupt
      puts "\nGoodbye!"
      break
    rescue => e
      puts "Error: #{e.message}"
    end
  end
end

def handle_slash_command(input, provider_manager, tool_manager, conversation)
  parts = input[1..-1].split(' ')
  command = parts[0]
  args = parts[1..-1].join(' ')

  case command
  when 'help'
    puts <<~HELP
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
    HELP

  when 'model'
    if args.empty?
      puts 'Usage: /model <model-name>'
    else
      provider_manager.current_model = args
      puts "Switched to model: #{args}"
    end

  when 'provider'
    if args.empty?
      puts 'Usage: /provider <provider-name> [model]'
    else
      provider, model = args.split(' ', 2)
      provider_manager.switch_provider(provider, model)
      info = provider_manager.get_provider_info
      puts "Switched to provider: #{info[:provider]}, model: #{info[:model]}"
    end

  when 'models'
    models = provider_manager.list_models
    puts 'Available models:'
    models.each { |m| puts "  - #{m}" }

  when 'tool'
    if args.empty?
      puts 'Usage: /tool <tool-name> <arguments>'
    else
      tool_name, *tool_args = args.split(' ')
      result = tool_manager.execute_tool(tool_name, tool_args.join(' '))
      puts result
    end

  when 'tools'
    tools = tool_manager.list_available_tools
    puts 'Available tools:'
    tools.each { |t| puts "  - #{t}" }

  when 'clear'
    conversation.clear
    puts 'Conversation cleared.'

  when 'info'
    info = provider_manager.get_provider_info
    puts "Provider: #{info[:provider]}"
    puts "Model: #{info[:model]}"

  when 'env'
    puts 'Environment Variables Status:'
    %w[openai anthropic gemini mistral perplexity].each do |provider|
      key = "#{provider.upcase}_API_KEY"
      puts "#{key}: #{ENV[key] ? '✓ Set' : '✗ Not set'}"
    end
    puts "AWS_PROFILE: #{ENV['AWS_PROFILE'] || 'default'}"
    puts "AWS_REGION: #{ENV['AWS_REGION'] || 'not set'}"

  else
    puts "Unknown command: /#{command}. Type /help for available commands."
  end
rescue => e
  puts "Error: #{e.message}"
end

def main
  options = {
    model: 'cogito',
    provider: 'ollama',
    url: 'http://localhost:11434',
    region: 'us-east-1',
    secret: 'lantae/api-keys',
    temperature: 0.1
  }

  OptionParser.new do |opts|
    opts.banner = "Usage: #{$0} [options] [prompt]"
    
    opts.on('-m', '--model MODEL', 'Specify the model to use') { |v| options[:model] = v }
    opts.on('-p', '--provider PROVIDER', 'Specify the provider') { |v| options[:provider] = v }
    opts.on('-u', '--url URL', 'Ollama server URL') { |v| options[:url] = v }
    opts.on('-r', '--region REGION', 'AWS region') { |v| options[:region] = v }
    opts.on('-s', '--secret SECRET', 'AWS Secrets Manager secret name') { |v| options[:secret] = v }
    opts.on('-t', '--temperature TEMP', 'Temperature for responses') { |v| options[:temperature] = v.to_f }
    opts.on('-v', '--version', 'Show version') { puts VERSION; exit }
    opts.on('-h', '--help', 'Show this help') { puts opts; exit }
  end.parse!

  if ARGV.empty?
    start_repl(options)
  else
    send_single_prompt(ARGV.join(' '), options)
  end
end

if __FILE__ == $0
  main
end