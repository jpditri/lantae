require_relative 'base_provider'
require_relative '../default_model_selector'
require 'net/http'
require 'json'

module Lantae
  module Providers
    class OllamaProvider < BaseProvider
      def initialize(base_url = 'http://localhost:11434')
        # Use intelligent model selection instead of hardcoded default
        default_model = DefaultModelSelector.get_default_model(base_url)
        super('ollama', default_model)
        @base_url = base_url
        @tool_manager = nil
      end

      def set_tool_manager(tool_manager)
        @tool_manager = tool_manager
      end

      def supports_tools?
        !@tool_manager.nil?
      end

      def chat(model, messages, options = {})
        validate_chat_params(model, messages, options)

        uri = URI("#{@base_url}/api/chat")
        http = Net::HTTP.new(uri.host, uri.port)
        http.read_timeout = 300
        http.open_timeout = 30
        
        # Add tool context to the system message
        enhanced_messages = enhance_messages_with_tools(messages)
        
        request = Net::HTTP::Post.new(uri)
        request['Content-Type'] = 'application/json'
        request.body = build_request_body(model, enhanced_messages, options)

        begin
          response = send_request_with_spinner(http, request, options)
          
          # Handle model not found errors
          if response.code == '404'
            handle_model_not_found_error(model)
          end
          
          handle_api_error(response, 'Ollama') unless response.code.start_with?('2')
          
          data = JSON.parse(response.body)
          content = data['message']['content']
          
          # Process tool calls in the response
          content = process_tool_calls(content) if @tool_manager
          
          content
        rescue Errno::ECONNREFUSED
          raise 'Cannot connect to Ollama server. Make sure Ollama is running.'
        end
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

      def max_tokens
        8192 # Ollama supports larger contexts
      end
      
      def context_window
        case @default_model
        when /cogito/
          32768
        when /qwen/
          32768
        when /llama3/
          8192
        when /mistral/
          32768
        when /gemma/
          8192
        else
          8192
        end
      end

      private
      
      def handle_model_not_found_error(model)
        # Get model validation results with suggestions
        validation = DefaultModelSelector.validate_model(model, @base_url)
        
        if validation[:suggested]
          error_msg = "Model '#{model}' not found. Suggested alternative: #{validation[:suggested]}"
          
          if validation[:available] && validation[:available].any?
            error_msg += "\n\nAvailable models: #{validation[:available].join(', ')}"
          end
          
          error_msg += "\n\nTo pull a recommended model, run: ollama pull #{validation[:suggested]}"
        else
          error_msg = "Model '#{model}' not found and no models are available. Please install Ollama models first."
          error_msg += "\n\nQuick start: ollama pull llama3.2:1b"
        end
        
        raise error_msg
      end

      def enhance_messages_with_tools(messages)
        enhanced_messages = messages.dup
        
        if @tool_manager && !enhanced_messages.empty?
          tools_context = @tool_manager.get_tools_context
          system_message = {
            role: 'system',
            content: build_system_message(tools_context)
          }
          
          # Insert or merge with existing system message
          if enhanced_messages[0] && enhanced_messages[0][:role] == 'system'
            enhanced_messages[0][:content] = system_message[:content] + "\n\n" + enhanced_messages[0][:content]
          else
            enhanced_messages.unshift(system_message)
          end
        end
        
        enhanced_messages
      end

      def build_system_message(tools_context)
        <<~SYSTEM
          You are an AI assistant with access to various tools for file operations and system commands. #{tools_context}

          When you want to use a tool, include a TOOL_CALL in your response. You can make multiple tool calls in a single response. After each tool call, I will provide the result, and you can continue your response or make additional tool calls as needed.

          Always explain what you're doing and why before using tools. Be helpful and thorough in your responses.
        SYSTEM
      end

      def build_request_body(model, messages, options)
        {
          model: model,
          messages: messages,
          stream: false,
          options: {
            temperature: (options[:temperature] || default_temperature).to_f
          }
        }.to_json
      end

      def send_request_with_spinner(http, request, options)
        spinner_thread = nil
        
        unless options[:no_spinner]
          spinner_thread = start_spinner
        end
        
        response = http.request(request)
        
        if spinner_thread
          spinner_thread.kill
          print "\r" + " " * 20 + "\r"  # Clear spinner line
        end
        
        response
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
  end
end