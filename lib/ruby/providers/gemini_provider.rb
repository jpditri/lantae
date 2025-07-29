require_relative 'base_provider'
require 'net/http'
require 'json'

module Lantae
  module Providers
    class GeminiProvider < BaseProvider
      def initialize(secret_manager)
        super('gemini', 'gemini-1.5-pro')
        @secret_manager = secret_manager
      end

      def chat(model, messages, options = {})
        validate_chat_params(model, messages, options)
        
        api_key = get_api_key
        
        # Convert messages to Gemini format
        contents = convert_messages_to_gemini_format(messages)

        uri = URI("https://generativelanguage.googleapis.com/v1beta/models/#{model}:generateContent?key=#{api_key}")
        http = Net::HTTP.new(uri.host, uri.port)
        http.use_ssl = true

        request = build_request(uri, contents, options)
        response = http.request(request)
        
        if response.body.include?('API_KEY_INVALID')
          raise 'Invalid Gemini API key. Check your credentials.'
        end

        data = JSON.parse(response.body)
        candidate = data['candidates'][0]
        
        # Check for function calls
        if candidate['content']['parts'].any? { |part| part['functionCall'] }
          handle_function_calls(candidate['content'], model, messages, options)
        else
          candidate['content']['parts'][0]['text']
        end
      end

      def list_models
        %w[gemini-1.5-pro gemini-1.5-flash gemini-1.0-pro]
      end

      def max_tokens
        case @default_model
        when /1.5/
          8192
        else
          4096
        end
      end
      
      def context_window
        case @default_model
        when /1.5-pro/
          1000000  # 1M tokens
        when /1.5-flash/
          1000000  # 1M tokens
        when /1.0-pro/
          32768
        else
          32768
        end
      end

      private

      def get_api_key
        api_key = @secret_manager.get_api_key('gemini')
        raise 'Gemini API key not found in environment or AWS Secrets Manager' unless api_key
        api_key
      end

      def convert_messages_to_gemini_format(messages)
        messages.map do |msg|
          {
            role: msg[:role] == 'assistant' ? 'model' : 'user',
            parts: [{ text: msg[:content] }]
          }
        end
      end

      def build_request(uri, contents, options)
        request = Net::HTTP::Post.new(uri)
        request['Content-Type'] = 'application/json'
        
        body = {
          contents: contents,
          generationConfig: {
            temperature: (options[:temperature] || default_temperature).to_f,
            maxOutputTokens: options[:max_tokens] || max_tokens
          }
        }
        
        # Add tools if available (Gemini uses "tools" with "functionDeclarations")
        if options[:tools] && !options[:tools].empty?
          body[:tools] = [{
            functionDeclarations: options[:tools].map { |tool| convert_tool_to_gemini_format(tool) }
          }]
        end
        
        request.body = body.to_json
        request
      end
      
      def convert_tool_to_gemini_format(tool)
        {
          name: tool[:name],
          description: tool[:description],
          parameters: tool[:parameters] || {}
        }
      end
      
      def handle_function_calls(content, model, messages, options)
        function_responses = []
        
        content['parts'].each do |part|
          next unless part['functionCall']
          
          function_name = part['functionCall']['name']
          function_args = part['functionCall']['args'] || {}
          
          # Execute tool if tool_manager is available
          if @tool_manager && @tool_manager.has_tool?(function_name)
            result = @tool_manager.execute_tool(function_name, function_args)
            function_responses << {
              functionResponse: {
                name: function_name,
                response: result[:success] ? { result: result[:result] } : { error: result[:error] }
              }
            }
          else
            function_responses << {
              functionResponse: {
                name: function_name,
                response: { error: "Function '#{function_name}' not available" }
              }
            }
          end
        end
        
        # If we have function responses, make another API call with them
        if function_responses.any?
          # Add model's message with function calls and user's function responses
          new_messages = messages + [
            { role: 'assistant', content: content['parts'].map { |p| p.to_json }.join(', ') },
            { role: 'user', content: function_responses.map { |r| r.to_json }.join(', ') }
          ]
          
          # Make follow-up request
          chat(model, new_messages, options)
        else
          # Return text content if available
          text_part = content['parts'].find { |p| p['text'] }
          text_part ? text_part['text'] : ''
        end
      end
    end
  end
end