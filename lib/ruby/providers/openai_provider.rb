require_relative 'base_provider'
require 'net/http'
require 'json'

module Lantae
  module Providers
    class OpenAIProvider < BaseProvider
      def initialize(secret_manager)
        super('openai', 'gpt-4o')
        @secret_manager = secret_manager
      end

      def chat(model, messages, options = {})
        validate_chat_params(model, messages, options)
        
        api_key = get_api_key
        
        uri = URI('https://api.openai.com/v1/chat/completions')
        http = Net::HTTP.new(uri.host, uri.port)
        http.use_ssl = true

        request = build_request(uri, api_key, model, messages, options)
        response = http.request(request)
        
        handle_api_error(response, 'OpenAI') unless response.code.start_with?('2')

        data = JSON.parse(response.body)
        choice = data['choices'][0]
        
        # Handle function/tool calls
        if choice['message']['tool_calls']
          handle_tool_calls(choice['message'], model, messages, options)
        else
          choice['message']['content']
        end
      end

      def list_models
        %w[
          gpt-4o gpt-4o-mini gpt-4-turbo gpt-4
          o1-preview o1-mini
          gpt-3.5-turbo gpt-3.5-turbo-16k
        ]
      end

      def supports_streaming?
        true
      end

      def max_tokens
        case @default_model
        when /o1/
          32768
        when /gpt-4/
          8192
        else
          4096
        end
      end

      private

      def get_api_key
        api_key = @secret_manager.get_api_key('openai')
        raise 'OpenAI API key not found in environment or AWS Secrets Manager' unless api_key
        api_key
      end

      def build_request(uri, api_key, model, messages, options)
        request = Net::HTTP::Post.new(uri)
        request['Authorization'] = "Bearer #{api_key}"
        request['Content-Type'] = 'application/json'
        
        body = {
          model: model,
          messages: messages,
          temperature: (options[:temperature] || default_temperature).to_f,
          max_tokens: options[:max_tokens] || max_tokens
        }
        
        # Add tools if available
        if options[:tools] && !options[:tools].empty?
          body[:tools] = options[:tools]
          body[:tool_choice] = options[:tool_choice] if options[:tool_choice]
        end
        
        request.body = body.to_json
        request
      end
      
      def handle_tool_calls(assistant_message, model, messages, options)
        tool_results = []
        
        assistant_message['tool_calls'].each do |tool_call|
          tool_name = tool_call['function']['name']
          tool_args = JSON.parse(tool_call['function']['arguments'])
          tool_id = tool_call['id']
          
          # Execute tool if tool_manager is available
          if @tool_manager && @tool_manager.has_tool?(tool_name)
            result = @tool_manager.execute_tool(tool_name, tool_args)
            tool_results << {
              tool_call_id: tool_id,
              role: 'tool',
              name: tool_name,
              content: result[:success] ? result[:result].to_s : "Error: #{result[:error]}"
            }
          else
            tool_results << {
              tool_call_id: tool_id,
              role: 'tool',
              name: tool_name,
              content: "Tool '#{tool_name}' not available"
            }
          end
        end
        
        # If we have tool results, make another API call with them
        if tool_results.any?
          # Add assistant's message and tool results to conversation
          new_messages = messages + [assistant_message] + tool_results
          
          # Make follow-up request
          chat(model, new_messages, options)
        else
          # Return content if available
          assistant_message['content'] || ''
        end
      end
    end
  end
end