require_relative 'base_provider'
require 'net/http'
require 'json'

module Lantae
  module Providers
    class MistralProvider < BaseProvider
      def initialize(secret_manager)
        super('mistral', 'mistral-large-latest')
        @secret_manager = secret_manager
      end

      def chat(model, messages, options = {})
        validate_chat_params(model, messages, options)
        
        api_key = get_api_key
        
        uri = URI('https://api.mistral.ai/v1/chat/completions')
        http = Net::HTTP.new(uri.host, uri.port)
        http.use_ssl = true

        request = build_request(uri, api_key, model, messages, options)
        response = http.request(request)
        
        handle_api_error(response, 'Mistral') unless response.code.start_with?('2')

        data = JSON.parse(response.body)
        choice = data['choices'][0]
        
        # Handle tool calls
        if choice['message']['tool_calls']
          handle_tool_calls(choice['message'], model, messages, options)
        else
          choice['message']['content']
        end
      end

      def list_models
        %w[
          mistral-large-latest mistral-medium-latest mistral-small-latest
          open-mistral-7b open-mixtral-8x7b open-mixtral-8x22b
        ]
      end

      def supports_streaming?
        true
      end

      private

      def get_api_key
        api_key = @secret_manager.get_api_key('mistral')
        raise 'Mistral API key not found in environment or AWS Secrets Manager' unless api_key
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