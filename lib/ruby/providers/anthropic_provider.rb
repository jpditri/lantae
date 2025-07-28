require_relative 'base_provider'
require 'net/http'
require 'json'

module Lantae
  module Providers
    class AnthropicProvider < BaseProvider
      def initialize(secret_manager)
        super('anthropic', 'claude-3-5-sonnet-20241022')
        @secret_manager = secret_manager
      end

      def chat(model, messages, options = {})
        validate_chat_params(model, messages, options)
        
        api_key = get_api_key
        
        uri = URI('https://api.anthropic.com/v1/messages')
        http = Net::HTTP.new(uri.host, uri.port)
        http.use_ssl = true

        request = build_request(uri, api_key, model, messages, options)
        response = http.request(request)
        
        handle_api_error(response, 'Anthropic') unless response.code.start_with?('2')

        data = JSON.parse(response.body)
        
        # Handle tool use responses
        if data['content'].any? { |c| c['type'] == 'tool_use' }
          handle_tool_use(data, model, messages, options)
        else
          data['content'][0]['text']
        end
      end

      def list_models
        %w[
          claude-3-5-sonnet-20241022 claude-3-5-haiku-20241022
          claude-3-opus-20240229 claude-3-sonnet-20240229 claude-3-haiku-20240307
        ]
      end

      def supports_streaming?
        true
      end

      def max_tokens
        case @default_model
        when /opus/
          4096
        when /sonnet/
          4096
        when /haiku/
          4096
        else
          4096
        end
      end

      private

      def get_api_key
        api_key = @secret_manager.get_api_key('anthropic')
        raise 'Anthropic API key not found in environment or AWS Secrets Manager' unless api_key
        api_key
      end

      def build_request(uri, api_key, model, messages, options)
        request = Net::HTTP::Post.new(uri)
        request['x-api-key'] = api_key
        request['anthropic-version'] = '2023-06-01'
        request['Content-Type'] = 'application/json'
        
        body = {
          model: model,
          max_tokens: options[:max_tokens] || max_tokens,
          temperature: (options[:temperature] || default_temperature).to_f,
          messages: messages
        }
        
        # Add tools if available
        if options[:tools] && !options[:tools].empty?
          body[:tools] = options[:tools]
          body[:tool_choice] = options[:tool_choice] if options[:tool_choice]
        end
        
        request.body = body.to_json
        request
      end
      
      def handle_tool_use(response_data, model, messages, options)
        tool_results = []
        
        response_data['content'].each do |content|
          next unless content['type'] == 'tool_use'
          
          tool_name = content['name']
          tool_input = content['input']
          tool_use_id = content['id']
          
          # Execute tool if tool_manager is available
          if @tool_manager && @tool_manager.has_tool?(tool_name)
            result = @tool_manager.execute_tool(tool_name, tool_input)
            tool_results << {
              type: 'tool_result',
              tool_use_id: tool_use_id,
              content: result[:success] ? result[:result].to_s : "Error: #{result[:error]}"
            }
          else
            tool_results << {
              type: 'tool_result',
              tool_use_id: tool_use_id,
              content: "Tool '#{tool_name}' not available"
            }
          end
        end
        
        # If we have tool results, make another API call with them
        if tool_results.any?
          # Add assistant's tool use message and tool results to conversation
          new_messages = messages + [
            { role: 'assistant', content: response_data['content'] },
            { role: 'user', content: tool_results }
          ]
          
          # Make follow-up request
          chat(model, new_messages, options)
        else
          # Return any text content if no tools were executed
          text_content = response_data['content'].find { |c| c['type'] == 'text' }
          text_content ? text_content['text'] : ''
        end
      end
    end
  end
end