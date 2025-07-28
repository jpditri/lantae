require_relative 'base_provider'
require 'aws-sdk-bedrockruntime'
require 'json'

module Lantae
  module Providers
    class BedrockProvider < BaseProvider
      def initialize(region = 'us-east-1')
        super('bedrock', 'claude-3-sonnet')
        @region = region
        @client = nil
      end

      def chat(model, messages, options = {})
        validate_chat_params(model, messages, options)
        
        init_client
        
        bedrock_model_id = get_bedrock_model_id(model)
        body = build_request_body(bedrock_model_id, messages, options)

        response = @client.invoke_model({
          model_id: bedrock_model_id,
          body: body
        })

        parse_response(response, bedrock_model_id, messages, options)
      rescue Aws::Errors::MissingCredentialsError
        raise 'AWS credentials not found. Configure AWS CLI or environment variables.'
      end

      def list_models
        %w[
          claude-3-5-sonnet claude-3-5-haiku claude-3-sonnet claude-3-haiku claude-3-opus
          titan-text-g1-large titan-text-g1-express
        ]
      end

      def max_tokens
        case @default_model
        when /claude/
          4096
        when /titan/
          4096
        else
          4096
        end
      end

      private

      def init_client
        return if @client
        @client = Aws::BedrockRuntime::Client.new(region: @region)
      end

      def get_bedrock_model_id(model)
        model_map = {
          'claude-3-sonnet' => 'anthropic.claude-3-sonnet-20240229-v1:0',
          'claude-3-haiku' => 'anthropic.claude-3-haiku-20240307-v1:0',
          'claude-3-opus' => 'anthropic.claude-3-opus-20240229-v1:0',
          'claude-3-5-sonnet' => 'anthropic.claude-3-5-sonnet-20240620-v1:0',
          'claude-3-5-haiku' => 'anthropic.claude-3-5-haiku-20241022-v1:0',
          'titan-text-g1-large' => 'amazon.titan-text-lite-v1',
          'titan-text-g1-express' => 'amazon.titan-text-express-v1'
        }

        model_map[model] || model
      end

      def build_request_body(bedrock_model_id, messages, options)
        if bedrock_model_id.include?('anthropic.claude')
          body = {
            anthropic_version: 'bedrock-2023-05-31',
            max_tokens: options[:max_tokens] || max_tokens,
            temperature: (options[:temperature] || default_temperature).to_f,
            messages: messages
          }
          
          # Add tools if available (only for Claude models)
          if options[:tools] && !options[:tools].empty?
            body[:tools] = options[:tools]
            body[:tool_choice] = options[:tool_choice] if options[:tool_choice]
          end
          
          body.to_json
        elsif bedrock_model_id.include?('amazon.titan')
          prompt = messages.map { |m| "#{m[:role]}: #{m[:content]}" }.join("\n")
          {
            inputText: prompt,
            textGenerationConfig: {
              temperature: (options[:temperature] || default_temperature).to_f,
              maxTokenCount: options[:max_tokens] || max_tokens
            }
          }.to_json
        else
          raise "Unsupported model format: #{bedrock_model_id}"
        end
      end

      def parse_response(response, bedrock_model_id, messages, options)
        response_body = JSON.parse(response.body.read)

        if bedrock_model_id.include?('anthropic.claude')
          # Handle tool use responses for Claude models
          if response_body['content'].any? { |c| c['type'] == 'tool_use' }
            handle_tool_use(response_body, bedrock_model_id, messages, options)
          else
            response_body['content'][0]['text']
          end
        elsif bedrock_model_id.include?('amazon.titan')
          response_body['results'][0]['outputText']
        else
          raise "Unsupported model format: #{bedrock_model_id}"
        end
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