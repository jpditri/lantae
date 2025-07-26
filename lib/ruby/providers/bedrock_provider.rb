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

        parse_response(response, bedrock_model_id)
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
          {
            anthropic_version: 'bedrock-2023-05-31',
            max_tokens: options[:max_tokens] || max_tokens,
            temperature: (options[:temperature] || default_temperature).to_f,
            messages: messages
          }.to_json
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

      def parse_response(response, bedrock_model_id)
        response_body = JSON.parse(response.body.read)

        if bedrock_model_id.include?('anthropic.claude')
          response_body['content'][0]['text']
        elsif bedrock_model_id.include?('amazon.titan')
          response_body['results'][0]['outputText']
        else
          raise "Unsupported model format: #{bedrock_model_id}"
        end
      end
    end
  end
end