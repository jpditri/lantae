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
        data['content'][0]['text']
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
        request.body = {
          model: model,
          max_tokens: options[:max_tokens] || max_tokens,
          temperature: (options[:temperature] || default_temperature).to_f,
          messages: messages
        }.to_json
        request
      end
    end
  end
end