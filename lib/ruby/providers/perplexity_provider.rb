require_relative 'base_provider'
require 'net/http'
require 'json'

module Lantae
  module Providers
    class PerplexityProvider < BaseProvider
      def initialize(secret_manager)
        super('perplexity', 'llama-3.1-sonar-large-128k-online')
        @secret_manager = secret_manager
      end

      def chat(model, messages, options = {})
        validate_chat_params(model, messages, options)
        
        api_key = get_api_key
        
        uri = URI('https://api.perplexity.ai/chat/completions')
        http = Net::HTTP.new(uri.host, uri.port)
        http.use_ssl = true

        request = build_request(uri, api_key, model, messages, options)
        response = http.request(request)
        
        handle_api_error(response, 'Perplexity') unless response.code.start_with?('2')

        data = JSON.parse(response.body)
        data['choices'][0]['message']['content']
      end

      def list_models
        %w[
          llama-3.1-sonar-large-128k-online llama-3.1-sonar-small-128k-online
          llama-3.1-sonar-large-128k-chat llama-3.1-sonar-small-128k-chat
          llama-3.1-8b-instruct llama-3.1-70b-instruct
        ]
      end

      def supports_streaming?
        true
      end

      def max_tokens
        case @default_model
        when /128k/
          4096
        else
          4096
        end
      end

      private

      def get_api_key
        api_key = @secret_manager.get_api_key('perplexity')
        raise 'Perplexity API key not found in environment or AWS Secrets Manager' unless api_key
        api_key
      end

      def build_request(uri, api_key, model, messages, options)
        request = Net::HTTP::Post.new(uri)
        request['Authorization'] = "Bearer #{api_key}"
        request['Content-Type'] = 'application/json'
        request.body = {
          model: model,
          messages: messages,
          temperature: (options[:temperature] || default_temperature).to_f,
          max_tokens: options[:max_tokens] || max_tokens
        }.to_json
        request
      end
    end
  end
end