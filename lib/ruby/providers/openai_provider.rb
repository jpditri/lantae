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
        data['choices'][0]['message']['content']
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