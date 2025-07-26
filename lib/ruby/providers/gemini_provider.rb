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
        data['candidates'][0]['content']['parts'][0]['text']
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
        request.body = {
          contents: contents,
          generationConfig: {
            temperature: (options[:temperature] || default_temperature).to_f,
            maxOutputTokens: options[:max_tokens] || max_tokens
          }
        }.to_json
        request
      end
    end
  end
end