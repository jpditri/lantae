module Lantae
  module Providers
    # Base interface for all LLM providers
    class BaseProvider
      attr_reader :name, :default_model

      def initialize(name, default_model = nil)
        @name = name
        @default_model = default_model
      end

      # Abstract methods that must be implemented by subclasses
      def chat(model, messages, options = {})
        raise NotImplementedError, "#{self.class} must implement #chat"
      end

      def list_models
        raise NotImplementedError, "#{self.class} must implement #list_models"
      end

      # Common utility methods
      def supports_streaming?
        false
      end

      def supports_tools?
        false
      end

      def max_tokens
        4096
      end

      def default_temperature
        0.1
      end

      # Health check for provider availability
      def health_check
        begin
          list_models
          { status: :healthy, message: "Provider #{@name} is available" }
        rescue => e
          { status: :unhealthy, message: "Provider #{@name} error: #{e.message}" }
        end
      end

      protected

      # Common response processing
      def process_response(response, &block)
        if block_given?
          yield response
        else
          response
        end
      end

      # Common error handling
      def handle_api_error(response, provider_name)
        case response.code
        when '401'
          raise "Invalid #{provider_name} API key. Check your credentials."
        when '429'
          raise "Rate limit exceeded for #{provider_name}. Please wait and try again."
        when '500', '502', '503'
          raise "#{provider_name} service temporarily unavailable. Please try again later."
        else
          raise "#{provider_name} API error (#{response.code}): #{response.body}"
        end
      end

      # Validate common parameters
      def validate_chat_params(model, messages, options)
        raise ArgumentError, "Model cannot be nil" if model.nil?
        raise ArgumentError, "Messages must be an array" unless messages.is_a?(Array)
        raise ArgumentError, "Messages cannot be empty" if messages.empty?
        raise ArgumentError, "Temperature must be between 0 and 2" if options[:temperature] && (options[:temperature] < 0 || options[:temperature] > 2)
      end
    end
  end
end