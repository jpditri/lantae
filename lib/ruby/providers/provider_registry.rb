require_relative 'base_provider'
require_relative 'ollama_provider'
require_relative 'openai_provider'
require_relative 'anthropic_provider'
require_relative 'gemini_provider'
require_relative 'mistral_provider'
require_relative 'perplexity_provider'
require_relative 'bedrock_provider'
require_relative '../default_model_selector'

module Lantae
  module Providers
    class ProviderRegistry
      attr_reader :providers, :current_provider, :current_model

      def initialize(secret_manager, tool_manager = nil)
        @secret_manager = secret_manager
        @tool_manager = tool_manager
        @providers = {}
        @current_provider = 'ollama'
        # Use intelligent model selection for default
        @current_model = DefaultModelSelector.get_default_model
        
        register_default_providers
      end

      def register_provider(name, provider)
        @providers[name] = provider
        puts "Registered provider: #{name}" if ENV['DEBUG']
      end

      def get_provider(name = @current_provider)
        provider = @providers[name]
        raise "Provider '#{name}' not found. Available: #{available_providers.join(', ')}" unless provider
        provider
      end

      def switch_provider(provider_name, model = nil)
        raise "Provider '#{provider_name}' not supported. Available: #{available_providers.join(', ')}" unless @providers[provider_name]
        
        @current_provider = provider_name
        
        if model
          @current_model = model
        else
          # Set default model for provider
          @current_model = @providers[provider_name].default_model
        end
        
        { provider: @current_provider, model: @current_model }
      end

      def available_providers
        @providers.keys
      end

      def chat(messages, options = {})
        provider = get_provider(@current_provider)
        provider.chat(@current_model, messages, options)
      end

      def list_models(provider_name = @current_provider)
        provider = get_provider(provider_name)
        provider.list_models
      end

      def get_provider_info
        { provider: @current_provider, model: @current_model }
      end

      def health_check_all
        results = {}
        
        @providers.each do |name, provider|
          results[name] = provider.health_check
        end
        
        results
      end

      def get_provider_capabilities(provider_name = @current_provider)
        provider = get_provider(provider_name)

        {
          name: provider.name,
          supports_streaming: provider.supports_streaming?,
          supports_tools: provider.supports_tools?,
          max_tokens: provider.max_tokens,
          default_temperature: provider.default_temperature,
          models: provider.list_models
        }
      rescue => e
        {
          name: provider_name,
          error: e.message
        }
      end

      # Stream API for token-by-token output when supported.
      # Yields each token chunk for real-time feedback.
      def stream(messages, options = {}, &block)
        provider = get_provider(@current_provider)
        if provider.respond_to?(:stream) && provider.supports_streaming?
          provider.stream(@current_model, messages, options, &block)
        elsif block_given?
          block.call(provider.chat(@current_model, messages, options))
        end
      end

      private

      def register_default_providers
        # Register Ollama with tool manager
        ollama = OllamaProvider.new
        ollama.set_tool_manager(@tool_manager) if @tool_manager
        register_provider('ollama', ollama)
        
        # Register cloud providers
        register_provider('openai', OpenAIProvider.new(@secret_manager))
        register_provider('anthropic', AnthropicProvider.new(@secret_manager))
        register_provider('gemini', GeminiProvider.new(@secret_manager))
        register_provider('mistral', MistralProvider.new(@secret_manager))
        register_provider('perplexity', PerplexityProvider.new(@secret_manager))
        register_provider('bedrock', BedrockProvider.new)
      end
    end
  end
end