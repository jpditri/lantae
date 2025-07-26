module Lantae
  module Providers
    # Manages a chain of providers with fallback and escalation logic
    class ProviderChain
      attr_reader :providers, :abort_strategy

      def initialize(abort_strategy = nil)
        @providers = []
        @abort_strategy = abort_strategy || MissionAbort.create_strategy
        @current_provider_index = 0
        @chain_metrics = {
          total_requests: 0,
          provider_switches: 0,
          chain_exhausted: 0,
          successful_escalations: 0
        }
      end

      # Add provider to chain with configuration
      def add_provider(provider, config = {})
        abort_aware = provider.is_a?(AbortAwareProvider) ? provider : 
                      AbortAwareProvider.new(provider, @abort_strategy)
        
        @providers << {
          provider: abort_aware,
          priority: config[:priority] || 0,
          conditions: config[:conditions] || {},
          capabilities: config[:capabilities] || detect_capabilities(provider),
          cost_tier: config[:cost_tier] || :standard,
          max_retries: config[:max_retries] || 3,
          retry_count: 0
        }
        
        # Sort by priority (higher first)
        @providers.sort_by! { |p| -p[:priority] }
        
        # Add to abort strategy escalation chain
        @abort_strategy.add_escalation_provider(
          abort_aware,
          priority: config[:priority] || 0,
          conditions: config[:conditions] || {}
        )
      end

      # Execute chat with automatic escalation
      def chat(messages, options = {})
        @chain_metrics[:total_requests] += 1
        
        # Start with primary provider or best match
        provider_config = select_best_provider(messages, options)
        attempted_providers = []
        last_error = nil

        while provider_config
          provider = provider_config[:provider]
          attempted_providers << provider.name

          begin
            # Attempt chat with current provider
            result = provider.chat(
              options[:model] || provider.default_model,
              messages,
              options.merge(
                escalation_handler: lambda do |error, original_messages|
                  handle_escalation(error, original_messages, attempted_providers, options)
                end
              )
            )

            # Success - reset retry count and return
            provider_config[:retry_count] = 0
            return result

          rescue MissionAbort::AbortError => e
            last_error = e
            @chain_metrics[:provider_switches] += 1
            
            # Try next provider in chain
            provider_config = select_next_provider(attempted_providers, e)
            
          rescue => e
            # Handle other errors
            last_error = e
            provider_config[:retry_count] += 1
            
            if provider_config[:retry_count] < provider_config[:max_retries]
              # Retry with same provider
              sleep(calculate_backoff(provider_config[:retry_count]))
            else
              # Move to next provider
              @chain_metrics[:provider_switches] += 1
              provider_config = select_next_provider(attempted_providers, e)
            end
          end
        end

        # All providers exhausted
        @chain_metrics[:chain_exhausted] += 1
        handle_chain_exhaustion(last_error, messages, attempted_providers)
      end

      # Get provider by name
      def get_provider(name)
        config = @providers.find { |p| p[:provider].name == name }
        config[:provider] if config
      end

      # List all providers in chain
      def list_providers
        @providers.map { |p| p[:provider].name }
      end

      # Get chain metrics
      def metrics
        provider_metrics = @providers.map do |config|
          {
            name: config[:provider].name,
            priority: config[:priority],
            retry_count: config[:retry_count],
            metrics: config[:provider].metrics
          }
        end

        {
          chain_metrics: @chain_metrics,
          providers: provider_metrics,
          escalation_success_rate: @chain_metrics[:provider_switches] > 0 ?
            @chain_metrics[:successful_escalations].to_f / @chain_metrics[:provider_switches] : 0
        }
      end

      private

      def detect_capabilities(provider)
        capabilities = []
        
        # Basic capability detection
        capabilities << :streaming if provider.supports_streaming?
        capabilities << :tools if provider.supports_tools?
        
        # Model-based capabilities
        if provider.name.include?('ollama')
          capabilities << :local
          capabilities << :fast
        elsif provider.name.include?('openai')
          capabilities << :cloud
          capabilities << :advanced_reasoning
        elsif provider.name.include?('anthropic')
          capabilities << :cloud
          capabilities << :long_context
        end
        
        capabilities
      end

      def select_best_provider(messages, options)
        # Select provider based on request characteristics
        if options[:require_local]
          @providers.find { |p| p[:capabilities].include?(:local) }
        elsif options[:require_streaming]
          @providers.find { |p| p[:capabilities].include?(:streaming) }
        elsif options[:require_tools]
          @providers.find { |p| p[:capabilities].include?(:tools) }
        elsif requires_advanced_reasoning?(messages)
          @providers.find { |p| p[:capabilities].include?(:advanced_reasoning) }
        else
          # Default to highest priority
          @providers.first
        end
      end

      def select_next_provider(attempted_providers, error)
        # Find next provider that hasn't been attempted
        @providers.find do |config|
          !attempted_providers.include?(config[:provider].name) &&
          provider_can_handle_error?(config, error)
        end
      end

      def provider_can_handle_error?(provider_config, error)
        return true if provider_config[:conditions].empty?
        
        # Check if provider is suitable for error type
        conditions = provider_config[:conditions]
        
        if conditions[:handles_errors]
          conditions[:handles_errors].include?(error.reason)
        elsif conditions[:min_capability_level]
          capability_level(provider_config) >= conditions[:min_capability_level]
        else
          true
        end
      end

      def capability_level(provider_config)
        # Simple capability scoring
        score = 0
        score += 1 if provider_config[:capabilities].include?(:cloud)
        score += 2 if provider_config[:capabilities].include?(:advanced_reasoning)
        score += 1 if provider_config[:capabilities].include?(:long_context)
        score
      end

      def requires_advanced_reasoning?(messages)
        # Simple heuristic to detect complex requests
        message_text = messages.map { |m| m[:content] || m['content'] || '' }.join(' ')
        
        message_text.match?(/analyze|compare|evaluate|explain why|prove|derive/) ||
        message_text.length > 1000 ||
        messages.length > 10
      end

      def handle_escalation(error, messages, attempted_providers, options)
        # This is called when a provider requests escalation
        next_provider_config = select_next_provider(attempted_providers, error)
        
        if next_provider_config
          @chain_metrics[:successful_escalations] += 1
          
          # Enhance messages with context from error
          enhanced_messages = enhance_messages_for_escalation(messages, error)
          
          next_provider_config[:provider].chat(
            options[:model] || next_provider_config[:provider].default_model,
            enhanced_messages,
            options
          )
        else
          # No more providers available
          raise error
        end
      end

      def enhance_messages_for_escalation(messages, error)
        # Add context about the failure to help the next provider
        context_message = {
          role: "system",
          content: <<~CONTEXT
            Previous attempt failed. Please provide a comprehensive response.
            Failure reason: #{error.reason}
            Additional context: #{error.context.to_json}
          CONTEXT
        }
        
        [context_message] + messages
      end

      def handle_chain_exhaustion(last_error, messages, attempted_providers)
        # All providers failed - provide comprehensive error
        error_message = <<~ERROR
          All available providers failed to handle the request.
          
          Attempted providers: #{attempted_providers.join(', ')}
          Last error: #{last_error&.message}
          
          Suggestions:
          1. Simplify your request
          2. Break it into smaller parts
          3. Try again later
          4. Check provider availability
        ERROR

        raise MissionAbort::AbortError.new(
          error_message,
          reason: :chain_exhausted,
          context: {
            attempted_providers: attempted_providers,
            original_request: messages,
            chain_metrics: @chain_metrics
          }
        )
      end

      def calculate_backoff(retry_count)
        # Exponential backoff with jitter
        base_delay = 1.0
        max_delay = 30.0
        
        delay = [base_delay * (2 ** retry_count), max_delay].min
        delay + (rand * 0.3 * delay) # Add 0-30% jitter
      end
    end
  end
end