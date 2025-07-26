module Lantae
  module Providers
    # Wrapper for providers that adds mission abort capabilities
    class AbortAwareProvider
      attr_reader :provider, :abort_strategy

      def initialize(provider, abort_strategy = nil)
        @provider = provider
        @abort_strategy = abort_strategy || MissionAbort.create_strategy
        @metrics = {
          total_requests: 0,
          aborted_requests: 0,
          escalated_requests: 0,
          recovered_requests: 0
        }
      end

      # Delegate basic provider methods
      def name
        @provider.name
      end

      def default_model
        @provider.default_model
      end

      def list_models
        @provider.list_models
      end

      def supports_streaming?
        @provider.supports_streaming?
      end

      def supports_tools?
        @provider.supports_tools?
      end

      # Enhanced chat method with abort handling
      def chat(model, messages, options = {})
        @metrics[:total_requests] += 1

        @abort_strategy.execute_with_abort_handling(@provider, messages, options) do |provider, request|
          provider.chat(model, request, options)
        end
      rescue MissionAbort::AbortError => e
        @metrics[:aborted_requests] += 1
        
        # If escalation handler is provided, use it
        if options[:escalation_handler]
          @metrics[:escalated_requests] += 1
          options[:escalation_handler].call(e, messages)
        else
          # Default behavior: try to provide helpful response
          create_abort_response(e)
        end
      end

      # Stream with abort detection
      def stream(model, messages, options = {}, &block)
        return unless @provider.supports_streaming?

        accumulated_response = ""
        abort_check_interval = options[:abort_check_interval] || 100 # characters

        @provider.stream(model, messages, options) do |chunk|
          accumulated_response += chunk
          
          # Periodic abort checking during streaming
          if accumulated_response.length % abort_check_interval == 0
            if @abort_strategy.should_abort?(accumulated_response)
              # Stop streaming and handle abort
              abort_error = MissionAbort::AbortError.new(
                "Stream aborted due to detected issues",
                reason: :stream_abort,
                context: { partial_response: accumulated_response }
              )
              
              if options[:stream_abort_handler]
                options[:stream_abort_handler].call(abort_error, accumulated_response)
              else
                block.call("\n\n[Stream aborted: #{abort_error.reason}]")
              end
              
              return
            end
          end
          
          block.call(chunk)
        end
      end

      # Add escalation provider to strategy
      def add_escalation_provider(provider, priority: 0, conditions: {})
        @abort_strategy.add_escalation_provider(provider, priority: priority, conditions: conditions)
      end

      # Get current metrics
      def metrics
        @metrics.merge(
          abort_rate: @metrics[:total_requests] > 0 ? 
            @metrics[:aborted_requests].to_f / @metrics[:total_requests] : 0,
          escalation_rate: @metrics[:aborted_requests] > 0 ?
            @metrics[:escalated_requests].to_f / @metrics[:aborted_requests] : 0
        )
      end

      # Health check with abort awareness
      def health_check
        base_health = @provider.health_check
        
        if base_health[:status] == :healthy
          # Add abort metrics to health status
          base_health.merge(
            abort_metrics: metrics,
            abort_strategy: @abort_strategy.class.name
          )
        else
          base_health
        end
      end

      private

      def create_abort_response(abort_error)
        response = <<~RESPONSE
          I apologize, but I'm unable to properly handle this request at the moment.
          
          Reason: #{humanize_abort_reason(abort_error.reason)}
          
          #{format_suggestions(abort_error.suggestions)}
          
          Would you like me to try a different approach or provider?
        RESPONSE

        response
      end

      def humanize_abort_reason(reason)
        case reason
        when :insufficient_context
          "I need more context or information to provide a helpful response"
        when :capability_limit
          "This request is beyond my current capabilities"
        when :hallucination_detected
          "I detected potential inaccuracies in my response"
        when :timeout
          "The request took too long to process"
        when :rate_limit
          "Rate limit exceeded - please try again later"
        else
          "An unexpected issue occurred"
        end
      end

      def format_suggestions(suggestions)
        return "" if suggestions.empty?

        "Here are some suggestions:\n" + 
        suggestions.map { |s| "• #{s}" }.join("\n")
      end
    end

    # Factory method to wrap existing providers
    def self.make_abort_aware(provider, strategy = nil)
      AbortAwareProvider.new(provider, strategy)
    end
  end
end