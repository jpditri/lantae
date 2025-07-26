module Lantae
  # Mission Abort Strategy for handling failures in local models
  # Provides escalation paths when models cannot handle requests
  module MissionAbort
    class AbortError < StandardError
      attr_reader :reason, :context, :suggestions

      def initialize(message, reason: nil, context: {}, suggestions: [])
        super(message)
        @reason = reason
        @context = context
        @suggestions = suggestions
      end
    end

    class Strategy
      attr_reader :escalation_chain, :abort_conditions, :recovery_options

      def initialize
        @escalation_chain = []
        @abort_conditions = default_abort_conditions
        @recovery_options = default_recovery_options
        @abort_history = []
      end

      # Add provider to escalation chain with priority
      def add_escalation_provider(provider, priority: 0, conditions: {})
        @escalation_chain << {
          provider: provider,
          priority: priority,
          conditions: conditions,
          attempts: 0,
          failures: []
        }
        @escalation_chain.sort_by! { |p| -p[:priority] }
      end

      # Check if request should be aborted based on response
      def should_abort?(response, model_info = {})
        @abort_conditions.any? do |condition|
          condition[:check].call(response, model_info)
        end
      end

      # Execute request with abort handling
      def execute_with_abort_handling(provider, request, options = {})
        response = nil
        abort_error = nil

        begin
          response = yield(provider, request)
          
          if should_abort?(response, { provider: provider, request: request })
            abort_error = create_abort_error(response, provider, request)
            handle_abort(abort_error, provider, request, options)
          else
            response
          end
        rescue => e
          abort_error = create_abort_error(e, provider, request)
          handle_abort(abort_error, provider, request, options)
        end
      end

      # Handle abort situation with escalation
      def handle_abort(abort_error, failed_provider, request, options = {})
        @abort_history << {
          timestamp: Time.now,
          provider: failed_provider,
          error: abort_error,
          request: request
        }

        # Try escalation chain
        escalation_result = try_escalation_chain(request, failed_provider, abort_error)
        return escalation_result if escalation_result

        # Try recovery options
        recovery_result = try_recovery_options(abort_error, request, options)
        return recovery_result if recovery_result

        # If all else fails, raise enhanced error
        raise enhance_abort_error(abort_error, failed_provider)
      end

      private

      def default_abort_conditions
        [
          {
            name: :insufficient_context,
            check: lambda do |response, _|
              response.is_a?(String) && (
                response.match?(/insufficient.{0,20}context/i) ||
                response.match?(/cannot.{0,20}understand/i) ||
                response.match?(/need.{0,20}more.{0,20}information/i)
              )
            end
          },
          {
            name: :capability_limit,
            check: lambda do |response, _|
              response.is_a?(String) && (
                response.match?(/cannot.{0,20}perform/i) ||
                response.match?(/unable.{0,20}to.{0,20}complete/i) ||
                response.match?(/beyond.{0,20}capabilities/i)
              )
            end
          },
          {
            name: :hallucination_detected,
            check: lambda do |response, _|
              response.is_a?(String) && (
                response.match?(/\[HALLUCINATION\]/i) ||
                response.match?(/made.{0,20}up/i) ||
                response.match?(/fictional.{0,20}information/i)
              )
            end
          },
          {
            name: :confidence_threshold,
            check: lambda do |response, model_info|
              # Check if model returns confidence scores
              if response.is_a?(Hash) && response[:confidence]
                response[:confidence] < 0.7
              else
                false
              end
            end
          },
          {
            name: :repetitive_failure,
            check: lambda do |response, model_info|
              # Check if same error repeats
              recent_errors = @abort_history.last(3).map { |h| h[:error].message }
              recent_errors.count(response) >= 2
            end
          }
        ]
      end

      def default_recovery_options
        [
          {
            name: :prompt_enhancement,
            applicable: ->(error) { error.reason == :insufficient_context },
            action: lambda do |request, error|
              enhanced_prompt = enhance_prompt_with_context(request, error.context)
              { type: :retry, request: enhanced_prompt }
            end
          },
          {
            name: :decomposition,
            applicable: ->(error) { error.reason == :capability_limit },
            action: lambda do |request, error|
              subtasks = decompose_complex_request(request)
              { type: :subtasks, tasks: subtasks }
            end
          },
          {
            name: :validation_prompt,
            applicable: ->(error) { error.reason == :hallucination_detected },
            action: lambda do |request, error|
              validation_prompt = create_validation_prompt(request)
              { type: :validate, request: validation_prompt }
            end
          }
        ]
      end

      def try_escalation_chain(request, failed_provider, error)
        @escalation_chain.each do |escalation|
          next if escalation[:provider] == failed_provider
          next if escalation[:attempts] >= 3

          if escalation[:conditions].empty? || conditions_met?(escalation[:conditions], error)
            begin
              escalation[:attempts] += 1
              result = escalation[:provider].chat(
                escalation[:provider].default_model,
                enhance_request_for_escalation(request, error)
              )
              return result if result && !should_abort?(result)
            rescue => e
              escalation[:failures] << e
            end
          end
        end
        nil
      end

      def try_recovery_options(error, request, options)
        @recovery_options.each do |recovery|
          if recovery[:applicable].call(error)
            result = recovery[:action].call(request, error)
            
            case result[:type]
            when :retry
              return options[:retry_handler]&.call(result[:request])
            when :subtasks
              return options[:subtask_handler]&.call(result[:tasks])
            when :validate
              return options[:validation_handler]&.call(result[:request])
            end
          end
        end
        nil
      end

      def create_abort_error(response_or_error, provider, request)
        if response_or_error.is_a?(Exception)
          reason = determine_abort_reason(response_or_error.message)
          AbortError.new(
            "Provider #{provider.name} failed: #{response_or_error.message}",
            reason: reason,
            context: { provider: provider.name, request: request },
            suggestions: generate_suggestions(reason, provider)
          )
        else
          reason = determine_abort_reason(response_or_error)
          AbortError.new(
            "Provider #{provider.name} cannot handle request",
            reason: reason,
            context: { provider: provider.name, response: response_or_error, request: request },
            suggestions: generate_suggestions(reason, provider)
          )
        end
      end

      def determine_abort_reason(content)
        content_str = content.to_s.downcase
        
        if content_str.match?(/insufficient|need.{0,20}more|context/)
          :insufficient_context
        elsif content_str.match?(/cannot|unable|beyond/)
          :capability_limit
        elsif content_str.match?(/hallucin|made.{0,20}up|fictional/)
          :hallucination_detected
        elsif content_str.match?(/timeout|timed.{0,20}out/)
          :timeout
        elsif content_str.match?(/rate.{0,20}limit|too.{0,20}many/)
          :rate_limit
        else
          :unknown
        end
      end

      def generate_suggestions(reason, provider)
        case reason
        when :insufficient_context
          [
            "Provide more context or background information",
            "Break down the request into smaller, specific questions",
            "Use a more capable model or provider"
          ]
        when :capability_limit
          [
            "Simplify the request",
            "Use a specialized provider for this task",
            "Consider using tools or plugins for enhanced capabilities"
          ]
        when :hallucination_detected
          [
            "Verify information with reliable sources",
            "Use a model with better factual accuracy",
            "Add validation steps to the workflow"
          ]
        when :timeout
          [
            "Reduce request complexity",
            "Increase timeout settings",
            "Check network connectivity"
          ]
        when :rate_limit
          [
            "Wait before retrying",
            "Use a different provider temporarily",
            "Upgrade API plan for higher limits"
          ]
        else
          [
            "Try a different provider",
            "Reformulate the request",
            "Check provider status and availability"
          ]
        end
      end

      def enhance_prompt_with_context(request, context)
        # Add relevant context to the original request
        if request.is_a?(Array)
          request + [{
            role: "system",
            content: "Additional context: #{context.to_json}"
          }]
        else
          "#{request}\n\nAdditional context: #{context.to_json}"
        end
      end

      def decompose_complex_request(request)
        # Break down complex request into subtasks
        # This is a simplified version - could use AI for better decomposition
        [
          { task: "Understand the main objective", request: request },
          { task: "Identify required steps", request: request },
          { task: "Execute each step", request: request }
        ]
      end

      def create_validation_prompt(request)
        if request.is_a?(Array)
          request + [{
            role: "system",
            content: "Please validate the previous response for accuracy and cite sources if possible."
          }]
        else
          "#{request}\n\nPlease ensure accuracy and avoid speculation."
        end
      end

      def enhance_request_for_escalation(request, error)
        if request.is_a?(Array)
          [
            {
              role: "system",
              content: "Previous attempt failed with: #{error.message}. Please provide a comprehensive response."
            }
          ] + request
        else
          "Previous attempt failed with: #{error.message}\n\nRequest: #{request}"
        end
      end

      def conditions_met?(conditions, error)
        conditions.all? do |key, value|
          case key
          when :min_priority
            true # Would check against current priority
          when :error_types
            value.include?(error.reason)
          when :max_attempts
            @abort_history.count { |h| h[:error].reason == error.reason } < value
          else
            true
          end
        end
      end

      def enhance_abort_error(error, provider)
        enhanced_message = <<~MSG
          Mission Abort: #{error.message}
          
          Provider: #{provider.name}
          Reason: #{error.reason}
          
          Suggestions:
          #{error.suggestions.map { |s| "- #{s}" }.join("\n")}
          
          Abort History: #{@abort_history.size} previous aborts
        MSG

        AbortError.new(
          enhanced_message,
          reason: error.reason,
          context: error.context.merge(history: @abort_history),
          suggestions: error.suggestions
        )
      end
    end

    # Convenience method to create a new strategy
    def self.create_strategy
      Strategy.new
    end
  end
end