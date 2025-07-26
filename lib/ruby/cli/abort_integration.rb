module Lantae
  module CLI
    # Integration of mission abort strategy with CLI
    module AbortIntegration
      def self.setup_provider_chain(config)
        # Create abort strategy with custom conditions
        abort_strategy = MissionAbort.create_strategy
        
        # Create provider chain
        chain = Providers::ProviderChain.new(abort_strategy)
        
        # Add local provider as primary (fast, but limited)
        if config[:providers][:ollama][:enabled]
          ollama = Providers::OllamaProvider.new(config[:providers][:ollama])
          chain.add_provider(
            ollama,
            priority: 10,
            cost_tier: :free,
            capabilities: [:local, :fast],
            conditions: {
              handles_errors: [:timeout, :rate_limit]
            }
          )
        end
        
        # Add cloud providers as escalation options
        if config[:providers][:openai][:enabled] && ENV['OPENAI_API_KEY']
          openai = Providers::OpenAIProvider.new(config[:providers][:openai])
          chain.add_provider(
            openai,
            priority: 8,
            cost_tier: :standard,
            capabilities: [:cloud, :advanced_reasoning, :tools],
            conditions: {
              handles_errors: [:insufficient_context, :capability_limit],
              min_capability_level: 2
            }
          )
        end
        
        if config[:providers][:anthropic][:enabled] && ENV['ANTHROPIC_API_KEY']
          anthropic = Providers::AnthropicProvider.new(config[:providers][:anthropic])
          chain.add_provider(
            anthropic,
            priority: 7,
            cost_tier: :premium,
            capabilities: [:cloud, :long_context, :advanced_reasoning],
            conditions: {
              handles_errors: [:capability_limit, :hallucination_detected],
              min_capability_level: 3
            }
          )
        end
        
        chain
      end

      def self.enhance_chat_interface(interface, provider_chain)
        # Monkey-patch or extend the chat interface
        interface.define_singleton_method(:chat_with_abort_handling) do |messages, options = {}|
          begin
            # Show which provider is being used
            if options[:verbose]
              puts "Using provider chain: #{provider_chain.list_providers.join(' -> ')}"
            end
            
            # Execute with chain
            response = provider_chain.chat(messages, options)
            
            # Show metrics if requested
            if options[:show_metrics]
              metrics = provider_chain.metrics
              puts "\nProvider Chain Metrics:"
              puts "- Total requests: #{metrics[:chain_metrics][:total_requests]}"
              puts "- Provider switches: #{metrics[:chain_metrics][:provider_switches]}"
              puts "- Success rate: #{(1 - metrics[:chain_metrics][:chain_exhausted].to_f / metrics[:chain_metrics][:total_requests]) * 100}%"
            end
            
            response
          rescue MissionAbort::AbortError => e
            handle_abort_error(e, options)
          end
        end
        
        interface.define_singleton_method(:handle_abort_error) do |error, options|
          if options[:interactive]
            puts "\n⚠️  Mission Abort Detected"
            puts "Reason: #{error.reason}"
            puts "\nSuggestions:"
            error.suggestions.each { |s| puts "  • #{s}" }
            
            print "\nWould you like to try a different approach? (y/n): "
            if gets.chomp.downcase == 'y'
              puts "Please rephrase your request or provide more context:"
              new_input = gets.chomp
              # Retry with new input
              chat_with_abort_handling(
                [{ role: 'user', content: new_input }],
                options
              )
            else
              puts "Aborted."
            end
          else
            # Non-interactive mode - just show error
            puts "\nError: #{error.message}"
            puts "Suggestions: #{error.suggestions.join(', ')}"
          end
        end
      end

      def self.add_cli_commands(command_registry, provider_chain)
        # Add chain status command
        command_registry.register_command(
          'chain-status',
          lambda do |_args|
            metrics = provider_chain.metrics
            
            puts "Provider Chain Status"
            puts "=" * 50
            
            puts "\nProviders in chain:"
            metrics[:providers].each_with_index do |provider_info, index|
              puts "#{index + 1}. #{provider_info[:name]} (priority: #{provider_info[:priority]})"
              if provider_info[:metrics][:total_requests] > 0
                puts "   - Requests: #{provider_info[:metrics][:total_requests]}"
                puts "   - Abort rate: #{(provider_info[:metrics][:abort_rate] * 100).round(2)}%"
              end
            end
            
            puts "\nChain Statistics:"
            chain_metrics = metrics[:chain_metrics]
            puts "- Total requests: #{chain_metrics[:total_requests]}"
            puts "- Provider switches: #{chain_metrics[:provider_switches]}"
            puts "- Chain exhausted: #{chain_metrics[:chain_exhausted]}"
            puts "- Escalation success rate: #{(metrics[:escalation_success_rate] * 100).round(2)}%"
          end,
          description: "Show provider chain status and metrics"
        )
        
        # Add abort strategy configuration command
        command_registry.register_command(
          'abort-config',
          lambda do |args|
            case args.first
            when 'show'
              puts "Current abort conditions:"
              provider_chain.abort_strategy.abort_conditions.each do |condition|
                puts "- #{condition[:name]}"
              end
            when 'test'
              # Test abort detection
              test_phrase = args[1..-1].join(' ')
              if provider_chain.abort_strategy.should_abort?(test_phrase)
                puts "✓ Would trigger abort"
              else
                puts "✗ Would not trigger abort"
              end
            else
              puts "Usage: abort-config [show|test <phrase>]"
            end
          end,
          description: "Configure and test abort strategy"
        )
      end

      # Helper to create abort-aware configuration
      def self.create_abort_config(base_config)
        base_config.merge(
          abort_handling: {
            enabled: true,
            escalation_enabled: true,
            show_metrics: false,
            verbose_errors: true,
            retry_on_abort: true,
            max_escalations: 3
          }
        )
      end
    end
  end
end