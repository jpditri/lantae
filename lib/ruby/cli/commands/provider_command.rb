require_relative '../base_command'

module Lantae
  module CLI
    module Commands
      class ProviderCommand < BaseCommand
        def initialize
          super('provider', 'Switch provider (ollama, openai, anthropic, bedrock, gemini, mistral, perplexity)', 'provider <provider-name> [model]')
        end

        def execute(args, context = {})
          provider_registry = context[:provider_registry]
          
          return help unless validate_args(args)
          
          provider_name = args[0]
          model = args[1]
          
          begin
            result = provider_registry.switch_provider(provider_name, model)
            success("Switched to provider: #{result[:provider]}, model: #{result[:model]}")
            
            # Show provider capabilities
            show_provider_info(provider_registry, provider_name)
          rescue => e
            error(e.message)
          end
        end

        def complete(args, context = {})
          provider_registry = context[:provider_registry]
          
          if args.length <= 1
            # Complete provider names
            providers = provider_registry.available_providers
            current_input = args[0] || ''
            providers.select { |p| p.start_with?(current_input) }
          elsif args.length == 2
            # Complete model names for the selected provider
            provider_name = args[0]
            current_input = args[1] || ''
            
            begin
              models = provider_registry.list_models(provider_name)
              models.select { |m| m.start_with?(current_input) }
            rescue
              []
            end
          else
            []
          end
        end

        def validate_args(args)
          require_args(args, 1, '/provider <provider-name> [model]')
        end

        private

        def show_provider_info(provider_registry, provider_name)
          capabilities = provider_registry.get_provider_capabilities(provider_name)
          
          if capabilities[:error]
            warning("Could not retrieve provider capabilities: #{capabilities[:error]}")
            return
          end
          
          puts "\nProvider Capabilities:"
          puts "  Name: #{capabilities[:name]}"
          puts "  Streaming: #{capabilities[:supports_streaming] ? '✓' : '✗'}"
          puts "  Tools: #{capabilities[:supports_tools] ? '✓' : '✗'}"
          puts "  Max Tokens: #{capabilities[:max_tokens]}"
          puts "  Default Temperature: #{capabilities[:default_temperature]}"
          
          if capabilities[:models] && capabilities[:models].any?
            puts "  Available Models:"
            capabilities[:models].first(5).each do |model|
              puts "    - #{model}"
            end
            puts "    ... and #{capabilities[:models].length - 5} more" if capabilities[:models].length > 5
          end
          
          puts
        end
      end
    end
  end
end