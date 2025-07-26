require_relative '../base_command'

module Lantae
  module CLI
    module Commands
      class ModelCommand < BaseCommand
        def initialize
          super('model', 'Switch to a different model', 'model <model-name>')
        end

        def execute(args, context = {})
          provider_registry = context[:provider_registry]
          
          return help unless validate_args(args)
          
          model_name = args.join(' ') # Handle model names with spaces
          
          begin
            # Update current model
            provider_registry.instance_variable_set(:@current_model, model_name)
            success("Switched to model: #{model_name}")
            
            # Show model info if available
            show_model_info(provider_registry, model_name)
          rescue => e
            error("Failed to switch model: #{e.message}")
          end
        end

        def complete(args, context = {})
          provider_registry = context[:provider_registry]
          current_input = args.join(' ')
          
          begin
            models = provider_registry.list_models
            models.select { |m| m.start_with?(current_input) }
          rescue
            []
          end
        end

        def validate_args(args)
          require_args(args, 1, '/model <model-name>')
        end

        private

        def show_model_info(provider_registry, model_name)
          capabilities = provider_registry.get_provider_capabilities
          
          if capabilities[:models] && capabilities[:models].include?(model_name)
            info("Model '#{model_name}' is available for provider '#{capabilities[:name]}'")
          else
            warning("Model '#{model_name}' may not be available for current provider")
            puts "Available models: #{capabilities[:models]&.first(3)&.join(', ')}#{capabilities[:models]&.length > 3 ? '...' : ''}"
          end
        end
      end
    end
  end
end