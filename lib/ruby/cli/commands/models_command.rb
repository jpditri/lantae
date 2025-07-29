require_relative '../base_command'

module Lantae
  module CLI
    module Commands
      class ModelsCommand < BaseCommand
        def initialize
          super('models', 'List available models for current provider')
        end

        def execute(args, context = {})
          provider_registry = context[:provider_registry]
          
          begin
            models = provider_registry.list_models
            
            if models.empty?
              info("No models available for current provider")
              return
            end
            
            # Get provider instance to check if it's Ollama and get host info
            provider = provider_registry.instance_variable_get(:@current_provider)
            
            if provider&.respond_to?(:instance_variable_get) && provider.class.name.include?('OllamaProvider')
              base_url = provider.instance_variable_get(:@base_url)
              host_info = base_url ? " (#{base_url})" : ""
              puts "Available models#{host_info}:"
            else
              puts "Available models:"
            end
            
            models.each { |model| puts "  - #{model}" }
            
            success("Found #{models.length} models")
          rescue => e
            error("Failed to list models: #{e.message}")
          end
        end
      end
    end
  end
end