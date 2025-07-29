module Lantae
  class DefaultModelSelector
    # Small, fast models in order of preference
    SMALL_MODELS = [
      'qwen2:0.5b',        # Very small Qwen2 model
      'qwen2.5:1.5b',      # Qwen2.5 1.5B
      'tinyllama:1.1b',    # TinyLlama 1.1B
      'phi3:mini',         # Microsoft Phi-3 Mini (3.8B)
      'gemma:2b',          # Google Gemma 2B
      'llama3.2:1b',       # Llama 3.2 1B
      'llama3.2:3b',       # Llama 3.2 3B
      'qwen2.5:3b',        # Qwen2.5 3B
    ].freeze
    
    # Fallback to popular models if no small ones available
    POPULAR_MODELS = [
      'cogito:latest',     # Cogito reasoning model
      'llama3.1:8b',       # Llama 3.1 8B
      'llama3:8b',         # Llama 3 8B
      'mistral:7b',        # Mistral 7B
      'codellama:7b',      # CodeLlama 7B
      'qwen2:7b',          # Qwen2 7B
      'qwen3:14b',         # Qwen3 14B  
      'phi3:14b',          # Phi-3 14B
      'gemma:7b',          # Gemma 7B
    ].freeze
    
    # Last resort - very common models
    COMMON_MODELS = [
      'llama3:latest',
      'mistral:latest', 
      'codellama:latest',
      'qwen2:latest',
      'phi3:latest',
      'gemma:latest',
    ].freeze
    
    def self.get_default_model(base_url = 'http://localhost:11434')
      # Try to get available models from Ollama
      available_models = get_available_models(base_url)
      
      # If we can't get the list, use a safe fallback
      if available_models.empty?
        return 'llama3.2:1b'  # Best guess for a small, common model
      end
      
      # Look for small models first
      SMALL_MODELS.each do |model|
        return model if available_models.any? { |available| model_matches?(available, model) }
      end
      
      # Look for popular models
      POPULAR_MODELS.each do |model|
        return model if available_models.any? { |available| model_matches?(available, model) }
      end
      
      # Look for common models
      COMMON_MODELS.each do |model|
        return model if available_models.any? { |available| model_matches?(available, model) }
      end
      
      # If nothing matches, return the first available model or a fallback
      available_models.first || 'llama3.2:1b'
    end
    
    private
    
    def self.get_available_models(base_url)
      require 'net/http'
      require 'json'
      
      begin
        uri = URI("#{base_url}/api/tags")
        http = Net::HTTP.new(uri.host, uri.port)
        http.read_timeout = 5  # Quick timeout
        
        response = http.get(uri.path)
        
        if response.code == '200'
          data = JSON.parse(response.body)
          models = data['models'] || []
          return models.map { |model| model['name'] }.compact
        end
      rescue => e
        # Silently handle connection errors
        warn "Could not fetch Ollama models: #{e.message}" if ENV['DEBUG']
      end
      
      []
    end
    
    def self.model_matches?(available_name, target_name)
      # Handle cases where available model might have full names like "llama3.2:1b-instruct-fp16"
      # but we're looking for "llama3.2:1b"
      
      # Exact match
      return true if available_name == target_name
      
      # Partial match (available name starts with target)
      return true if available_name.start_with?(target_name)
      
      # Handle :latest tags
      if target_name.end_with?(':latest')
        base_name = target_name.gsub(':latest', '')
        return true if available_name.start_with?(base_name)
      end
      
      false
    end
    
    # Get recommended model based on use case
    def self.get_recommended_model(use_case, base_url = 'http://localhost:11434')
      available_models = get_available_models(base_url)
      
      case use_case.to_sym
      when :code
        code_models = ['codellama:7b', 'llama3.1:8b', 'qwen2.5-coder:7b', 'deepseek-coder:6.7b']
        code_models.each do |model|
          return model if available_models.any? { |available| model_matches?(available, model) }
        end
        
      when :fast
        return get_default_model(base_url)  # Already optimized for speed
        
      when :general
        general_models = ['llama3.1:8b', 'mistral:7b', 'qwen2:7b']
        general_models.each do |model|
          return model if available_models.any? { |available| model_matches?(available, model) }
        end
      end
      
      # Fallback to default
      get_default_model(base_url)
    end
    
    # Check if model exists and suggest alternatives
    def self.validate_model(model_name, base_url = 'http://localhost:11434')
      available_models = get_available_models(base_url)
      
      # Check if the model exists
      exists = available_models.any? { |available| model_matches?(available, model_name) }
      
      if exists
        return { valid: true, model: model_name }
      else
        # Suggest alternatives
        suggested = get_default_model(base_url)
        return { 
          valid: false, 
          model: model_name,
          suggested: suggested,
          available: available_models.first(5)  # Show first 5 available
        }
      end
    end
  end
end