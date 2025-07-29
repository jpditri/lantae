module Lantae
  class ModelManagement
    def self.list_and_recommend(base_url = 'http://localhost:11434')
      puts "\n🤖 \e[1mOllama Model Management\e[0m\n\n"
      
      # Get current default
      default_model = DefaultModelSelector.get_default_model(base_url)
      puts "📌 \e[1mCurrent default:\e[0m #{default_model}\n\n"
      
      # Get available models
      available = DefaultModelSelector.send(:get_available_models, base_url)
      
      if available.empty?
        puts "❌ \e[31mNo models found or Ollama not running\e[0m"
        puts "💡 Make sure Ollama is running: \e[33mollama serve\e[0m"
        puts "💡 Pull a small model: \e[33mollama pull llama3.2:1b\e[0m"
        return
      end
      
      # Categorize models
      small_models = available.select { |m| is_small_model?(m) }
      code_models = available.select { |m| is_code_model?(m) }
      general_models = available.select { |m| is_general_model?(m) }
      
      # Show categories
      if small_models.any?
        puts "🚀 \e[1;32mSmall/Fast Models (< 2GB):\e[0m"
        small_models.each { |model| puts "   #{format_model_line(model, default_model)}" }
        puts
      end
      
      if code_models.any?
        puts "💻 \e[1;34mCode-Specialized Models:\e[0m"
        code_models.each { |model| puts "   #{format_model_line(model, default_model)}" }
        puts
      end
      
      if general_models.any?
        puts "🧠 \e[1;35mGeneral Purpose Models:\e[0m"
        general_models.each { |model| puts "   #{format_model_line(model, default_model)}" }
        puts
      end
      
      # Show other models
      other_models = available - small_models - code_models - general_models
      if other_models.any?
        puts "📋 \e[1;37mOther Models:\e[0m"
        other_models.each { |model| puts "   #{format_model_line(model, default_model)}" }
        puts
      end
      
      # Show recommendations
      puts "💡 \e[1;33mRecommendations:\e[0m"
      puts "   For speed: \e[36m#{DefaultModelSelector.get_recommended_model(:fast, base_url)}\e[0m"
      puts "   For coding: \e[36m#{DefaultModelSelector.get_recommended_model(:code, base_url)}\e[0m"
      puts "   For general use: \e[36m#{DefaultModelSelector.get_recommended_model(:general, base_url)}\e[0m"
      puts
      
      # Show suggested models to pull if missing good options
      if small_models.empty?
        puts "📥 \e[1;33mSuggested models to pull:\e[0m"
        puts "   \e[33mollama pull llama3.2:1b\e[0m    # Very fast, small model"
        puts "   \e[33mollama pull phi3:mini\e[0m      # Microsoft's compact model"
        puts "   \e[33mollama pull gemma:2b\e[0m       # Google's efficient model"
      end
    end
    
    def self.format_model_line(model, default_model)
      indicator = model == default_model ? "▶ " : "  "
      size_info = get_model_size_info(model)
      "#{indicator}\e[37m#{model}\e[0m#{size_info}"
    end
    
    def self.get_model_size_info(model_name)
      # Extract size information from model name
      case model_name.downcase
      when /0\.5b|500m/
        " \e[32m(~0.5GB)\e[0m"
      when /1\.1b|1b/
        " \e[32m(~1GB)\e[0m"
      when /2b/
        " \e[33m(~2GB)\e[0m"
      when /3b/
        " \e[33m(~3GB)\e[0m"
      when /7b/
        " \e[31m(~7GB)\e[0m"
      when /8b/
        " \e[31m(~8GB)\e[0m"
      when /13b|14b/
        " \e[35m(~13GB)\e[0m"
      when /70b/
        " \e[35m(~70GB)\e[0m"
      else
        ""
      end
    end
    
    def self.is_small_model?(model_name)
      model_name.match?(/0\.5b|1\.1b|1b|2b|mini|tiny/i)
    end
    
    def self.is_code_model?(model_name)
      model_name.match?(/code|coder|programming/i)
    end
    
    def self.is_general_model?(model_name)
      model_name.match?(/llama|mistral|qwen|phi3|gemma/i) && !is_code_model?(model_name)
    end
    
    # Install recommended model
    def self.install_recommended_small_model
      puts "\n🚀 Installing recommended small model...\n"
      
      recommended_models = [
        'llama3.2:1b',
        'phi3:mini', 
        'gemma:2b'
      ]
      
      recommended_models.each do |model|
        puts "⬇️  Pulling #{model}..."
        system("ollama pull #{model}")
        
        # Check if successful
        if $?.success?
          puts "✅ Successfully installed #{model}"
          return model
        else
          puts "❌ Failed to install #{model}, trying next..."
        end
      end
      
      puts "❌ Could not install any recommended models"
      nil
    end
  end
end