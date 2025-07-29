require 'net/http'
require 'json'

module Lantae
  class APIKeyAuthorizer
    # Console URLs for different providers
    CONSOLE_URLS = {
      'anthropic' => "https://console.anthropic.com/settings/keys",
      'openai' => "https://platform.openai.com/api-keys",
      'gemini' => "https://makersuite.google.com/app/apikey",
      'mistral' => "https://console.mistral.ai/api-keys",
      'perplexity' => "https://www.perplexity.ai/settings/api"
    }
    
    # Key prefixes for validation
    KEY_PREFIXES = {
      'anthropic' => 'sk-ant-',
      'openai' => 'sk-',
      'gemini' => 'AIza',
      'mistral' => nil,  # Mistral keys don't have a specific prefix
      'perplexity' => 'pplx-'
    }
    
    def self.request_api_key(provider)
      provider_name = provider.capitalize
      console_url = CONSOLE_URLS[provider]
      
      puts "\n🔑 #{provider_name} API Key Required"
      puts "━" * 50
      puts
      puts "To use #{provider_name}, you need an API key."
      puts
      puts "You have two options:"
      puts
      puts "1. \e[32mAutomatic\e[0m: Open #{provider_name} Console in your browser"
      puts "   We'll help you get a key and save it automatically"
      puts
      puts "2. \e[33mManual\e[0m: Enter an existing API key"
      puts "   If you already have one from #{console_url}"
      puts
      print "Choose an option (1 or 2): "
      
      choice = gets.chomp
      
      case choice
      when '1'
        authorize_via_browser(provider)
      when '2'
        manual_key_entry(provider)
      else
        puts "\n❌ Invalid choice. Please run lantae again."
        nil
      end
    end
    
    # Legacy method for backward compatibility
    def self.request_anthropic_key
      request_api_key('anthropic')
    end
    
    private
    
    def self.authorize_via_browser(provider)
      provider_name = provider.capitalize
      console_url = CONSOLE_URLS[provider]
      
      puts "\n🌐 Opening #{provider_name} Console..."
      puts "Please follow these steps:"
      puts
      puts "1. Sign in or create an account"
      puts "2. Navigate to API Keys section" 
      puts "3. Create a new API key"
      
      if KEY_PREFIXES[provider]
        puts "4. Copy the key (it starts with '#{KEY_PREFIXES[provider]}')"
      else
        puts "4. Copy the key"
      end
      puts
      
      # Try to open browser
      open_browser(console_url)
      
      puts "\n⏳ Waiting for you to copy your API key..."
      puts
      print "Paste your API key here: "
      
      # Disable echo for security with proper cleanup
      begin
        system("stty -echo") if RUBY_PLATFORM =~ /darwin|linux/
        api_key = gets.chomp
      ensure
        # Always restore echo, even if interrupted
        system("stty echo") if RUBY_PLATFORM =~ /darwin|linux/
      end
      puts # New line after hidden input
      
      if validate_api_key(provider, api_key)
        save_api_key(provider, api_key)
        puts "\n✅ API key saved successfully!"
        api_key
      else
        if KEY_PREFIXES[provider]
          puts "\n❌ Invalid API key format. Keys should start with '#{KEY_PREFIXES[provider]}'"
        else
          puts "\n❌ Invalid API key format."
        end
        nil
      end
    end
    
    def self.manual_key_entry(provider)
      provider_name = provider.capitalize
      print "\nEnter your #{provider_name} API key: "
      
      # Disable echo for security with proper cleanup
      begin
        system("stty -echo") if RUBY_PLATFORM =~ /darwin|linux/
        api_key = gets.chomp
      ensure
        # Always restore echo, even if interrupted
        system("stty echo") if RUBY_PLATFORM =~ /darwin|linux/
      end
      puts # New line after hidden input
      
      if validate_api_key(provider, api_key)
        save_api_key(provider, api_key)
        puts "\n✅ API key saved successfully!"
        api_key
      else
        if KEY_PREFIXES[provider]
          puts "\n❌ Invalid API key format. Keys should start with '#{KEY_PREFIXES[provider]}'"
        else
          puts "\n❌ Invalid API key format."
        end
        nil
      end
    end
    
    def self.validate_api_key(provider, key)
      return false unless key && key.strip.length > 10
      
      prefix = KEY_PREFIXES[provider]
      if prefix
        key.strip.start_with?(prefix)
      else
        # For providers without specific prefixes, just check length
        key.strip.length > 20
      end
    end
    
    # Legacy method for backward compatibility
    def self.validate_anthropic_key(key)
      validate_api_key('anthropic', key)
    end
    
    def self.save_api_key(provider, api_key)
      # Save to environment file
      env_file = File.expand_path('~/.lantae_env')
      
      # Environment variable name
      env_var = "#{provider.upcase}_API_KEY"
      
      # Read existing content
      existing_content = File.exist?(env_file) ? File.read(env_file) : ""
      
      # Update or add the key
      if existing_content.include?("#{env_var}=")
        new_content = existing_content.gsub(/#{env_var}=.*/, "#{env_var}=#{api_key}")
      else
        new_content = existing_content + "\n#{env_var}=#{api_key}\n"
      end
      
      File.write(env_file, new_content)
      File.chmod(0600, env_file)  # Secure the file
      
      # Also set in current environment
      ENV[env_var] = api_key
      
      # Add to shell profile if not already there
      add_to_shell_profile
    end
    
    def self.add_to_shell_profile
      shell_profile = if File.exist?(File.expand_path('~/.zshrc'))
        File.expand_path('~/.zshrc')
      elsif File.exist?(File.expand_path('~/.bashrc'))
        File.expand_path('~/.bashrc')
      else
        File.expand_path('~/.profile')
      end
      
      profile_content = File.read(shell_profile)
      lantae_source = "source ~/.lantae_env"
      
      unless profile_content.include?(lantae_source)
        File.open(shell_profile, 'a') do |f|
          f.puts "\n# Lantae API keys"
          f.puts "[ -f ~/.lantae_env ] && #{lantae_source}"
        end
        
        puts "\n📝 Added API key loading to #{File.basename(shell_profile)}"
        puts "   Run: source #{shell_profile}"
        puts "   Or restart your terminal"
      end
    end
    
    def self.open_browser(url)
      case RUBY_PLATFORM
      when /darwin/
        system("open '#{url}'")
      when /linux/
        system("xdg-open '#{url}' 2>/dev/null || firefox '#{url}' 2>/dev/null || google-chrome '#{url}' 2>/dev/null")
      when /mswin|mingw/
        system("start '#{url}'")
      else
        puts "⚠️  Please manually open: #{url}"
      end
    end
    
    # Load API keys from .lantae_env file on startup
    def self.load_env_file
      env_file = File.expand_path('~/.lantae_env')
      if File.exist?(env_file)
        File.readlines(env_file).each do |line|
          next if line.strip.empty? || line.start_with?('#')
          key, value = line.strip.split('=', 2)
          ENV[key] = value if key && value
        end
      end
    end
  end
end

# Load environment file on require
Lantae::APIKeyAuthorizer.load_env_file