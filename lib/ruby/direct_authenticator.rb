require 'net/http'
require 'json'

module Lantae
  class DirectAuthenticator
    # Direct console URLs for different providers
    CONSOLE_URLS = {
      'anthropic' => "https://console.anthropic.com/settings/keys",
      'openai' => "https://platform.openai.com/api-keys", 
      'gemini' => "https://makersuite.google.com/app/apikey"
    }
    
    # Key validation patterns
    KEY_PATTERNS = {
      'anthropic' => /^sk-ant-/,
      'openai' => /^sk-/,
      'gemini' => /^AIza/
    }
    
    def self.login(provider)
      provider = provider.to_s.downcase
      console_url = CONSOLE_URLS[provider]
      
      unless console_url
        return { success: false, error: "Unsupported provider: #{provider}" }
      end
      
      puts "\n🔐 #{provider.capitalize} Authentication"
      puts "━" * 60
      puts 
      puts "🌐 Opening #{provider.capitalize} Console in your browser..."
      puts "📋 Create an API key and copy it"
      puts
      
      # Open browser directly to provider console
      open_browser(console_url)
      
      # Wait a moment for browser to open
      sleep 1
      
      puts "⏳ Waiting for API key..."
      print "📝 Paste your API key here: "
      
      # Get the key with echo disabled
      api_key = get_hidden_input
      
      if api_key && validate_key(provider, api_key)
        save_api_key(provider, api_key)
        puts "\n✅ Authentication successful!"
        return { success: true, api_key: api_key }
      else
        puts "\n❌ Invalid API key format"
        return { success: false, error: "Invalid key format" }
      end
    end
    
    private
    
    def self.open_browser(url)
      case RUBY_PLATFORM
      when /darwin/
        system("open '#{url}'")
      when /linux/
        system("xdg-open '#{url}' 2>/dev/null")
      when /mswin|mingw/
        system("start \"\" \"#{url}\"")
      else
        puts "Please open: #{url}"
      end
    end
    
    def self.get_hidden_input
      begin
        system("stty -echo") if STDIN.tty?
        input = gets&.chomp
      ensure
        system("stty echo") if STDIN.tty?
        puts # New line after hidden input
      end
      input
    end
    
    def self.validate_key(provider, key)
      return false unless key && key.length > 10
      
      pattern = KEY_PATTERNS[provider]
      pattern ? key.match?(pattern) : true
    end
    
    def self.save_api_key(provider, api_key)
      # Save to environment
      ENV["#{provider.upcase}_API_KEY"] = api_key
      
      # Save to file
      env_file = File.expand_path('~/.lantae_env')
      env_var = "#{provider.upcase}_API_KEY"
      
      # Read existing content
      existing = File.exist?(env_file) ? File.read(env_file) : ""
      
      # Update or add key
      if existing.include?("#{env_var}=")
        new_content = existing.gsub(/#{env_var}=.*/, "#{env_var}=#{api_key}")
      else
        new_content = existing + "\n#{env_var}=#{api_key}\n"
      end
      
      File.write(env_file, new_content)
      File.chmod(0600, env_file)
    end
  end
end