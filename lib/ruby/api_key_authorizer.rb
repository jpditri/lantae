require 'net/http'
require 'json'

module Lantae
  class APIKeyAuthorizer
    ANTHROPIC_CONSOLE_URL = "https://console.anthropic.com/settings/keys"
    
    def self.request_anthropic_key
      puts "\n🔑 Anthropic API Key Required"
      puts "━" * 50
      puts
      puts "To use Anthropic's Claude models, you need an API key."
      puts
      puts "You have two options:"
      puts
      puts "1. \e[32mAutomatic\e[0m: Open Anthropic Console in your browser"
      puts "   We'll help you get a key and save it automatically"
      puts
      puts "2. \e[33mManual\e[0m: Enter an existing API key"
      puts "   If you already have one from https://console.anthropic.com"
      puts
      print "Choose an option (1 or 2): "
      
      choice = gets.chomp
      
      case choice
      when '1'
        authorize_via_browser
      when '2'
        manual_key_entry
      else
        puts "\n❌ Invalid choice. Please run lantae again."
        nil
      end
    end
    
    private
    
    def self.authorize_via_browser
      puts "\n🌐 Opening Anthropic Console..."
      puts "Please follow these steps:"
      puts
      puts "1. Sign in or create an account"
      puts "2. Navigate to API Keys section" 
      puts "3. Create a new API key"
      puts "4. Copy the key (it starts with 'sk-ant-')"
      puts
      
      # Try to open browser
      open_browser(ANTHROPIC_CONSOLE_URL)
      
      puts "\n⏳ Waiting for you to copy your API key..."
      puts
      print "Paste your API key here: "
      
      # Disable echo for security
      system("stty -echo") if RUBY_PLATFORM =~ /darwin|linux/
      api_key = gets.chomp
      system("stty echo") if RUBY_PLATFORM =~ /darwin|linux/
      puts # New line after hidden input
      
      if validate_anthropic_key(api_key)
        save_api_key(api_key)
        puts "\n✅ API key saved successfully!"
        api_key
      else
        puts "\n❌ Invalid API key format. Keys should start with 'sk-ant-'"
        nil
      end
    end
    
    def self.manual_key_entry
      print "\nEnter your Anthropic API key: "
      
      # Disable echo for security
      system("stty -echo") if RUBY_PLATFORM =~ /darwin|linux/
      api_key = gets.chomp
      system("stty echo") if RUBY_PLATFORM =~ /darwin|linux/
      puts # New line after hidden input
      
      if validate_anthropic_key(api_key)
        save_api_key(api_key)
        puts "\n✅ API key saved successfully!"
        api_key
      else
        puts "\n❌ Invalid API key format. Keys should start with 'sk-ant-'"
        nil
      end
    end
    
    def self.validate_anthropic_key(key)
      key && key.strip.start_with?('sk-ant-') && key.length > 20
    end
    
    def self.save_api_key(api_key)
      # Save to environment file
      env_file = File.expand_path('~/.lantae_env')
      
      # Read existing content
      existing_content = File.exist?(env_file) ? File.read(env_file) : ""
      
      # Update or add the key
      if existing_content.include?('ANTHROPIC_API_KEY=')
        new_content = existing_content.gsub(/ANTHROPIC_API_KEY=.*/, "ANTHROPIC_API_KEY=#{api_key}")
      else
        new_content = existing_content + "\nANTHROPIC_API_KEY=#{api_key}\n"
      end
      
      File.write(env_file, new_content)
      File.chmod(0600, env_file)  # Secure the file
      
      # Also set in current environment
      ENV['ANTHROPIC_API_KEY'] = api_key
      
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