require 'net/http'
require 'uri'
require 'socket'
require 'timeout'

module Lantae
  class ProviderDetector
    OLLAMA_DEFAULT_PORT = 11434
    NETWORK_SCAN_TIMEOUT = 1 # seconds per host
    
    def self.detect_best_provider(options = {})
      detector = new(options)
      detector.detect
    end
    
    def initialize(options = {})
      @secret_manager = options[:secret_manager]
      @logger = options[:logger] || Logger.new(STDOUT)
      @interactive = options[:interactive] != false
    end
    
    def detect
      puts "🔍 Detecting available AI providers..." if @interactive
      
      # 1. Check local Ollama
      if ollama_available?
        puts "✅ Found local Ollama server" if @interactive
        return { provider: 'ollama', model: 'cogito:latest', source: 'local' }
      end
      
      # 2. Ask about network scan if interactive
      if @interactive
        puts "\n❌ Local Ollama not found."
        print "Would you like to scan the local network for Ollama servers? (y/n): "
        
        response = STDIN.gets.chomp.downcase
        if response == 'y' || response == 'yes'
          if network_ollama = scan_network_for_ollama
            return network_ollama
          end
        end
      end
      
      # 3. Check AWS credentials for Bedrock
      if bedrock_available?
        puts "\n✅ AWS Bedrock is available" if @interactive
        puts "Defaulting to Claude 3 Haiku (fast & affordable)" if @interactive
        return { 
          provider: 'bedrock', 
          model: 'claude-3-haiku',
          source: 'aws',
          info: 'Using AWS Bedrock with Claude 3 Haiku model'
        }
      end
      
      # 4. Check for API keys
      available_providers = check_api_keys
      
      if available_providers.any?
        # Prefer certain providers based on cost/performance
        preferred_order = ['anthropic', 'openai', 'gemini', 'mistral', 'perplexity']
        
        selected = preferred_order.find { |p| available_providers.include?(p) }
        if selected
          model = default_model_for_provider(selected)
          puts "\n✅ Found API key for #{selected}" if @interactive
          return { provider: selected, model: model, source: 'api_key' }
        end
      end
      
      # 5. No providers available
      if @interactive
        show_setup_instructions
      end
      
      # Default to Ollama anyway (user might start it later)
      { provider: 'ollama', model: 'cogito:latest', source: 'fallback' }
    end
    
    private
    
    def ollama_available?(host = 'localhost', port = OLLAMA_DEFAULT_PORT)
      uri = URI("http://#{host}:#{port}/api/tags")
      
      begin
        http = Net::HTTP.new(uri.host, uri.port)
        http.open_timeout = 2
        http.read_timeout = 2
        
        response = http.get(uri)
        response.code == '200'
      rescue
        false
      end
    end
    
    def scan_network_for_ollama
      puts "\n🔍 Scanning local network for Ollama servers..."
      
      # Get local network info
      local_ip = get_local_ip
      return nil unless local_ip
      
      subnet = local_ip.split('.')[0..2].join('.')
      found_servers = []
      
      # Common Ollama ports
      ports = [11434, 11435, 8080]
      
      # Scan common IPs in parallel
      threads = []
      mutex = Mutex.new
      
      # Scan gateway and common server IPs
      ips_to_scan = [
        "#{subnet}.1",    # Gateway
        "#{subnet}.100",  # Common server range
        "#{subnet}.101",
        "#{subnet}.110",
        "#{subnet}.200",
      ]
      
      # Add current machine's IP neighbors
      current_last_octet = local_ip.split('.').last.to_i
      (-2..2).each do |offset|
        neighbor = current_last_octet + offset
        ips_to_scan << "#{subnet}.#{neighbor}" if neighbor > 0 && neighbor < 255
      end
      
      ips_to_scan.uniq.each do |ip|
        next if ip == local_ip # Skip self
        
        ports.each do |port|
          threads << Thread.new do
            if ollama_available?(ip, port)
              mutex.synchronize do
                found_servers << { host: ip, port: port }
                puts "  ✅ Found Ollama at #{ip}:#{port}"
              end
            end
          end
        end
      end
      
      # Wait for scan to complete
      threads.each { |t| t.join(NETWORK_SCAN_TIMEOUT) }
      
      if found_servers.empty?
        puts "❌ No Ollama servers found on local network"
        return nil
      end
      
      # If multiple found, let user choose
      selected = if found_servers.size == 1
        found_servers.first
      else
        puts "\nFound multiple Ollama servers:"
        found_servers.each_with_index do |server, i|
          puts "  #{i + 1}. #{server[:host]}:#{server[:port]}"
        end
        
        print "Select server (1-#{found_servers.size}): "
        choice = STDIN.gets.chomp.to_i - 1
        
        if choice >= 0 && choice < found_servers.size
          found_servers[choice]
        else
          found_servers.first
        end
      end
      
      {
        provider: 'ollama',
        model: 'cogito:latest',
        source: 'network',
        url: "http://#{selected[:host]}:#{selected[:port]}",
        info: "Using network Ollama at #{selected[:host]}:#{selected[:port]}"
      }
    end
    
    def get_local_ip
      begin
        # Create a UDP socket and connect to an external IP
        # This doesn't actually send data but helps determine local IP
        Socket.ip_address_list.detect do |intf|
          intf.ipv4_private?
        end&.ip_address
      rescue
        nil
      end
    end
    
    def bedrock_available?
      # Check for AWS credentials
      return false unless ENV['AWS_ACCESS_KEY_ID'] || ENV['AWS_PROFILE'] || File.exist?(File.expand_path('~/.aws/credentials'))
      
      # Try to initialize Bedrock client
      begin
        require 'aws-sdk-bedrockruntime'
        client = Aws::BedrockRuntime::Client.new(region: ENV['AWS_REGION'] || 'us-east-1')
        
        # Quick check - list models doesn't require permissions
        true
      rescue LoadError
        # AWS SDK not available
        false
      rescue Aws::Errors::MissingCredentialsError
        false
      rescue
        # Assume available if we have creds but can't verify
        true
      end
    end
    
    def check_api_keys
      available = []
      
      providers = {
        'openai' => 'OPENAI_API_KEY',
        'anthropic' => 'ANTHROPIC_API_KEY',
        'gemini' => 'GEMINI_API_KEY',
        'mistral' => 'MISTRAL_API_KEY',
        'perplexity' => 'PERPLEXITY_API_KEY'
      }
      
      providers.each do |provider, env_var|
        if ENV[env_var] || (@secret_manager && has_secret_key?(provider))
          available << provider
        end
      end
      
      available
    end
    
    def has_secret_key?(provider)
      begin
        @secret_manager.get_api_key(provider)
        true
      rescue
        false
      end
    end
    
    def default_model_for_provider(provider)
      case provider
      when 'openai'
        'gpt-4o-mini'  # Affordable default
      when 'anthropic'
        'claude-3-haiku-20240307'  # Fast and cheap
      when 'gemini'
        'gemini-1.5-flash'  # Fast variant
      when 'mistral'
        'mistral-small-latest'
      when 'perplexity'
        'llama-3.1-sonar-small-128k-online'
      else
        'default'
      end
    end
    
    def show_setup_instructions
      puts <<~INSTRUCTIONS
        
        ⚠️  No AI providers are currently available.
        
        To get started, you can:
        
        1. Install Ollama (recommended for local AI):
           • Mac/Linux: curl -fsSL https://ollama.com/install.sh | sh
           • Then run: ollama pull cogito
        
        2. Use AWS Bedrock:
           • Configure AWS credentials: aws configure
           • Ensure you have Bedrock access in your AWS account
        
        3. Use cloud providers with API keys:
           • Set environment variables:
             - export OPENAI_API_KEY="your-key"
             - export ANTHROPIC_API_KEY="your-key"
             - export GEMINI_API_KEY="your-key"
        
        For now, defaulting to Ollama (you can start it later).
        
      INSTRUCTIONS
    end
  end
  
  # Provider initialization helper
  class SmartProviderManager
    def self.initialize_with_detection(secret_manager, tool_manager, options = {})
      detector_result = ProviderDetector.detect_best_provider(
        secret_manager: secret_manager,
        interactive: options[:interactive] != false
      )
      
      # Create provider manager
      provider_manager = ProviderManager.new(secret_manager, tool_manager)
      
      # Configure based on detection
      case detector_result[:source]
      when 'network'
        # Update Ollama URL for network server
        if detector_result[:url]
          provider_manager.providers['ollama'].instance_variable_set(:@base_url, detector_result[:url])
        end
      when 'aws', 'api_key'
        # Switch to detected provider
        provider_manager.switch_provider(detector_result[:provider], detector_result[:model])
      end
      
      # Return both manager and detection info
      {
        provider_manager: provider_manager,
        detection_info: detector_result
      }
    end
  end
end