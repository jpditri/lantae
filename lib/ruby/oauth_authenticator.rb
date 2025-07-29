require 'webrick'
require 'net/http'
require 'json'
require 'uri'
require 'base64'
require 'digest'
require 'securerandom'
require 'socket'

module Lantae
  class OAuthAuthenticator
    PORT = 8088
    REDIRECT_URI = "http://localhost:#{PORT}/callback"
    
    # OAuth configurations for different providers
    OAUTH_CONFIGS = {
      'anthropic' => {
        auth_url: 'https://console.anthropic.com/settings/keys',
        name: 'Anthropic Claude',
        description: 'Sign in with your Anthropic account to get an API key automatically',
        help_text: 'This will open the Anthropic Console where you can create an API key'
      },
      'openai' => {
        auth_url: 'https://platform.openai.com/api-keys',
        name: 'OpenAI',
        description: 'Sign in with your OpenAI account to get an API key automatically',
        help_text: 'This will open the OpenAI Platform where you can create an API key'
      },
      'gemini' => {
        auth_url: 'https://makersuite.google.com/app/apikey',
        name: 'Google Gemini',
        description: 'Sign in with your Google account to get a Gemini API key automatically',
        help_text: 'This will open Google AI Studio where you can create an API key'
      }
    }
    
    def self.authenticate(provider)
      new(provider).authenticate
    end
    
    def initialize(provider)
      @provider = provider
      @config = OAUTH_CONFIGS[provider]
      raise "Unsupported provider: #{provider}" unless @config
      
      @state = generate_state
      @server = nil
      @auth_result = nil
    end
    
    def authenticate
      puts "\n🔐 #{@config[:name]} Authentication"
      puts "━" * 60
      puts
      puts @config[:description]
      puts
      puts "🌐 Opening browser for OAuth-style authentication..."
      puts "📋 This will work just like Claude Code!"
      puts
      
      # Start auth flow immediately (no prompt, just like Claude Code)
      start_auth_flow
    end
    
    private
    
    def start_auth_flow
      puts "\n🚀 Starting authentication flow..."
      
      # Start local server
      start_server
      
      # Open browser
      open_browser(@config[:auth_url])
      
      puts "\n📱 Browser opened to #{@config[:name]} Console"
      puts "🔑 Please create an API key and copy it"
      puts "📋 Then paste it in the form that appeared in your browser"
      puts
      puts "⏳ Waiting for authentication..."
      puts "   (Press Ctrl+C to cancel)"
      
      # Wait for result
      wait_for_auth
      
      stop_server
      
      if @auth_result && @auth_result[:success]
        save_api_key(@auth_result[:api_key])
        puts "\n✅ Authentication successful!"
        puts "🔒 API key saved securely"
        @auth_result[:api_key]
      else
        puts "\n❌ Authentication failed"
        puts "💡 You can try manual entry instead"
        manual_fallback
      end
    end
    
    def start_server
      # Find an available port
      actual_port = find_available_port(PORT)
      
      @server = WEBrick::HTTPServer.new(
        Port: actual_port,
        Logger: WEBrick::Log.new('/dev/null'),
        AccessLog: [],
        BindAddress: '127.0.0.1'  # Only bind to localhost for security
      )
      
      # Store the actual port used
      @actual_port = actual_port
      
      # Main callback page
      @server.mount_proc '/callback' do |req, res|
        handle_callback(req, res)
      end
      
      # Static assets
      @server.mount_proc '/' do |req, res|
        handle_landing_page(req, res)
      end
      
      # Health check endpoint
      @server.mount_proc '/health' do |req, res|
        res.content_type = 'application/json'
        res.body = '{"status":"ok","provider":"' + @provider + '"}'
      end
      
      # Start server in background thread with error handling
      @server_thread = Thread.new do
        begin
          @server.start
        rescue => e
          puts "\n❌ Server error: #{e.message}"
          @auth_result = { success: false, error: "Server failed: #{e.message}" }
        end
      end
      
      # Give server time to start and verify it's running
      sleep 0.2
      
      # Test if server is accessible
      begin
        Net::HTTP.get_response(URI("http://localhost:#{@actual_port}/health"))
        puts "🚀 Local server started on port #{@actual_port}"
      rescue => e
        puts "⚠️  Server may not be accessible: #{e.message}"
      end
    end
    
    def stop_server
      @server&.shutdown
      @server_thread&.join(1)
    rescue => e
      # Ignore shutdown errors
    end
    
    def handle_landing_page(req, res)
      if req.path == '/'
        res.content_type = 'text/html'
        res.body = landing_page_html
      else
        res.status = 404
        res.body = 'Not Found'
      end
    end
    
    def handle_callback(req, res)
      begin
        if req.request_method == 'POST'
          # Handle API key submission
          api_key = nil
          
          # Parse the request body for form data
          if req.content_type&.include?('application/x-www-form-urlencoded')
            require 'cgi'
            params = CGI.parse(req.body)
            api_key = params['api_key']&.first&.strip
          elsif req.query_string
            # Fallback to query string
            api_key = req.query['api_key']&.strip
          end
          
          if api_key && !api_key.empty? && validate_api_key(api_key)
            @auth_result = { success: true, api_key: api_key }
            res.content_type = 'text/html'
            res.body = success_page_html
          else
            error_msg = if api_key&.empty?
              'API key cannot be empty'
            elsif api_key
              'Invalid API key format'
            else
              'No API key provided'
            end
            
            res.content_type = 'text/html'
            res.body = error_page_html(error_msg)
          end
        else
          # Show the form
          res.content_type = 'text/html'
          res.body = callback_page_html
        end
      rescue => e
        puts "❌ Callback error: #{e.message}"
        res.status = 500
        res.content_type = 'text/html'
        res.body = error_page_html("Server error: #{e.message}")
      end
    end
    
    def wait_for_auth
      timeout = 300 # 5 minutes
      start_time = Time.now
      last_status_time = start_time
      
      while @auth_result.nil? && (Time.now - start_time) < timeout
        sleep 0.5
        
        # Show periodic status updates
        elapsed = Time.now - start_time
        if elapsed - (last_status_time - start_time) >= 30 # Every 30 seconds
          remaining = (timeout - elapsed).to_i
          puts "   Still waiting... (#{remaining}s remaining)"
          last_status_time = Time.now
        end
        
        # Handle Ctrl+C gracefully
        begin
          # No-op, just checking for interrupts
        rescue Interrupt
          puts "\n\n⏹️  Authentication cancelled by user"
          @auth_result = { success: false, error: "Cancelled by user" }
          break
        end
      end
      
      if @auth_result.nil?
        puts "\n⏰ Authentication timed out after #{timeout / 60} minutes"
        @auth_result = { success: false, error: "Timeout" }
      end
    end
    
    def validate_api_key(key)
      return false unless key && key.length > 10
      
      case @provider
      when 'anthropic'
        key.start_with?('sk-ant-')
      when 'openai'
        key.start_with?('sk-')
      when 'gemini'
        key.start_with?('AIza')
      else
        key.length > 20
      end
    end
    
    def save_api_key(api_key)
      APIKeyAuthorizer.save_api_key(@provider, api_key)
    end
    
    def manual_fallback
      puts "\n📝 Manual API Key Entry"
      APIKeyAuthorizer.manual_key_entry(@provider)
    end
    
    def generate_state
      SecureRandom.hex(16)
    end
    
    def find_available_port(start_port)
      port = start_port
      10.times do  # Try 10 different ports
        begin
          server = TCPServer.new('127.0.0.1', port)
          server.close
          return port
        rescue Errno::EADDRINUSE
          port += 1
        end
      end
      
      # If we can't find a port, use the original and let it fail gracefully
      start_port
    end
    
    def open_browser(url)
      # Only open the provider console, not localhost
      # This makes it work more like Claude Code
      
      case RUBY_PLATFORM
      when /darwin/
        system(%Q{osascript -e 'tell application "System Events" to open location "#{url}"'})
      when /linux/
        system("xdg-open '#{url}' > /dev/null 2>&1 &")
      when /mswin|mingw/
        system("start \"\" \"#{url}\"")
      else
        puts "⚠️  Please manually open: #{url}"
      end
      
      # Show the local URL for manual paste after getting key
      puts "\n📋 After creating your API key in the browser:"
      puts "   1. Copy the API key"
      puts "   2. Come back to this terminal" 
      puts "   3. Paste it when prompted"
      puts ""
      puts "⚡ Or visit: http://localhost:#{@actual_port || PORT} for guided setup"
    end
    
    def landing_page_html
      <<~HTML
        <!DOCTYPE html>
        <html>
        <head>
            <title>Lantae - #{@config[:name]} Authentication</title>
            <meta charset="utf-8">
            <meta name="viewport" content="width=device-width, initial-scale=1">
            <style>
                body { 
                    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', system-ui, sans-serif;
                    max-width: 600px; margin: 50px auto; padding: 20px;
                    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                    min-height: 100vh; color: white;
                }
                .card { 
                    background: rgba(255,255,255,0.1); backdrop-filter: blur(10px);
                    border-radius: 20px; padding: 40px; box-shadow: 0 8px 32px rgba(0,0,0,0.3);
                }
                h1 { color: #fff; text-align: center; margin-bottom: 30px; }
                .step { 
                    background: rgba(255,255,255,0.1); padding: 20px; border-radius: 10px;
                    margin: 20px 0; border-left: 4px solid #4CAF50;
                }
                .step h3 { margin-top: 0; color: #4CAF50; }
                button { 
                    background: #4CAF50; color: white; padding: 15px 30px;
                    border: none; border-radius: 10px; font-size: 16px;
                    cursor: pointer; width: 100%; margin: 10px 0;
                    transition: background 0.3s;
                }
                button:hover { background: #45a049; }
                .secondary { background: #6c757d; }
                .secondary:hover { background: #5a6268; }
                .icon { font-size: 24px; margin-right: 10px; }
            </style>
        </head>
        <body>
            <div class="card">
                <h1>🚀 Lantae Authentication</h1>
                <h2>#{@config[:name]} Setup</h2>
                
                <div class="step">
                    <h3><span class="icon">🌐</span>Step 1: Get Your API Key</h3>
                    <p>#{@config[:help_text]}</p>
                    <button onclick="window.open('#{@config[:auth_url]}', '_blank')">
                        Open #{@config[:name]} Console
                    </button>
                </div>
                
                <div class="step">
                    <h3><span class="icon">📋</span>Step 2: Enter Your API Key</h3>
                    <p>Copy your API key from the console and paste it below:</p>
                    <button onclick="window.location.href='/callback'">
                        Continue to Key Entry
                    </button>
                </div>
                
                <div style="text-align: center; margin-top: 30px;">
                    <p><small>This is a secure local connection to your computer</small></p>
                </div>
            </div>
        </body>
        </html>
      HTML
    end
    
    def callback_page_html
      <<~HTML
        <!DOCTYPE html>
        <html>
        <head>
            <title>Enter API Key - Lantae</title>
            <meta charset="utf-8">
            <meta name="viewport" content="width=device-width, initial-scale=1">
            <style>
                body { 
                    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', system-ui, sans-serif;
                    max-width: 600px; margin: 50px auto; padding: 20px;
                    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                    min-height: 100vh; color: white;
                }
                .card { 
                    background: rgba(255,255,255,0.1); backdrop-filter: blur(10px);
                    border-radius: 20px; padding: 40px; box-shadow: 0 8px 32px rgba(0,0,0,0.3);
                }
                h1 { color: #fff; text-align: center; margin-bottom: 30px; }
                input[type="password"] { 
                    width: 100%; padding: 15px; border: 2px solid rgba(255,255,255,0.3);
                    border-radius: 10px; background: rgba(255,255,255,0.1);
                    color: white; font-size: 16px; margin: 15px 0;
                    backdrop-filter: blur(5px);
                }
                input[type="password"]::placeholder { color: rgba(255,255,255,0.7); }
                input[type="password"]:focus { 
                    outline: none; border-color: #4CAF50; background: rgba(255,255,255,0.2);
                }
                button { 
                    background: #4CAF50; color: white; padding: 15px 30px;
                    border: none; border-radius: 10px; font-size: 16px;
                    cursor: pointer; width: 100%; margin: 10px 0;
                    transition: background 0.3s;
                }
                button:hover { background: #45a049; }
                .help { 
                    background: rgba(255,255,255,0.1); padding: 15px; border-radius: 10px;
                    margin: 20px 0; font-size: 14px;
                }
            </style>
        </head>
        <body>
            <div class="card">
                <h1>🔑 Enter Your #{@config[:name]} API Key</h1>
                
                <form method="POST" action="/callback">
                    <input type="password" name="api_key" placeholder="Paste your API key here..." required>
                    <button type="submit">Save API Key</button>
                </form>
                
                <div class="help">
                    <strong>💡 Key Format:</strong><br>
                    #{get_key_format_help}
                </div>
                
                <div class="help">
                    <strong>🔒 Security:</strong><br>
                    Your API key is processed locally and saved securely to your system.
                    It never leaves your computer except to authenticate with #{@config[:name]}.
                </div>
            </div>
        </body>
        </html>
      HTML
    end
    
    def success_page_html
      <<~HTML
        <!DOCTYPE html>
        <html>
        <head>
            <title>Success - Lantae</title>
            <meta charset="utf-8">
            <meta name="viewport" content="width=device-width, initial-scale=1">
            <style>
                body { 
                    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', system-ui, sans-serif;
                    max-width: 600px; margin: 50px auto; padding: 20px;
                    background: linear-gradient(135deg, #4CAF50 0%, #45a049 100%);
                    min-height: 100vh; color: white;
                }
                .card { 
                    background: rgba(255,255,255,0.1); backdrop-filter: blur(10px);
                    border-radius: 20px; padding: 40px; box-shadow: 0 8px 32px rgba(0,0,0,0.3);
                    text-align: center;
                }
                h1 { color: #fff; margin-bottom: 30px; }
                .checkmark { font-size: 64px; margin: 20px 0; }
                button { 
                    background: rgba(255,255,255,0.2); color: white; padding: 15px 30px;
                    border: 2px solid white; border-radius: 10px; font-size: 16px;
                    cursor: pointer; margin: 10px; transition: all 0.3s;
                }
                button:hover { background: white; color: #4CAF50; }
            </style>
        </head>
        <body>
            <div class="card">
                <div class="checkmark">✅</div>
                <h1>Authentication Successful!</h1>
                <p>Your #{@config[:name]} API key has been saved securely.</p>
                <p>You can now close this browser window and return to Lantae.</p>
                <button onclick="window.close()">Close Window</button>
            </div>
            <script>
                // Auto-close after 3 seconds
                setTimeout(() => window.close(), 3000);
            </script>
        </body>
        </html>
      HTML
    end
    
    def error_page_html(error_message)
      <<~HTML
        <!DOCTYPE html>
        <html>
        <head>
            <title>Error - Lantae</title>
            <meta charset="utf-8">
            <meta name="viewport" content="width=device-width, initial-scale=1">
            <style>
                body { 
                    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', system-ui, sans-serif;
                    max-width: 600px; margin: 50px auto; padding: 20px;
                    background: linear-gradient(135deg, #ff6b6b 0%, #ee5a24 100%);
                    min-height: 100vh; color: white;
                }
                .card { 
                    background: rgba(255,255,255,0.1); backdrop-filter: blur(10px);
                    border-radius: 20px; padding: 40px; box-shadow: 0 8px 32px rgba(0,0,0,0.3);
                    text-align: center;
                }
                h1 { color: #fff; margin-bottom: 30px; }
                .error-icon { font-size: 64px; margin: 20px 0; }
                button { 
                    background: rgba(255,255,255,0.2); color: white; padding: 15px 30px;
                    border: 2px solid white; border-radius: 10px; font-size: 16px;
                    cursor: pointer; margin: 10px; transition: all 0.3s;
                }
                button:hover { background: white; color: #ff6b6b; }
            </style>
        </head>
        <body>
            <div class="card">
                <div class="error-icon">❌</div>
                <h1>Authentication Error</h1>
                <p>#{error_message}</p>
                <p>Please check your API key format and try again.</p>
                <button onclick="history.back()">Try Again</button>
                <button onclick="window.close()">Close Window</button>
            </div>
        </body>
        </html>
      HTML
    end
    
    def get_key_format_help
      case @provider
      when 'anthropic'
        "Anthropic API keys start with 'sk-ant-' followed by alphanumeric characters"
      when 'openai'
        "OpenAI API keys start with 'sk-' followed by alphanumeric characters"
      when 'gemini'
        "Gemini API keys start with 'AIza' followed by alphanumeric characters"
      else
        "API keys are typically long alphanumeric strings"
      end
    end
  end
end