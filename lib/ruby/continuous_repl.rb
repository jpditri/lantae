require 'io/console'
require 'thread'
require 'time'
require 'timeout'
require_relative 'async_repl'
require_relative 'ollama_discovery'
require_relative 'providers/ollama_provider'

module Lantae
  class ContinuousREPL < AsyncREPL
    def initialize(provider_manager, tool_manager, options = {})
      super(provider_manager, tool_manager, options)
      
      # Continuous typing state
      @input_buffer = ""
      @cursor_position = 0
      @input_thread = nil
      @display_thread = nil
      @running = true
      @raw_mode = false
      
      # Display management
      @terminal_width = `tput cols`.to_i rescue 80
      @terminal_height = `tput lines`.to_i rescue 24
      @input_line = @terminal_height - 1
      @status_line = @terminal_height - 2
      @response_start_line = 5
      
      # Input synchronization
      @input_mutex = Mutex.new
      @display_mutex = Mutex.new
      
      # Message queue for continuous input
      @message_queue = Queue.new
      @queue_thread = nil
      
      # Processing state
      @ai_thinking = false
      @current_response = ""
    end
    
    def start
      print_header if @show_banner
      check_api_key_availability
      
      # Enable continuous mode
      start_continuous_mode
    end
    
    # Override setup_autocomplete to prevent conflicts
    def setup_autocomplete
      # Disable readline autocomplete in continuous mode since we use raw input
    end
    
    private
    
    def start_continuous_mode
      puts "\n🚀 Starting continuous typing mode..."
      puts "Type freely while AI thinks. Press Ctrl+C to exit.\n"
      
      setup_terminal
      start_threads
      
      begin
        main_loop
      rescue Interrupt
        puts "\n\nExiting continuous mode..."
      ensure
        cleanup_continuous
      end
    end
    
    def setup_terminal
      # Only enable raw mode if STDIN is a TTY
      if STDIN.tty?
        @raw_mode = true
        STDIN.raw!
        
        # Hide cursor initially
        print "\e[?25l"
        
        # Clear screen and set up layout
        print "\e[2J\e[H"
        draw_initial_layout
      else
        puts "Continuous mode requires an interactive terminal"
        @running = false
      end
    end
    
    def cleanup_continuous
      @running = false
      
      # Wait for threads to finish
      [@input_thread, @display_thread, @queue_thread].compact.each do |thread|
        thread.join(1) # Wait up to 1 second
        thread.kill if thread.alive?
      end
      
      # Restore terminal
      if @raw_mode
        STDIN.cooked!
        print "\e[?25h" # Show cursor
        print "\n"
      end
      
      # Call parent cleanup
      cleanup
    end
    
    def draw_initial_layout
      # Draw status bar
      draw_status_line
      
      # Draw input prompt
      draw_input_line
      
      # Position cursor at input
      move_cursor_to_input
    end
    
    def draw_status_line
      @display_mutex.synchronize do
        print "\e[#{@status_line};1H"
        
        status = if @ai_thinking
          "🤖 AI is thinking... | Queue: #{@message_queue.size} | Type to queue next message"
        else
          "✅ Ready | Queue: #{@message_queue.size} | Type your message below"
        end
        
        # Clear line and draw status
        print "\e[2K\e[33m#{status}\e[0m"
      end
    end
    
    def draw_input_line
      @display_mutex.synchronize do
        print "\e[#{@input_line};1H"
        print "\e[2K> #{@input_buffer}"
        
        # Position cursor correctly
        cursor_col = 3 + @cursor_position
        print "\e[#{@input_line};#{cursor_col}H"
      end
    end
    
    def move_cursor_to_input
      cursor_col = 3 + @cursor_position
      print "\e[#{@input_line};#{cursor_col}H"
    end
    
    def start_threads
      # Input processing thread
      @input_thread = Thread.new { input_loop }
      
      # Display update thread
      @display_thread = Thread.new { display_loop }
      
      # Message queue processing thread
      @queue_thread = Thread.new { queue_processing_loop }
    end
    
    def main_loop
      # Main thread just waits and handles cleanup
      while @running
        sleep(0.1)
      end
    end
    
    def input_loop
      return unless @raw_mode
      
      while @running
        begin
          if STDIN.ready?
            char = STDIN.read_nonblock(1)
            handle_input_char(char)
          else
            sleep(0.01) # Small delay to prevent CPU spinning
          end
        rescue IO::WaitReadable
          sleep(0.01)
        rescue => e
          # Handle input errors gracefully
          break if e.is_a?(EOFError)
        end
      end
    end
    
    def handle_input_char(char)
      @input_mutex.synchronize do
        case char.ord
        when 3 # Ctrl+C
          @running = false
        when 13 # Enter
          handle_enter
        when 127 # Backspace
          handle_backspace
        when 27 # ESC sequence (arrow keys, etc.)
          handle_escape_sequence
        else
          # Regular character
          if char.ord >= 32 && char.ord <= 126 # Printable ASCII
            @input_buffer.insert(@cursor_position, char)
            @cursor_position += 1
            update_input_display
          end
        end
      end
    end
    
    def handle_enter
      message = @input_buffer.strip
      return if message.empty?
      
      # Handle special commands
      if message.start_with?('/')
        parts = message[1..-1].split(' ')
        cmd = parts[0]
        args = parts[1..-1].join(' ')
        
        case cmd
        when 'clear'
          clear_screen
          clear_input_and_update
          return
        when 'status'
          show_status
          clear_input_and_update
          return
        when 'exit', 'quit'
          @running = false
          return
        when 'help'
          show_help
          clear_input_and_update
          return
        when 'model'
          handle_model_command(args)
          clear_input_and_update
          return
        when 'provider'
          handle_provider_command(args)
          clear_input_and_update
          return
        when 'models'
          handle_models_command
          clear_input_and_update
          return
        when 'side'
          handle_side_command
          clear_input_and_update
          return
        when 'connect'
          handle_connect_command(args)
          clear_input_and_update
          return
        else
          show_unknown_command(cmd)
          clear_input_and_update
          return
        end
      end
      
      # Queue the message
      @message_queue.push(message)
      
      # Clear input buffer
      @input_buffer = ""
      @cursor_position = 0
      
      # Update display
      update_input_display
      draw_status_line
      
      # Show queued message
      show_queued_message(message)
    end
    
    def handle_backspace
      if @cursor_position > 0
        @input_buffer.slice!(@cursor_position - 1)
        @cursor_position -= 1
        update_input_display
      end
    end
    
    def handle_escape_sequence
      # Read additional characters for escape sequences
      begin
        if STDIN.ready?
          seq = STDIN.read_nonblock(2)
          case seq
          when '[C' # Right arrow
            @cursor_position = [@cursor_position + 1, @input_buffer.length].min
            move_cursor_to_input
          when '[D' # Left arrow
            @cursor_position = [@cursor_position - 1, 0].max
            move_cursor_to_input
          when '[A', '[B' # Up/Down arrows - could implement history
            # For now, ignore
          end
        end
      rescue IO::WaitReadable
        # Ignore incomplete sequences
      end
    end
    
    def clear_input_and_update
      # Clear input buffer
      @input_buffer = ""
      @cursor_position = 0
      
      # Update display
      update_input_display
    end
    
    def update_input_display
      draw_input_line
    end
    
    def display_loop
      while @running
        # Periodically update display elements
        draw_status_line
        sleep(0.1)
      end
    end
    
    def queue_processing_loop
      while @running
        begin
          # Wait for message from queue
          message = @message_queue.pop
          next if message.nil?
          
          # Mark AI as thinking
          @ai_thinking = true
          draw_status_line
          
          # Process the message
          process_queued_message(message)
          
          # Mark AI as done
          @ai_thinking = false
          draw_status_line
          
        rescue => e
          show_error("Queue processing error: #{e.message}")
          @ai_thinking = false
        end
      end
    end
    
    def show_queued_message(message)
      @display_mutex.synchronize do
        # Find a good place to show the queued message
        print "\e[#{@status_line - 1};1H"
        print "\e[2K\e[94m[QUEUED]\e[0m #{message}"
      end
    end
    
    def process_queued_message(message)
      begin
        # Add to conversation
        @conversation << { role: 'user', content: message }
        
        # Show processing indicator
        show_processing_message(message)
        
        # Get AI response
        response = @provider_manager.chat(@conversation, {})
        
        # Add response to conversation
        @conversation << { role: 'assistant', content: response }
        
        # Display response
        show_ai_response(response)
        
      rescue => e
        show_error("Error processing message: #{e.message}")
      end
    end
    
    def show_processing_message(message)
      @display_mutex.synchronize do
        print "\e[#{@response_start_line};1H"
        print "\e[2K\e[96m[PROCESSING]\e[0m #{message}"
      end
    end
    
    def show_ai_response(response)
      @display_mutex.synchronize do
        # Clear processing line
        print "\e[#{@response_start_line};1H\e[2K"
        
        # Show response in a box
        lines = response.split("\n")
        max_width = [@terminal_width - 4, 80].min
        
        print "\e[#{@response_start_line};1H"
        print "\e[34m╭─ AI Response " + "─" * (max_width - 14) + "╮\e[0m\n"
        
        lines.each_with_index do |line, idx|
          wrapped_lines = wrap_text(line, max_width - 4)
          wrapped_lines.each do |wrapped_line|
            print "\e[34m│\e[0m #{wrapped_line.ljust(max_width - 4)} \e[34m│\e[0m\n"
          end
        end
        
        print "\e[34m╰" + "─" * (max_width - 2) + "╯\e[0m\n"
      end
    end
    
    def wrap_text(text, width)
      return [""] if text.empty?
      
      words = text.split(' ')
      lines = []
      current_line = ""
      
      words.each do |word|
        if current_line.empty?
          current_line = word
        elsif (current_line + " " + word).length <= width
          current_line += " " + word
        else
          lines << current_line
          current_line = word
        end
      end
      
      lines << current_line unless current_line.empty?
      lines.empty? ? [""] : lines
    end
    
    def show_error(message)
      @display_mutex.synchronize do
        print "\e[#{@response_start_line};1H"
        print "\e[2K\e[31mError: #{message}\e[0m"
      end
    end
    
    def clear_screen
      print "\e[2J\e[H"
      draw_initial_layout
    end
    
    def show_status
      @display_mutex.synchronize do
        print "\e[#{@response_start_line};1H"
        print "\e[2K\e[33mStatus: Queue: #{@message_queue.size}, AI Thinking: #{@ai_thinking}\e[0m"
      end
    end
    
    def handle_model_command(args)
      begin
        if args.empty?
          current_model = @provider_manager.get_provider_info[:model]
          show_command_output("Current model: #{current_model}")
        else
          @provider_manager.current_model = args
          show_command_output("Model set to: #{args}")
        end
      rescue => e
        show_command_output("Error with model command: #{e.message}")
      end
    end
    
    def handle_provider_command(args)
      begin
        if args.empty?
          current_provider = @provider_manager.get_provider_info[:provider]
          show_command_output("Current provider: #{current_provider}")
        else
          parts = args.split(' ', 2)
          provider = parts[0]
          model = parts[1]
          
          begin
            @provider_manager.switch_provider(provider, model)
            info = @provider_manager.get_provider_info
            show_command_output("Switched to: #{info[:provider]} (#{info[:model]})")
          rescue => e
            show_command_output("Error switching provider: #{e.message}")
          end
        end
      rescue => e
        show_command_output("Error with provider command: #{e.message}")
      end
    end
    
    def handle_models_command
      begin
        models = @provider_manager.list_models
        provider_info = @provider_manager.get_provider_info
        provider_name = provider_info[:provider]
        
        output_lines = ["Available models for #{provider_name}:"]
        models.each { |model| output_lines << "  - #{model}" }
        
        show_command_output(output_lines.join("\n"))
      rescue => e
        show_command_output("Error listing models: #{e.message}")
      end
    end
    
    def handle_side_command
      # Since ContinuousREPL doesn't have side panel support, just show a message
      show_command_output("Side panel not supported in continuous mode")
    end
    
    def handle_connect_command(args)
      if args.empty? || args == 'scan'
        # Default: Quick scan for fast results
        show_command_output("🔍 Quick scan for Ollama instances...")
        
        Thread.new do
          begin
            show_command_output("DEBUG: Starting quick scan...")
            instances = Lantae::OllamaDiscovery.quick_scan(timeout: 2)
            show_command_output("DEBUG: Quick scan found #{instances.size} instances")
            display_scan_results(instances, "quick")
          rescue => e
            show_command_output("❌ Quick scan error: #{e.message}")
            show_command_output("    Stack trace: #{e.backtrace.first(3).join(', ')}")
            show_command_output("    Try: /connect <host:port> to connect manually")
          end
        end
        
      elsif args == 'full'
        # Full network scan
        show_command_output("🔍 Full network scan (this may take a few seconds)...")
        
        Thread.new do
          begin
            show_command_output("DEBUG: Starting full scan...")
            instances = Lantae::OllamaDiscovery.scan_local_network(timeout: 1, max_concurrent: 15)
            show_command_output("DEBUG: Full scan found #{instances.size} instances")
            display_scan_results(instances, "full")
          rescue => e
            show_command_output("❌ Full scan error: #{e.message}")
            show_command_output("    Stack trace: #{e.backtrace.first(3).join(', ')}")
            show_command_output("    Try: /connect <host:port> to connect manually")
          end
        end
          
      elsif args == 'list'
        # Show current connection info
        current_url = get_current_ollama_url
        info = Lantae::OllamaDiscovery.get_instance_info(current_url)
        
        output_lines = ["Current Ollama connection:"]
        output_lines << "  URL: #{current_url}"
        output_lines << "  Status: #{info[:status]}"
        output_lines << "  Version: #{info[:version]}"
        output_lines << "  Models: #{info[:model_count]} available"
        
        if info[:models].any?
          output_lines << "  Recent: #{info[:models].first(5).join(', ')}"
        end
        
        show_command_output(output_lines.join("\n"))
        
      else
        # Connect to specific instance
        connect_to_instance(args)
      end
    end
    
    def connect_to_instance(host_port)
      # Clean up the input
      host_port = host_port.to_s.strip
      
      # Remove common URL prefixes if present
      host_port = host_port.gsub(/^https?:\/\//, '')
      host_port = host_port.gsub(/^\/\//, '')
      
      # Validate input
      if host_port.empty?
        show_command_output("❌ Invalid input: host:port cannot be empty")
        return
      end
      
      # Parse host:port
      begin
        if host_port.include?(':')
          host, port_str = host_port.split(':', 2)
          port_str = port_str.strip
          
          # Check if port string is valid before converting
          if port_str.empty?
            show_command_output("❌ Port cannot be empty. Use format: host:port")
            return
          end
          
          if !port_str.match?(/^\d+$/)
            show_command_output("❌ Port must be numeric. Got: '#{port_str}'")
            return
          end
          
          port = port_str.to_i
        else
          host = host_port
          port = 11434
        end
        
        # Clean up host
        host = host.strip
        
        # Validate parsed values
        if host.empty?
          show_command_output("❌ Invalid host: cannot be empty")
          return
        end
        
        if port <= 0 || port > 65535
          show_command_output("❌ Invalid port: must be between 1 and 65535 (received: #{port})")
          return
        end
        
        url = "http://#{host}:#{port}"
        
        # Test connection with timeout
        show_command_output("🔄 Testing connection to #{url}...")
        
        # Use a separate thread for connection test to avoid blocking
        connection_thread = Thread.new do
          Lantae::OllamaDiscovery.check_ollama_instance(host, port, 3)
        end
        
        # Wait for connection test with timeout
        connected = false
        begin
          Timeout.timeout(5) do
            connected = connection_thread.value
          end
        rescue Timeout::Error
          connection_thread.kill
          show_command_output("❌ Connection test timed out after 5 seconds")
          return
        end
        
        if connected
          show_command_output("✅ Connection successful! Switching provider...")
          
          # Switch provider in a safe way
          begin
            # Create new provider without tool manager first (simpler)
            new_provider = Lantae::Providers::OllamaProvider.new(url)
            
            # Test that the provider works by listing models
            test_models = new_provider.list_models
            
            # If we get here, the provider works, so set the tool manager
            new_provider.set_tool_manager(@tool_manager) if @tool_manager
            
            # Store the actual provider instance in the provider manager
            # This is a workaround for the stub ProviderManager
            @provider_manager.instance_variable_set(:@ollama_provider, new_provider)
            
            # Monkey patch the chat method to use our real provider
            def @provider_manager.chat(prompt, options = {})
              if @current_provider == 'ollama' && @ollama_provider
                messages = prompt.is_a?(Array) ? prompt : [{ role: 'user', content: prompt }]
                @ollama_provider.chat(@current_model, messages, options)
              else
                # Fall back to original behavior
                super(prompt, options)
              end
            end
            
            # Monkey patch list_models to use our real provider
            def @provider_manager.list_models
              if @current_provider == 'ollama' && @ollama_provider
                @ollama_provider.list_models
              else
                # Fall back to original behavior
                super()
              end
            end
            
            output_lines = ["✅ Successfully connected to #{url}"]
            output_lines << "   Found #{test_models.size} models available"
            
            if test_models.any?
              # Set first model as default
              default_model = test_models.first
              @provider_manager.switch_provider('ollama', default_model)
              output_lines << "   Using model: #{default_model}"
            else
              # Just switch to ollama provider without changing model
              @provider_manager.switch_provider('ollama')
            end
            
            show_command_output(output_lines.join("\n"))
            
          rescue => e
            show_command_output("❌ Error switching provider: #{e.message}")
            show_command_output("   Error class: #{e.class}")
            show_command_output("   Backtrace: #{e.backtrace.first(3).join(', ')}")
            show_command_output("   Connection test passed but provider creation failed")
          end
        else
          show_command_output("❌ Cannot connect to #{url}")
          show_command_output("   Make sure Ollama is running and accessible")
        end
        
      rescue => e
        show_command_output("❌ Connection error: #{e.message}")
      end
    end
    
    def display_scan_results(instances, scan_type)
      if instances.empty?
        if scan_type == "quick"
          show_command_output("❌ No Ollama instances found in quick scan")
          show_command_output("    Try: /connect full    - for complete network scan")
        else
          show_command_output("❌ No Ollama instances found on local network")
        end
        show_command_output("    Or: /connect <host:port> - to connect manually")
      else
        scan_label = scan_type == "quick" ? "Quick scan" : "Full network scan"
        output_lines = ["✅ #{scan_label} found #{instances.size} Ollama instance(s):"]
        output_lines << ""
        
        instances.each_with_index do |instance, idx|
          current = get_current_ollama_url == instance[:url] ? " (current)" : ""
          model_list = instance[:models].empty? ? "no models" : 
                      "#{instance[:models].size} models"
          
          if instance[:models].any?
            sample_models = instance[:models].first(3).join(', ')
            model_list += " (#{sample_models}#{instance[:models].size > 3 ? '...' : ''})"
          end
          
          output_lines << "  #{idx + 1}. #{instance[:url]}#{current}"
          output_lines << "     #{model_list}"
        end
        
        output_lines << ""
        output_lines << "💡 Commands:"
        output_lines << "   /connect <host:port>  - Connect to specific instance"
        if scan_type == "quick"
          output_lines << "   /connect full         - Scan entire network"
        end
        output_lines << "   /connect list         - Show current connection"
        
        show_command_output(output_lines.join("\n"))
      end
    end
    
    def get_current_ollama_url
      # Try to get current Ollama URL from provider
      begin
        provider = @provider_manager.instance_variable_get(:@current_provider)
        if provider.respond_to?(:instance_variable_get)
          base_url = provider.instance_variable_get(:@base_url)
          return base_url if base_url
        end
      rescue
        # Fallback to default
      end
      
      "http://localhost:11434"
    end
    
    def show_unknown_command(cmd)
      show_command_output("Unknown command: /#{cmd}. Type /help for available commands.")
    end
    
    def show_command_output(text)
      lines = text.split("\n")
      print "\e[#{@response_start_line};1H"
      print "\e[J" # Clear from cursor to end of screen
      
      lines.each_with_index do |line, index|
        print "\e[#{@response_start_line + index};1H"
        print "\e[2K\e[33m#{line}\e[0m"
      end
      
      # Redraw the input line to make sure it's visible
      draw_input_line
    end
    
    def show_help
      @display_mutex.synchronize do
        help_text = [
          "Available Commands:",
          "  /help             - Show this help message",
          "  /clear            - Clear the screen",
          "  /status           - Show current status",
          "  /model [name]     - Show or set current model",
          "  /provider [name]  - Show or switch provider",
          "  /models           - List available models",
          "  /connect          - Quick scan for Ollama instances",
          "  /connect full     - Full network scan for instances", 
          "  /connect <host:port> - Connect to specific instance",
          "  /connect list     - Show current connection info",
          "  /side             - Side panel info (not supported in continuous mode)",
          "  /exit             - Exit the application",
          "  /quit             - Exit the application",
          "",
          "Regular text is sent to the AI for processing.",
          "Type freely while the AI processes your requests."
        ]
        
        print "\e[#{@response_start_line};1H"
        print "\e[J" # Clear from cursor to end of screen
        
        help_text.each_with_index do |line, index|
          print "\e[#{@response_start_line + index};1H"
          print "\e[2K\e[36m#{line}\e[0m"
        end
        
        # Move cursor back to input line
        print "\e[#{@input_line};1H"
      end
    end
  end
end