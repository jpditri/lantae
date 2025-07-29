require 'io/console'
require 'thread'

module Lantae
  class SplitScreenUI
    attr_reader :command_queue, :output_buffer
    attr_accessor :completion_provider
    
    def initialize(options = {})
      @command_queue = Queue.new
      @output_buffer = []
      @output_mutex = Mutex.new
      @running = false
      @terminal_width, @terminal_height = IO.console.winsize
      @split_ratio = options[:split_ratio] || 0.6  # 60% for main, 40% for right panel
      @max_output_lines = options[:max_output_lines] || 100
      @max_queue_display = options[:max_queue_display] || 10
      
      # UI components
      @input_line = ""
      @cursor_pos = 0
      @scroll_offset = 0
      
      # Completion state
      @completion_provider = nil
      @completion_mode = false
      @completion_matches = []
      @completion_index = 0
      @completion_start_pos = 0
      @completion_prefix = ""
      
      # Command history
      @history = []
      @history_index = 0
      
      # Set up signal handlers for terminal resize
      # Store original handler so we can restore it later
      @original_winch_handler = trap('WINCH') { handle_resize }
    end
    
    def start
      @running = true
      clear_screen
      draw_ui
      
      # Start input handling in main thread
      Thread.current[:ui] = self
    end
    
    def stop
      @running = false
      restore_terminal
      
      # Restore original signal handler
      begin
        trap('WINCH', @original_winch_handler || 'DEFAULT')
      rescue
        # Ignore signal restoration errors
      end
    end
    
    def add_output(text, type = :info)
      @output_mutex.synchronize do
        timestamp = Time.now.strftime("%H:%M:%S")
        @output_buffer << { time: timestamp, text: text, type: type }
        
        # Limit buffer size
        if @output_buffer.size > @max_output_lines
          @output_buffer.shift
        end
      end
      
      draw_right_panel if @running
    end
    
    def queue_command(command)
      @command_queue << command
      draw_right_panel
    end
    
    def get_next_command
      @command_queue.pop(true) rescue nil
    end
    
    def handle_input(char)
      case char
      when "\t"  # Tab
        handle_tab_completion
      when "\r", "\n"  # Enter
        if @completion_mode
          apply_completion
          @completion_mode = false
          draw_input_area
        elsif @input_line.strip.length > 0
          command = @input_line.strip
          @history << command
          @history_index = @history.length
          @input_line = ""
          @cursor_pos = 0
          draw_input_area
          return command
        end
      when "\x7F", "\b"  # Backspace
        @completion_mode = false
        if @cursor_pos > 0
          @input_line = @input_line[0...@cursor_pos-1] + @input_line[@cursor_pos..-1]
          @cursor_pos -= 1
          draw_input_area
        end
      when "\e"  # Escape sequence
        if @completion_mode
          @completion_mode = false
          draw_input_area
        else
          handle_escape_sequence
        end
      when /\A[\x20-\x7E]\z/  # Printable characters
        @completion_mode = false
        @input_line = @input_line[0...@cursor_pos] + char + @input_line[@cursor_pos..-1]
        @cursor_pos += 1
        draw_input_area
      end
      
      nil
    end
    
    def draw_ui
      return unless @running
      
      clear_screen
      draw_borders
      draw_input_area
      draw_right_panel
      position_cursor
    end
    
    private
    
    def clear_screen
      print "\e[2J\e[H"
    end
    
    def draw_borders
      main_width = (@terminal_width * @split_ratio).to_i
      
      # Draw top border
      print "\e[1;1H"
      print "─" * @terminal_width
      
      # Draw vertical separator
      (2..@terminal_height-3).each do |row|
        print "\e[#{row};#{main_width}H│"
      end
      
      # Draw horizontal separator for input area
      print "\e[#{@terminal_height-2};1H"
      print "─" * main_width
      print "\e[#{@terminal_height-2};#{main_width}H┴"
      print "─" * (@terminal_width - main_width)
      
      # Draw section headers
      print "\e[1;2H[ Command Input ]"
      print "\e[1;#{main_width+2}H[ Queue & Output ]"
      print "\e[#{@terminal_height-2};2H[ Input ]"
    end
    
    def draw_input_area
      main_width = (@terminal_width * @split_ratio).to_i
      
      # Clear input line
      print "\e[#{@terminal_height-1};1H"
      print " " * (main_width - 1)
      
      # Draw prompt and input
      print "\e[#{@terminal_height-1};2H"
      prompt = "> "
      print prompt
      
      # Calculate visible portion of input
      available_width = main_width - 4
      if @input_line.length > available_width
        visible_start = [@cursor_pos - available_width + 10, 0].max
        visible_text = @input_line[visible_start, available_width]
        print visible_text
      else
        print @input_line
      end
      
      # Draw completion hint if in tab completion
      if @completion_mode && @completion_matches.any?
        hint_y = @terminal_height - 3
        print "\e[#{hint_y};2H"
        print " " * (main_width - 2)
        print "\e[#{hint_y};3H"
        print "\e[90mTab: select | ↑↓: navigate | Enter: confirm | Esc: cancel\e[0m"
      end
    end
    
    def draw_right_panel
      @output_mutex.synchronize do
        main_width = (@terminal_width * @split_ratio).to_i
        panel_width = @terminal_width - main_width - 1
        
        # Clear right panel
        (2..@terminal_height-3).each do |row|
          print "\e[#{row};#{main_width+1}H"
          print " " * panel_width
        end
        
        current_row = 2
        
        # Draw command queue
        print "\e[#{current_row};#{main_width+2}H\e[33m═ Command Queue ═\e[0m"
        current_row += 1
        
        queue_items = []
        begin
          # Non-blocking peek at queue items
          temp_queue = []
          while !@command_queue.empty? && temp_queue.size < @max_queue_display
            item = @command_queue.pop(true)
            temp_queue << item
            queue_items << item
          end
          # Put items back
          temp_queue.each { |item| @command_queue << item }
        rescue ThreadError
          # Queue is empty
        end
        
        if queue_items.empty?
          print "\e[#{current_row};#{main_width+3}H\e[90m(empty)\e[0m"
          current_row += 1
        else
          queue_items.each_with_index do |cmd, idx|
            break if current_row >= @terminal_height - 4
            print "\e[#{current_row};#{main_width+3}H"
            cmd_display = "#{idx+1}. #{cmd}"
            if cmd_display.length > panel_width - 4
              cmd_display = cmd_display[0, panel_width-7] + "..."
            end
            print "\e[36m#{cmd_display}\e[0m"
            current_row += 1
          end
        end
        
        # Draw output section
        current_row += 1
        if current_row < @terminal_height - 3
          print "\e[#{current_row};#{main_width+2}H\e[33m═ Output ═\e[0m"
          current_row += 1
          
          # Calculate how many output lines we can show
          available_lines = @terminal_height - current_row - 2
          
          if @output_buffer.empty?
            print "\e[#{current_row};#{main_width+3}H\e[90m(no output)\e[0m"
          else
            # Show most recent outputs
            display_items = @output_buffer.last(available_lines)
            
            display_items.each do |item|
              break if current_row >= @terminal_height - 2
              
              print "\e[#{current_row};#{main_width+3}H"
              
              # Color based on type
              color = case item[:type]
                      when :error then "\e[31m"
                      when :success then "\e[32m"
                      when :warning then "\e[33m"
                      else "\e[0m"
                      end
              
              output_text = "[#{item[:time]}] #{item[:text]}"
              if output_text.length > panel_width - 4
                output_text = output_text[0, panel_width-7] + "..."
              end
              
              print "#{color}#{output_text}\e[0m"
              current_row += 1
            end
          end
        end
      end
    end
    
    def position_cursor
      main_width = (@terminal_width * @split_ratio).to_i
      prompt_length = 2
      
      # Position cursor in input area
      cursor_x = prompt_length + @cursor_pos + 1
      if @input_line.length > main_width - 4
        visible_start = [@cursor_pos - (main_width - 4) + 10, 0].max
        cursor_x = prompt_length + (@cursor_pos - visible_start) + 1
      end
      
      print "\e[#{@terminal_height-1};#{cursor_x}H"
    end
    
    def handle_escape_sequence
      seq = STDIN.read_nonblock(10) rescue ""
      
      case seq
      when "[D"  # Left arrow
        if @completion_mode
          @completion_index = (@completion_index - 1) % @completion_matches.length
          draw_completion_menu
        elsif @cursor_pos > 0
          @cursor_pos -= 1
          draw_input_area
        end
      when "[C"  # Right arrow
        if @completion_mode
          @completion_index = (@completion_index + 1) % @completion_matches.length
          draw_completion_menu
        elsif @cursor_pos < @input_line.length
          @cursor_pos += 1
          draw_input_area
        end
      when "[A"  # Up arrow
        if @completion_mode
          @completion_index = (@completion_index - 1) % @completion_matches.length
          draw_completion_menu
        elsif @history_index > 0
          @history_index -= 1
          @input_line = @history[@history_index] || ""
          @cursor_pos = @input_line.length
          draw_input_area
        end
      when "[B"  # Down arrow
        if @completion_mode
          @completion_index = (@completion_index + 1) % @completion_matches.length
          draw_completion_menu
        elsif @history_index < @history.length
          @history_index += 1
          @input_line = @history[@history_index] || ""
          @cursor_pos = @input_line.length
          draw_input_area
        end
      when "[H", "[1~"  # Home
        @cursor_pos = 0
        draw_input_area
      when "[F", "[4~"  # End
        @cursor_pos = @input_line.length
        draw_input_area
      end
    end
    
    def handle_tab_completion
      return unless @completion_provider
      
      # Parse current input to determine context
      if @input_line =~ /^\/(\w+)\s*(.*?)$/
        command = $1
        args = $2
        
        case command
        when 'provider'
          complete_provider(args)
        when 'model'
          complete_model(args)
        else
          # Complete command itself if partial
          if args.empty? && command.length > 0
            complete_command(command)
          end
        end
      elsif @input_line.start_with?('/')
        # Complete slash commands
        complete_command(@input_line[1..-1])
      end
    end
    
    def complete_provider(partial)
      providers = @completion_provider.get_providers
      matches = providers.select { |p| p.start_with?(partial) }
      
      if matches.empty?
        return
      elsif matches.length == 1
        # Single match - complete it
        @input_line = "/provider #{matches[0]}"
        @cursor_pos = @input_line.length
        draw_input_area
      else
        # Multiple matches - show menu
        @completion_mode = true
        @completion_matches = matches
        @completion_index = 0
        @completion_prefix = "/provider "
        @completion_start_pos = @completion_prefix.length
        draw_completion_menu
      end
    end
    
    def complete_model(partial)
      models = @completion_provider.get_models
      matches = models.select { |m| m.downcase.include?(partial.downcase) }
      
      if matches.empty?
        return
      elsif matches.length == 1
        # Single match - complete it
        @input_line = "/model #{matches[0]}"
        @cursor_pos = @input_line.length
        draw_input_area
      else
        # Multiple matches - show menu
        @completion_mode = true
        @completion_matches = matches
        @completion_index = 0
        @completion_prefix = "/model "
        @completion_start_pos = @completion_prefix.length
        draw_completion_menu
      end
    end
    
    def complete_command(partial)
      commands = %w[help model provider models tool tools mcp clear info env lsp agent squad task status cancel]
      matches = commands.select { |c| c.start_with?(partial) }
      
      if matches.empty?
        return
      elsif matches.length == 1
        # Single match - complete it
        @input_line = "/#{matches[0]} "
        @cursor_pos = @input_line.length
        draw_input_area
      else
        # Multiple matches - show menu
        @completion_mode = true
        @completion_matches = matches.map { |c| "/#{c}" }
        @completion_index = 0
        @completion_prefix = ""
        @completion_start_pos = 0
        draw_completion_menu
      end
    end
    
    def apply_completion
      if @completion_mode && @completion_matches.any?
        selected = @completion_matches[@completion_index]
        @input_line = @completion_prefix + selected
        @cursor_pos = @input_line.length
        
        # Add space after provider/model selection for convenience
        if @input_line =~ /^\/(provider|model)\s+\w+$/
          @input_line += " "
          @cursor_pos += 1
        end
      end
    end
    
    def draw_completion_menu
      return unless @completion_mode && @completion_matches.any?
      
      main_width = (@terminal_width * @split_ratio).to_i
      
      # Calculate menu position (above input line)
      menu_y = @terminal_height - 2 - [@completion_matches.length, 10].min
      
      # Clear menu area
      [@completion_matches.length, 10].min.times do |i|
        print "\e[#{menu_y + i};2H"
        print " " * (main_width - 2)
      end
      
      # Draw matches
      @completion_matches.each_with_index do |match, idx|
        break if idx >= 10  # Limit display
        
        print "\e[#{menu_y + idx};3H"
        if idx == @completion_index
          print "\e[7m"  # Reverse video for selection
        end
        
        display_text = match
        if display_text.length > main_width - 6
          display_text = display_text[0, main_width - 9] + "..."
        end
        
        print display_text
        print "\e[0m" if idx == @completion_index
      end
      
      # Restore cursor position
      position_cursor
    end
    
    def handle_resize
      @terminal_width, @terminal_height = IO.console.winsize
      draw_ui
    end
    
    def restore_terminal
      clear_screen
      print "\e[?25h"  # Show cursor
    end
  end
  
  # Command processor that works with the UI
  class CommandProcessor
    def initialize(ui, provider_manager, tool_manager)
      @ui = ui
      @provider_manager = provider_manager
      @tool_manager = tool_manager
      @processing = false
      @worker_thread = nil
    end
    
    def start
      @processing = true
      @worker_thread = Thread.new { process_queue }
    end
    
    def stop
      @processing = false
      @worker_thread&.join
    end
    
    private
    
    def process_queue
      while @processing
        command = @ui.command_queue.pop(true) rescue nil
        
        if command
          @ui.add_output("Processing: #{command}", :info)
          
          begin
            # Process the command
            result = execute_command(command)
            @ui.add_output("✓ #{command}", :success)
          rescue => e
            @ui.add_output("✗ #{command}: #{e.message}", :error)
          end
        else
          sleep 0.1
        end
      end
    end
    
    def execute_command(command)
      # This is where you'd integrate with the existing command handling
      # For now, just a placeholder
      if command.start_with?('/')
        handle_slash_command(command)
      else
        # Send to AI provider
        @provider_manager.chat([{role: 'user', content: command}])
      end
    end
    
    def handle_slash_command(command)
      # Simplified slash command handling
      parts = command[1..-1].split(' ')
      cmd = parts[0]
      
      case cmd
      when 'models'
        models = @provider_manager.list_models
        @ui.add_output("Available models: #{models.join(', ')}", :info)
      when 'provider'
        provider = parts[1]
        @provider_manager.switch_provider(provider) if provider
        @ui.add_output("Switched to provider: #{provider}", :success)
      else
        @ui.add_output("Unknown command: #{command}", :warning)
      end
    end
  end
end