require 'io/console'

module Lantae
  class StableUI
    def initialize
      @terminal_width = IO.console&.winsize&.[](1) || 80
      @terminal_height = IO.console&.winsize&.[](0) || 24
      @last_prompt_line = nil
      @header_height = 0
    end
    
    # ANSI escape codes
    CLEAR_LINE = "\e[2K"
    CURSOR_UP = "\e[A"
    CURSOR_DOWN = "\e[B"
    CURSOR_HOME = "\e[H"
    SAVE_CURSOR = "\e[s"
    RESTORE_CURSOR = "\e[u"
    HIDE_CURSOR = "\e[?25l"
    SHOW_CURSOR = "\e[?25h"
    
    def setup_screen(header_text = nil)
      # Clear screen once at start
      print "\e[2J\e[H"
      
      if header_text
        print_header(header_text)
        @header_height = header_text.lines.count + 2  # +2 for borders
      end
      
      # Position for input line
      @input_line = @header_height + 2
      move_to_input_line
    end
    
    def print_header(text)
      # Print header at top of screen
      print "\e[H"  # Move to top
      puts text
    end
    
    def move_to_input_line
      print "\e[#{@input_line};1H"
    end
    
    def clear_input_line
      print "\e[#{@input_line};1H#{CLEAR_LINE}"
    end
    
    def update_status_line(text)
      # Save current position
      print SAVE_CURSOR
      
      # Move to status line (just above input)
      status_line = @input_line - 1
      print "\e[#{status_line};1H#{CLEAR_LINE}"
      print text
      
      # Restore position
      print RESTORE_CURSOR
    end
    
    def append_output(text, preserve_prompt = true)
      if preserve_prompt
        # Save current line content
        print SAVE_CURSOR
        
        # Move up to insert new content
        print "\e[#{@input_line - 1};1H"
        
        # Insert new lines
        text.lines.each do |line|
          print "\n#{line.chomp}"
        end
        
        # Restore prompt position
        print RESTORE_CURSOR
      else
        puts text
      end
    end
    
    def show_command_result(command_id, status, output)
      # Create a compact result display
      print SAVE_CURSOR
      
      # Move to output area
      output_line = @input_line - 2
      print "\e[#{output_line};1H"
      
      # Clear and show result
      print CLEAR_LINE
      case status
      when :completed
        print "\e[32m✓\e[0m [#{command_id}] "
      when :failed
        print "\e[31m✗\e[0m [#{command_id}] "
      else
        print "\e[33m…\e[0m [#{command_id}] "
      end
      
      # Show truncated output
      max_width = @terminal_width - 15
      truncated = output.lines.first&.strip&.slice(0, max_width) || ""
      print truncated
      
      print RESTORE_CURSOR
    end
    
    def with_side_panel(main_content, side_content)
      # Calculate widths
      side_width = 30
      main_width = @terminal_width - side_width - 3  # -3 for borders
      
      # Split content into lines
      main_lines = wrap_text(main_content, main_width).lines
      side_lines = side_content.lines
      
      # Ensure we have enough lines
      max_lines = [main_lines.length, side_lines.length].max
      
      # Build combined output
      output = []
      max_lines.times do |i|
        main_line = (main_lines[i] || "").chomp.ljust(main_width)
        side_line = (side_lines[i] || "").chomp.ljust(side_width - 2)
        output << "#{main_line} │ #{side_line}"
      end
      
      output.join("\n")
    end
    
    def update_side_panel(content)
      # Update just the side panel without redrawing everything
      print SAVE_CURSOR
      
      # Move to side panel area
      side_start_col = @terminal_width - 30
      content.lines.each_with_index do |line, i|
        print "\e[#{@header_height + i + 1};#{side_start_col}H"
        print " │ #{line.chomp.ljust(28)}"
      end
      
      print RESTORE_CURSOR
    end
    
    private
    
    def wrap_text(text, width)
      text.lines.map do |line|
        if line.length <= width
          line
        else
          line.scan(/.{1,#{width}}/).join("\n")
        end
      end.join
    end
  end
end