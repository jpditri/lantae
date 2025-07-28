module Lantae
  module UIComponents
    def self.draw_panel(title, content, width: 80, color: "\e[96m")
      border_char = "═"
      vertical_char = "║"
      
      # Top border
      puts "#{color}╔#{border_char * (width - 2)}╗\e[0m"
      
      # Title
      if title
        title_padding = (width - title.length - 4) / 2
        puts "#{color}║#{' ' * title_padding}#{title}#{' ' * (width - title_padding - title.length - 2)}║\e[0m"
        puts "#{color}╠#{border_char * (width - 2)}╣\e[0m"
      end
      
      # Content
      content_lines = content.is_a?(Array) ? content : content.split("\n")
      content_lines.each do |line|
        # Strip ANSI codes for length calculation
        clean_line = line.gsub(/\e\[[0-9;]*m/, '')
        remaining_space = width - clean_line.length - 2
        
        if remaining_space >= 0
          puts "#{color}#{vertical_char}\e[0m #{line}#{' ' * remaining_space}#{color}#{vertical_char}\e[0m"
        else
          # Truncate if too long
          truncated = clean_line[0, width - 5] + "..."
          puts "#{color}#{vertical_char}\e[0m #{truncated} #{color}#{vertical_char}\e[0m"
        end
      end
      
      # Bottom border
      puts "#{color}╚#{border_char * (width - 2)}╝\e[0m"
    end
    
    def self.draw_command_status(command_id, status, command_text, provider: nil, model: nil, elapsed: nil)
      status_icon = case status
                    when :running then "\e[33m⏳\e[0m"
                    when :completed then "\e[32m✓\e[0m"
                    when :failed then "\e[31m✗\e[0m"
                    when :queued then "\e[94m🕐\e[0m"
                    else "?"
                    end
      
      status_text = case status
                    when :running then "\e[33mProcessing\e[0m"
                    when :completed then "\e[32mCompleted\e[0m"
                    when :failed then "\e[31mFailed\e[0m"
                    when :queued then "\e[94mQueued\e[0m"
                    else status.to_s
                    end
      
      lines = []
      lines << "#{status_icon} [#{command_id}] #{status_text}#{elapsed ? " (#{elapsed}s)" : ""}"
      lines << "   Command: #{command_text}"
      lines << "   Model: #{provider}/#{model}" if provider && model
      
      lines
    end
    
    def self.format_output_block(output, max_lines: 10)
      lines = output.split("\n")
      if lines.length > max_lines
        lines[0...max_lines] + ["... (#{lines.length - max_lines} more lines)"]
      else
        lines
      end
    end
    
    def self.terminal_width
      `tput cols`.to_i rescue 80
    end
    
    def self.clear_screen
      system('clear') || system('cls')
    end
  end
end