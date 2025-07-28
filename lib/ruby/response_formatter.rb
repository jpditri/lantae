require 'io/console'

module Lantae
  class ResponseFormatter
    def self.format_response(response, options = {})
      # Get terminal width
      terminal_width = IO.console&.winsize&.last || 80
      
      # Configure formatting options
      max_line_width = options[:max_width] || [terminal_width - 4, 80].min
      indent = options[:indent] || "  "
      show_markdown = options[:markdown] != false
      
      lines = []
      
      # Process the response
      response.split("\n").each do |line|
        # Handle code blocks
        if line.match(/^```/)
          lines << line
          next
        end
        
        # Handle headers in markdown
        if show_markdown && line.match(/^#+\s/)
          lines << ""
          lines << "\e[1;36m#{line}\e[0m"  # Bold cyan for headers
          lines << ""
          next
        end
        
        # Handle bullet points
        if line.match(/^\s*[-*]\s/)
          wrapped = wrap_text(line, max_line_width - 2)
          wrapped.each_with_index do |wrapped_line, idx|
            if idx == 0
              lines << wrapped_line
            else
              lines << "  #{wrapped_line}"  # Indent continuation
            end
          end
          next
        end
        
        # Handle numbered lists
        if line.match(/^\s*\d+\.\s/)
          wrapped = wrap_text(line, max_line_width - 3)
          wrapped.each_with_index do |wrapped_line, idx|
            if idx == 0
              lines << wrapped_line
            else
              lines << "   #{wrapped_line}"  # Indent continuation
            end
          end
          next
        end
        
        # Regular text - wrap it
        if line.strip.empty?
          lines << ""
        else
          wrapped = wrap_text(line, max_line_width)
          lines.concat(wrapped)
        end
      end
      
      # Add response box if requested
      if options[:boxed]
        format_boxed_response(lines, terminal_width)
      else
        lines.join("\n")
      end
    end
    
    def self.format_boxed_response(lines, terminal_width)
      max_width = [terminal_width - 6, 100].min
      
      output = []
      output << "\n\e[34m╭─ AI Response " + "─" * (max_width - 14) + "╮\e[0m"
      
      lines.each do |line|
        # Remove ANSI codes for length calculation
        clean_line = line.gsub(/\e\[[0-9;]*m/, '')
        padding = max_width - clean_line.length
        output << "\e[34m│\e[0m #{line}#{' ' * [padding, 0].max} \e[34m│\e[0m"
      end
      
      output << "\e[34m╰" + "─" * (max_width + 2) + "╯\e[0m\n"
      output.join("\n")
    end
    
    def self.wrap_text(text, max_width)
      return [text] if text.length <= max_width
      
      words = text.split(' ')
      lines = []
      current_line = ''
      
      words.each do |word|
        if current_line.empty?
          current_line = word
        elsif (current_line + ' ' + word).length <= max_width
          current_line += ' ' + word
        else
          lines << current_line
          current_line = word
        end
      end
      
      lines << current_line unless current_line.empty?
      lines
    end
    
    # Format tool output differently
    def self.format_tool_output(tool_name, output, success = true)
      terminal_width = IO.console&.winsize&.last || 80
      max_width = [terminal_width - 6, 100].min
      
      status_color = success ? "\e[32m" : "\e[31m"  # Green for success, red for failure
      status_text = success ? "✓ Success" : "✗ Failed"
      
      lines = output.split("\n").map { |line| wrap_text(line, max_width - 4) }.flatten
      
      result = []
      result << "\n#{status_color}╭─ Tool: #{tool_name} #{status_text} " + "─" * (max_width - tool_name.length - status_text.length - 10) + "╮\e[0m"
      
      lines.each do |line|
        padding = max_width - line.length
        result << "#{status_color}│\e[0m #{line}#{' ' * [padding, 0].max} #{status_color}│\e[0m"
      end
      
      result << "#{status_color}╰" + "─" * (max_width + 2) + "╯\e[0m"
      result.join("\n")
    end
    
    # Create a simple side panel display
    def self.with_side_panel(main_content, side_content, options = {})
      terminal_width = IO.console&.winsize&.last || 80
      terminal_height = IO.console&.winsize&.first || 24
      
      # Only show side panel if terminal is wide enough
      return main_content if terminal_width < 100
      
      split_ratio = options[:split_ratio] || 0.7
      main_width = (terminal_width * split_ratio).to_i
      side_width = terminal_width - main_width - 3
      
      main_lines = main_content.split("\n")
      side_lines = side_content.split("\n")
      
      # Ensure we have enough lines
      max_lines = [main_lines.length, side_lines.length].max
      
      output = []
      (0...max_lines).each do |i|
        main_line = main_lines[i] || ""
        side_line = side_lines[i] || ""
        
        # Truncate lines if needed
        main_line = main_line[0...main_width-1] if main_line.length > main_width-1
        side_line = side_line[0...side_width-1] if side_line.length > side_width-1
        
        # Pad main line
        main_padding = main_width - main_line.gsub(/\e\[[0-9;]*m/, '').length
        padded_main = main_line + (' ' * [main_padding, 0].max)
        
        # Add separator and side content
        if i == 0
          output << "#{padded_main} \e[90m│\e[0m \e[33m#{side_line}\e[0m"
        else
          output << "#{padded_main} \e[90m│\e[0m #{side_line}"
        end
      end
      
      output.join("\n")
    end
  end
end