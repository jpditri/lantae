module Lantae
  class SidePanelManager
    def self.generate_side_content(options = {})
      content = []
      
      # Command queue overview (if available)
      if options[:commands]
        running = options[:commands].values.count { |c| c[:status] == :running }
        queued  = options[:commands].values.count { |c| c[:status] == :queued }
        # ASCII mini table for queue status
        col1, col2 = 11, 8
        content << "\e[1;34m📋 Command Queue\e[0m"
        content << "┌#{'─'*col1}┬#{'─'*col2}┐"
        content << "│ Status#{' '*(col1-6)}│ Count#{' '*(col2-5)}│"
        content << "├#{'─'*col1}┼#{'─'*col2}┤"
        content << "│ Running#{' '*(col1-7)}│#{running.to_s.rjust(col2)}│"
        content << "│ Queued #{' '*(col1-7)}│#{queued.to_s.rjust(col2)}│"
        content << "└#{'─'*col1}┴#{'─'*col2}┘"
        content << ""
      end
      
      # Current session info
      content << "\e[1;33m📊 Session Info\e[0m"
      content << "Provider: #{options[:provider] || 'unknown'}"
      content << "Model: #{options[:model] || 'unknown'}"
      content << "Temperature: #{options[:temperature] || 0.1}"
      content << ""
      
      # Conversation stats
      if options[:conversation]
        total_messages = options[:conversation].length
        user_messages = options[:conversation].count { |msg| msg[:role] == 'user' }
        assistant_messages = options[:conversation].count { |msg| msg[:role] == 'assistant' }
        
        content << "\e[1;36m💬 Conversation\e[0m"
        content << "Total: #{total_messages} messages"
        content << "User: #{user_messages}"
        content << "Assistant: #{assistant_messages}"
        content << ""
      end
      
      # Cost tracking (if available)
      if options[:cost_info]
        content << "\e[1;32m💰 Usage\e[0m"
        content << "Tokens: #{options[:cost_info][:tokens] || 'N/A'}"
        content << "Cost: $#{options[:cost_info][:cost] || '0.00'}"
        content << ""
      end
      
      # Tools status (if available)
      if options[:tools_available]
        content << "\e[1;35m🔧 Tools\e[0m"
        if options[:tools_available].any?
          options[:tools_available].first(3).each do |tool|
            content << "• #{tool}"
          end
          if options[:tools_available].length > 3
            content << "• +#{options[:tools_available].length - 3} more"
          end
        else
          content << "No tools available"
        end
        content << ""
      end
      
      # Recent commands or shortcuts
      content << "\e[1;37m⌨️  Quick Commands\e[0m"
      content << "/help - Show commands"
      content << "/model - Switch model"
      content << "/provider - Switch provider"
      content << "/clear - Clear conversation"
      content << "/side - Toggle side panel"
      content << ""
      
      # System info
      content << "\e[1;90m🖥️  System\e[0m"
      content << "Terminal: #{IO.console&.winsize&.join('x') || 'unknown'}"
      content << "Time: #{Time.now.strftime('%H:%M:%S')}"
      
      content.join("\n")
    end
    
    def self.generate_help_content
      content = []
      
      content << "\e[1;33m📖 Available Commands\e[0m"
      content << ""
      content << "\e[1mConversation:\e[0m"
      content << "/clear - Clear history"
      content << "/save - Save conversation"
      content << "/load - Load conversation"
      content << ""
      content << "\e[1mConfiguration:\e[0m"
      content << "/model <name> - Switch model"
      content << "/provider <name> - Switch provider"
      content << "/temp <value> - Set temperature"
      content << ""
      content << "\e[1mTools:\e[0m"
      content << "/tools - List available tools"
      content << "/tool <name> - Execute tool"
      content << ""
      content << "\e[1mInterface:\e[0m"
      content << "/side   - Toggle side panel"
      content << "/split  - Toggle split screen"
      content << "/stream - Toggle token streaming output"
      content << "/help   - Show this help"
      content << "/quit   - Exit lantae"
      
      content.join("\n")
    end
    
    def self.generate_model_info_content(options = {})
      content = []
      
      content << "\e[1;36m🤖 Model Information\e[0m"
      content << ""
      content << "Current: #{options[:model] || 'unknown'}"
      content << "Provider: #{options[:provider] || 'unknown'}"
      content << ""
      
      if options[:model_info]
        content << "\e[1mCapabilities:\e[0m"
        if options[:model_info][:supports_tools]
          content << "✓ Tools supported"
        else
          content << "✗ No tool support"
        end
        
        if options[:model_info][:supports_streaming]
          content << "✓ Streaming supported"
        else
          content << "✗ No streaming"
        end
        
        content << ""
        content << "\e[1mLimits:\e[0m"
        content << "Context: #{options[:model_info][:context_window] || 'unknown'}"
        content << "Max tokens: #{options[:model_info][:max_tokens] || 'unknown'}"
      end
      
      if options[:available_models]
        content << ""
        content << "\e[1mAvailable Models:\e[0m"
        options[:available_models].first(5).each do |model|
          marker = model == options[:model] ? "▶ " : "  "
          content << "#{marker}#{model}"
        end
        if options[:available_models].length > 5
          content << "  +#{options[:available_models].length - 5} more"
        end
      end
      
      content.join("\n")
    end
  end
end