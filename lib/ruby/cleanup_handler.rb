require 'readline'

module Lantae
  class CleanupHandler
    @original_state = {}
    @cleanup_registered = false
    
    class << self
      attr_reader :original_state
      
      def register_cleanup
        return if @cleanup_registered
        
        # Store original state
        store_original_state
        
        # Register cleanup handlers
        at_exit { cleanup_on_exit }
        
        # Handle interrupt signals gracefully
        trap('INT') { cleanup_and_exit }
        trap('TERM') { cleanup_and_exit }
        
        @cleanup_registered = true
      end
      
      def store_original_state
        @original_state[:readline_completion_proc] = Readline.completion_proc
        @original_state[:readline_completion_append_character] = Readline.completion_append_character
        @original_state[:winch_handler] = Signal.trap('WINCH', 'IGNORE')
        
        # Store terminal state only if we're in a terminal
        if STDIN.tty?
          @original_state[:terminal_echo] = system('stty -a | grep -q "echo" 2>/dev/null')
        else
          @original_state[:terminal_echo] = false
        end
      end
      
      def cleanup_on_exit
        restore_readline_state
        restore_terminal_state
        restore_signal_handlers
        cleanup_temp_files
      end
      
      def cleanup_and_exit
        cleanup_on_exit
        exit(0)
      end
      
      private
      
      def restore_readline_state
        # Restore original Readline configuration
        Readline.completion_proc = @original_state[:readline_completion_proc]
        Readline.completion_append_character = @original_state[:readline_completion_append_character]
      rescue => e
        # Silently handle any restoration errors
        warn "Warning: Could not restore Readline state: #{e.message}" if ENV['DEBUG']
      end
      
      def restore_terminal_state
        # Only run stty commands if we're in a terminal
        if STDIN.tty?
          # Ensure terminal echo is restored
          system('stty echo 2>/dev/null')
          
          # Reset terminal to sane state
          system('stty sane 2>/dev/null')
        end
        
        # Clear any remaining terminal escape sequences (works for all outputs)
        print "\e[0m" # Reset all formatting
        print "\e[?25h" # Show cursor
      rescue => e
        warn "Warning: Could not restore terminal state: #{e.message}" if ENV['DEBUG']
      end
      
      def restore_signal_handlers
        # Restore original signal handlers
        Signal.trap('WINCH', @original_state[:winch_handler] || 'DEFAULT')
      rescue => e
        warn "Warning: Could not restore signal handlers: #{e.message}" if ENV['DEBUG']
      end
      
      def cleanup_temp_files
        # Clean up any temporary files that might have been created
        temp_files = [
          File.expand_path('~/.lantae_session'),
          '/tmp/lantae_*'
        ]
        
        temp_files.each do |pattern|
          Dir.glob(pattern).each do |file|
            File.delete(file) if File.exist?(file)
          end
        end
      rescue => e
        warn "Warning: Could not clean up temp files: #{e.message}" if ENV['DEBUG']
      end
    end
  end
end