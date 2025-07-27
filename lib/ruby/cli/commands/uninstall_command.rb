require_relative '../base_command'

module Lantae
  module CLI
    module Commands
      class UninstallCommand < BaseCommand
        def name
          'uninstall'
        end

        def description
          'Uninstall Lantae and optionally associated components'
        end

        def help
          <<~HELP
            Usage: /uninstall [options]
            
            Uninstall Lantae and optionally associated components like Ruby, Node.js, and Ollama.
            
            Options:
              --confirm        Skip confirmation prompt
              --app-only       Remove only Lantae app (keep Ruby, Node.js, Ollama)
              --complete       Remove everything (Lantae, Ruby, Node.js, Ollama)
              --help           Show this help message
            
            Examples:
              /uninstall                # Interactive uninstall with prompts
              /uninstall --app-only     # Remove only Lantae application
              /uninstall --complete     # Remove everything
              /uninstall --confirm      # Skip confirmation prompts
            
            Note: This will run the uninstall.sh script with appropriate options.
          HELP
        end

        def execute(args, context = {})
          # Parse arguments
          options = parse_args(args)
          
          if options[:help]
            puts help
            return
          end
          
          # Find the uninstall script
          script_path = find_uninstall_script
          
          unless script_path
            puts "❌ Uninstall script not found. Please run uninstall.sh manually from the project directory."
            return
          end
          
          # Show warning
          puts "🗑️  Lantae Uninstall"
          puts ""
          puts "⚠️  This will remove Lantae components from your system."
          puts "   Log will be saved to: #{File.dirname(script_path)}/uninstall.log"
          puts ""
          
          # Confirm unless --confirm flag is used
          unless options[:confirm]
            print "Continue with uninstall? (y/N): "
            response = $stdin.gets.chomp
            unless response.downcase.start_with?('y')
              puts "❌ Uninstall cancelled."
              return
            end
          end
          
          # Build command based on options
          cmd_args = []
          
          if options[:app_only]
            cmd_args << "--app-only"
          elsif options[:complete]
            cmd_args << "--complete"
          end
          
          cmd_args << "--confirm" if options[:confirm]
          
          # Execute uninstall script
          puts "🧹 Starting uninstall process..."
          puts ""
          
          begin
            # Change to script directory and execute
            Dir.chdir(File.dirname(script_path)) do
              if cmd_args.empty?
                # Interactive mode
                system(script_path)
              else
                # With arguments
                system(script_path, *cmd_args)
              end
            end
            
            if $?.success?
              puts ""
              puts "✅ Uninstall completed successfully."
              puts "👋 Thank you for using Lantae!"
              puts ""
              puts "ℹ️  This CLI session will now exit."
              exit(0)
            else
              puts ""
              puts "❌ Uninstall script returned an error."
              puts "   Check the uninstall.log for details."
            end
          rescue => e
            puts "❌ Error running uninstall script: #{e.message}"
          end
        end

        def complete(args, context = {})
          # Completion for uninstall command arguments
          options = ['--help', '--confirm', '--app-only', '--complete']
          
          if args.empty?
            options
          else
            last_arg = args.last
            options.select { |opt| opt.start_with?(last_arg) }
          end
        end

        private

        def parse_args(args)
          options = {
            help: false,
            confirm: false,
            app_only: false,
            complete: false
          }
          
          args.each do |arg|
            case arg
            when '--help', '-h'
              options[:help] = true
            when '--confirm', '-y'
              options[:confirm] = true
            when '--app-only'
              options[:app_only] = true
            when '--complete'
              options[:complete] = true
            end
          end
          
          options
        end

        def find_uninstall_script
          # Try to find uninstall.sh in various locations
          possible_paths = [
            # Current directory
            './uninstall.sh',
            # Parent directory (if running from bin)
            '../uninstall.sh',
            # Project root (if CLI is in lib/ruby structure)
            '../../uninstall.sh',
            # Absolute path based on this file's location
            File.join(File.dirname(__FILE__), '../../../../uninstall.sh')
          ]
          
          possible_paths.each do |path|
            full_path = File.expand_path(path)
            return full_path if File.exist?(full_path) && File.executable?(full_path)
          end
          
          nil
        end
      end
    end
  end
end