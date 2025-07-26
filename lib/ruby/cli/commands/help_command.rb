require_relative '../base_command'

module Lantae
  module CLI
    module Commands
      class HelpCommand < BaseCommand
        def initialize
          super('help', 'Show available commands and their usage')
        end

        def execute(args, context = {})
          command_registry = context[:command_registry]
          
          if args.empty?
            show_general_help(command_registry)
          else
            show_command_help(args[0], command_registry)
          end
        end

        private

        def show_general_help(command_registry)
          puts <<~HELP
            Available commands:
            
            #{format_commands(command_registry)}
            
            Usage:
              /command [arguments]       - Execute a command
              /help [command]           - Show help for specific command
              exit or quit              - End the session
          HELP
        end

        def show_command_help(command_name, command_registry)
          command = command_registry.get_command(command_name)
          
          if command
            command.help
          else
            error("Unknown command: #{command_name}")
            puts "Type '/help' to see available commands."
          end
        end

        def format_commands(command_registry)
          commands = command_registry.list_commands
          
          # Group commands by category for better organization
          categories = {
            'Model & Provider' => %w[model provider models],
            'Tools & Execution' => %w[tool tools],
            'Language Server' => %w[lsp],
            'Agent System' => %w[agent],
            'MCP Integration' => %w[mcp],
            'Session Management' => %w[clear info env],
            'Help & Information' => %w[help]
          }
          
          output = []
          
          categories.each do |category, command_names|
            output << "  #{category}:"
            command_names.each do |name|
              command = commands[name]
              next unless command
              output << "    /#{command.name.ljust(20)} - #{command.description}"
            end
            output << ""
          end
          
          output.join("\n")
        end
      end
    end
  end
end