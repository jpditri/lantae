require_relative 'base_command'
require_relative 'commands/help_command'
require_relative 'commands/provider_command'
require_relative 'commands/model_command'
require_relative 'commands/lsp_command'

module Lantae
  module CLI
    class CommandRegistry
      attr_reader :commands

      def initialize
        @commands = {}
        @aliases = {}
        register_default_commands
      end

      def register_command(command)
        @commands[command.name] = command
        puts "Registered command: #{command.name}" if ENV['DEBUG']
      end

      def register_alias(alias_name, command_name)
        @aliases[alias_name] = command_name
      end

      def get_command(name)
        # Check aliases first
        actual_name = @aliases[name] || name
        @commands[actual_name]
      end

      def execute_command(command_line, context = {})
        return false if command_line.nil? || command_line.strip.empty?
        
        # Remove leading slash if present
        command_line = command_line[1..-1] if command_line.start_with?('/')
        
        parts = command_line.split(' ')
        command_name = parts[0]
        args = parts[1..-1] || []
        
        command = get_command(command_name)
        
        if command
          begin
            # Add command registry to context for commands that need it
            context = context.merge(command_registry: self)
            command.execute(args, context)
            true
          rescue => e
            puts "❌ Error executing command '#{command_name}': #{e.message}"
            puts e.backtrace.first(3).join("\n") if ENV['DEBUG']
            false
          end
        else
          puts "❌ Unknown command: /#{command_name}. Type '/help' for available commands."
          false
        end
      end

      def list_commands
        @commands
      end

      def complete_command(input, context = {})
        return [] if input.nil? || input.empty?
        
        # Remove leading slash if present
        input = input[1..-1] if input.start_with?('/')
        
        parts = input.split(' ')
        command_name = parts[0]
        args = parts[1..-1] || []
        
        if parts.length == 1 && !input.end_with?(' ')
          # Complete command name
          matching_commands = @commands.keys.select { |name| name.start_with?(command_name) }
          matching_aliases = @aliases.keys.select { |name| name.start_with?(command_name) }
          (matching_commands + matching_aliases).map { |cmd| "/#{cmd}" }
        else
          # Complete command arguments
          command = get_command(command_name)
          if command
            context = context.merge(command_registry: self)
            completions = command.complete(args, context)
            completions.map { |comp| "/#{command_name} #{comp}" }
          else
            []
          end
        end
      end

      def command_exists?(name)
        @commands.key?(name) || @aliases.key?(name)
      end

      def get_command_help(name)
        command = get_command(name)
        command&.help
      end

      private

      def register_default_commands
        # Core commands
        register_command(Commands::HelpCommand.new)
        register_command(Commands::ProviderCommand.new)
        register_command(Commands::ModelCommand.new)
        register_command(Commands::LspCommand.new)
        
        # Command aliases
        register_alias('h', 'help')
        register_alias('p', 'provider')
        register_alias('m', 'model')
        register_alias('l', 'lsp')
      end
    end
  end
end