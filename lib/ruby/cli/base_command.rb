module Lantae
  module CLI
    # Base class for all CLI commands
    class BaseCommand
      attr_reader :name, :description, :usage

      def initialize(name, description, usage = nil)
        @name = name
        @description = description
        @usage = usage || "#{name} [arguments]"
      end

      # Abstract method that must be implemented by subclasses
      def execute(args, context = {})
        raise NotImplementedError, "#{self.class} must implement #execute"
      end

      # Override for commands that support tab completion
      def complete(args, context = {})
        []
      end

      # Validate arguments - override if needed
      def validate_args(args)
        true
      end

      # Show help for this command
      def help
        puts "Usage: /#{@usage}"
        puts "Description: #{@description}"
      end

      protected

      # Common output formatting
      def success(message)
        puts "✅ #{message}"
      end

      def error(message)
        puts "❌ #{message}"
      end

      def warning(message)
        puts "⚠️  #{message}"
      end

      def info(message)
        puts "ℹ️  #{message}"
      end

      # Parse key=value arguments into hash
      def parse_key_value_args(args)
        result = {}
        args.each do |arg|
          if arg.include?('=')
            key, value = arg.split('=', 2)
            result[key.to_sym] = value
          end
        end
        result
      end

      # Check if required arguments are present
      def require_args(args, min_count, usage_example = nil)
        if args.length < min_count
          error("Insufficient arguments. Expected at least #{min_count}, got #{args.length}")
          puts "Usage: #{usage_example}" if usage_example
          return false
        end
        true
      end
    end
  end
end