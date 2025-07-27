require_relative '../base_command'
require_relative '../../prompt_templates'

module Lantae
  module CLI
    module Commands
      class TemplateCommand < BaseCommand
        def initialize
          super('template', 'Manage prompt templates and snippets')
          @template_manager = PromptTemplateManager.new
        end
        
        def execute(args, context)
          subcommand = args.shift
          
          case subcommand
          when 'save'
            save_template(args)
          when 'use'
            use_template(args, context)
          when 'list'
            list_templates
          when 'delete'
            delete_template(args)
          when 'snippet'
            handle_snippet(args, context)
          else
            show_help
          end
        end
        
        def complete(args, context)
          subcommand = args.first
          
          if args.empty?
            %w[save use list delete snippet]
          elsif args.length == 1
            %w[save use list delete snippet].select { |cmd| cmd.start_with?(subcommand) }
          elsif args.length == 2
            case subcommand
            when 'use', 'delete'
              @template_manager.list_templates.map { |t| t[:name] }
            when 'snippet'
              %w[save use list delete]
            else
              []
            end
          elsif subcommand == 'snippet' && args.length == 3
            snippet_cmd = args[1]
            if %w[use delete].include?(snippet_cmd)
              @template_manager.list_snippets.map { |s| s[:name] }
            else
              []
            end
          else
            []
          end
        end
        
        private
        
        def save_template(args)
          name = args.shift
          
          if name.nil? || name.empty?
            puts "Usage: /template save <name>"
            puts "Then enter the template content. Use <%= variable %> for placeholders."
            return
          end
          
          puts "Enter template content (end with '###' on a new line):"
          
          template_lines = []
          loop do
            line = STDIN.gets
            break if line.strip == '###'
            template_lines << line
          end
          
          template_content = template_lines.join
          
          # Ask for metadata
          print "Enter keywords (comma-separated, optional): "
          keywords = STDIN.gets.chomp.split(',').map(&:strip)
          
          metadata = {}
          metadata[:keywords] = keywords if keywords.any?
          
          @template_manager.save_template(name, template_content, metadata)
          puts "✅ Template '#{name}' saved"
          
          # Show detected variables
          template = @template_manager.get_template(name)
          if template[:variables].any?
            puts "   Variables: #{template[:variables].join(', ')}"
          end
        end
        
        def use_template(args, context)
          name = args.shift
          
          if name.nil? || name.empty?
            puts "Usage: /template use <name>"
            return
          end
          
          begin
            template = @template_manager.get_template(name)
            
            # Collect variable values
            variables = {}
            template[:variables].each do |var|
              print "Enter value for '#{var}': "
              value = STDIN.gets.chomp
              variables[var.to_sym] = value
            end
            
            # Render template
            rendered = @template_manager.render_template(name, variables)
            
            # Add to conversation
            if context[:conversation]
              context[:conversation] << { role: 'user', content: rendered }
              
              # Process as regular input
              response = context[:provider_manager].chat(context[:conversation], context[:options] || {})
              context[:conversation] << { role: 'assistant', content: response }
              puts response
            else
              puts "Rendered template:"
              puts rendered
            end
            
          rescue => e
            puts "❌ #{e.message}"
          end
        end
        
        def list_templates
          templates = @template_manager.list_templates
          
          if templates.empty?
            puts "No templates saved yet."
          else
            puts "Available templates:"
            templates.each do |template|
              puts "  • #{template[:name]}"
              puts "    Variables: #{template[:variables].join(', ')}" if template[:variables].any?
              puts "    Usage count: #{template[:usage_count]}"
              puts "    Created: #{template[:created_at]}"
            end
          end
        end
        
        def delete_template(args)
          name = args.first
          
          if name.nil? || name.empty?
            puts "Usage: /template delete <name>"
            return
          end
          
          @template_manager.delete_template(name)
          puts "✅ Template '#{name}' deleted"
        end
        
        def handle_snippet(args, context)
          snippet_command = args.shift
          
          case snippet_command
          when 'save'
            save_snippet(args)
          when 'use'
            use_snippet(args, context)
          when 'list'
            list_snippets
          when 'delete'
            delete_snippet(args)
          else
            puts <<~HELP
              Snippet Commands:
                /template snippet save <name>    - Save a text snippet
                /template snippet use <name>     - Insert a snippet
                /template snippet list           - List all snippets
                /template snippet delete <name>  - Delete a snippet
            HELP
          end
        end
        
        def save_snippet(args)
          name = args.first
          
          if name.nil? || name.empty?
            puts "Usage: /template snippet save <name>"
            return
          end
          
          puts "Enter snippet content (end with '###' on a new line):"
          
          snippet_lines = []
          loop do
            line = STDIN.gets
            break if line.strip == '###'
            snippet_lines << line
          end
          
          content = snippet_lines.join
          
          @template_manager.save_snippet(name, content)
          puts "✅ Snippet '#{name}' saved"
        end
        
        def use_snippet(args, context)
          name = args.first
          
          if name.nil? || name.empty?
            puts "Usage: /template snippet use <name>"
            return
          end
          
          begin
            content = @template_manager.get_snippet(name)
            
            # Add to conversation or display
            if context[:conversation]
              context[:conversation] << { role: 'user', content: content }
              
              response = context[:provider_manager].chat(context[:conversation], context[:options] || {})
              context[:conversation] << { role: 'assistant', content: response }
              puts response
            else
              puts content
            end
            
          rescue => e
            puts "❌ #{e.message}"
          end
        end
        
        def list_snippets
          snippets = @template_manager.list_snippets
          
          if snippets.empty?
            puts "No snippets saved yet."
          else
            puts "Available snippets:"
            snippets.each do |snippet|
              puts "  • #{snippet[:name]}"
              puts "    Preview: #{snippet[:preview]}"
              puts "    Usage count: #{snippet[:usage_count]}"
            end
          end
        end
        
        def delete_snippet(args)
          name = args.first
          
          if name.nil? || name.empty?
            puts "Usage: /template snippet delete <name>"
            return
          end
          
          @template_manager.delete_snippet(name)
          puts "✅ Snippet '#{name}' deleted"
        end
        
        def show_help
          puts <<~HELP
            Template Commands:
              /template save <name>        - Save a new template
              /template use <name>         - Use a template
              /template list               - List all templates
              /template delete <name>      - Delete a template
              /template snippet <command>  - Manage snippets
              
            Templates support ERB syntax for variables:
              <%= variable_name %>
              
            Example:
              /template save code_review
              Please review this <%= language %> code:
              ```<%= language %>
              <%= code %>
              ```
          HELP
        end
      end
    end
  end
end