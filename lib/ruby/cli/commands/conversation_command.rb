require_relative '../base_command'
require_relative '../../conversation_manager'

module Lantae
  module CLI
    module Commands
      class ConversationCommand < BaseCommand
        def initialize
          super('conversation', 'Manage conversation sessions')
          @manager = ConversationManager.new
        end
        
        def execute(args, context)
          subcommand = args.shift
          
          case subcommand
          when 'save'
            save_conversation(args, context)
          when 'load'
            load_conversation(args, context)
          when 'list'
            list_conversations
          when 'search'
            search_conversations(args)
          when 'delete'
            delete_conversation(args)
          when 'export'
            export_conversation(args)
          when 'branch'
            branch_conversation(args, context)
          when 'stats'
            show_stats(args)
          else
            show_help
          end
        end
        
        def complete(args, context)
          subcommand = args.first
          
          if args.empty?
            %w[save load list search delete export branch stats]
          elsif args.length == 1
            %w[save load list search delete export branch stats].select { |cmd| cmd.start_with?(subcommand) }
          elsif args.length == 2 && %w[load delete export branch stats].include?(subcommand)
            # Complete with conversation names
            @manager.list_conversations.map { |c| c[:name] }
          elsif args.length == 3 && subcommand == 'export'
            %w[markdown json html]
          else
            []
          end
        end
        
        private
        
        def save_conversation(args, context)
          name = args.first || generate_session_name
          conversation = context[:conversation] || []
          
          metadata = {
            provider: context[:provider_manager]&.current_provider,
            model: context[:provider_manager]&.current_model
          }
          
          path = @manager.save_conversation(name, conversation, metadata)
          puts "✅ Conversation saved as '#{name}'"
          puts "   Path: #{path}"
          
          # Set as current session for auto-save
          context[:current_session] = name
        end
        
        def load_conversation(args, context)
          name = args.first
          
          if name.nil? || name.empty?
            puts "Usage: /conversation load <name>"
            return
          end
          
          begin
            result = @manager.load_conversation(name)
            
            # Replace current conversation
            if context[:conversation]
              context[:conversation].clear
              context[:conversation].concat(result[:conversation])
            end
            
            puts "✅ Loaded conversation '#{name}'"
            puts "   Messages: #{result[:conversation].size}"
            puts "   Created: #{result[:metadata][:timestamp]}"
            
            # Set as current session
            context[:current_session] = name
            
          rescue => e
            puts "❌ #{e.message}"
          end
        end
        
        def list_conversations
          conversations = @manager.list_conversations
          
          if conversations.empty?
            puts "No saved conversations found."
          else
            puts "Saved conversations:"
            conversations.each do |conv|
              puts "  • #{conv[:name]}"
              puts "    Messages: #{conv[:message_count]}, Last modified: #{conv[:timestamp]}"
              puts "    Tags: #{conv[:tags].join(', ')}" if conv[:tags]&.any?
            end
          end
        end
        
        def search_conversations(args)
          query = args.join(' ')
          
          if query.empty?
            puts "Usage: /conversation search <query>"
            return
          end
          
          results = @manager.search_conversations(query)
          
          if results.empty?
            puts "No conversations found matching '#{query}'"
          else
            puts "Found #{results.size} conversation(s):"
            results.each do |conv|
              puts "  • #{conv[:name]} - #{conv[:summary]}"
            end
          end
        end
        
        def delete_conversation(args)
          name = args.first
          
          if name.nil? || name.empty?
            puts "Usage: /conversation delete <name>"
            return
          end
          
          print "Are you sure you want to delete '#{name}'? (y/n): "
          response = STDIN.gets.chomp.downcase
          
          if response == 'y' || response == 'yes'
            if @manager.delete_conversation(name)
              puts "✅ Conversation '#{name}' deleted"
            else
              puts "❌ Conversation '#{name}' not found"
            end
          else
            puts "Deletion cancelled"
          end
        end
        
        def export_conversation(args)
          name = args[0]
          format = (args[1] || 'markdown').to_sym
          
          if name.nil? || name.empty?
            puts "Usage: /conversation export <name> [format]"
            puts "Formats: markdown, json, html"
            return
          end
          
          begin
            content = @manager.export_conversation(name, format)
            
            filename = "#{name}_export.#{format == :markdown ? 'md' : format.to_s}"
            File.write(filename, content)
            
            puts "✅ Exported to #{filename}"
          rescue => e
            puts "❌ Export failed: #{e.message}"
          end
        end
        
        def branch_conversation(args, context)
          base_name = args[0]
          branch_name = args[1]
          
          if base_name.nil? || branch_name.nil?
            puts "Usage: /conversation branch <base_name> <branch_name>"
            return
          end
          
          conversation = context[:conversation] || []
          
          begin
            @manager.create_branch(base_name, branch_name, conversation)
            puts "✅ Created branch '#{branch_name}' from '#{base_name}'"
            
            # Switch to branch
            context[:current_session] = branch_name
          rescue => e
            puts "❌ #{e.message}"
          end
        end
        
        def show_stats(args)
          name = args.first
          
          if name.nil? || name.empty?
            puts "Usage: /conversation stats <name>"
            return
          end
          
          begin
            stats = @manager.get_conversation_stats(name)
            
            puts "📊 Statistics for '#{name}':"
            puts "  Total messages: #{stats[:message_count]}"
            puts "  User messages: #{stats[:user_messages]}"
            puts "  Assistant messages: #{stats[:assistant_messages]}"
            puts "  Average message length: #{stats[:average_message_length].round} chars"
            puts "  Total length: #{stats[:total_length]} chars"
            puts "  Created: #{stats[:created_at]}"
            puts "  Last modified: #{stats[:last_modified]}"
            
          rescue => e
            puts "❌ #{e.message}"
          end
        end
        
        def show_help
          puts <<~HELP
            Conversation Commands:
              /conversation save [name]       - Save current conversation
              /conversation load <name>       - Load a saved conversation
              /conversation list              - List all saved conversations
              /conversation search <query>    - Search conversations
              /conversation delete <name>     - Delete a conversation
              /conversation export <name> [format] - Export conversation (markdown/json/html)
              /conversation branch <base> <new> - Create a branch from existing conversation
              /conversation stats <name>      - Show conversation statistics
          HELP
        end
        
        private
        
        def generate_session_name
          "session_#{Time.now.strftime('%Y%m%d_%H%M%S')}"
        end
      end
    end
  end
end