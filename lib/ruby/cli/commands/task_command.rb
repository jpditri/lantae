require_relative '../base_command'

module Lantae
  module CLI
    module Commands
      class TaskCommand < BaseCommand
        def initialize
          super('task', 'Manage and assign tasks to squads')
        end
        
        def execute(args, context)
          subcommand = args.shift
          
          case subcommand
          when 'assign'
            assign_task(args, context)
          when 'list'
            list_tasks(args, context)
          when 'clear'
            clear_tasks(args, context)
          else
            show_help
          end
        end
        
        private
        
        def assign_task(args, context)
          squad_name = args.shift
          task_description = args.join(' ')
          
          if squad_name.nil? || task_description.empty?
            puts "Usage: /task assign <squad_name> <task_description>"
            return
          end
          
          squad = context[:squads]&.[](squad_name)
          
          if squad.nil?
            puts "❌ Squad '#{squad_name}' not found. Create it first with: /squad create #{squad_name}"
            return
          end
          
          # Parse priority if specified
          priority = :normal
          if task_description =~ /\[priority:(high|low|urgent)\]/i
            priority = $1.downcase.to_sym
            task_description.gsub!(/\[priority:\w+\]/i, '').strip
          end
          
          # Assign task
          task = squad.assign_task(task_description, priority: priority)
          
          puts "✅ Task assigned to #{task[:assigned_to][:name]} in squad '#{squad_name}'"
          puts "   Priority: #{priority}"
          puts "   Task ID: #{task[:id]}"
        end
        
        def list_tasks(args, context)
          squad_name = args.shift
          
          if squad_name.nil?
            # List tasks for all squads
            if context[:squads].nil? || context[:squads].empty?
              puts "No squads created yet."
              return
            end
            
            context[:squads].each do |name, squad|
              if squad.tasks.any?
                puts "\n#{name} tasks:"
                display_squad_tasks(squad)
              end
            end
          else
            # List tasks for specific squad
            squad = context[:squads]&.[](squad_name)
            
            if squad.nil?
              puts "❌ Squad '#{squad_name}' not found"
              return
            end
            
            if squad.tasks.empty?
              puts "No tasks assigned to squad '#{squad_name}'"
            else
              puts "Tasks for squad '#{squad_name}':"
              display_squad_tasks(squad)
            end
          end
        end
        
        def clear_tasks(args, context)
          squad_name = args.shift
          
          if squad_name.nil?
            puts "Usage: /task clear <squad_name>"
            return
          end
          
          squad = context[:squads]&.[](squad_name)
          
          if squad.nil?
            puts "❌ Squad '#{squad_name}' not found"
            return
          end
          
          task_count = squad.tasks.size
          squad.instance_variable_set(:@tasks, [])
          
          puts "✅ Cleared #{task_count} tasks from squad '#{squad_name}'"
        end
        
        def display_squad_tasks(squad)
          squad.tasks.each_with_index do |task, index|
            status_icon = case task[:status]
                         when :completed then "✅"
                         when :in_progress then "🔄"
                         when :failed then "❌"
                         else "⏸️"
                         end
            
            priority_icon = case task[:priority]
                           when :urgent then "🔴"
                           when :high then "🟡"
                           when :low then "🟢"
                           else "⚪"
                           end
            
            puts "  #{index + 1}. #{status_icon} #{priority_icon} #{task[:description]}"
            puts "     Assigned to: #{task[:assigned_to][:name]}"
            puts "     Status: #{task[:status]}"
            
            if task[:error]
              puts "     Error: #{task[:error]}"
            elsif task[:completed_at]
              duration = (task[:completed_at] - task[:created_at]).round(2)
              puts "     Completed in: #{duration}s"
            end
          end
        end
        
        def show_help
          puts <<~HELP
            Task Commands:
              /task assign <squad> <description>  - Assign a task to a squad
              /task list [squad]                 - List tasks (all or for specific squad)
              /task clear <squad>                - Clear all tasks from a squad
              
            Priority levels (optional):
              Add [priority:urgent], [priority:high], or [priority:low] to task description
              
            Examples:
              /task assign dev-team Implement login feature
              /task assign dev-team Fix bug in payment system [priority:urgent]
              /task list dev-team
          HELP
        end
      end
    end
  end
end