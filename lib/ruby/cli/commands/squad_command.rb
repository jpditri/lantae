require_relative '../base_command'
require_relative '../../squad_deployment'

module Lantae
  module CLI
    module Commands
      class SquadCommand < BaseCommand
        def initialize
          super('squad', 'Manage squad deployments for coordinated task execution')
        end
        
        def execute(args, context)
          subcommand = args.shift
          
          case subcommand
          when 'create'
            create_squad(args, context)
          when 'deploy'
            deploy_squad(args, context)
          when 'status'
            squad_status(args, context)
          when 'list'
            list_squads(context)
          else
            show_help
          end
        end
        
        private
        
        def create_squad(args, context)
          squad_name = args.shift
          
          if squad_name.nil? || squad_name.empty?
            puts "Usage: /squad create <squad_name>"
            return
          end
          
          # Create new squad
          squad = Lantae::SquadDeployment.new(
            squad_name,
            provider_manager: context[:provider_manager],
            tool_manager: context[:tool_manager],
            logger: context[:logger]
          )
          
          # Store in context
          context[:squads] ||= {}
          context[:squads][squad_name] = squad
          
          puts "✅ Squad '#{squad_name}' created"
          
          # Add default members
          puts "Adding default squad members..."
          
          squad.add_member(
            name: "Planner",
            role: :planner,
            capabilities: [:planning, :task_decomposition],
            model: "cogito:latest"
          )
          
          squad.add_member(
            name: "Executor-1", 
            role: :executor,
            capabilities: [:coding, :advanced_coding],
            model: "cogito:latest"
          )
          
          squad.add_member(
            name: "Reviewer",
            role: :reviewer,
            capabilities: [:code_review, :quality_assurance],
            model: "cogito:latest"
          )
          
          puts "Squad '#{squad_name}' is ready with #{squad.members.size} members"
        end
        
        def deploy_squad(args, context)
          squad_name = args.shift
          
          if squad_name.nil? || squad_name.empty?
            puts "Usage: /squad deploy <squad_name>"
            return
          end
          
          squad = context[:squads]&.[](squad_name)
          
          if squad.nil?
            puts "❌ Squad '#{squad_name}' not found. Create it first with: /squad create #{squad_name}"
            return
          end
          
          if squad.tasks.empty?
            puts "❌ No tasks assigned to squad. Assign tasks first."
            return
          end
          
          puts "🚀 Deploying squad '#{squad_name}'..."
          
          begin
            squad.deploy
            puts "✅ Squad deployment completed!"
            
            # Show results
            report = squad.get_status_report
            display_squad_report(report)
            
          rescue => e
            puts "❌ Deployment failed: #{e.message}"
          end
        end
        
        def squad_status(args, context)
          squad_name = args.shift
          
          if squad_name.nil? || squad_name.empty?
            # Show all squads status
            if context[:squads].nil? || context[:squads].empty?
              puts "No squads created yet."
              return
            end
            
            context[:squads].each do |name, squad|
              report = squad.get_status_report
              puts "\n#{name}:"
              display_squad_report(report)
            end
          else
            # Show specific squad status
            squad = context[:squads]&.[](squad_name)
            
            if squad.nil?
              puts "❌ Squad '#{squad_name}' not found"
              return
            end
            
            report = squad.get_status_report
            display_squad_report(report)
          end
        end
        
        def list_squads(context)
          if context[:squads].nil? || context[:squads].empty?
            puts "No squads created yet."
            return
          end
          
          puts "Available squads:"
          context[:squads].each do |name, squad|
            status_icon = case squad.status
                         when :deployed then "✅"
                         when :deploying then "🔄"
                         when :failed then "❌"
                         else "⏸️"
                         end
            
            puts "  #{status_icon} #{name} (#{squad.members.size} members, #{squad.tasks.size} tasks)"
          end
        end
        
        def display_squad_report(report)
          puts "  Status: #{report[:status]}"
          puts "  Completion: #{report[:completion_rate]}%"
          
          puts "  Members:"
          report[:members].each do |member|
            status_icon = member[:status] == :busy ? "🔄" : "✅"
            puts "    #{status_icon} #{member[:name]} (#{member[:role]})"
          end
          
          if report[:tasks].any?
            puts "  Tasks:"
            report[:tasks].each do |task|
              status_icon = case task[:status]
                           when :completed then "✅"
                           when :in_progress then "🔄"
                           when :failed then "❌"
                           else "⏸️"
                           end
              
              puts "    #{status_icon} #{task[:description]} → #{task[:assigned_to]}"
            end
          end
        end
        
        def show_help
          puts <<~HELP
            Squad Commands:
              /squad create <name>     - Create a new squad
              /squad deploy <name>     - Deploy a squad to execute tasks
              /squad status [name]     - Show squad status
              /squad list             - List all squads
              
            Task Assignment (after creating squad):
              Use /task assign <squad_name> <task_description>
              
            Example workflow:
              /squad create dev-team
              /task assign dev-team "Implement user authentication"
              /task assign dev-team "Write unit tests"
              /squad deploy dev-team
          HELP
        end
      end
    end
  end
end