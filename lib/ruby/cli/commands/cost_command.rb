require_relative '../base_command'
require_relative '../../cost_tracker'

module Lantae
  module CLI
    module Commands
      class CostCommand < BaseCommand
        def initialize
          super('cost', 'Track and manage API usage costs')
          @tracker = CostTracker.new
        end
        
        def execute(args, context)
          subcommand = args.shift
          
          case subcommand
          when 'status'
            show_status(context)
          when 'report'
            show_report(args)
          when 'budget'
            manage_budget(args)
          when 'export'
            export_data(args)
          when 'session'
            show_session_summary
          else
            show_help
          end
        end
        
        def complete(args, context)
          subcommand = args.first
          
          if args.empty?
            %w[status report budget export session]
          elsif args.length == 1
            %w[status report budget export session].select { |cmd| cmd.start_with?(subcommand) }
          elsif args.length == 2
            case subcommand
            when 'report'
              %w[daily weekly monthly]
            when 'budget'
              %w[set show]
            when 'export'
              %w[csv json]
            else
              []
            end
          elsif subcommand == 'budget' && args[1] == 'set' && args.length == 3
            %w[ollama openai anthropic bedrock gemini mistral perplexity]
          else
            []
          end
        end
        
        def track_request(provider, model, input_tokens, output_tokens, context)
          # This method is called automatically by providers
          @tracker.track_usage(provider, model, input_tokens, output_tokens)
        end
        
        private
        
        def show_status(context)
          provider = context[:provider_manager]&.current_provider || 'unknown'
          model = context[:provider_manager]&.current_model || 'unknown'
          
          puts "💰 Cost Tracking Status"
          puts "─" * 40
          
          # Current provider pricing
          cost_per_1m = @tracker.calculate_cost(provider, model, 1_000_000, 0)
          puts "Current provider: #{provider}"
          puts "Current model: #{model}"
          puts "Input cost: $#{sprintf('%.2f', cost_per_1m)}/1M tokens" if cost_per_1m > 0
          
          # Session summary
          summary = @tracker.get_session_summary
          puts "\nSession totals:"
          puts "  Duration: #{format_duration(summary[:duration])}"
          puts "  Total cost: $#{sprintf('%.4f', summary[:total_cost])}"
          
          # Provider breakdown
          if summary[:providers].any?
            puts "\nBy provider:"
            summary[:providers].each do |prov, data|
              puts "  #{prov}: $#{sprintf('%.4f', data[:total_cost])} (#{data[:total_requests]} requests)"
            end
          end
          
          # Budget status
          budget_status = @tracker.get_budget_status(provider)
          if budget_status
            puts "\nDaily budget:"
            puts "  Limit: $#{sprintf('%.2f', budget_status[:limit])}"
            puts "  Spent: $#{sprintf('%.4f', budget_status[:spent])}"
            puts "  Remaining: $#{sprintf('%.4f', budget_status[:remaining])}"
            
            # Visual progress bar
            percentage = budget_status[:percentage]
            bar_width = 30
            filled = (percentage * bar_width / 100).to_i
            bar = "█" * filled + "░" * (bar_width - filled)
            color = percentage >= 90 ? "\e[91m" : percentage >= 75 ? "\e[93m" : "\e[92m"
            puts "  Progress: #{color}[#{bar}] #{percentage}%\e[0m"
          end
        end
        
        def show_report(args)
          period = (args.first || 'daily').to_sym
          
          begin
            report = @tracker.get_usage_report(period)
            
            case period
            when :daily
              show_daily_report(report)
            when :weekly
              show_weekly_report(report)
            when :monthly
              show_monthly_report(report)
            end
            
          rescue => e
            puts "❌ #{e.message}"
          end
        end
        
        def show_daily_report(report)
          puts "📊 Daily Usage Report - #{report[:date]}"
          puts "─" * 40
          puts "Total cost: $#{sprintf('%.4f', report[:total_cost])}"
          puts "Total requests: #{report[:total_requests]}"
          
          if report[:providers].any?
            puts "\nBreakdown by provider:"
            report[:providers].each do |provider, data|
              puts "\n#{provider}:"
              puts "  Cost: $#{sprintf('%.4f', data[:cost])}"
              puts "  Requests: #{data[:requests]}"
              puts "  Tokens: #{data[:input_tokens]} in / #{data[:output_tokens]} out"
            end
          end
        end
        
        def show_weekly_report(report)
          puts "📊 Weekly Usage Report"
          puts "Period: #{report[:period]}"
          puts "─" * 40
          puts "Total cost: $#{sprintf('%.4f', report[:total_cost])}"
          
          puts "\nDaily breakdown:"
          report[:daily_breakdown].each do |day|
            puts "  #{day[:date]}: $#{sprintf('%.4f', day[:cost])}"
          end
          
          # Show trend
          if report[:daily_breakdown].size > 1
            costs = report[:daily_breakdown].map { |d| d[:cost] }
            avg = costs.sum / costs.size
            trend = costs.last > avg ? "📈" : "📉"
            puts "\nAverage daily: $#{sprintf('%.4f', avg)} #{trend}"
          end
        end
        
        def show_monthly_report(report)
          puts "📊 Monthly Usage Report"
          puts "Period: #{report[:period]}"
          puts "─" * 40
          puts "Total cost: $#{sprintf('%.4f', report[:total_cost])}"
          puts "Daily average: $#{sprintf('%.4f', report[:daily_average])}"
          
          if report[:peak_day]
            puts "Peak day: #{report[:peak_day][:date]} ($#{sprintf('%.4f', report[:peak_day][:cost])})"
          end
        end
        
        def manage_budget(args)
          action = args.shift
          
          case action
          when 'set'
            set_budget(args)
          when 'show'
            show_budgets
          else
            puts "Usage: /cost budget <set|show>"
          end
        end
        
        def set_budget(args)
          provider = args[0]
          limit = args[1]&.to_f
          
          if provider.nil? || limit.nil?
            puts "Usage: /cost budget set <provider> <daily_limit>"
            puts "Example: /cost budget set openai 10.00"
            return
          end
          
          @tracker.set_budget_limit(provider, limit, :daily)
          puts "✅ Set daily budget for #{provider}: $#{sprintf('%.2f', limit)}"
        end
        
        def show_budgets
          providers = %w[openai anthropic gemini mistral perplexity]
          
          puts "💰 Budget Limits"
          puts "─" * 40
          
          providers.each do |provider|
            status = @tracker.get_budget_status(provider)
            if status
              percentage = status[:percentage]
              color = percentage >= 90 ? "\e[91m" : percentage >= 75 ? "\e[93m" : "\e[92m"
              
              puts "#{provider}:"
              puts "  Daily limit: $#{sprintf('%.2f', status[:limit])}"
              puts "  Spent today: #{color}$#{sprintf('%.4f', status[:spent])} (#{percentage}%)\e[0m"
              puts "  Remaining: $#{sprintf('%.4f', status[:remaining])}"
              puts
            end
          end
        end
        
        def export_data(args)
          format = (args.first || 'csv').to_sym
          
          begin
            data = @tracker.export_usage_data(format)
            
            filename = "usage_export_#{Time.now.strftime('%Y%m%d_%H%M%S')}.#{format}"
            File.write(filename, data)
            
            puts "✅ Exported usage data to #{filename}"
          rescue => e
            puts "❌ Export failed: #{e.message}"
          end
        end
        
        def show_session_summary
          summary = @tracker.get_session_summary
          
          puts "📊 Current Session Summary"
          puts "─" * 40
          puts "Duration: #{format_duration(summary[:duration])}"
          puts "Total cost: $#{sprintf('%.4f', summary[:total_cost])}"
          
          if summary[:providers].any?
            puts "\nCost by provider:"
            
            summary[:providers].each do |provider, prov_data|
              puts "\n#{provider}: $#{sprintf('%.4f', prov_data[:total_cost])}"
              
              if prov_data[:models].any?
                prov_data[:models].each do |model, usage|
                  puts "  #{model}:"
                  puts "    Requests: #{usage[:requests]}"
                  puts "    Input tokens: #{usage[:input_tokens]}"
                  puts "    Output tokens: #{usage[:output_tokens]}"
                  puts "    Cost: $#{sprintf('%.4f', usage[:cost])}"
                end
              end
            end
          else
            puts "\nNo API usage in this session yet."
          end
        end
        
        def show_help
          puts <<~HELP
            Cost Tracking Commands:
              /cost status         - Show current cost status and budget
              /cost report [period] - Show usage report (daily/weekly/monthly)
              /cost budget set <provider> <limit> - Set daily budget limit
              /cost budget show    - Show all budget limits
              /cost export [format] - Export usage data (csv/json)
              /cost session        - Show current session summary
              
            Examples:
              /cost budget set openai 10.00
              /cost report weekly
              /cost export csv
          HELP
        end
        
        private
        
        def format_duration(seconds)
          hours = seconds / 3600
          minutes = (seconds % 3600) / 60
          
          if hours > 0
            "#{hours.to_i}h #{minutes.to_i}m"
          else
            "#{minutes.to_i}m"
          end
        end
      end
    end
  end
end