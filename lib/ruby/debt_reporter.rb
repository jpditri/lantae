#!/usr/bin/env ruby

require 'json'
require 'time'
require 'csv'
require 'erb'

module Lantae
  class DebtReporter
    TECH_DEBT_FILE = File.expand_path('../../../data/tech_debt.json', __FILE__)
    
    def initialize
      @tech_debt = load_tech_debt
    end
    
    def show_summary
      summary = @tech_debt[:validation_results][:summary]
      last_run = @tech_debt[:validation_results][:last_run]
      
      puts "📊 TECH DEBT SUMMARY"
      puts "=" * 50
      puts "Last Updated: #{@tech_debt[:metadata][:last_updated]}"
      puts "Last Validation: #{last_run ? Time.parse(last_run).strftime('%Y-%m-%d %H:%M') : 'Never'}"
      puts
      puts "🔍 Validation Issues: #{summary[:total_issues]} total"
      puts "  💥 Critical: #{summary[:critical]}"
      puts "  🔴 High: #{summary[:high]}"
      puts "  🟡 Medium: #{summary[:medium]}"
      puts "  🔵 Low: #{summary[:low]}"
      puts "  ℹ️  Info: #{summary[:info]}"
      puts
      
      manual_debt = @tech_debt[:manual_debt] || []
      open_debt = manual_debt.select { |item| item[:status] != 'resolved' }
      
      puts "📝 Manual Tech Debt: #{open_debt.size} open items"
      
      if open_debt.any?
        puts
        puts "Top Priority Items:"
        priority_order = { 'critical' => 0, 'high' => 1, 'medium' => 2, 'low' => 3 }
        sorted_debt = open_debt.sort_by { |item| priority_order[item[:priority]] || 4 }
        
        sorted_debt.first(5).each_with_index do |item, idx|
          priority_icon = case item[:priority]
            when 'critical' then '💥'
            when 'high' then '🔴'
            when 'medium' then '🟡'
            when 'low' then '🔵'
            else '⚪'
          end
          
          puts "  #{idx + 1}. #{priority_icon} #{item[:title]}"
          puts "     Created: #{item[:created]} | Effort: #{item[:effort_estimate] || 'Unknown'}"
        end
      end
      
      puts
      puts "Run 'rake debt:report' for detailed information"
      puts "Run 'rake validate' to update validation results"
    end
    
    def show_detailed_report
      puts "🔍 DETAILED TECH DEBT REPORT"
      puts "=" * 80
      puts "Generated: #{Time.now.strftime('%Y-%m-%d %H:%M:%S')}"
      puts "Last Updated: #{@tech_debt[:metadata][:last_updated]}"
      puts
      
      show_validation_details
      show_manual_debt_details
      show_recommendations
    end
    
    def add_manual_debt(title, description, priority = 'medium')
      unless %w[critical high medium low].include?(priority.downcase)
        puts "❌ Invalid priority. Use: critical, high, medium, low"
        return false
      end
      
      new_id = generate_debt_id(title)
      
      debt_item = {
        id: new_id,
        title: title,
        description: description,
        category: 'manual',
        priority: priority.downcase,
        created: Time.now.strftime('%Y-%m-%d'),
        assignee: nil,
        effort_estimate: nil,
        files: [],
        status: 'open'
      }
      
      @tech_debt[:manual_debt] ||= []
      @tech_debt[:manual_debt] << debt_item
      
      save_tech_debt
      
      puts "✅ Added tech debt item: #{new_id}"
      puts "   Title: #{title}"
      puts "   Priority: #{priority}"
      puts
      puts "Use 'rake debt:resolve[#{new_id}]' to mark as resolved"
      
      true
    end
    
    def resolve_debt(item_id)
      manual_debt = @tech_debt[:manual_debt] || []
      item = manual_debt.find { |debt| debt[:id] == item_id }
      
      unless item
        puts "❌ Tech debt item not found: #{item_id}"
        return false
      end
      
      item[:status] = 'resolved'
      item[:resolved_date] = Time.now.strftime('%Y-%m-%d')
      
      save_tech_debt
      
      puts "✅ Resolved tech debt item: #{item_id}"
      puts "   Title: #{item[:title]}"
      
      true
    end
    
    def generate_html_report
      html_file = File.expand_path('../../../data/tech_debt_report.html', __FILE__)
      
      html_content = generate_html_template
      
      File.write(html_file, html_content)
      
      puts "📄 HTML report generated: #{html_file}"
      puts "   Open in browser: file://#{html_file}"
      
      html_file
    end
    
    def export_to_csv
      csv_file = File.expand_path('../../../data/tech_debt_export.csv', __FILE__)
      
      CSV.open(csv_file, 'w') do |csv|
        # Header
        csv << ['Type', 'ID', 'Title', 'Description', 'Priority', 'Category', 'File', 'Line', 'Status', 'Created', 'Effort']
        
        # Manual debt items
        manual_debt = @tech_debt[:manual_debt] || []
        manual_debt.each do |item|
          csv << [
            'Manual',
            item[:id],
            item[:title],
            item[:description],
            item[:priority],
            item[:category],
            item[:files]&.join('; '),
            '',
            item[:status],
            item[:created],
            item[:effort_estimate]
          ]
        end
        
        # Validation issues
        categories = @tech_debt[:validation_results][:categories] || {}
        categories.each do |category_name, checks|
          checks.each do |check_name, result|
            next unless result[:issues]
            
            result[:issues].each do |issue|
              csv << [
                'Validation',
                "#{category_name}_#{check_name}",
                "#{check_name}: #{issue[:message]}",
                issue[:details] || '',
                issue[:severity],
                category_name,
                issue[:file],
                issue[:line],
                'open',
                result[:last_run] ? Time.parse(result[:last_run]).strftime('%Y-%m-%d') : '',
                ''
              ]
            end
          end
        end
      end
      
      puts "📊 CSV export generated: #{csv_file}"
      
      csv_file
    end
    
    private
    
    def load_tech_debt
      return default_tech_debt unless File.exist?(TECH_DEBT_FILE)
      
      JSON.parse(File.read(TECH_DEBT_FILE), symbolize_names: true)
    rescue JSON::ParserError => e
      puts "⚠️  Warning: Could not parse tech debt file: #{e.message}"
      default_tech_debt
    end
    
    def default_tech_debt
      {
        metadata: {
          created: Time.now.strftime('%Y-%m-%d'),
          last_updated: Time.now.strftime('%Y-%m-%d'),
          version: '1.0.0'
        },
        validation_results: {
          last_run: nil,
          summary: { total_issues: 0, critical: 0, high: 0, medium: 0, low: 0, info: 0 },
          categories: {}
        },
        manual_debt: []
      }
    end
    
    def save_tech_debt
      File.write(TECH_DEBT_FILE, JSON.pretty_generate(@tech_debt))
    end
    
    def show_validation_details
      puts "🔍 VALIDATION RESULTS"
      puts "-" * 40
      
      categories = @tech_debt[:validation_results][:categories] || {}
      
      if categories.empty?
        puts "No validation results found. Run 'rake validate' first."
        puts
        return
      end
      
      categories.each do |category_name, checks|
        puts "\n📋 #{category_name.to_s.tr('_', ' ').capitalize}:"
        
        checks.each do |check_name, result|
          status_icon = case result[:status]
            when 'passed' then '✅'
            when 'failed' then '❌'
            when 'error' then '⚠️'
            else 'ℹ️'
          end
          
          issue_count = result[:issues]&.size || 0
          puts "  #{status_icon} #{check_name}: #{issue_count} issues"
          
          if result[:issues] && !result[:issues].empty?
            result[:issues].first(3).each do |issue|
              severity_icon = case issue[:severity]
                when 'critical' then '💥'
                when 'high' then '🔴'
                when 'medium' then '🟡'
                when 'low' then '🔵'
                else 'ℹ️'
              end
              
              location = issue[:file] ? "#{issue[:file]}:#{issue[:line]}" : 'N/A'
              puts "    #{severity_icon} #{location} - #{issue[:message]}"
            end
            
            if issue_count > 3
              puts "    ... and #{issue_count - 3} more issues"
            end
          end
        end
      end
      
      puts
    end
    
    def show_manual_debt_details
      puts "📝 MANUAL TECH DEBT"
      puts "-" * 40
      
      manual_debt = @tech_debt[:manual_debt] || []
      
      if manual_debt.empty?
        puts "No manual tech debt items found."
        puts "Add items with: rake debt:add['Title','Description','priority']"
        puts
        return
      end
      
      open_items = manual_debt.select { |item| item[:status] != 'resolved' }
      resolved_items = manual_debt.select { |item| item[:status] == 'resolved' }
      
      if open_items.any?
        puts "\n🔓 Open Items (#{open_items.size}):"
        
        priority_order = { 'critical' => 0, 'high' => 1, 'medium' => 2, 'low' => 3 }
        sorted_items = open_items.sort_by { |item| priority_order[item[:priority]] || 4 }
        
        sorted_items.each do |item|
          priority_icon = case item[:priority]
            when 'critical' then '💥'
            when 'high' then '🔴'
            when 'medium' then '🟡'
            when 'low' then '🔵'
            else '⚪'
          end
          
          puts "\n  #{priority_icon} #{item[:title]} (#{item[:id]})"
          puts "     Description: #{item[:description]}"
          puts "     Created: #{item[:created]}"
          puts "     Effort: #{item[:effort_estimate] || 'Not estimated'}"
          
          if item[:files] && !item[:files].empty?
            puts "     Files: #{item[:files].join(', ')}"
          end
          
          if item[:assignee]
            puts "     Assignee: #{item[:assignee]}"
          end
        end
      end
      
      if resolved_items.any?
        puts "\n✅ Resolved Items (#{resolved_items.size}):"
        resolved_items.last(3).each do |item|
          puts "  ✅ #{item[:title]} (resolved #{item[:resolved_date]})"
        end
        
        if resolved_items.size > 3
          puts "  ... and #{resolved_items.size - 3} more resolved items"
        end
      end
      
      puts
    end
    
    def show_recommendations
      puts "💡 RECOMMENDATIONS"
      puts "-" * 40
      
      summary = @tech_debt[:validation_results][:summary]
      
      recommendations = []
      
      # Critical issues
      if summary[:critical] > 0
        recommendations << "🚨 Address #{summary[:critical]} critical issues immediately (syntax errors, security vulnerabilities)"
      end
      
      # High priority issues
      if summary[:high] > 10
        recommendations << "🎯 Focus on reducing high-priority issues (currently #{summary[:high]})"
      end
      
      # Test coverage
      categories = @tech_debt[:validation_results][:categories] || {}
      coverage_result = categories.dig(:qa_checks, :test_coverage)
      if coverage_result && coverage_result[:coverage_percentage] && coverage_result[:coverage_percentage] < 80
        recommendations << "📊 Improve test coverage (currently #{coverage_result[:coverage_percentage]}%)"
      end
      
      # Documentation
      doc_result = categories.dig(:qa_checks, :documentation)
      if doc_result && doc_result[:issues] && doc_result[:issues].size > 10
        recommendations << "📚 Add documentation to #{doc_result[:issues].size} classes/methods"
      end
      
      # Manual debt
      manual_debt = @tech_debt[:manual_debt] || []
      critical_manual = manual_debt.select { |item| item[:priority] == 'critical' && item[:status] != 'resolved' }
      if critical_manual.any?
        recommendations << "⚡ Address #{critical_manual.size} critical manual tech debt items"
      end
      
      if recommendations.empty?
        puts "🎉 No major issues found! Consider:"
        puts "  • Running 'rake validate' regularly"
        puts "  • Adding more comprehensive tests"
        puts "  • Documenting complex code sections"
      else
        recommendations.each_with_index do |rec, idx|
          puts "  #{idx + 1}. #{rec}"
        end
      end
      
      puts
      puts "💡 Run 'rake validate:fix' to auto-fix issues where possible"
      puts "💡 Run 'rake debt:html' to generate a visual report"
    end
    
    def generate_debt_id(title)
      # Generate a URL-friendly ID from title
      base_id = title.downcase.gsub(/[^a-z0-9\s]/, '').gsub(/\s+/, '-')[0..30]
      
      # Ensure uniqueness
      existing_ids = (@tech_debt[:manual_debt] || []).map { |item| item[:id] }
      counter = 1
      
      id = base_id
      while existing_ids.include?(id)
        id = "#{base_id}-#{counter}"
        counter += 1
      end
      
      id
    end
    
    def generate_html_template
      template = <<~HTML
        <!DOCTYPE html>
        <html lang="en">
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <title>Lantae Tech Debt Report</title>
            <style>
                body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; margin: 0; padding: 20px; background: #f5f5f5; }
                .container { max-width: 1200px; margin: 0 auto; background: white; border-radius: 8px; box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
                .header { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 30px; border-radius: 8px 8px 0 0; }
                .content { padding: 30px; }
                .summary { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; margin-bottom: 30px; }
                .metric { background: #f8f9fa; padding: 20px; border-radius: 8px; text-align: center; border-left: 4px solid #667eea; }
                .metric h3 { margin: 0 0 10px 0; color: #333; font-size: 2em; }
                .metric p { margin: 0; color: #666; }
                .section { margin-bottom: 30px; }
                .section h2 { color: #333; border-bottom: 2px solid #eee; padding-bottom: 10px; }
                .issue { background: #fff; border: 1px solid #eee; border-radius: 4px; padding: 15px; margin-bottom: 10px; }
                .issue.critical { border-left: 4px solid #dc3545; }
                .issue.high { border-left: 4px solid #fd7e14; }
                .issue.medium { border-left: 4px solid #ffc107; }
                .issue.low { border-left: 4px solid #17a2b8; }
                .issue.info { border-left: 4px solid #6c757d; }
                .badge { display: inline-block; padding: 3px 8px; border-radius: 12px; font-size: 0.8em; font-weight: bold; }
                .badge.critical { background: #dc3545; color: white; }
                .badge.high { background: #fd7e14; color: white; }
                .badge.medium { background: #ffc107; color: black; }
                .badge.low { background: #17a2b8; color: white; }
                .badge.info { background: #6c757d; color: white; }
                .progress { background: #e9ecef; border-radius: 4px; height: 8px; overflow: hidden; }
                .progress-bar { background: #28a745; height: 100%; transition: width 0.3s; }
                .timestamp { color: #666; font-size: 0.9em; }
            </style>
        </head>
        <body>
            <div class="container">
                <div class="header">
                    <h1>🔍 Lantae Tech Debt Report</h1>
                    <p class="timestamp">Generated: #{Time.now.strftime('%Y-%m-%d %H:%M:%S')}</p>
                    <p class="timestamp">Last Updated: #{@tech_debt[:metadata][:last_updated]}</p>
                </div>
                
                <div class="content">
                    #{generate_html_summary}
                    #{generate_html_validation_results}
                    #{generate_html_manual_debt}
                </div>
            </div>
        </body>
        </html>
      HTML
      
      template
    end
    
    def generate_html_summary
      summary = @tech_debt[:validation_results][:summary]
      manual_debt = @tech_debt[:manual_debt] || []
      open_manual = manual_debt.select { |item| item[:status] != 'resolved' }
      
      <<~HTML
        <div class="section">
            <h2>📊 Summary</h2>
            <div class="summary">
                <div class="metric">
                    <h3>#{summary[:total_issues]}</h3>
                    <p>Total Issues</p>
                </div>
                <div class="metric">
                    <h3>#{summary[:critical]}</h3>
                    <p>Critical</p>
                </div>
                <div class="metric">
                    <h3>#{summary[:high]}</h3>
                    <p>High Priority</p>
                </div>
                <div class="metric">
                    <h3>#{open_manual.size}</h3>
                    <p>Manual Debt Items</p>
                </div>
            </div>
        </div>
      HTML
    end
    
    def generate_html_validation_results
      categories = @tech_debt[:validation_results][:categories] || {}
      
      html = '<div class="section"><h2>🔍 Validation Results</h2>'
      
      categories.each do |category_name, checks|
        html += "<h3>#{category_name.to_s.tr('_', ' ').capitalize}</h3>"
        
        checks.each do |check_name, result|
          next unless result[:issues]
          
          result[:issues].each do |issue|
            severity = issue[:severity] || 'medium'
            location = issue[:file] ? "#{issue[:file]}:#{issue[:line]}" : 'N/A'
            
            html += <<~HTML
              <div class="issue #{severity}">
                  <div>
                      <span class="badge #{severity}">#{severity.upcase}</span>
                      <strong>#{check_name}</strong> - #{location}
                  </div>
                  <p>#{issue[:message]}</p>
              </div>
            HTML
          end
        end
      end
      
      html += '</div>'
      html
    end
    
    def generate_html_manual_debt
      manual_debt = @tech_debt[:manual_debt] || []
      open_items = manual_debt.select { |item| item[:status] != 'resolved' }
      
      html = '<div class="section"><h2>📝 Manual Tech Debt</h2>'
      
      if open_items.empty?
        html += '<p>No manual tech debt items found.</p>'
      else
        open_items.each do |item|
          priority = item[:priority] || 'medium'
          
          html += <<~HTML
            <div class="issue #{priority}">
                <div>
                    <span class="badge #{priority}">#{priority.upcase}</span>
                    <strong>#{item[:title]}</strong>
                </div>
                <p>#{item[:description]}</p>
                <small>Created: #{item[:created]} | Effort: #{item[:effort_estimate] || 'Not estimated'}</small>
            </div>
          HTML
        end
      end
      
      html += '</div>'
      html
    end
  end
end

# CLI interface
if __FILE__ == $0
  reporter = Lantae::DebtReporter.new
  
  case ARGV[0]
  when 'summary'
    reporter.show_summary
  when 'report'
    reporter.show_detailed_report
  when 'add'
    if ARGV.size < 3
      puts "Usage: #{$0} add 'Title' 'Description' [priority]"
      exit 1
    end
    reporter.add_manual_debt(ARGV[1], ARGV[2], ARGV[3] || 'medium')
  when 'resolve'
    if ARGV.size < 2
      puts "Usage: #{$0} resolve item_id"
      exit 1
    end
    reporter.resolve_debt(ARGV[1])
  when 'html'
    reporter.generate_html_report
  when 'csv'
    reporter.export_to_csv
  else
    puts "Usage: #{$0} {summary|report|add|resolve|html|csv}"
    puts
    puts "Commands:"
    puts "  summary           Show tech debt summary"
    puts "  report            Show detailed report"
    puts "  add 'Title' 'Desc' [priority]  Add manual debt"
    puts "  resolve item_id   Mark item as resolved"
    puts "  html              Generate HTML report"
    puts "  csv               Export to CSV"
    exit 1
  end
end