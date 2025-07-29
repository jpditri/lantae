require 'json'
require 'fileutils'
require 'time'
require 'open3'

module Lantae
  class ValidationEngine
    TECH_DEBT_FILE = File.expand_path('../../../data/tech_debt.json', __FILE__)
    
    def initialize(options = {})
      @verbose = options[:verbose] || false
      @fix_issues = options[:fix] || false
      @tech_debt = load_tech_debt
    end
    
    def run_all_validations
      puts "🔍 Running all code validations..."
      
      results = {
        static_analysis: run_static_analysis,
        qa_checks: run_qa_checks,
        dry_run: run_dry_run_validation
      }
      
      update_tech_debt(results)
      generate_report(results)
      
      results
    end
    
    def run_static_analysis
      puts "\n📊 Running static analysis..."
      
      {
        rubocop: run_rubocop,
        security: run_security_check,
        complexity: check_code_complexity
      }
    end
    
    def run_qa_checks
      puts "\n✅ Running QA checks..."
      
      {
        test_coverage: check_test_coverage,
        dead_code: find_dead_code,
        documentation: check_documentation
      }
    end
    
    def run_dry_run_validation
      puts "\n🧪 Running dry-run validation..."
      
      {
        syntax_check: check_syntax,
        dependency_check: check_dependencies,
        integration_test: run_integration_smoke_test
      }
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
    
    def run_rubocop
      puts "  🔍 Running RuboCop analysis..."
      
      stdout, stderr, status = Open3.capture3('rubocop --format json lib/ spec/ bin/')
      
      if status.success? || status.exitstatus == 1 # RuboCop returns 1 when it finds offenses
        begin
          result = JSON.parse(stdout, symbolize_names: true)
          issues = extract_rubocop_issues(result)
          
          if @fix_issues && !issues.empty?
            puts "  🔧 Auto-fixing RuboCop issues..."
            system('rubocop --auto-correct lib/ spec/ bin/')
          end
          
          {
            last_run: Time.now.iso8601,
            status: issues.empty? ? 'passed' : 'failed',
            issues: issues
          }
        rescue JSON::ParserError
          puts "  ❌ RuboCop output parsing failed"
          { last_run: Time.now.iso8601, status: 'error', issues: [] }
        end
      else
        puts "  ❌ RuboCop execution failed: #{stderr}"
        { last_run: Time.now.iso8601, status: 'error', issues: [] }
      end
    end
    
    def run_security_check
      puts "  🔒 Running security analysis..."
      
      # Check for common security issues
      issues = []
      
      # Look for hardcoded secrets/keys
      stdout, _stderr, _status = Open3.capture3('grep -r -i "api.key\\|secret\\|password" lib/ --include="*.rb" || true')
      unless stdout.empty?
        stdout.each_line do |line|
          next if line.include?('API_KEY') && line.include?('ENV') # Skip environment variable usage
          
          issues << {
            file: line.split(':')[0],
            line: line.split(':')[1]&.to_i,
            message: 'Potential hardcoded secret detected',
            severity: 'high',
            category: 'security'
          }
        end
      end
      
      # Check for unsafe eval/system calls
      stdout, _stderr, _status = Open3.capture3('grep -r -n "eval\\|system\\|exec\\|`" lib/ --include="*.rb" || true')
      unless stdout.empty?
        stdout.each_line do |line|
          next if line.include?('system(') && (line.include?('clear') || line.include?('cls')) # Skip safe system calls
          
          file, line_num, content = line.split(':', 3)
          issues << {
            file: file,
            line: line_num.to_i,
            message: 'Potentially unsafe code execution detected',
            severity: 'medium',
            category: 'security',
            content: content&.strip
          }
        end
      end
      
      {
        last_run: Time.now.iso8601,
        status: issues.empty? ? 'passed' : 'failed',
        issues: issues
      }
    end
    
    def check_code_complexity
      puts "  📈 Checking code complexity..."
      
      issues = []
      
      # Find large files (>500 lines)
      Dir.glob('lib/**/*.rb').each do |file|
        lines = File.readlines(file).size
        if lines > 500
          issues << {
            file: file,
            message: "Large file: #{lines} lines (consider splitting)",
            severity: 'medium',
            category: 'complexity'
          }
        end
      end
      
      # Find long methods (>50 lines)
      Dir.glob('lib/**/*.rb').each do |file|
        content = File.read(file)
        method_lengths = analyze_method_lengths(content, file)
        issues.concat(method_lengths)
      end
      
      {
        last_run: Time.now.iso8601,
        status: issues.empty? ? 'passed' : 'failed',
        issues: issues
      }
    end
    
    def check_test_coverage
      puts "  📊 Checking test coverage..."
      
      # Run tests with coverage if SimpleCov is available
      stdout, stderr, status = Open3.capture3('rspec --require simplecov spec/ 2>/dev/null || rspec spec/')
      
      coverage_info = extract_coverage_info(stdout)
      issues = []
      
      if coverage_info[:percentage] && coverage_info[:percentage] < 80
        issues << {
          message: "Test coverage below 80%: #{coverage_info[:percentage]}%",
          severity: 'medium',
          category: 'test_coverage'
        }
      end
      
      {
        last_run: Time.now.iso8601,
        status: status.success? ? 'passed' : 'failed',
        coverage_percentage: coverage_info[:percentage],
        issues: issues
      }
    end
    
    def find_dead_code
      puts "  🔍 Looking for dead code..."
      
      issues = []
      
      # Find unused methods (basic heuristic)
      Dir.glob('lib/**/*.rb').each do |file|
        content = File.read(file)
        methods = extract_method_names(content)
        
        methods.each do |method|
          # Skip if method is called in the same file
          next if content.include?("#{method}(") || content.include?(".#{method}")
          
          # Check if method is used in other files
          stdout, _stderr, _status = Open3.capture3("grep -r '#{method}' lib/ spec/ bin/ --include='*.rb' | grep -v '^#{file}:' || true")
          
          if stdout.empty?
            issues << {
              file: file,
              message: "Potentially unused method: #{method}",
              severity: 'low',
              category: 'dead_code'
            }
          end
        end
      end
      
      {
        last_run: Time.now.iso8601,
        status: 'passed', # Dead code is informational
        issues: issues
      }
    end
    
    def check_documentation
      puts "  📚 Checking documentation..."
      
      issues = []
      
      # Check for classes without documentation
      Dir.glob('lib/**/*.rb').each do |file|
        content = File.read(file)
        classes = content.scan(/^class (\w+)/)
        
        classes.each do |class_name|
          class_name = class_name[0]
          # Look for comment above class definition
          class_line_index = content.lines.find_index { |line| line.include?("class #{class_name}") }
          
          if class_line_index && class_line_index > 0
            prev_line = content.lines[class_line_index - 1]
            unless prev_line.strip.start_with?('#')
              issues << {
                file: file,
                line: class_line_index + 1,
                message: "Class #{class_name} lacks documentation",
                severity: 'low',
                category: 'documentation'
              }
            end
          end
        end
      end
      
      {
        last_run: Time.now.iso8601,
        status: issues.empty? ? 'passed' : 'info',
        issues: issues
      }
    end
    
    def check_syntax
      puts "  ✅ Checking Ruby syntax..."
      
      issues = []
      
      Dir.glob('lib/**/*.rb').each do |file|
        stdout, stderr, status = Open3.capture3("ruby -c #{file}")
        
        unless status.success?
          issues << {
            file: file,
            message: "Syntax error: #{stderr.strip}",
            severity: 'critical',
            category: 'syntax'
          }
        end
      end
      
      {
        last_run: Time.now.iso8601,
        status: issues.empty? ? 'passed' : 'failed',
        issues: issues
      }
    end
    
    def check_dependencies
      puts "  📦 Checking dependencies..."
      
      issues = []
      
      # Check if Gemfile.lock is up to date
      if File.exist?('Gemfile') && File.exist?('Gemfile.lock')
        gemfile_time = File.mtime('Gemfile')
        lockfile_time = File.mtime('Gemfile.lock')
        
        if gemfile_time > lockfile_time
          issues << {
            file: 'Gemfile.lock',
            message: 'Gemfile.lock is older than Gemfile - run bundle install',
            severity: 'medium',
            category: 'dependencies'
          }
        end
      end
      
      # Check for missing required gems
      stdout, stderr, status = Open3.capture3('bundle check')
      unless status.success?
        issues << {
          message: 'Bundle check failed - missing dependencies',
          severity: 'high',
          category: 'dependencies',
          details: stderr
        }
      end
      
      {
        last_run: Time.now.iso8601,
        status: issues.empty? ? 'passed' : 'failed',
        issues: issues
      }
    end
    
    def run_integration_smoke_test
      puts "  🧪 Running integration smoke test..."
      
      # Run a basic smoke test
      stdout, stderr, status = Open3.capture3('timeout 30s rspec spec/smoke_test_spec.rb 2>&1 || true')
      
      issues = []
      unless status.success?
        issues << {
          message: 'Smoke test failed',
          severity: 'high',
          category: 'integration',
          details: stderr
        }
      end
      
      {
        last_run: Time.now.iso8601,
        status: issues.empty? ? 'passed' : 'failed',
        issues: issues
      }
    end
    
    def update_tech_debt(results)
      @tech_debt[:metadata][:last_updated] = Time.now.strftime('%Y-%m-%d')
      @tech_debt[:validation_results][:last_run] = Time.now.iso8601
      @tech_debt[:validation_results][:categories] = results
      
      # Calculate summary
      total_issues = 0
      severity_counts = { critical: 0, high: 0, medium: 0, low: 0, info: 0 }
      
      results.each do |_category, checks|
        checks.each do |_check, result|
          next unless result[:issues]
          
          result[:issues].each do |issue|
            total_issues += 1
            severity = issue[:severity]&.to_sym || :medium
            severity_counts[severity] = (severity_counts[severity] || 0) + 1
          end
        end
      end
      
      @tech_debt[:validation_results][:summary] = {
        total_issues: total_issues,
        **severity_counts
      }
      
      save_tech_debt
    end
    
    def save_tech_debt
      FileUtils.mkdir_p(File.dirname(TECH_DEBT_FILE))
      File.write(TECH_DEBT_FILE, JSON.pretty_generate(@tech_debt))
    end
    
    def generate_report(results)
      puts "\n" + "="*80
      puts "📊 VALIDATION REPORT"
      puts "="*80
      
      summary = @tech_debt[:validation_results][:summary]
      puts "\n📈 Summary:"
      puts "  Total Issues: #{summary[:total_issues]}"
      puts "  Critical: #{summary[:critical]} | High: #{summary[:high]} | Medium: #{summary[:medium]}"
      puts "  Low: #{summary[:low]} | Info: #{summary[:info]}"
      
      results.each do |category, checks|
        puts "\n📋 #{category.to_s.tr('_', ' ').capitalize}:"
        checks.each do |check, result|
          status_icon = result[:status] == 'passed' ? '✅' : 
                       result[:status] == 'failed' ? '❌' : 
                       result[:status] == 'error' ? '⚠️' : 'ℹ️'
          
          issue_count = result[:issues]&.size || 0
          puts "  #{status_icon} #{check}: #{issue_count} issues"
          
          if @verbose && result[:issues] && !result[:issues].empty?
            result[:issues].first(3).each do |issue|
              puts "    - #{issue[:file] || 'N/A'}: #{issue[:message]}"
            end
            puts "    ... and #{issue_count - 3} more" if issue_count > 3
          end
        end
      end
      
      puts "\n📂 Tech debt file updated: #{TECH_DEBT_FILE}"
      puts "="*80
    end
    
    # Helper methods
    
    def extract_rubocop_issues(result)
      issues = []
      
      result[:files]&.each do |file_result|
        file_result[:offenses]&.each do |offense|
          issues << {
            file: file_result[:path],
            line: offense[:location][:line],
            column: offense[:location][:column],
            message: offense[:message],
            severity: map_rubocop_severity(offense[:severity]),
            category: 'style',
            cop: offense[:cop_name]
          }
        end
      end
      
      issues
    end
    
    def map_rubocop_severity(severity)
      case severity.to_s.downcase
      when 'error', 'fatal' then 'high'
      when 'warning' then 'medium'
      when 'convention', 'refactor' then 'low'
      else 'info'
      end
    end
    
    def analyze_method_lengths(content, file)
      issues = []
      lines = content.lines
      current_method = nil
      method_start = 0
      indent_level = 0
      
      lines.each_with_index do |line, index|
        # Simple method detection
        if line.strip.match(/^def\s+(\w+)/)
          method_name = $1
          current_method = method_name
          method_start = index + 1
          indent_level = line.index('def') || 0
        elsif line.strip == 'end' && current_method
          current_indent = line.index('end') || 0
          if current_indent <= indent_level
            method_length = index - method_start + 1
            
            if method_length > 50
              issues << {
                file: file,
                line: method_start,
                message: "Long method: #{current_method} (#{method_length} lines)",
                severity: 'medium',
                category: 'complexity'
              }
            end
            
            current_method = nil
          end
        end
      end
      
      issues
    end
    
    def extract_coverage_info(output)
      # Try to extract coverage percentage from SimpleCov output
      coverage_match = output.match(/(\d+\.\d+)% covered/)
      percentage = coverage_match ? coverage_match[1].to_f : nil
      
      { percentage: percentage }
    end
    
    def extract_method_names(content)
      content.scan(/^\s*def\s+(\w+)/).flatten
    end
  end
end