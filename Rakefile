require 'rake/testtask'
require 'bundler/gem_tasks' if File.exist?('lantae.gemspec')

desc "Run all tests"
task :test => ['test:unit', 'test:integration', 'test:spec']

namespace :test do
  desc "Run unit tests"
  task :unit do
    sh 'rspec spec/ --exclude-pattern "spec/integration/**/*_spec.rb,spec/lsp/**/*_spec.rb"'
  end

  desc "Run integration tests"
  task :integration do
    sh 'rspec spec/integration/'
  end

  desc "Run LSP tests"
  task :lsp do
    sh 'rspec spec/lsp/'
  end

  desc "Run all specs"
  task :spec do
    sh 'rspec'
  end

  desc "Run smoke test"
  task :smoke do
    sh 'rspec spec/smoke_test_spec.rb'
  end
end

desc "Install dependencies"
task :install do
  sh 'bundle install'
  sh './install.sh'
end

desc "Setup API keys"
task :setup do
  sh 'ruby scripts/setup-secrets.rb'
end

desc "Start REPL"
task :repl do
  sh 'ruby lib/ruby/enhanced_repl.rb'
end


desc "Update documentation"
task :docs do
  sh './scripts/update-lantae-docs.sh'
end

desc "Clean temporary files"
task :clean do
  sh 'rm -rf tmp/*'
  sh 'rm -rf data/logs/*'
  sh 'rm -f install.log'
end

desc "Run linter"
task :lint do
  sh 'rubocop lib/ spec/'
end

desc "Start LSP server"
task :lsp do
  sh 'ruby lib/ruby/lsp/server_runner.rb'
end

desc "Show available models"
task :models do
  sh './lantae model list'
end

desc "Show cost tracking"
task :cost do
  sh './lantae cost'
end

# Validation and Tech Debt Tasks
namespace :validate do
  desc "Run all code validations (static, QA, dry-run)"
  task :all do
    require_relative 'lib/ruby/validation_engine'
    engine = Lantae::ValidationEngine.new(verbose: true)
    engine.run_all_validations
  end

  desc "Run static code analysis (RuboCop, security, complexity)"
  task :static do
    require_relative 'lib/ruby/validation_engine'
    engine = Lantae::ValidationEngine.new(verbose: true)
    engine.run_static_analysis
  end

  desc "Run QA checks (coverage, dead code, documentation)"
  task :qa do
    require_relative 'lib/ruby/validation_engine'
    engine = Lantae::ValidationEngine.new(verbose: true)
    engine.run_qa_checks
  end

  desc "Run dry-run validation (syntax, dependencies, smoke test)"
  task :dry_run do
    require_relative 'lib/ruby/validation_engine'
    engine = Lantae::ValidationEngine.new(verbose: true)
    engine.run_dry_run_validation
  end

  desc "Run validations and auto-fix issues where possible"
  task :fix do
    require_relative 'lib/ruby/validation_engine'
    engine = Lantae::ValidationEngine.new(verbose: true, fix: true)
    engine.run_all_validations
  end
end

namespace :debt do
  desc "Show tech debt summary"
  task :summary do
    ruby 'lib/ruby/debt_reporter.rb summary'
  end

  desc "Show detailed tech debt report"
  task :report do
    ruby 'lib/ruby/debt_reporter.rb report'
  end

  desc "Add manual tech debt item"
  task :add, [:title, :description, :priority] do |t, args|
    if args[:title].nil? || args[:description].nil?
      puts "Usage: rake debt:add['Title','Description','priority']"
      puts "Priority: critical, high, medium, low (default: medium)"
      exit 1
    end
    
    priority = args[:priority] || 'medium'
    ruby "lib/ruby/debt_reporter.rb add '#{args[:title]}' '#{args[:description]}' #{priority}"
  end

  desc "Mark tech debt item as resolved"
  task :resolve, [:id] do |t, args|
    if args[:id].nil?
      puts "Usage: rake debt:resolve[item_id]"
      exit 1
    end
    
    ruby "lib/ruby/debt_reporter.rb resolve #{args[:id]}"
  end

  desc "Generate tech debt HTML report"
  task :html do
    ruby 'lib/ruby/debt_reporter.rb html'
  end

  desc "Export tech debt to CSV"
  task :csv do
    ruby 'lib/ruby/debt_reporter.rb csv'
  end
end

# Aliases for convenience
desc "Run all validations (alias for validate:all)"
task :validate => 'validate:all'

desc "Show tech debt report (alias for debt:report)"
task :debt => 'debt:report'

task :default => :test