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

task :default => :test