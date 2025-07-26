require 'simplecov'
SimpleCov.start do
  add_filter '/spec/'
  add_filter '/tests/'
  add_group 'Agents', 'lib/ruby/planning_agent.rb'
  add_group 'Analysis', 'lib/ruby/task_analyzer.rb'
  add_group 'Execution', 'lib/ruby/execution_engine.rb'
  add_group 'Database', 'lib/ruby/task_database.rb'
  add_group 'Fixes', 'lib/ruby/auto_fixer.rb'
  add_group 'LSP Server', 'lib/ruby/lsp/server.rb'
  add_group 'LSP Client', 'lib/ruby/lsp/client.rb'
  add_group 'LSP Code Actions', 'lib/ruby/lsp/code_actions.rb'
end

require 'rspec'
require 'webmock/rspec'
require 'timecop'
require 'json'
require 'tempfile'
require 'fileutils'

# Load all library files
Dir[File.join(File.dirname(__FILE__), '..', 'lib', 'ruby', '*.rb')].each do |file|
  require file
end

# Shared test helpers
module TestHelpers
  def create_test_database
    Tempfile.new(['test_tasks', '.db']).tap do |file|
      TaskDatabase.new(file.path)
    end
  end

  def mock_provider_response(content)
    double('response', 
      body: { 
        'response' => content,
        'done' => true
      }.to_json
    )
  end

  def with_temp_file(content = '')
    Tempfile.new('test').tap do |file|
      file.write(content)
      file.flush
      yield file.path if block_given?
    end
  end
end

RSpec.configure do |config|
  config.include TestHelpers
  
  # Allow real HTTP connections for integration tests
  config.before(:each, :integration) do
    WebMock.allow_net_connect!
  end
  
  config.after(:each, :integration) do
    WebMock.disable_net_connect!
  end
  
  config.expect_with :rspec do |expectations|
    expectations.include_chain_clauses_in_custom_matcher_descriptions = true
  end

  config.mock_with :rspec do |mocks|
    mocks.verify_partial_doubles = true
  end

  config.shared_context_metadata_behavior = :apply_to_host_groups
  config.filter_run_when_matching :focus
  config.example_status_persistence_file_path = "spec/examples.txt"
  config.disable_monkey_patching!
  config.warnings = true

  if config.files_to_run.one?
    config.default_formatter = "doc"
  end

  config.profile_examples = 10
  config.order = :random
  Kernel.srand config.seed

  # Clean up temp files after each test
  config.after(:each) do
    Dir.glob('/tmp/test_tasks*.db').each { |f| File.delete(f) rescue nil }
    Dir.glob('/tmp/test*').select { |f| File.file?(f) }.each { |f| File.delete(f) rescue nil }
  end
end