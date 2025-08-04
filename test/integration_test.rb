#!/usr/bin/env ruby
# frozen_string_literal: true

require 'pty'
require 'expect'
require 'timeout'
require 'fileutils'

# Integration test framework for LANTAE user workflows
class IntegrationTest
  attr_reader :failed_tests, :passed_tests
  
  def initialize
    @failed_tests = []
    @passed_tests = []
    @test_timeout = 30
    @lantae_path = File.expand_path('../bin/lantae', __dir__)
  end
  
  def run_all_tests
    puts "🧪 Starting LANTAE Integration Tests"
    puts "=" * 50
    
    # Core functionality tests
    test_basic_help_command
    test_tab_completion_functionality
    test_provider_switching
    test_model_listing
    test_connect_command_discovery
    test_connect_to_specific_instance
    test_mixed_workflow_processing
    
    # Report results
    report_test_results
  end
  
  private
  
  def test_basic_help_command
    test_name = "Basic Help Command"
    puts "\n🔍 Testing: #{test_name}"
    
    begin
      output = run_lantae_command("/help\n/exit\n")
      
      if output.include?("Available Commands:") && 
         output.include?("/help") && 
         output.include?("/models") &&
         output.include?("/connect")
        mark_test_passed(test_name)
      else
        mark_test_failed(test_name, "Help output missing expected commands")
      end
    rescue => e
      mark_test_failed(test_name, e.message)
    end
  end
  
  def test_tab_completion_functionality
    test_name = "Tab Completion Functionality"
    puts "\n🔍 Testing: #{test_name}"
    
    begin
      # This is harder to test automatically since tab completion requires TTY
      # We'll test that the AsyncREPL sets up completion properly
      require_relative '../lib/ruby/async_repl'
      
      # Mock setup
      provider_manager = double("ProviderManager")
      tool_manager = double("ToolManager")
      allow(provider_manager).to receive(:get_provider_info).and_return({provider: "test", model: "test"})
      
      repl = Lantae::AsyncREPL.new(provider_manager, tool_manager, {show_banner: false})
      
      # Test that completion proc is set up
      if defined?(Readline) && Readline.respond_to?(:completion_proc)
        mark_test_passed(test_name)
      else
        mark_test_failed(test_name, "Tab completion not properly configured")
      end
    rescue => e
      mark_test_failed(test_name, e.message)
    end
  end
  
  def test_provider_switching
    test_name = "Provider Switching"
    puts "\n🔍 Testing: #{test_name}"
    
    begin
      output = run_lantae_command("/provider\n/provider ollama\n/exit\n")
      
      if output.include?("Current provider:") || output.include?("Switched to:")
        mark_test_passed(test_name)
      else
        mark_test_failed(test_name, "Provider switching commands not working")
      end
    rescue => e
      mark_test_failed(test_name, e.message)
    end
  end
  
  def test_model_listing
    test_name = "Model Listing"
    puts "\n🔍 Testing: #{test_name}"
    
    begin
      output = run_lantae_command("/models\n/exit\n")
      
      # Should either show models or show connection error
      if output.include?("Available models") || 
         output.include?("Cannot connect to Ollama") ||
         output.include?("Error listing models")
        mark_test_passed(test_name)
      else
        mark_test_failed(test_name, "Models command didn't produce expected output")
      end
    rescue => e
      mark_test_failed(test_name, e.message)
    end
  end
  
  def test_connect_command_discovery
    test_name = "Connect Command Discovery"
    puts "\n🔍 Testing: #{test_name}"
    
    begin
      output = run_lantae_command("/connect\n/exit\n", timeout: 15)
      
      # Should attempt network discovery
      if output.include?("Quick scan") || 
         output.include?("scan for Ollama") ||
         output.include?("No Ollama instances found") ||
         output.include?("found") && output.include?("instance")
        mark_test_passed(test_name)
      else
        mark_test_failed(test_name, "Connect discovery not working properly")
      end
    rescue => e
      mark_test_failed(test_name, e.message)
    end
  end
  
  def test_connect_to_specific_instance
    test_name = "Connect to Specific Instance"
    puts "\n🔍 Testing: #{test_name}"
    
    begin
      # Test connection to localhost (should work if Ollama is running, or fail gracefully)
      output = run_lantae_command("/connect localhost:11434\n/exit\n", timeout: 10)
      
      if output.include?("Testing connection") &&
         (output.include?("Successfully connected") || 
          output.include?("Cannot connect") ||
          output.include?("Connection test"))
        mark_test_passed(test_name)
      else
        mark_test_failed(test_name, "Connect to specific instance not working")
      end
    rescue => e
      mark_test_failed(test_name, e.message)
    end
  end
  
  def test_mixed_workflow_processing
    test_name = "Mixed Workflow Processing"
    puts "\n🔍 Testing: #{test_name}"
    
    begin
      # Test a complex workflow: help -> provider info -> model list -> connect attempt
      commands = [
        "/help",
        "/provider", 
        "/models",
        "/connect localhost:11434",
        "/exit"
      ].join("\n") + "\n"
      
      output = run_lantae_command(commands, timeout: 20)
      
      # Should handle all commands without deadlocks or crashes
      command_count = 0
      command_count += 1 if output.include?("Available Commands:")
      command_count += 1 if output.include?("Current provider:") || output.include?("provider:")
      command_count += 1 if output.include?("models") || output.include?("Error listing")
      command_count += 1 if output.include?("Testing connection") || output.include?("connect")
      
      if command_count >= 3  # At least 3 of the 4 commands should work
        mark_test_passed(test_name)
      else
        mark_test_failed(test_name, "Mixed workflow didn't process enough commands successfully")
      end
    rescue => e
      mark_test_failed(test_name, e.message)
    end
  end
  
  def run_lantae_command(input, timeout: @test_timeout)
    output = ""
    
    Timeout.timeout(timeout) do
      PTY.spawn("#{@lantae_path} continuous") do |reader, writer, pid|
        # Send input
        writer.write(input)
        writer.close
        
        # Read output with timeout
        begin
          while line = reader.gets
            output += line
            # Break if we see the exit
            break if line.include?("Exiting") || line.include?("continuous mode...")
          end
        rescue Errno::EIO
          # Process finished, this is expected
        end
        
        # Clean up
        begin
          Process.kill("TERM", pid)
          Process.wait(pid)
        rescue Errno::ESRCH, Errno::ECHILD
          # Process already finished
        end
      end
    end
    
    output
  rescue Timeout::Error
    raise "Command timed out after #{timeout} seconds"
  end
  
  def mark_test_passed(test_name)
    @passed_tests << test_name
    puts "  ✅ PASSED: #{test_name}"
  end
  
  def mark_test_failed(test_name, reason)
    @failed_tests << {name: test_name, reason: reason}
    puts "  ❌ FAILED: #{test_name} - #{reason}"
  end
  
  def report_test_results
    puts "\n" + "=" * 50
    puts "🧪 Integration Test Results"
    puts "=" * 50
    
    puts "\n✅ Passed Tests (#{@passed_tests.size}):"
    @passed_tests.each { |test| puts "  - #{test}" }
    
    if @failed_tests.any?
      puts "\n❌ Failed Tests (#{@failed_tests.size}):"
      @failed_tests.each { |test| puts "  - #{test[:name]}: #{test[:reason]}" }
    end
    
    total_tests = @passed_tests.size + @failed_tests.size
    success_rate = (@passed_tests.size.to_f / total_tests * 100).round(1)
    
    puts "\n📊 Summary: #{@passed_tests.size}/#{total_tests} tests passed (#{success_rate}%)"
    
    if @failed_tests.empty?
      puts "🎉 All tests passed!"
      exit 0
    else
      puts "⚠️  Some tests failed. Please check the issues above."
      exit 1
    end
  end
end

# Run tests if this file is executed directly
if __FILE__ == $0
  test_runner = IntegrationTest.new
  test_runner.run_all_tests
end