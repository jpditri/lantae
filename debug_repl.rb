#!/usr/bin/env ruby

# Load the lantae binary to test tab completion
require_relative 'bin/lantae'

# Override setup_autocomplete to add debugging
module Kernel
  alias_method :original_setup_autocomplete, :setup_autocomplete
  
  def setup_autocomplete(provider_manager, tool_manager, mcp_manager = nil)
    puts "DEBUG: Setting up autocomplete..."
    puts "DEBUG: Readline.completion_proc before: #{Readline.completion_proc.inspect}"
    
    # Call original
    original_setup_autocomplete(provider_manager, tool_manager, mcp_manager)
    
    puts "DEBUG: Readline.completion_proc after: #{Readline.completion_proc.inspect}"
    puts "DEBUG: completion_append_character: #{Readline.completion_append_character}"
    
    # Test the completion proc
    if Readline.completion_proc
      puts "\nDEBUG: Testing completion proc..."
      test_inputs = ["/", "/h", "/help", "/model", "/provider o"]
      test_inputs.each do |input|
        results = Readline.completion_proc.call(input)
        puts "  Input: '#{input}' => #{results.inspect}"
      end
    end
  end
end

# Create minimal test objects
class MockProviderManager
  def list_models
    %w[cogito:latest llama3:latest]
  end
end

class MockToolManager
  def list_available_tools
    %w[bash cat ls pwd]
  end
end

# Test the setup
puts "Testing Lantae tab completion setup..."
provider_manager = MockProviderManager.new
tool_manager = MockToolManager.new

setup_autocomplete(provider_manager, tool_manager, nil)