# Simplified provider manager for LSP components
# This is a stub that provides the minimal interface needed by LSP
require 'net/http'
require 'json'
require 'uri'

class ProviderManager
  attr_accessor :current_model, :current_provider

  def initialize
    @current_provider = 'ollama'
    @current_model = 'cogito:latest'
  end

  def ensure_provider(provider, options = {})
    @current_provider = provider.to_s
    @current_model = options[:model] if options[:model]
  end

  def chat(prompt, options = {})
    # For LSP tests, return a simple formatted response
    if prompt.is_a?(Array)
      # Handle message array format
      prompt = prompt.map { |m| m[:content] || m['content'] }.join("\n")
    end

    # Simulate AI response for tests
    case prompt
    when /refactor/i
      "Here's the refactored code:\n```ruby\ndef optimized_method\n  # Refactored code\nend\n```"
    when /optimize.*performance/i
      "```ruby\ndef fast_method\n  # Optimized for performance\nend\n```"
    when /generate.*test/i
      "```ruby\nRSpec.describe 'Test' do\n  it 'works' do\n    expect(true).to be true\n  end\nend\n```"
    when /documentation/i
      "```ruby\n# Well-documented method\n# @param x [Integer] the input\n# @return [Integer] the result\ndef documented_method(x)\n  x * 2\nend\n```"
    when /complexity/i
      "Complexity Analysis:\n- Cyclomatic complexity: 2\n- Time complexity: O(n)\n- Space complexity: O(1)"
    else
      "AI response for: #{prompt[0..50]}..."
    end
  end

  def switch_provider(provider, model = nil)
    @current_provider = provider
    @current_model = model if model
  end

  def list_models
    case @current_provider
    when 'ollama'
      ['cogito:latest', 'qwq:32b', 'llama3.1:latest']
    else
      ["#{@current_provider}-model-1", "#{@current_provider}-model-2"]
    end
  end

  def calculate_context_usage(messages)
    # Simple approximation: ~4 characters per token
    total_chars = messages.sum { |msg| msg[:content].to_s.length }
    (total_chars / 4.0).ceil
  end

  def context_window
    8192
  end

  def remaining_context(messages)
    used_tokens = calculate_context_usage(messages)
    context_window - used_tokens
  end

  def context_percentage_used(messages)
    used_tokens = calculate_context_usage(messages)
    ((used_tokens.to_f / context_window) * 100).round(1)
  end
end

# Stub for OllamaProvider used in tests
class OllamaProvider
  def initialize
    @base_url = 'http://localhost:11434'
  end

  def set_tool_manager(tool_manager)
    @tool_manager = tool_manager
  end

  def chat(model, messages, options = {})
    # Simple stub for testing
    "Response from #{model}"
  end

  def list_models
    ['cogito:latest', 'qwq:32b']
  end
end