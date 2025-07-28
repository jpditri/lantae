#!/usr/bin/env ruby
require 'readline'

# Extract the completion logic from lantae
slash_commands = %w[help model provider models tool tools mcp clear info env lsp agent squad task]
providers = %w[ollama openai anthropic claude bedrock gemini mistral perplexity]
models = %w[cogito:latest llama3:latest codellama:latest mistral:latest]
tools = %w[bash cat ls pwd git write_file edit_file create_file delete_file mkdir find]

comp = proc do |input|
  begin
    completions = []
    
    return [] if input.nil? || input.empty?
    
    if input.start_with?('/')
      parts = input[1..-1].split(' ', 2)
      command = parts[0] || ''
      args = parts[1] || ''
      
      if parts.length == 1
        # Complete slash command itself
        completions = slash_commands.select { |cmd| cmd.start_with?(command) }.map { |cmd| "/#{cmd}" }
      else
        # Complete command arguments
        case command
        when 'provider'
          if args.split(' ').length == 1
            completions = providers.select { |p| p.start_with?(args) }.map { |p| "/provider #{p}" }
          end
        when 'model'
          completions = models.select { |m| m.start_with?(args) }.map { |m| "/model #{m}" }
        end
      end
    else
      # Non-slash command completions (file paths)
      begin
        completions = Dir.glob("#{input}*").select { |f| File.file?(f) || File.directory?(f) }
      rescue => e
        completions = []
      end
    end
    
    completions || []
  rescue => e
    []
  end
end

# Test the completion function
puts "Testing Lantae completion logic..."
test_inputs = [
  "/",
  "/h",
  "/help",
  "/model",
  "/model c",
  "/provider", 
  "/provider o",
  "test",
  ""
]

test_inputs.each do |input|
  results = comp.call(input)
  puts "Input: '#{input}' => #{results.inspect}"
end

# Now test with Readline
puts "\nSetting up Readline with Lantae completion..."
Readline.completion_proc = comp
Readline.completion_append_character = ' '

puts "\nTest tab completion now (type 'exit' to quit):"
puts "Try typing:"
puts "  /h<TAB>      - should complete to /help"
puts "  /model<TAB>  - should show model options"
puts "  /provider o<TAB> - should complete ollama"

while line = Readline.readline('> ', true)
  break if line == 'exit'
  puts "You typed: #{line}"
end