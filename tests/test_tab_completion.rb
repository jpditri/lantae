#!/usr/bin/env ruby
require 'readline'

# Test if Readline tab completion is working
puts "Testing Readline tab completion..."
puts "Readline version: #{Readline::VERSION rescue 'Unknown'}"
puts "Completion enabled: #{Readline.respond_to?(:completion_proc=)}"

# Simple test completion
test_words = %w[hello help history home house]

Readline.completion_proc = proc do |str|
  test_words.grep(/^#{Regexp.escape(str)}/)
end

puts "\nType 'h' and press TAB to test completion (type 'exit' to quit):"
while line = Readline.readline('> ', true)
  break if line == 'exit'
  puts "You typed: #{line}"
end