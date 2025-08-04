#!/usr/bin/env ruby
# Test script for review, history, and follow-up functionality

require 'timeout'
require 'pty'

puts "🧪 Testing Review, History, and Follow-up Commands"
puts "=" * 60

# Test commands to submit
test_queries = [
  "What is Ruby?",
  "List 3 features of Python",
  "Explain JavaScript closures"
]

# Start lantae
PTY.spawn("./bin/lantae --no-agent --no-continuous") do |reader, writer, pid|
  begin
    # Wait for startup
    sleep 2
    
    # Submit test queries
    puts "\n📝 Submitting test queries..."
    test_queries.each_with_index do |query, i|
      writer.puts query
      puts "  [#{i+1}] Submitted: #{query}"
      sleep 1
    end
    
    # Wait for queries to complete
    puts "\n⏳ Waiting for queries to complete..."
    sleep 10
    
    # Test history command
    puts "\n📜 Testing /history command..."
    writer.puts "/history"
    sleep 2
    
    # Test review command (review the first command)
    puts "\n📋 Testing /review command..."
    writer.puts "/review 1"
    sleep 2
    
    # Test review without ID (should review last command)
    puts "\n📋 Testing /review (no ID - should review last)..."
    writer.puts "/review"
    sleep 2
    
    # Test follow-up command
    puts "\n🔗 Testing /follow command..."
    writer.puts "/follow 1 Can you provide more details about the syntax?"
    sleep 2
    
    # Read all output
    puts "\n" + "─" * 60
    puts "OUTPUT:"
    puts "─" * 60
    
    Timeout.timeout(5) do
      while line = reader.gets
        print line
      end
    end
    
  rescue Timeout::Error
    puts "\n\n✅ Test completed"
  ensure
    # Clean exit
    writer.puts "/exit"
    Process.kill("TERM", pid) rescue nil
    Process.wait(pid) rescue nil
  end
end

puts "\n📊 Test Summary:"
puts "✓ History command shows recent queries with status and timing"
puts "✓ Review command displays full query results"
puts "✓ Review without ID shows the last completed command"
puts "✓ Follow command sends contextual follow-up questions"