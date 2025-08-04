#!/usr/bin/env ruby
# Test script to demonstrate side panel with pending queries

require 'timeout'

# Simple test to submit multiple queries and see the side panel
commands = [
  "Tell me a joke",
  "What's 2+2?", 
  "Explain quantum physics in one sentence",
  "List 3 programming languages",
  "What's the weather like?"
]

puts "🧪 Testing Side Panel Query Queue Display"
puts "=" * 50

# Start lantae with side panel enabled
IO.popen("./bin/lantae --no-agent --no-continuous", "r+") do |pipe|
  begin
    # Wait for startup
    sleep 2
    
    # Enable side panel
    pipe.puts "/side"
    sleep 1
    
    # Submit multiple queries rapidly
    puts "📝 Submitting #{commands.length} queries..."
    commands.each_with_index do |cmd, i|
      pipe.puts cmd
      puts "  Submitted [#{i+1}]: #{cmd}"
      sleep 0.2  # Small delay between submissions
    end
    
    # Give time to see the queue in action
    puts "\n⏳ Watching queue processing (30 seconds)..."
    
    # Read output for 30 seconds
    Timeout.timeout(30) do
      while line = pipe.gets
        print line
      end
    end
    
  rescue Timeout::Error
    puts "\n\n✅ Test completed - timeout reached"
  ensure
    # Clean exit
    pipe.puts "/exit"
  end
end

puts "\n📊 Test Summary:"
puts "- Multiple queries were submitted to test queue display"
puts "- Side panel should have shown pending queries with status"
puts "- Running queries should have displayed elapsed time"