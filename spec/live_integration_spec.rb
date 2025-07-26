require 'spec_helper'
require 'net/http'
require 'json'

RSpec.describe 'Live Model Integration Tests', :integration do
  let(:ollama_url) { 'http://localhost:11434' }
  let(:test_model) { 'qwen2.5:1.5b' }

  before do
    # Skip tests if Ollama is not running
    begin
      response = Net::HTTP.get_response(URI("#{ollama_url}/api/tags"))
      skip "Ollama is not running on localhost:11434" unless response.code == '200'
      
      models = JSON.parse(response.body)['models']
      model_names = models.map { |m| m['name'] }
      skip "Test model #{test_model} not available" unless model_names.include?(test_model)
    rescue Errno::ECONNREFUSED
      skip "Cannot connect to Ollama service"
    end
  end

  describe 'Basic Model Communication' do
    it 'can communicate with Ollama API' do
      uri = URI("#{ollama_url}/api/generate")
      request = Net::HTTP::Post.new(uri)
      request['Content-Type'] = 'application/json'
      request.body = {
        model: test_model,
        prompt: "Say hello",
        stream: false
      }.to_json
      
      response = Net::HTTP.start(uri.hostname, uri.port, read_timeout: 30) do |http|
        http.request(request)
      end
      
      expect(response.code).to eq('200')
      result = JSON.parse(response.body)
      expect(result['response']).to be_a(String)
      expect(result['response'].downcase).to include('hello')
    end
  end

  describe 'Code Generation Test' do
    it 'generates valid Ruby code' do
      uri = URI("#{ollama_url}/api/generate")
      request = Net::HTTP::Post.new(uri)
      request['Content-Type'] = 'application/json'
      request.body = {
        model: test_model,
        prompt: "Write a Ruby function that calculates factorial of a number",
        stream: false
      }.to_json
      
      response = Net::HTTP.start(uri.hostname, uri.port, read_timeout: 60) do |http|
        http.request(request)
      end
      
      expect(response.code).to eq('200')
      result = JSON.parse(response.body)
      code = result['response']
      
      expect(code).to be_a(String)
      expect(code.downcase).to include('def')
      expect(code.downcase).to include('factorial')
    end
  end

  describe 'Task Complexity vs Model Performance' do
    let(:simple_task) { "Create a function that adds two numbers" }
    let(:medium_task) { "Create a class with methods for basic calculator operations" }
    let(:complex_task) { "Implement a recursive algorithm with error handling" }

    [
      { task: "simple_task", complexity: 1 },
      { task: "medium_task", complexity: 3 },
      { task: "complex_task", complexity: 5 }
    ].each do |test_case|
      it "handles #{test_case[:task]} (complexity: #{test_case[:complexity]})" do
        task_description = case test_case[:task]
                         when "simple_task" then simple_task
                         when "medium_task" then medium_task
                         when "complex_task" then complex_task
                         end

        uri = URI("#{ollama_url}/api/generate")
        request = Net::HTTP::Post.new(uri)
        request['Content-Type'] = 'application/json'
        request.body = {
          model: test_model,
          prompt: task_description,
          stream: false,
          options: {
            temperature: 0.1,
            num_predict: 300
          }
        }.to_json
        
        start_time = Time.now
        response = Net::HTTP.start(uri.hostname, uri.port, read_timeout: 90) do |http|
          http.request(request)
        end
        execution_time = Time.now - start_time
        
        expect(response.code).to eq('200')
        result = JSON.parse(response.body)
        
        # Basic validation
        expect(result['response']).to be_a(String)
        expect(result['response'].length).to be > 10
        
        # Log performance for analysis
        puts "\n#{test_case[:task].upcase}:"
        puts "  Execution time: #{execution_time.round(2)}s"
        puts "  Response length: #{result['response'].length} chars"
        puts "  Contains code: #{result['response'].include?('def') || result['response'].include?('class')}"
      end
    end
  end

  describe 'Planning Agent with Real Model' do
    let(:mock_provider) do
      double('ProviderManager').tap do |pm|
        allow(pm).to receive(:chat) do |options|
          # Extract the actual prompt
          prompt = if options[:messages]
                    options[:messages].last[:content]
                  else
                    options[:prompt] || "default task"
                  end

          # Make real API call
          uri = URI("#{ollama_url}/api/generate")
          request = Net::HTTP::Post.new(uri)
          request['Content-Type'] = 'application/json'
          request.body = {
            model: test_model,
            prompt: prompt,
            stream: false,
            options: {
              temperature: 0.1,
              num_predict: 200
            }
          }.to_json
          
          response = Net::HTTP.start(uri.hostname, uri.port, read_timeout: 60) do |http|
            http.request(request)
          end
          
          if response.code == '200'
            double('response', body: response.body)
          else
            raise StandardError, "Model request failed: #{response.code}"
          end
        end
      end
    end

    let(:tool_manager) { double('ToolManager') }
    let(:agent) { PlanningAgent.new(mock_provider, tool_manager) }

    it 'plans and analyzes a task with real model' do
      task_description = "Create a simple calculator function"
      
      # Plan the task
      task = agent.plan_task(task_description)
      
      expect(task).to be_a(Task)
      expect(task.description).to eq(task_description)
      
      # Check if complexity was assessed
      expect(task.complexity).to respond_to(:score) if task.complexity
      
      puts "\nPLANNING AGENT TEST:"
      puts "  Task: #{task.description}"
      puts "  Subtasks: #{task.subtasks.size}"
      puts "  Complexity: #{task.complexity&.score || 'Not assessed'}"
    end
  end

  describe 'Performance Benchmark' do
    it 'measures model response times for different request types' do
      test_cases = [
        { name: "Simple greeting", prompt: "Hello", expected_time: 5 },
        { name: "Code generation", prompt: "def add(a, b); a + b; end", expected_time: 10 },
        { name: "Algorithm request", prompt: "Write a bubble sort in Ruby", expected_time: 15 }
      ]

      results = []
      
      test_cases.each do |test_case|
        uri = URI("#{ollama_url}/api/generate")
        request = Net::HTTP::Post.new(uri)
        request['Content-Type'] = 'application/json'
        request.body = {
          model: test_model,
          prompt: test_case[:prompt],
          stream: false
        }.to_json
        
        start_time = Time.now
        response = Net::HTTP.start(uri.hostname, uri.port, read_timeout: 30) do |http|
          http.request(request)
        end
        execution_time = Time.now - start_time
        
        result = {
          name: test_case[:name],
          time: execution_time,
          success: response.code == '200',
          within_expected: execution_time <= test_case[:expected_time]
        }
        results << result
        
        expect(response.code).to eq('200')
        expect(execution_time).to be < 30 # Maximum reasonable time
      end
      
      puts "\nPERFORMANCE BENCHMARK RESULTS:"
      results.each do |result|
        status = result[:success] ? "✓" : "✗"
        timing = result[:within_expected] ? "FAST" : "SLOW"
        puts "  #{status} #{result[:name]}: #{result[:time].round(2)}s (#{timing})"
      end
      
      # At least 2 out of 3 should be within expected time
      fast_responses = results.count { |r| r[:within_expected] }
      expect(fast_responses).to be >= 2
    end
  end
end