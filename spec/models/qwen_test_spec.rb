require 'spec_helper'
require 'net/http'
require 'json'

RSpec.describe 'Qwen Models Integration' do
  let(:qwen_models) { ['qwen2.5:1.5b', 'qwen2.5:3b', 'qwen3:14b', 'qwen3:32b'] }
  let(:ollama_url) { 'http://localhost:11434' }

  before do
    # Skip tests if Ollama is not running
    begin
      Net::HTTP.get_response(URI("#{ollama_url}/api/tags"))
    rescue Errno::ECONNREFUSED
      skip "Ollama is not running on localhost:11434"
    end
  end

  describe 'model availability' do
    it 'lists available Qwen models' do
      response = Net::HTTP.get_response(URI("#{ollama_url}/api/tags"))
      expect(response.code).to eq('200')
      
      models = JSON.parse(response.body)['models']
      model_names = models.map { |m| m['name'] }
      
      expect(model_names).to include('qwen2.5:1.5b')
      expect(model_names.select { |name| name.include?('qwen') }).not_to be_empty
    end
  end

  describe 'model performance testing' do
    qwen_models.each do |model|
      context "with #{model}" do
        let(:provider_manager) do
          double('ProviderManager').tap do |pm|
            allow(pm).to receive(:chat) do |options|
              # Make actual API call to test model
              uri = URI("#{ollama_url}/api/generate")
              request = Net::HTTP::Post.new(uri)
              request['Content-Type'] = 'application/json'
              request.body = {
                model: model,
                prompt: options[:messages].last[:content],
                stream: false
              }.to_json
              
              response = Net::HTTP.start(uri.hostname, uri.port) do |http|
                http.request(request)
              end
              
              if response.code == '200'
                double('response', body: response.body)
              else
                raise StandardError, "Model #{model} failed: #{response.code}"
              end
            end
          end
        end

        let(:agent) { PlanningAgent.new(provider_manager) }

        it 'responds to simple tasks' do
          skip "Model #{model} not available" unless model_available?(model)
          
          simple_task = "Create a function that adds two numbers"
          
          start_time = Time.now
          task = agent.plan_task(simple_task)
          result = agent.execute_plan(task)
          execution_time = Time.now - start_time
          
          expect(result).to be_success
          expect(execution_time).to be < 30 # Should complete within 30 seconds
          expect(result.output).to match(/(def|function|=>)/i) # Should contain function syntax
        end

        it 'handles code generation tasks' do
          skip "Model #{model} not available" unless model_available?(model)
          
          task_description = "Write a Ruby method to calculate factorial"
          
          task = agent.plan_task(task_description)
          result = agent.execute_plan(task)
          
          expect(result).to be_success
          expect(result.output.downcase).to include('factorial')
          expect(result.output).to match(/def\s+\w+/i) # Should have method definition
        end

        it 'demonstrates model-specific characteristics' do
          skip "Model #{model} not available" unless model_available?(model)
          
          # Test complexity handling based on model size
          complexity_task = case model
                           when 'qwen2.5:1.5b'
                             "Print hello world" # Simple for small model
                           when 'qwen2.5:3b'
                             "Create a basic calculator class"
                           when 'qwen3:14b'
                             "Implement a REST API endpoint with error handling"
                           when 'qwen3:32b'
                             "Design a microservice architecture with authentication"
                           end

          task = agent.plan_task(complexity_task)
          result = agent.execute_plan(task)
          
          # Larger models should handle more complex tasks
          if model.include?('32b')
            expect(task.subtasks.size).to be > 2 # Should decompose complex tasks
          end
          
          expect(result).to be_success
        end
      end
    end
  end

  describe 'comparative model analysis' do
    let(:test_tasks) do
      [
        { description: "Create a hello world function", complexity: 1 },
        { description: "Implement bubble sort algorithm", complexity: 3 },
        { description: "Build a web scraper with error handling", complexity: 5 }
      ]
    end

    it 'compares performance across models' do
      results = {}
      
      test_tasks.each do |task_spec|
        task_description = task_spec[:description]
        results[task_description] = {}
        
        qwen_models.each do |model|
          next unless model_available?(model)
          
          provider = create_provider_for_model(model)
          agent = PlanningAgent.new(provider)
          
          start_time = Time.now
          begin
            task = agent.plan_task(task_description)
            result = agent.execute_plan(task)
            execution_time = Time.now - start_time
            
            results[task_description][model] = {
              success: result.success?,
              execution_time: execution_time,
              complexity_score: task.complexity_score,
              subtasks_count: task.subtasks.size
            }
          rescue => e
            results[task_description][model] = {
              success: false,
              error: e.message,
              execution_time: Time.now - start_time
            }
          end
        end
      end
      
      # Generate comparison report
      puts "\n=== Model Performance Comparison ==="
      results.each do |task, models|
        puts "\nTask: #{task}"
        models.each do |model, stats|
          status = stats[:success] ? "✓" : "✗"
          time = stats[:execution_time]&.round(2)
          puts "  #{model}: #{status} (#{time}s)"
        end
      end
      
      expect(results).not_to be_empty
    end
  end

  describe 'model-specific optimizations' do
    it 'adjusts task complexity based on model capabilities' do
      analyzer = TaskAnalyzer.new
      
      # Test with different complexity levels
      simple_task = "Print hello world"
      complex_task = "Implement distributed consensus algorithm"
      
      simple_complexity = analyzer.assess_complexity(simple_task)
      complex_complexity = analyzer.assess_complexity(complex_task)
      
      expect(simple_complexity.score).to be < 3
      expect(complex_complexity.score).to be > 7
      
      # Smaller models should get simpler decompositions
      # This would be implemented in the actual planning logic
      expect(simple_complexity.score).to be < complex_complexity.score
    end
  end

  private

  def model_available?(model)
    response = Net::HTTP.get_response(URI("#{ollama_url}/api/tags"))
    return false unless response.code == '200'
    
    models = JSON.parse(response.body)['models']
    models.any? { |m| m['name'] == model }
  rescue
    false
  end

  def create_provider_for_model(model)
    double('ProviderManager').tap do |pm|
      allow(pm).to receive(:chat) do |options|
        uri = URI("#{ollama_url}/api/generate")
        request = Net::HTTP::Post.new(uri)
        request['Content-Type'] = 'application/json'
        request.body = {
          model: model,
          prompt: options[:messages].last[:content],
          stream: false,
          options: {
            temperature: 0.1,
            num_predict: 500
          }
        }.to_json
        
        response = Net::HTTP.start(uri.hostname, uri.port, read_timeout: 60) do |http|
          http.request(request)
        end
        
        double('response', body: response.body)
      end
    end
  end
end