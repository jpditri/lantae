require 'spec_helper'
require 'net/http'
require 'json'
require 'fileutils'
require 'benchmark'

RSpec.describe 'Focused Batch Testing with Performance Analysis', :integration do
  let(:ollama_url) { 'http://localhost:11434' }
  
  OUTPUT_DIR = File.join(Dir.pwd, 'batch_language_results')
  
  # Test with 2 models: smallest and largest available
  FOCUSED_MODELS = ['qwen2.5:1.5b', 'qwen3:32b']
  
  # Reduced set of languages for focused testing
  FOCUSED_LANGUAGES = ['python', 'javascript', 'rust']
  
  # Core tasks that showcase different capabilities
  FOCUSED_TASKS = [
    {
      id: 'fibonacci',
      name: 'Fibonacci Implementation',
      prompt: 'Write a function to calculate the nth Fibonacci number efficiently'
    },
    {
      id: 'api_server',
      name: 'REST API Server',
      prompt: 'Create a simple REST API server with GET and POST endpoints for a todo list'
    },
    {
      id: 'concurrent',
      name: 'Concurrent Processing',
      prompt: 'Write code that processes a list of URLs concurrently and returns their response times'
    }
  ]
  
  LANGUAGE_PROMPTS = {
    'python' => "Write Python code: %s. Use type hints and follow PEP 8.",
    'javascript' => "Write JavaScript code: %s. Use async/await and ES6+ features.",
    'rust' => "Write Rust code: %s. Ensure memory safety and use proper error handling."
  }
  
  before(:all) do
    FileUtils.mkdir_p(OUTPUT_DIR)
  end
  
  before do
    begin
      response = Net::HTTP.get_response(URI("#{ollama_url}/api/tags"))
      skip "Ollama is not running" unless response.code == '200'
    rescue Errno::ECONNREFUSED
      skip "Cannot connect to Ollama service"
    end
  end
  
  describe 'Batch vs Sequential Performance Comparison' do
    it 'compares batch processing efficiency against sequential processing' do
      response = Net::HTTP.get_response(URI("#{ollama_url}/api/tags"))
      available_models = JSON.parse(response.body)['models'].map { |m| m['name'] }
      
      models_to_test = FOCUSED_MODELS.select { |m| available_models.include?(m) }
      
      if models_to_test.empty?
        skip "No focused models available"
      end
      
      puts "\n" + "="*100
      puts "BATCH PROCESSING EFFICIENCY TEST"
      puts "="*100
      puts "Models: #{models_to_test.join(', ')}"
      puts "Languages: #{FOCUSED_LANGUAGES.join(', ')}"
      puts "Tasks: #{FOCUSED_TASKS.map { |t| t[:name] }.join(', ')}"
      puts "Total prompts per model: #{FOCUSED_LANGUAGES.size * FOCUSED_TASKS.size}"
      puts "="*100
      
      results = {
        batch: {},
        sequential: {},
        comparison: {}
      }
      
      models_to_test.each do |model|
        puts "\n\nTESTING MODEL: #{model}"
        puts "="*80
        
        # Test 1: Batch Processing (keep model loaded)
        puts "\n1. BATCH PROCESSING (with keep_alive)"
        batch_start = Time.now
        batch_results = test_batch_processing(model)
        batch_time = Time.now - batch_start
        
        results[:batch][model] = {
          time: batch_time,
          results: batch_results,
          avg_per_prompt: batch_time / (FOCUSED_LANGUAGES.size * FOCUSED_TASKS.size)
        }
        
        # Allow model to unload
        unload_model(model)
        sleep(2)
        
        # Test 2: Sequential Processing (model loads/unloads)
        puts "\n2. SEQUENTIAL PROCESSING (without keep_alive)"
        seq_start = Time.now
        seq_results = test_sequential_processing(model)
        seq_time = Time.now - seq_start
        
        results[:sequential][model] = {
          time: seq_time,
          results: seq_results,
          avg_per_prompt: seq_time / (FOCUSED_LANGUAGES.size * FOCUSED_TASKS.size)
        }
        
        # Calculate efficiency gain
        efficiency_gain = ((seq_time - batch_time) / seq_time * 100).round(1)
        speedup = (seq_time / batch_time).round(2)
        
        results[:comparison][model] = {
          batch_time: batch_time,
          sequential_time: seq_time,
          efficiency_gain: efficiency_gain,
          speedup: speedup,
          time_saved: seq_time - batch_time
        }
        
        display_comparison(model, results[:comparison][model])
      end
      
      # Generate detailed report
      generate_efficiency_report(results)
      
      # Analyze code quality
      analyze_code_samples(results)
      
      # Save results
      save_efficiency_results(results)
      
      expect(results[:batch]).not_to be_empty
    end
  end
  
  private
  
  def test_batch_processing(model)
    all_results = []
    
    FOCUSED_LANGUAGES.each do |language|
      puts "  Processing #{language.upcase} batch..."
      
      FOCUSED_TASKS.each_with_index do |task, index|
        prompt = LANGUAGE_PROMPTS[language] % task[:prompt]
        
        start = Time.now
        response = make_batch_request(model, prompt, keep_alive: "5m")
        request_time = Time.now - start
        
        all_results << {
          language: language,
          task: task[:id],
          response: response,
          time: request_time,
          success: !response.nil?
        }
        
        print "    #{task[:name]}: #{request_time.round(2)}s\n"
      end
    end
    
    all_results
  end
  
  def test_sequential_processing(model)
    all_results = []
    
    FOCUSED_LANGUAGES.each do |language|
      puts "  Processing #{language.upcase} sequentially..."
      
      FOCUSED_TASKS.each do |task|
        prompt = LANGUAGE_PROMPTS[language] % task[:prompt]
        
        start = Time.now
        response = make_sequential_request(model, prompt)
        request_time = Time.now - start
        
        all_results << {
          language: language,
          task: task[:id],
          response: response,
          time: request_time,
          success: !response.nil?
        }
        
        print "    #{task[:name]}: #{request_time.round(2)}s\n"
        
        # Ensure model unloads between requests
        unload_model(model)
        sleep(0.5)
      end
    end
    
    all_results
  end
  
  def make_batch_request(model, prompt, keep_alive: "5m")
    uri = URI("#{ollama_url}/api/generate")
    request = Net::HTTP::Post.new(uri)
    request['Content-Type'] = 'application/json'
    request.body = {
      model: model,
      prompt: prompt,
      stream: false,
      keep_alive: keep_alive,
      options: {
        temperature: 0.1,
        num_predict: 400,
        num_gpu: -1
      }
    }.to_json
    
    response = Net::HTTP.start(uri.hostname, uri.port, read_timeout: 120) do |http|
      http.request(request)
    end
    
    if response.code == '200'
      result = JSON.parse(response.body)
      result['response']
    else
      nil
    end
  rescue => e
    puts "    Error: #{e.message}"
    nil
  end
  
  def make_sequential_request(model, prompt)
    make_batch_request(model, prompt, keep_alive: "0s")
  end
  
  def unload_model(model)
    uri = URI("#{ollama_url}/api/generate")
    request = Net::HTTP::Post.new(uri)
    request['Content-Type'] = 'application/json'
    request.body = {
      model: model,
      keep_alive: "0s"
    }.to_json
    
    Net::HTTP.start(uri.hostname, uri.port) do |http|
      http.request(request)
    end
  rescue
    # Ignore errors
  end
  
  def display_comparison(model, comparison)
    puts "\n  EFFICIENCY COMPARISON:"
    puts "  " + "-"*60
    puts "  Batch Processing Time: #{comparison[:batch_time].round(2)}s"
    puts "  Sequential Processing Time: #{comparison[:sequential_time].round(2)}s"
    puts "  Time Saved: #{comparison[:time_saved].round(2)}s"
    puts "  Efficiency Gain: #{comparison[:efficiency_gain]}%"
    puts "  Speedup Factor: #{comparison[:speedup]}x"
  end
  
  def generate_efficiency_report(results)
    puts "\n\n" + "="*100
    puts "BATCH PROCESSING EFFICIENCY REPORT"
    puts "="*100
    
    # Overall statistics
    total_batch_time = results[:batch].values.sum { |r| r[:time] }
    total_seq_time = results[:sequential].values.sum { |r| r[:time] }
    total_saved = total_seq_time - total_batch_time
    overall_efficiency = ((total_saved / total_seq_time) * 100).round(1)
    
    puts "\n## Overall Performance"
    puts "Total Batch Processing Time: #{total_batch_time.round(2)}s"
    puts "Total Sequential Processing Time: #{total_seq_time.round(2)}s"
    puts "Total Time Saved: #{total_saved.round(2)}s"
    puts "Overall Efficiency Gain: #{overall_efficiency}%"
    
    # Per-model analysis
    puts "\n## Model-Specific Performance"
    puts "\nModel" + " "*20 + "Batch" + " "*10 + "Sequential" + " "*5 + "Speedup"
    puts "-"*70
    
    results[:comparison].each do |model, comp|
      puts "#{model.ljust(25)} #{comp[:batch_time].round(1).to_s.rjust(8)}s  " +
           "#{comp[:sequential_time].round(1).to_s.rjust(10)}s  " +
           "#{comp[:speedup].to_s.rjust(7)}x"
    end
    
    # Task timing analysis
    puts "\n## Average Response Times by Task"
    task_times = {}
    
    FOCUSED_TASKS.each do |task|
      batch_times = []
      seq_times = []
      
      results[:batch].each do |model, data|
        task_results = data[:results].select { |r| r[:task] == task[:id] }
        batch_times.concat(task_results.map { |r| r[:time] })
      end
      
      results[:sequential].each do |model, data|
        task_results = data[:results].select { |r| r[:task] == task[:id] }
        seq_times.concat(task_results.map { |r| r[:time] })
      end
      
      task_times[task[:name]] = {
        batch_avg: batch_times.sum / batch_times.size,
        seq_avg: seq_times.sum / seq_times.size
      }
    end
    
    puts "-"*70
    task_times.each do |task_name, times|
      improvement = ((times[:seq_avg] - times[:batch_avg]) / times[:seq_avg] * 100).round(1)
      puts "#{task_name.ljust(30)} Batch: #{times[:batch_avg].round(2)}s  " +
           "Sequential: #{times[:seq_avg].round(2)}s  (#{improvement}% faster)"
    end
  end
  
  def analyze_code_samples(results)
    puts "\n\n## Code Quality Analysis"
    puts "="*80
    
    # Find best examples from batch processing
    best_examples = []
    
    results[:batch].each do |model, data|
      data[:results].each do |result|
        next unless result[:success] && result[:response]
        
        quality = simple_quality_score(result[:response], result[:language])
        
        best_examples << {
          model: model,
          language: result[:language],
          task: result[:task],
          response: result[:response],
          quality: quality,
          time: result[:time]
        }
      end
    end
    
    # Show top examples
    top_examples = best_examples.sort_by { |e| -e[:quality] }.take(3)
    
    puts "\n### Highest Quality Code Samples"
    top_examples.each_with_index do |example, index|
      puts "\n#{index + 1}. #{example[:model]} - #{example[:language].upcase} - #{FOCUSED_TASKS.find { |t| t[:id] == example[:task] }[:name]}"
      puts "   Quality: #{example[:quality]}/10 | Time: #{example[:time].round(2)}s"
      puts "   " + "-"*60
      puts truncate_code(example[:response], 300)
      puts "   " + "-"*60
    end
  end
  
  def simple_quality_score(response, language)
    return 0 if response.nil? || response.empty?
    
    score = 0
    score += 2 if response.include?('```')
    score += 1 if response.length > 200
    score += 1 if response.length > 500
    
    case language
    when 'python'
      score += 2 if response.match?(/def\s+\w+.*:/)
      score += 1 if response.match?(/async\s+def/)
      score += 1 if response.include?('->') # type hints
    when 'javascript'
      score += 2 if response.match?(/function|const.*=.*=>/)
      score += 1 if response.include?('async')
      score += 1 if response.include?('await')
    when 'rust'
      score += 2 if response.match?(/fn\s+\w+/)
      score += 1 if response.include?('Result<')
      score += 1 if response.include?('impl')
    end
    
    [score, 10].min
  end
  
  def truncate_code(code, max_length)
    return code if code.length <= max_length
    code[0...max_length] + "\n... (truncated)"
  end
  
  def save_efficiency_results(results)
    timestamp = Time.now.strftime('%Y%m%d_%H%M%S')
    
    # Save JSON results
    File.write(
      File.join(OUTPUT_DIR, "efficiency_test_#{timestamp}.json"),
      JSON.pretty_generate({
        test_date: Time.now,
        configuration: {
          models: results[:batch].keys,
          languages: FOCUSED_LANGUAGES,
          tasks: FOCUSED_TASKS
        },
        results: results
      })
    )
    
    # Save markdown report
    File.open(File.join(OUTPUT_DIR, "efficiency_report_#{timestamp}.md"), 'w') do |f|
      f.puts "# Batch Processing Efficiency Report"
      f.puts "Generated: #{Time.now}"
      
      f.puts "\n## Key Findings"
      
      total_saved = results[:comparison].values.sum { |c| c[:time_saved] }
      avg_speedup = results[:comparison].values.map { |c| c[:speedup] }.sum / results[:comparison].size
      
      f.puts "- Total time saved using batch processing: #{total_saved.round(2)}s"
      f.puts "- Average speedup factor: #{avg_speedup.round(2)}x"
      f.puts "- Batch processing with `keep_alive` prevents model reloading overhead"
      
      f.puts "\n## Model Performance"
      results[:comparison].each do |model, comp|
        f.puts "\n### #{model}"
        f.puts "- Batch time: #{comp[:batch_time].round(2)}s"
        f.puts "- Sequential time: #{comp[:sequential_time].round(2)}s"
        f.puts "- Efficiency gain: #{comp[:efficiency_gain]}%"
      end
      
      f.puts "\n## Recommendations"
      f.puts "1. Use batch processing with `keep_alive` for multiple prompts"
      f.puts "2. Group related prompts to maintain context"
      f.puts "3. Larger models benefit more from batch processing due to higher load times"
    end
    
    puts "\n\nResults saved to: #{OUTPUT_DIR}"
  end
end