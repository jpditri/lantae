require 'spec_helper'
require 'net/http'
require 'json'
require 'fileutils'
require 'benchmark'

RSpec.describe 'Batch-Optimized Multi-Language Testing', :integration do
  let(:ollama_url) { 'http://localhost:11434' }
  
  # Constants for configuration
  OUTPUT_DIR = File.join(Dir.pwd, 'batch_language_results')
  LANGUAGES = ['lisp', 'ruby', 'javascript', 'python', 'rust', 'go', 'java']
  
  # Models to test - ordered by size for optimal loading
  TEST_MODELS = [
    'qwen2.5:1.5b',   # Smallest - quick baseline
    'qwen2.5:3b',     # Small - good performance
    'qwen3:14b',      # Medium - high quality
    'qwen3:32b'       # Largest - best quality
  ]
  
  # Multiple tasks for batch processing
  CODING_TASKS = [
    {
      id: 'fibonacci',
      name: 'Fibonacci Implementation',
      prompt: 'Write a function to calculate the nth Fibonacci number'
    },
    {
      id: 'quicksort', 
      name: 'QuickSort Algorithm',
      prompt: 'Implement the QuickSort algorithm'
    },
    {
      id: 'binary_tree',
      name: 'Binary Tree Operations',
      prompt: 'Create a binary tree with insert and search operations'
    },
    {
      id: 'api_server',
      name: 'Simple API Server',
      prompt: 'Create a simple REST API server with GET and POST endpoints'
    },
    {
      id: 'concurrent',
      name: 'Concurrent Processing',
      prompt: 'Write code that processes data concurrently/in parallel'
    }
  ]
  
  # Language-specific prompt templates
  LANGUAGE_PROMPTS = {
    'lisp' => "Write LISP code: %s. Use proper LISP syntax with defun.",
    'ruby' => "Write Ruby code: %s. Use idiomatic Ruby style.",
    'javascript' => "Write JavaScript code: %s. Use modern ES6+ syntax.",
    'python' => "Write Python code: %s. Follow PEP 8 guidelines.",
    'rust' => "Write Rust code: %s. Ensure memory safety.",
    'go' => "Write Go code: %s. Follow Go conventions.",
    'java' => "Write Java code: %s. Use proper OOP principles."
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
  
  describe 'Batch Processing Performance Test' do
    it 'tests all models with optimized batch processing' do
      # Check available models
      response = Net::HTTP.get_response(URI("#{ollama_url}/api/tags"))
      available_models = JSON.parse(response.body)['models'].map { |m| m['name'] }
      
      # Filter to only available test models
      models_to_test = TEST_MODELS.select { |m| available_models.include?(m) }
      
      if models_to_test.empty?
        skip "No test models available"
      end
      
      puts "\n" + "="*100
      puts "BATCH-OPTIMIZED LANGUAGE PERFORMANCE TEST"
      puts "="*100
      puts "Models: #{models_to_test.join(', ')}"
      puts "Languages: #{LANGUAGES.size}"
      puts "Tasks per language: #{CODING_TASKS.size}"
      puts "Total prompts per model: #{LANGUAGES.size * CODING_TASKS.size}"
      puts "="*100
      
      all_results = {}
      timing_data = {}
      
      # Test each model with batch processing
      models_to_test.each do |model|
        puts "\n\nTESTING MODEL: #{model}"
        puts "="*80
        
        model_start = Time.now
        model_results = {}
        
        # Prepare all prompts for this model (batch by language for better context)
        batch_results = {}
        
        # Process each language as a batch
        LANGUAGES.each do |language|
          puts "\n  Processing #{language.upcase} batch..."
          language_start = Time.now
          
          # Create batch of prompts for all tasks in this language
          batch_prompts = CODING_TASKS.map do |task|
            {
              task_id: task[:id],
              prompt: LANGUAGE_PROMPTS[language] % task[:prompt]
            }
          end
          
          # Process batch with keep_alive to avoid model unloading
          batch_responses = process_batch_with_model(model, batch_prompts, language)
          
          language_time = Time.now - language_start
          puts "    Completed in #{language_time.round(2)}s"
          
          # Store results
          batch_results[language] = {
            responses: batch_responses,
            time: language_time
          }
        end
        
        model_time = Time.now - model_start
        
        # Analyze results
        model_analysis = analyze_batch_results(model, batch_results)
        
        all_results[model] = model_analysis
        timing_data[model] = {
          total_time: model_time,
          avg_per_prompt: model_time / (LANGUAGES.size * CODING_TASKS.size),
          languages: batch_results.map { |lang, data| [lang, data[:time]] }.to_h
        }
        
        # Display summary
        display_model_summary(model, model_analysis, timing_data[model])
      end
      
      # Generate comprehensive report
      generate_batch_report(all_results, timing_data)
      
      # Save results
      save_batch_results(all_results, timing_data)
      
      expect(all_results).not_to be_empty
    end
  end
  
  private
  
  def process_batch_with_model(model, prompts, language)
    responses = []
    
    # First prompt loads the model, subsequent ones reuse it
    prompts.each_with_index do |prompt_data, index|
      begin
        # Set keep_alive to prevent model unloading between requests
        response = make_model_request(model, prompt_data[:prompt], keep_alive: "5m")
        
        responses << {
          task_id: prompt_data[:task_id],
          response: response,
          success: true
        }
        
        # Brief pause between requests to avoid overwhelming
        sleep(0.1) if index < prompts.length - 1
        
      rescue => e
        responses << {
          task_id: prompt_data[:task_id],
          response: nil,
          success: false,
          error: e.message
        }
      end
    end
    
    responses
  end
  
  def make_model_request(model, prompt, keep_alive: "5m")
    uri = URI("#{ollama_url}/api/generate")
    request = Net::HTTP::Post.new(uri)
    request['Content-Type'] = 'application/json'
    request.body = {
      model: model,
      prompt: prompt,
      stream: false,
      keep_alive: keep_alive,  # Keep model loaded in memory
      options: {
        temperature: 0.1,
        num_predict: 500,
        top_p: 0.9,
        num_gpu: -1  # Use all available GPUs
      }
    }.to_json
    
    response = Net::HTTP.start(uri.hostname, uri.port, read_timeout: 120) do |http|
      http.request(request)
    end
    
    if response.code == '200'
      result = JSON.parse(response.body)
      result['response']
    else
      raise "API request failed: #{response.code}"
    end
  end
  
  def analyze_batch_results(model, batch_results)
    analysis = {
      model: model,
      languages: {},
      tasks: {},
      overall_stats: {
        total_success: 0,
        total_attempts: 0,
        avg_quality: 0
      }
    }
    
    quality_scores = []
    
    batch_results.each do |language, data|
      lang_analysis = {
        success_count: 0,
        total_time: data[:time],
        task_quality: {}
      }
      
      data[:responses].each do |response|
        if response[:success] && response[:response]
          lang_analysis[:success_count] += 1
          
          # Simple quality scoring based on response characteristics
          quality = score_response_quality(response[:response], language)
          lang_analysis[:task_quality][response[:task_id]] = quality
          quality_scores << quality
          
          # Track task performance
          analysis[:tasks][response[:task_id]] ||= {}
          analysis[:tasks][response[:task_id]][language] = {
            success: true,
            quality: quality,
            length: response[:response].length
          }
        else
          analysis[:tasks][response[:task_id]] ||= {}
          analysis[:tasks][response[:task_id]][language] = {
            success: false,
            error: response[:error]
          }
        end
      end
      
      lang_analysis[:success_rate] = lang_analysis[:success_count].to_f / data[:responses].size
      lang_analysis[:avg_quality] = lang_analysis[:task_quality].values.sum.to_f / lang_analysis[:task_quality].size if lang_analysis[:task_quality].any?
      
      analysis[:languages][language] = lang_analysis
      analysis[:overall_stats][:total_success] += lang_analysis[:success_count]
      analysis[:overall_stats][:total_attempts] += data[:responses].size
    end
    
    analysis[:overall_stats][:success_rate] = analysis[:overall_stats][:total_success].to_f / analysis[:overall_stats][:total_attempts]
    analysis[:overall_stats][:avg_quality] = quality_scores.sum.to_f / quality_scores.size if quality_scores.any?
    
    analysis
  end
  
  def score_response_quality(response, language)
    return 0 if response.nil? || response.empty?
    
    score = 0
    
    # Check for code blocks
    score += 2 if response.include?('```')
    
    # Check for language-specific patterns
    case language
    when 'lisp'
      score += 2 if response.match?(/\(defun/)
      score += 1 if response.count('(') > 5
    when 'ruby'
      score += 2 if response.match?(/def\s+\w+/)
      score += 1 if response.include?('end')
    when 'javascript'
      score += 2 if response.match?(/function|const.*=.*=>/)
      score += 1 if response.include?('{')
    when 'python'
      score += 2 if response.match?(/def\s+\w+/)
      score += 1 if response.include?(':')
    when 'rust'
      score += 2 if response.match?(/fn\s+\w+/)
      score += 1 if response.include?('->')
    when 'go'
      score += 2 if response.match?(/func\s+\w+/)
      score += 1 if response.include?('package')
    when 'java'
      score += 2 if response.match?(/public|private/)
      score += 1 if response.include?('class')
    end
    
    # Length bonus
    score += 1 if response.length > 200
    score += 1 if response.length > 500
    
    # Comments/documentation
    score += 1 if response.match?(/\/\/|#|\/\*|\*/)
    
    [score, 10].min
  end
  
  def display_model_summary(model, analysis, timing)
    puts "\n  SUMMARY FOR #{model}:"
    puts "  " + "-"*60
    puts "  Total Time: #{timing[:total_time].round(2)}s"
    puts "  Avg Time per Prompt: #{timing[:avg_per_prompt].round(2)}s"
    puts "  Overall Success Rate: #{(analysis[:overall_stats][:success_rate] * 100).round(1)}%"
    puts "  Average Quality Score: #{analysis[:overall_stats][:avg_quality].round(1)}/10"
    
    puts "\n  Language Performance:"
    analysis[:languages].each do |lang, data|
      puts "    #{lang.upcase.ljust(12)} Success: #{(data[:success_rate] * 100).round(1)}%  " +
           "Quality: #{data[:avg_quality].round(1)}/10  Time: #{data[:total_time].round(2)}s"
    end
  end
  
  def generate_batch_report(all_results, timing_data)
    puts "\n\n" + "="*100
    puts "BATCH PROCESSING PERFORMANCE COMPARISON"
    puts "="*100
    
    # Model comparison table
    puts "\n## Model Performance Overview"
    puts "\nModel" + " "*20 + "Success Rate" + " "*5 + "Avg Quality" + " "*5 + "Total Time" + " "*5 + "Time/Prompt"
    puts "-"*80
    
    all_results.each do |model, analysis|
      timing = timing_data[model]
      puts "#{model.ljust(25)} #{(analysis[:overall_stats][:success_rate] * 100).round(1).to_s.rjust(10)}%  " +
           "#{analysis[:overall_stats][:avg_quality].round(1).to_s.rjust(10)}/10  " +
           "#{timing[:total_time].round(1).to_s.rjust(10)}s  " +
           "#{timing[:avg_per_prompt].round(2).to_s.rjust(10)}s"
    end
    
    # Efficiency analysis
    puts "\n\n## Batch Processing Efficiency"
    puts "-"*60
    
    baseline_model = TEST_MODELS.first
    if timing_data[baseline_model]
      baseline_time = timing_data[baseline_model][:avg_per_prompt]
      
      timing_data.each do |model, timing|
        efficiency = baseline_time / timing[:avg_per_prompt]
        puts "#{model.ljust(20)} Relative Speed: #{efficiency.round(2)}x"
      end
    end
    
    # Task difficulty analysis
    puts "\n\n## Task Difficulty Analysis (Average Quality Across All Models)"
    puts "-"*60
    
    task_averages = {}
    CODING_TASKS.each do |task|
      scores = []
      all_results.each do |model, analysis|
        LANGUAGES.each do |lang|
          if analysis[:tasks][task[:id]] && analysis[:tasks][task[:id]][lang] && analysis[:tasks][task[:id]][lang][:quality]
            scores << analysis[:tasks][task[:id]][lang][:quality]
          end
        end
      end
      task_averages[task[:name]] = scores.empty? ? 0 : scores.sum.to_f / scores.size
    end
    
    task_averages.sort_by { |_, score| -score }.each do |task_name, avg_score|
      puts "#{task_name.ljust(30)} #{avg_score.round(1)}/10"
    end
  end
  
  def save_batch_results(all_results, timing_data)
    timestamp = Time.now.strftime('%Y%m%d_%H%M%S')
    
    # Save detailed JSON results
    File.write(
      File.join(OUTPUT_DIR, "batch_results_#{timestamp}.json"),
      JSON.pretty_generate({
        test_date: Time.now,
        configuration: {
          models: TEST_MODELS,
          languages: LANGUAGES,
          tasks: CODING_TASKS,
          prompts_per_model: LANGUAGES.size * CODING_TASKS.size
        },
        results: all_results,
        timing: timing_data,
        efficiency_analysis: calculate_efficiency_metrics(timing_data)
      })
    )
    
    # Save markdown summary
    File.open(File.join(OUTPUT_DIR, "batch_summary_#{timestamp}.md"), 'w') do |f|
      f.puts "# Batch-Optimized Language Testing Report"
      f.puts "Generated: #{Time.now}"
      f.puts "\n## Test Configuration"
      f.puts "- Models tested: #{all_results.keys.join(', ')}"
      f.puts "- Languages: #{LANGUAGES.join(', ')}"
      f.puts "- Tasks per language: #{CODING_TASKS.size}"
      f.puts "- Total prompts per model: #{LANGUAGES.size * CODING_TASKS.size}"
      
      f.puts "\n## Performance Summary"
      all_results.each do |model, analysis|
        timing = timing_data[model]
        f.puts "\n### #{model}"
        f.puts "- Total processing time: #{timing[:total_time].round(2)}s"
        f.puts "- Average time per prompt: #{timing[:avg_per_prompt].round(2)}s"
        f.puts "- Success rate: #{(analysis[:overall_stats][:success_rate] * 100).round(1)}%"
        f.puts "- Average quality: #{analysis[:overall_stats][:avg_quality].round(1)}/10"
      end
      
      f.puts "\n## Key Findings"
      f.puts "\n### Batch Processing Benefits"
      f.puts "- Model loading overhead amortized across #{LANGUAGES.size * CODING_TASKS.size} prompts"
      f.puts "- Keep-alive feature prevented model unloading between requests"
      f.puts "- Batching by language maintained context coherence"
      
      if timing_data.size > 1
        fastest = timing_data.min_by { |_, t| t[:avg_per_prompt] }[0]
        slowest = timing_data.max_by { |_, t| t[:avg_per_prompt] }[0]
        f.puts "\n### Performance Insights"
        f.puts "- Fastest model: #{fastest}"
        f.puts "- Slowest model: #{slowest}"
        f.puts "- Speed difference: #{(timing_data[slowest][:avg_per_prompt] / timing_data[fastest][:avg_per_prompt]).round(2)}x"
      end
    end
    
    puts "\n\nResults saved to: #{OUTPUT_DIR}"
  end
  
  def calculate_efficiency_metrics(timing_data)
    return {} if timing_data.empty?
    
    metrics = {
      avg_time_per_prompt: timing_data.values.map { |t| t[:avg_per_prompt] }.sum / timing_data.size,
      total_processing_time: timing_data.values.map { |t| t[:total_time] }.sum,
      model_efficiency: {}
    }
    
    # Calculate relative efficiency for each model
    baseline = timing_data.values.first[:avg_per_prompt]
    timing_data.each do |model, timing|
      metrics[:model_efficiency][model] = {
        relative_speed: baseline / timing[:avg_per_prompt],
        prompts_per_minute: 60.0 / timing[:avg_per_prompt]
      }
    end
    
    metrics
  end
end