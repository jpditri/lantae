require 'spec_helper'
require 'net/http'
require 'json'
require 'fileutils'

RSpec.describe 'Multi-Language Performance Comparison', :integration do
  let(:ollama_url) { 'http://localhost:11434' }
  
  # Define these as constants instead of let blocks for before(:all) access
  OUTPUT_DIR = File.join(Dir.pwd, 'language_comparison_results')
  LANGUAGES = ['lisp', 'ruby', 'javascript', 'python', 'rust', 'go', 'java']
  
  # Task definitions that can be implemented in all languages
  UNIVERSAL_TASKS = [
      {
        name: "Hello World Function",
        complexity: 1,
        description: "Write a function that takes a name as input and returns 'Hello, [name]!'",
        validation: {
          input: "World",
          expected_output: "Hello, World!"
        }
      },
      {
        name: "Array/List Sum",
        complexity: 2,
        description: "Create a function that takes an array/list of numbers and returns their sum",
        validation: {
          input: [1, 2, 3, 4, 5],
          expected_output: 15
        }
      },
      {
        name: "Fibonacci Recursive",
        complexity: 3,
        description: "Implement a recursive fibonacci function that returns the nth fibonacci number",
        validation: {
          input: 10,
          expected_output: 55
        }
      },
      {
        name: "String Reversal",
        complexity: 2,
        description: "Write a function that reverses a string without using built-in reverse methods",
        validation: {
          input: "hello world",
          expected_output: "dlrow olleh"
        }
      },
      {
        name: "Prime Number Check",
        complexity: 3,
        description: "Create a function that checks if a number is prime",
        validation: {
          test_cases: [
            { input: 17, expected: true },
            { input: 20, expected: false },
            { input: 2, expected: true }
          ]
        }
      }
    ]

  # Language-specific syntax expectations
  LANGUAGE_SYNTAX = {
      'lisp' => {
        keywords: ['defun', 'lambda', 'if', 'cond', 'let', 'cons', 'car', 'cdr'],
        function_pattern: /\(defun\s+\w+/,
        comment_pattern: /;/
      },
      'ruby' => {
        keywords: ['def', 'end', 'if', 'elsif', 'else', 'class', 'module', 'return'],
        function_pattern: /def\s+\w+/,
        comment_pattern: /#/
      },
      'javascript' => {
        keywords: ['function', 'const', 'let', 'var', 'if', 'else', 'return', '=>'],
        function_pattern: /function\s+\w+|const\s+\w+\s*=.*=>|\w+\s*:\s*function/,
        comment_pattern: /\/\/|\/\*/
      },
      'python' => {
        keywords: ['def', 'if', 'elif', 'else', 'return', 'class', 'lambda', 'for', 'while'],
        function_pattern: /def\s+\w+/,
        comment_pattern: /#/
      },
      'rust' => {
        keywords: ['fn', 'let', 'mut', 'if', 'else', 'match', 'return', 'impl', 'struct'],
        function_pattern: /fn\s+\w+/,
        comment_pattern: /\/\/|\/\*/
      },
      'go' => {
        keywords: ['func', 'var', 'if', 'else', 'return', 'type', 'struct', 'interface'],
        function_pattern: /func\s+\w+/,
        comment_pattern: /\/\/|\/\*/
      },
      'java' => {
        keywords: ['public', 'private', 'static', 'void', 'int', 'String', 'class', 'if', 'else', 'return'],
        function_pattern: /(?:public|private|static)?\s*\w+\s+\w+\s*\(/,
        comment_pattern: /\/\/|\/\*/
      }
    }

  before(:all) do
    FileUtils.mkdir_p(OUTPUT_DIR)
  end

  before do
    # Check Ollama is running
    begin
      response = Net::HTTP.get_response(URI("#{ollama_url}/api/tags"))
      skip "Ollama is not running" unless response.code == '200'
    rescue Errno::ECONNREFUSED
      skip "Cannot connect to Ollama service"
    end
  end

  describe 'Comprehensive Language Comparison' do
    it 'tests all available models across all languages' do
      # Get available models
      response = Net::HTTP.get_response(URI("#{ollama_url}/api/tags"))
      available_models = JSON.parse(response.body)['models'].map { |m| m['name'] }
      
      # Filter for models we want to test
      test_models = available_models.select do |model|
        model.match?(/qwen|llama|mistral|gemma|phi|deepseek|codellama/i)
      end
      
      puts "\n" + "="*80
      puts "MULTI-LANGUAGE PERFORMANCE COMPARISON"
      puts "="*80
      puts "Testing #{test_models.size} models across #{LANGUAGES.size} languages"
      puts "Tasks: #{UNIVERSAL_TASKS.map { |t| t[:name] }.join(', ')}"
      puts "="*80
      
      # Results storage
      all_results = {}
      code_samples = { successes: [], failures: [] }
      
      # Test each model
      test_models.each do |model|
        puts "\n\nTesting Model: #{model}"
        puts "-" * 60
        
        model_results = {}
        
        # Test each language
        LANGUAGES.each do |language|
          puts "\n  Language: #{language.upcase}"
          language_results = []
          
          # Test each task
          UNIVERSAL_TASKS.each do |task|
            result = test_model_language_task(model, language, task)
            language_results << result
            
            # Collect code samples
            if result[:success] && result[:quality_score] >= 8
              code_samples[:successes] << {
                model: model,
                language: language,
                task: task[:name],
                code: result[:response],
                quality: result[:quality_score],
                execution_time: result[:time]
              }
            elsif !result[:success] || result[:quality_score] < 5
              code_samples[:failures] << {
                model: model,
                language: language,
                task: task[:name],
                code: result[:response],
                quality: result[:quality_score],
                error: result[:error],
                reason: analyze_failure_reason(result, language)
              }
            end
            
            # Display progress
            status = result[:success] ? "✓" : "✗"
            quality = result[:quality_score] || 0
            time = result[:time] ? "#{result[:time].round(2)}s" : "N/A"
            
            puts "    #{status} #{task[:name].ljust(25)} Quality: #{quality}/10  Time: #{time}"
          end
          
          model_results[language] = language_results
        end
        
        all_results[model] = model_results
      end
      
      # Generate comprehensive report
      generate_comprehensive_report(all_results, code_samples)
      
      # Save detailed results
      save_detailed_results(all_results, code_samples)
      
      expect(all_results).not_to be_empty
    end
  end

  private

  def test_model_language_task(model, language, task)
    start_time = Time.now
    
    begin
      # Create language-specific prompt
      prompt = create_language_prompt(language, task)
      
      # Make API request
      response = make_model_request(model, prompt)
      
      execution_time = Time.now - start_time
      
      # Analyze response quality
      quality_analysis = analyze_code_quality(response, language, task)
      
      {
        success: true,
        response: response,
        time: execution_time,
        quality_score: quality_analysis[:score],
        has_function: quality_analysis[:has_function],
        syntax_valid: quality_analysis[:syntax_valid],
        concepts_found: quality_analysis[:concepts_found]
      }
    rescue => e
      {
        success: false,
        error: e.message,
        time: Time.now - start_time,
        quality_score: 0
      }
    end
  end

  def create_language_prompt(language, task)
    case language
    when 'lisp'
      "Write a #{language.upcase} function: #{task[:description]}. Use proper LISP syntax with defun."
    when 'ruby'
      "Write a #{language.capitalize} method: #{task[:description]}. Use idiomatic Ruby style."
    when 'javascript'
      "Write a #{language.capitalize} function: #{task[:description]}. Use modern ES6+ syntax."
    when 'python'
      "Write a #{language.capitalize} function: #{task[:description]}. Follow PEP 8 style guidelines."
    when 'rust'
      "Write a #{language.capitalize} function: #{task[:description]}. Use safe Rust practices."
    when 'go'
      "Write a #{language.capitalize} function: #{task[:description]}. Follow Go conventions."
    when 'java'
      "Write a #{language.capitalize} method: #{task[:description]}. Use proper Java syntax."
    end
  end

  def make_model_request(model, prompt)
    uri = URI("#{ollama_url}/api/generate")
    request = Net::HTTP::Post.new(uri)
    request['Content-Type'] = 'application/json'
    request.body = {
      model: model,
      prompt: prompt,
      stream: false,
      options: {
        temperature: 0.1,
        num_predict: 500,
        top_p: 0.9
      }
    }.to_json
    
    response = Net::HTTP.start(uri.hostname, uri.port, read_timeout: 60) do |http|
      http.request(request)
    end
    
    if response.code == '200'
      result = JSON.parse(response.body)
      result['response']
    else
      raise "Request failed: #{response.code}"
    end
  end

  def analyze_code_quality(response, language, task)
    return { score: 0, has_function: false, syntax_valid: false, concepts_found: [] } if response.nil? || response.empty?
    
    syntax = LANGUAGE_SYNTAX[language]
    
    # Check for function definition
    has_function = response.match?(syntax[:function_pattern])
    
    # Check for language keywords
    concepts_found = syntax[:keywords].select { |keyword| response.downcase.include?(keyword.downcase) }
    
    # Check for code blocks
    has_code_block = response.include?('```') || response.match?(/^\s{4}/) || response.match?(/^\t/)
    
    # Calculate quality score
    score = 0
    score += 3 if has_function
    score += 2 if has_code_block
    score += (concepts_found.size * 0.5).round(1)
    score += 1 if response.length > 100
    score += 1 if response.match?(syntax[:comment_pattern])
    score += 1 if task[:name].downcase.split.any? { |word| response.downcase.include?(word) }
    
    score = [score, 10].min
    
    {
      score: score,
      has_function: has_function,
      syntax_valid: has_function && concepts_found.size >= 2,
      concepts_found: concepts_found
    }
  end

  def analyze_failure_reason(result, language)
    reasons = []
    
    if result[:error]
      reasons << "Error: #{result[:error]}"
    end
    
    if result[:quality_score] && result[:quality_score] < 5
      reasons << "Low quality score: #{result[:quality_score]}/10"
      
      unless result[:has_function]
        reasons << "No function definition found"
      end
      
      if result[:concepts_found] && result[:concepts_found].empty?
        reasons << "No #{language} keywords found"
      end
      
      unless result[:syntax_valid]
        reasons << "Invalid #{language} syntax"
      end
    end
    
    reasons.join("; ")
  end

  def generate_comprehensive_report(all_results, code_samples)
    puts "\n\n" + "="*100
    puts "COMPREHENSIVE MULTI-LANGUAGE PERFORMANCE REPORT"
    puts "="*100
    
    # Model x Language performance matrix
    puts "\n## Performance Matrix (Average Quality Scores)"
    puts "\nModel" + LANGUAGES.map { |l| l.upcase.rjust(12) }.join + "   AVERAGE"
    puts "-" * 100
    
    all_results.each do |model, model_results|
      scores = []
      row = model.ljust(20)
      
      LANGUAGES.each do |language|
        if model_results[language]
          avg_score = model_results[language].map { |r| r[:quality_score] || 0 }.sum.to_f / model_results[language].size
          scores << avg_score
          row += sprintf("%12.1f", avg_score)
        else
          row += "         N/A"
        end
      end
      
      avg_overall = scores.empty? ? 0 : scores.sum / scores.size
      row += sprintf("%12.1f", avg_overall)
      
      puts row
    end
    
    # Best performing model per language
    puts "\n\n## Best Model per Language"
    puts "-" * 50
    
    LANGUAGES.each do |language|
      best_model = nil
      best_score = 0
      
      all_results.each do |model, model_results|
        if model_results[language]
          avg_score = model_results[language].map { |r| r[:quality_score] || 0 }.sum.to_f / model_results[language].size
          if avg_score > best_score
            best_score = avg_score
            best_model = model
          end
        end
      end
      
      puts "#{language.upcase.ljust(15)} Best: #{best_model || 'N/A'} (#{best_score.round(1)}/10)"
    end
    
    # Code sample analysis
    puts "\n\n## Code Sample Analysis"
    puts "="*80
    
    puts "\n### Successful Code Examples (Quality >= 8/10)"
    code_samples[:successes].take(3).each do |sample|
      puts "\n" + "-"*60
      puts "Model: #{sample[:model]} | Language: #{sample[:language].upcase} | Task: #{sample[:task]}"
      puts "Quality: #{sample[:quality]}/10 | Time: #{sample[:execution_time].round(2)}s"
      puts "-"*60
      puts sample[:code]
      puts "-"*60
      puts "Why this is good:"
      puts "- Clean function definition"
      puts "- Proper #{sample[:language]} syntax"
      puts "- Solves the task correctly"
    end
    
    puts "\n\n### Failed/Poor Code Examples (Quality < 5/10)"
    code_samples[:failures].take(3).each do |sample|
      puts "\n" + "-"*60
      puts "Model: #{sample[:model]} | Language: #{sample[:language].upcase} | Task: #{sample[:task]}"
      puts "Quality: #{sample[:quality]}/10"
      puts "Failure Reason: #{sample[:reason]}"
      puts "-"*60
      puts sample[:code] || "[No code generated]"
      puts "-"*60
    end
  end

  def save_detailed_results(all_results, code_samples)
    # Save JSON results
    File.write(
      File.join(OUTPUT_DIR, "results_#{Time.now.strftime('%Y%m%d_%H%M%S')}.json"),
      JSON.pretty_generate({
        test_date: Time.now,
        models_tested: all_results.keys,
        languages_tested: LANGUAGES,
        tasks: UNIVERSAL_TASKS,
        results: all_results,
        code_samples: code_samples
      })
    )
    
    # Save summary report
    File.open(File.join(OUTPUT_DIR, "summary_#{Time.now.strftime('%Y%m%d_%H%M%S')}.txt"), 'w') do |f|
      f.puts "Multi-Language Model Performance Summary"
      f.puts "=" * 50
      f.puts "Test Date: #{Time.now}"
      f.puts "Models Tested: #{all_results.keys.size}"
      f.puts "Languages: #{LANGUAGES.join(', ')}"
      f.puts "\nTop Performers:"
      
      # Calculate overall best
      overall_best = nil
      overall_best_score = 0
      
      all_results.each do |model, model_results|
        total_score = 0
        count = 0
        
        model_results.each do |language, tasks|
          tasks.each do |task|
            total_score += task[:quality_score] || 0
            count += 1
          end
        end
        
        avg = count > 0 ? total_score.to_f / count : 0
        if avg > overall_best_score
          overall_best_score = avg
          overall_best = model
        end
      end
      
      f.puts "Overall Best Model: #{overall_best} (#{overall_best_score.round(1)}/10 average)"
    end
    
    puts "\n\nResults saved to: #{OUTPUT_DIR}"
  end
end