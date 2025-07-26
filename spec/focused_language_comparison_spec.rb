require 'spec_helper'
require 'net/http'
require 'json'
require 'fileutils'

RSpec.describe 'Focused Multi-Language Comparison with Code Samples', :integration do
  let(:ollama_url) { 'http://localhost:11434' }
  
  # Define constants for test configuration
  OUTPUT_DIR = File.join(Dir.pwd, 'language_comparison_results')
  LANGUAGES = ['lisp', 'ruby', 'javascript', 'python', 'rust', 'go', 'java']
  
  # Focused set of models for detailed analysis
  FOCUSED_MODELS = ['qwen2.5:1.5b', 'qwen2.5:3b']
  
  # Single task for detailed analysis across all languages
  TEST_TASK = {
    name: "Fibonacci Implementation",
    description: "Write a function that calculates the nth Fibonacci number. Include both recursive and iterative versions if possible.",
    validation: {
      test_cases: [
        { n: 0, expected: 0 },
        { n: 1, expected: 1 },
        { n: 10, expected: 55 },
        { n: 15, expected: 610 }
      ]
    }
  }
  
  # Language-specific prompts and validation
  LANGUAGE_CONFIG = {
    'lisp' => {
      prompt: "Write LISP functions to calculate the nth Fibonacci number. Include both recursive and iterative versions using defun.",
      keywords: ['defun', 'if', 'cond', 'let', 'loop', 'do'],
      function_pattern: /\(defun\s+\w+/,
      good_example: "(defun fib-recursive (n)\n  (if (<= n 1)\n      n\n      (+ (fib-recursive (- n 1))\n         (fib-recursive (- n 2)))))"
    },
    'ruby' => {
      prompt: "Write Ruby methods to calculate the nth Fibonacci number. Include both recursive and iterative versions.",
      keywords: ['def', 'if', 'elsif', 'else', 'end', 'while', 'each', 'times'],
      function_pattern: /def\s+\w+/,
      good_example: "def fibonacci_recursive(n)\n  return n if n <= 1\n  fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2)\nend"
    },
    'javascript' => {
      prompt: "Write JavaScript functions to calculate the nth Fibonacci number. Use modern ES6+ syntax with arrow functions.",
      keywords: ['function', 'const', 'let', 'if', 'else', 'return', '=>', 'for', 'while'],
      function_pattern: /function\s+\w+|const\s+\w+\s*=.*=>/,
      good_example: "const fibonacci = (n) => {\n  if (n <= 1) return n;\n  return fibonacci(n - 1) + fibonacci(n - 2);\n};"
    },
    'python' => {
      prompt: "Write Python functions to calculate the nth Fibonacci number. Include recursive and iterative versions following PEP 8.",
      keywords: ['def', 'if', 'elif', 'else', 'return', 'while', 'for', 'range'],
      function_pattern: /def\s+\w+/,
      good_example: "def fibonacci_recursive(n):\n    if n <= 1:\n        return n\n    return fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2)"
    },
    'rust' => {
      prompt: "Write Rust functions to calculate the nth Fibonacci number. Use proper type annotations and handle edge cases.",
      keywords: ['fn', 'let', 'mut', 'if', 'else', 'match', 'return', 'u32', 'i32'],
      function_pattern: /fn\s+\w+/,
      good_example: "fn fibonacci(n: u32) -> u32 {\n    match n {\n        0 => 0,\n        1 => 1,\n        _ => fibonacci(n - 1) + fibonacci(n - 2)\n    }\n}"
    },
    'go' => {
      prompt: "Write Go functions to calculate the nth Fibonacci number. Include proper error handling.",
      keywords: ['func', 'if', 'else', 'return', 'var', 'for', 'int', 'uint'],
      function_pattern: /func\s+\w+/,
      good_example: "func fibonacci(n int) int {\n    if n <= 1 {\n        return n\n    }\n    return fibonacci(n-1) + fibonacci(n-2)\n}"
    },
    'java' => {
      prompt: "Write Java methods to calculate the nth Fibonacci number. Use proper method signatures with public/private modifiers.",
      keywords: ['public', 'private', 'static', 'int', 'long', 'if', 'else', 'return', 'for', 'while'],
      function_pattern: /(?:public|private)?\s*(?:static)?\s*\w+\s+\w+\s*\(/,
      good_example: "public static int fibonacci(int n) {\n    if (n <= 1) {\n        return n;\n    }\n    return fibonacci(n - 1) + fibonacci(n - 2);\n}"
    }
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
  
  describe 'Detailed Language Comparison with Code Analysis' do
    it 'analyzes model performance across languages with code samples' do
      # Check which models are available
      response = Net::HTTP.get_response(URI("#{ollama_url}/api/tags"))
      available_models = JSON.parse(response.body)['models'].map { |m| m['name'] }
      test_models = FOCUSED_MODELS.select { |m| available_models.include?(m) }
      
      if test_models.empty?
        skip "No focused models available. Available: #{available_models.join(', ')}"
      end
      
      results = {}
      detailed_samples = []
      
      puts "\n" + "="*100
      puts "FOCUSED LANGUAGE COMPARISON: #{TEST_TASK[:name]}"
      puts "="*100
      puts "Models: #{test_models.join(', ')}"
      puts "Languages: #{LANGUAGES.join(', ')}"
      puts "="*100
      
      test_models.each do |model|
        puts "\n\nMODEL: #{model}"
        puts "="*80
        
        model_results = {}
        
        LANGUAGES.each do |language|
          config = LANGUAGE_CONFIG[language]
          
          puts "\n#{language.upcase}:"
          puts "-" * 40
          
          # Make request
          start_time = Time.now
          begin
            response = make_model_request(model, config[:prompt])
            execution_time = Time.now - start_time
            
            # Analyze response
            analysis = analyze_response(response, language, config)
            
            # Store results
            result = {
              model: model,
              language: language,
              response: response,
              execution_time: execution_time,
              analysis: analysis,
              success: analysis[:has_function] && analysis[:quality_score] >= 6
            }
            
            model_results[language] = result
            detailed_samples << result
            
            # Display results
            puts "Time: #{execution_time.round(2)}s"
            puts "Quality Score: #{analysis[:quality_score]}/10"
            puts "Has Function: #{analysis[:has_function] ? 'Yes' : 'No'}"
            puts "Keywords Found: #{analysis[:keywords_found].join(', ')}"
            
            if analysis[:has_function]
              puts "\nCODE GENERATED:"
              puts "-" * 40
              puts truncate_code(response, 500)
              puts "-" * 40
              
              puts "\nANALYSIS:"
              if analysis[:quality_score] >= 8
                puts "✓ GOOD: #{analysis[:good_reasons].join('; ')}"
              elsif analysis[:quality_score] < 5
                puts "✗ POOR: #{analysis[:poor_reasons].join('; ')}"
              else
                puts "~ ADEQUATE: Basic implementation present"
              end
            else
              puts "\nNO VALID CODE GENERATED"
              puts "Response preview: #{response[0..200]}..."
            end
            
          rescue => e
            puts "ERROR: #{e.message}"
            model_results[language] = {
              model: model,
              language: language,
              error: e.message,
              success: false,
              analysis: { quality_score: 0 }
            }
          end
        end
        
        results[model] = model_results
      end
      
      # Generate comparative analysis
      generate_comparative_analysis(results, detailed_samples)
      
      # Save detailed results
      save_analysis_results(results, detailed_samples)
      
      expect(results).not_to be_empty
      expect(detailed_samples.select { |s| s[:success] }.size).to be > 0
    end
  end
  
  private
  
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
        num_predict: 800,
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
      raise "API request failed: #{response.code}"
    end
  end
  
  def analyze_response(response, language, config)
    analysis = {
      has_function: false,
      keywords_found: [],
      quality_score: 0,
      good_reasons: [],
      poor_reasons: []
    }
    
    return analysis if response.nil? || response.empty?
    
    # Check for function definition
    analysis[:has_function] = response.match?(config[:function_pattern])
    
    # Check keywords
    analysis[:keywords_found] = config[:keywords].select { |kw| response.include?(kw) }
    
    # Calculate quality score
    score = 0
    
    if analysis[:has_function]
      score += 3
      analysis[:good_reasons] << "Valid function definition"
    else
      analysis[:poor_reasons] << "No function definition found"
    end
    
    if analysis[:keywords_found].size >= 3
      score += 2
      analysis[:good_reasons] << "Good keyword usage"
    elsif analysis[:keywords_found].empty?
      analysis[:poor_reasons] << "No #{language} keywords found"
    end
    
    if response.include?('recursive') || response.include?('iterative')
      score += 1
      analysis[:good_reasons] << "Mentions implementation approach"
    end
    
    if response.match?(/```|^\s{4}|^\t/)
      score += 1
      analysis[:good_reasons] << "Proper code formatting"
    else
      analysis[:poor_reasons] << "Poor code formatting"
    end
    
    if response.length > 200
      score += 1
      analysis[:good_reasons] << "Comprehensive response"
    elsif response.length < 50
      analysis[:poor_reasons] << "Response too short"
    end
    
    # Language-specific checks
    case language
    when 'lisp'
      if response.include?('(') && response.include?(')')
        score += 1
        analysis[:good_reasons] << "Proper parentheses usage"
      else
        analysis[:poor_reasons] << "Missing LISP parentheses"
      end
    when 'python'
      if response.match?(/:\s*\n\s+/)
        score += 1
        analysis[:good_reasons] << "Proper Python indentation"
      end
    when 'rust'
      if response.include?('->') || response.include?('mut')
        score += 1
        analysis[:good_reasons] << "Rust-specific syntax"
      end
    end
    
    analysis[:quality_score] = [score, 10].min
    analysis
  end
  
  def truncate_code(code, max_length)
    return code if code.length <= max_length
    code[0...max_length] + "\n... (truncated)"
  end
  
  def generate_comparative_analysis(results, samples)
    puts "\n\n" + "="*100
    puts "COMPARATIVE ANALYSIS"
    puts "="*100
    
    # Success rate by language
    puts "\n## Success Rate by Language"
    puts "-" * 50
    LANGUAGES.each do |language|
      lang_samples = samples.select { |s| s[:language] == language }
      success_count = lang_samples.count { |s| s[:success] }
      success_rate = lang_samples.empty? ? 0 : (success_count.to_f / lang_samples.size * 100)
      avg_quality = lang_samples.map { |s| s[:analysis][:quality_score] }.sum.to_f / lang_samples.size
      
      puts "#{language.upcase.ljust(15)} Success: #{success_rate.round(1)}%  Avg Quality: #{avg_quality.round(1)}/10"
    end
    
    # Model comparison
    puts "\n## Model Performance Comparison"
    puts "-" * 50
    results.each do |model, model_results|
      scores = model_results.values.map { |r| r[:analysis][:quality_score] }
      avg_score = scores.sum.to_f / scores.size
      success_count = model_results.values.count { |r| r[:success] }
      
      puts "#{model.ljust(20)} Avg Quality: #{avg_score.round(1)}/10  Success: #{success_count}/#{LANGUAGES.size}"
    end
    
    # Best and worst examples
    puts "\n## Example Analysis"
    puts "="*80
    
    # Best example
    best = samples.select { |s| s[:success] }.max_by { |s| s[:analysis][:quality_score] }
    if best
      puts "\n### BEST EXAMPLE"
      puts "Model: #{best[:model]} | Language: #{best[:language].upcase}"
      puts "Quality: #{best[:analysis][:quality_score]}/10 | Time: #{best[:execution_time].round(2)}s"
      puts "Reasons: #{best[:analysis][:good_reasons].join('; ')}"
      puts "\nCode:"
      puts "-" * 40
      puts truncate_code(best[:response], 400)
      puts "-" * 40
    end
    
    # Worst example
    worst = samples.reject { |s| s[:error] }.min_by { |s| s[:analysis][:quality_score] }
    if worst
      puts "\n### WORST EXAMPLE"
      puts "Model: #{worst[:model]} | Language: #{worst[:language].upcase}"
      puts "Quality: #{worst[:analysis][:quality_score]}/10"
      puts "Issues: #{worst[:analysis][:poor_reasons].join('; ')}"
      puts "\nCode:"
      puts "-" * 40
      puts truncate_code(worst[:response], 400)
      puts "-" * 40
    end
  end
  
  def save_analysis_results(results, samples)
    timestamp = Time.now.strftime('%Y%m%d_%H%M%S')
    
    # Save detailed JSON
    File.write(
      File.join(OUTPUT_DIR, "focused_analysis_#{timestamp}.json"),
      JSON.pretty_generate({
        task: TEST_TASK,
        models: results.keys,
        languages: LANGUAGES,
        results: results,
        samples: samples.map do |s|
          {
            model: s[:model],
            language: s[:language],
            success: s[:success],
            quality_score: s[:analysis][:quality_score],
            execution_time: s[:execution_time],
            code_length: s[:response]&.length || 0,
            has_function: s[:analysis][:has_function],
            keywords_found: s[:analysis][:keywords_found]
          }
        end
      })
    )
    
    # Save markdown report
    File.open(File.join(OUTPUT_DIR, "focused_report_#{timestamp}.md"), 'w') do |f|
      f.puts "# Language Comparison Report: #{TEST_TASK[:name]}"
      f.puts "Generated: #{Time.now}"
      f.puts "\n## Summary"
      
      f.puts "\n### Success Rates"
      LANGUAGES.each do |lang|
        lang_samples = samples.select { |s| s[:language] == lang }
        success_rate = lang_samples.count { |s| s[:success] }.to_f / lang_samples.size * 100
        f.puts "- **#{lang.capitalize}**: #{success_rate.round(1)}%"
      end
      
      f.puts "\n### Code Examples"
      
      # Include best examples for each language
      LANGUAGES.each do |lang|
        lang_samples = samples.select { |s| s[:language] == lang && s[:success] }
        best = lang_samples.max_by { |s| s[:analysis][:quality_score] }
        
        if best
          f.puts "\n#### #{lang.capitalize} - Best Example"
          f.puts "Model: #{best[:model]} | Quality: #{best[:analysis][:quality_score]}/10"
          f.puts "\n```#{lang}"
          f.puts best[:response]
          f.puts "```"
        end
      end
    end
    
    puts "\n\nResults saved to: #{OUTPUT_DIR}"
  end
end