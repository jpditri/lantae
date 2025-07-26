require 'spec_helper'
require 'net/http'
require 'json'

RSpec.describe 'Programming Language Comparison: LISP vs Ruby', :integration do
  let(:ollama_url) { 'http://localhost:11434' }
  let(:test_model) { 'qwen2.5:1.5b' }

  # Parallel task definitions for fair comparison
  let(:task_pairs) do
    [
      {
        name: "Simple Function",
        ruby: {
          description: "Write a Ruby method that adds two numbers and returns the result",
          expected_concepts: ['def', 'end', 'return', 'method']
        },
        lisp: {
          description: "Write a LISP function that adds two numbers and returns the result", 
          expected_concepts: ['defun', 'lambda', 'function']
        }
      },
      {
        name: "List Operations",
        ruby: {
          description: "Create Ruby methods to build an array [1,2,3,4,5] and calculate its length",
          expected_concepts: ['array', 'length', 'size', 'def', 'end']
        },
        lisp: {
          description: "Write LISP functions to create a list (1 2 3 4 5) and calculate its length",
          expected_concepts: ['list', 'defun', 'cons', 'length']
        }
      },
      {
        name: "Recursive Functions", 
        ruby: {
          description: "Implement factorial and fibonacci functions in Ruby using recursion",
          expected_concepts: ['def', 'if', 'elsif', 'recursion', 'factorial']
        },
        lisp: {
          description: "Create LISP functions for factorial and fibonacci using recursion",
          expected_concepts: ['defun', 'if', 'cond', 'recursion', 'factorial']
        }
      },
      {
        name: "Higher-Order Functions",
        ruby: {
          description: "Write Ruby methods using blocks to map, select, and reduce an array",
          expected_concepts: ['map', 'select', 'reduce', 'block', 'each']
        },
        lisp: {
          description: "Implement LISP functions for map, filter, and reduce with lambda expressions",
          expected_concepts: ['mapcar', 'lambda', 'funcall', 'filter', 'reduce']
        }
      }
    ]
  end

  before do
    # Skip tests if Ollama is not running or model not available
    begin
      response = Net::HTTP.get_response(URI("#{ollama_url}/api/tags"))
      skip "Ollama is not running" unless response.code == '200'
      
      models = JSON.parse(response.body)['models']
      model_names = models.map { |m| m['name'] }
      skip "Model #{test_model} not available" unless model_names.include?(test_model)
    rescue Errno::ECONNREFUSED
      skip "Cannot connect to Ollama service"
    end
  end

  describe 'Ruby vs LISP Task Performance' do
    it "compares Simple Function task" do
      task_pair = task_pairs[0]
      results = {}
      
      puts "\n" + "="*60
      puts "TASK 1: #{task_pair[:name]}"
      puts "="*60
      
      ruby_start = Time.now
      ruby_response = make_model_request(test_model, task_pair[:ruby][:description])
      ruby_time = Time.now - ruby_start
      ruby_quality = analyze_code_quality(ruby_response, task_pair[:ruby][:expected_concepts], 'ruby')
      
      results[:ruby] = {
        time: ruby_time,
        quality: ruby_quality,
        response: ruby_response
      }
      
      lisp_start = Time.now
      lisp_response = make_model_request(test_model, task_pair[:lisp][:description])
      lisp_time = Time.now - lisp_start
      lisp_quality = analyze_code_quality(lisp_response, task_pair[:lisp][:expected_concepts], 'lisp')
      
      results[:lisp] = {
        time: lisp_time,
        quality: lisp_quality,
        response: lisp_response
      }
      
      display_task_comparison(task_pair[:name], results)
      
      expect(results[:ruby][:response]).to be_a(String)
      expect(results[:lisp][:response]).to be_a(String)
      expect(results[:ruby][:time]).to be < 60
      expect(results[:lisp][:time]).to be < 60
    end

    it "compares List Operations task" do
      task_pair = task_pairs[1]
      results = {}
      
      puts "\n" + "="*60
      puts "TASK 2: #{task_pair[:name]}"
      puts "="*60
      
      ruby_start = Time.now
      ruby_response = make_model_request(test_model, task_pair[:ruby][:description])
      ruby_time = Time.now - ruby_start
      ruby_quality = analyze_code_quality(ruby_response, task_pair[:ruby][:expected_concepts], 'ruby')
      
      results[:ruby] = {
        time: ruby_time,
        quality: ruby_quality,
        response: ruby_response
      }
      
      lisp_start = Time.now
      lisp_response = make_model_request(test_model, task_pair[:lisp][:description])
      lisp_time = Time.now - lisp_start
      lisp_quality = analyze_code_quality(lisp_response, task_pair[:lisp][:expected_concepts], 'lisp')
      
      results[:lisp] = {
        time: lisp_time,
        quality: lisp_quality,
        response: lisp_response
      }
      
      display_task_comparison(task_pair[:name], results)
      
      expect(results[:ruby][:response]).to be_a(String)
      expect(results[:lisp][:response]).to be_a(String)
      expect(results[:ruby][:time]).to be < 60
      expect(results[:lisp][:time]).to be < 60
    end

    it "compares Recursive Functions task" do
      task_pair = task_pairs[2]
      results = {}
      
      puts "\n" + "="*60
      puts "TASK 3: #{task_pair[:name]}"
      puts "="*60
      
      ruby_start = Time.now
      ruby_response = make_model_request(test_model, task_pair[:ruby][:description])
      ruby_time = Time.now - ruby_start
      ruby_quality = analyze_code_quality(ruby_response, task_pair[:ruby][:expected_concepts], 'ruby')
      
      results[:ruby] = {
        time: ruby_time,
        quality: ruby_quality,
        response: ruby_response
      }
      
      lisp_start = Time.now
      lisp_response = make_model_request(test_model, task_pair[:lisp][:description])
      lisp_time = Time.now - lisp_start
      lisp_quality = analyze_code_quality(lisp_response, task_pair[:lisp][:expected_concepts], 'lisp')
      
      results[:lisp] = {
        time: lisp_time,
        quality: lisp_quality,
        response: lisp_response
      }
      
      display_task_comparison(task_pair[:name], results)
      
      expect(results[:ruby][:response]).to be_a(String)
      expect(results[:lisp][:response]).to be_a(String)
      expect(results[:ruby][:time]).to be < 60
      expect(results[:lisp][:time]).to be < 60
    end

    it "compares Higher-Order Functions task" do
      task_pair = task_pairs[3]
      results = {}
      
      puts "\n" + "="*60
      puts "TASK 4: #{task_pair[:name]}"
      puts "="*60
      
      ruby_start = Time.now
      ruby_response = make_model_request(test_model, task_pair[:ruby][:description])
      ruby_time = Time.now - ruby_start
      ruby_quality = analyze_code_quality(ruby_response, task_pair[:ruby][:expected_concepts], 'ruby')
      
      results[:ruby] = {
        time: ruby_time,
        quality: ruby_quality,
        response: ruby_response
      }
      
      lisp_start = Time.now
      lisp_response = make_model_request(test_model, task_pair[:lisp][:description])
      lisp_time = Time.now - lisp_start
      lisp_quality = analyze_code_quality(lisp_response, task_pair[:lisp][:expected_concepts], 'lisp')
      
      results[:lisp] = {
        time: lisp_time,
        quality: lisp_quality,
        response: lisp_response
      }
      
      display_task_comparison(task_pair[:name], results)
      
      expect(results[:ruby][:response]).to be_a(String)
      expect(results[:lisp][:response]).to be_a(String)
      expect(results[:ruby][:time]).to be < 60
      expect(results[:lisp][:time]).to be < 60
    end
  end

  describe 'Language Preference Analysis' do
    it 'analyzes which language the model performs better with' do
      all_results = {
        ruby: { times: [], qualities: [], scores: [] },
        lisp: { times: [], qualities: [], scores: [] }
      }
      
      task_pairs.each do |task_pair|
        # Ruby task
        ruby_start = Time.now
        ruby_response = make_model_request(test_model, task_pair[:ruby][:description])
        ruby_time = Time.now - ruby_start
        ruby_quality = analyze_code_quality(ruby_response, task_pair[:ruby][:expected_concepts], 'ruby')
        
        all_results[:ruby][:times] << ruby_time
        all_results[:ruby][:qualities] << ruby_quality[:score]
        all_results[:ruby][:scores] << ruby_quality
        
        # LISP task
        lisp_start = Time.now
        lisp_response = make_model_request(test_model, task_pair[:lisp][:description])
        lisp_time = Time.now - lisp_start
        lisp_quality = analyze_code_quality(lisp_response, task_pair[:lisp][:expected_concepts], 'lisp')
        
        all_results[:lisp][:times] << lisp_time
        all_results[:lisp][:qualities] << lisp_quality[:score]
        all_results[:lisp][:scores] << lisp_quality
      end
      
      generate_language_comparison_report(all_results)
      
      # Verify we got results for both languages
      expect(all_results[:ruby][:qualities]).not_to be_empty
      expect(all_results[:lisp][:qualities]).not_to be_empty
    end
  end

  describe 'Code Quality Deep Dive' do
    it 'analyzes specific code quality metrics' do
      # Test with the recursive functions task for detailed analysis
      task = task_pairs[2] # Recursive functions
      
      ruby_response = make_model_request(test_model, task[:ruby][:description])
      lisp_response = make_model_request(test_model, task[:lisp][:description])
      
      ruby_analysis = deep_code_analysis(ruby_response, 'ruby')
      lisp_analysis = deep_code_analysis(lisp_response, 'lisp')
      
      puts "\n" + "="*80
      puts "DETAILED CODE QUALITY ANALYSIS: #{task[:name]}"
      puts "="*80
      
      display_deep_analysis('Ruby', ruby_analysis)
      display_deep_analysis('LISP', lisp_analysis)
      
      expect(ruby_analysis[:has_functions]).to be true
      expect(lisp_analysis[:has_functions]).to be true
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
        num_predict: 1000,
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

  def analyze_code_quality(response, expected_concepts, language)
    # Language-specific keywords
    if language == 'ruby'
      keywords = ['def', 'end', 'class', 'module', 'if', 'elsif', 'else', 'unless', 'while', 'for', 'each', 'map', 'select', 'reduce']
      code_patterns = [/def\s+\w+/, /class\s+\w+/, /\.each/, /\.map/, /\.select/]
    else # lisp
      keywords = ['defun', 'lambda', 'cons', 'car', 'cdr', 'list', 'if', 'cond', 'let', 'mapcar', 'funcall']
      code_patterns = [/\(defun/, /\(lambda/, /\(cons/, /\(list/, /\(if/]
    end
    
    # Check for language-specific concepts
    concepts_found = keywords.select { |keyword| response.downcase.include?(keyword) }
    expected_found = expected_concepts.select { |concept| response.downcase.include?(concept.downcase) }
    
    # Check for code patterns
    has_code = code_patterns.any? { |pattern| response.match?(pattern) }
    
    # Calculate quality score (0-10)
    score = 0
    score += 3 if has_code
    score += (concepts_found.size * 0.7).round(1)
    score += (expected_found.size * 1.5).round(1)
    score += 1 if response.length > 200
    score += 1 if response.include?('example') || response.include?('usage')
    score = [score, 10].min
    
    {
      score: score,
      concepts_found: concepts_found,
      expected_found: expected_found,
      has_code: has_code,
      response_length: response.length,
      language: language
    }
  end

  def deep_code_analysis(response, language)
    analysis = {
      language: language,
      total_lines: response.lines.count,
      code_lines: 0,
      comment_lines: 0,
      blank_lines: 0,
      has_functions: false,
      function_count: 0,
      has_examples: false,
      complexity_indicators: []
    }
    
    response.lines.each do |line|
      line = line.strip
      
      if line.empty?
        analysis[:blank_lines] += 1
      elsif (language == 'ruby' && line.start_with?('#')) || 
            (language == 'lisp' && line.start_with?(';'))
        analysis[:comment_lines] += 1
      else
        analysis[:code_lines] += 1
      end
      
      # Check for functions
      if language == 'ruby'
        if line.match?(/def\s+\w+/)
          analysis[:has_functions] = true
          analysis[:function_count] += 1
        end
        
        # Complexity indicators
        analysis[:complexity_indicators] << 'conditional' if line.match?(/if|elsif|unless|case/)
        analysis[:complexity_indicators] << 'loop' if line.match?(/while|for|each|map|select/)
        analysis[:complexity_indicators] << 'recursion' if line.include?(line.match(/def\s+(\w+)/)&.[](1) || '')
      else # lisp
        if line.match?(/\(defun\s+\w+/)
          analysis[:has_functions] = true
          analysis[:function_count] += 1
        end
        
        # Complexity indicators
        analysis[:complexity_indicators] << 'conditional' if line.match?(/\(if|\(cond/)
        analysis[:complexity_indicators] << 'recursion' if line.include?('defun') && line.scan(/\w+/).uniq.size < line.scan(/\w+/).size
        analysis[:complexity_indicators] << 'higher-order' if line.match?(/mapcar|lambda|funcall/)
      end
      
      # Check for examples
      analysis[:has_examples] = true if line.downcase.include?('example') || line.downcase.include?('usage')
    end
    
    analysis[:complexity_indicators].uniq!
    analysis
  end

  def display_task_comparison(task_name, results)
    puts "\nRESULTS FOR: #{task_name}"
    puts "-" * 50
    
    ruby_result = results[:ruby]
    lisp_result = results[:lisp]
    
    puts "RUBY:"
    puts "  ⏱️  Time: #{ruby_result[:time].round(2)}s"
    puts "  📊 Quality: #{ruby_result[:quality][:score]}/10"
    puts "  📝 Length: #{ruby_result[:quality][:response_length]} chars"
    puts "  🔧 Concepts: #{ruby_result[:quality][:concepts_found].join(', ')}"
    puts "  ✅ Has Code: #{ruby_result[:quality][:has_code] ? 'Yes' : 'No'}"
    
    puts "\nLISP:"
    puts "  ⏱️  Time: #{lisp_result[:time].round(2)}s"
    puts "  📊 Quality: #{lisp_result[:quality][:score]}/10"
    puts "  📝 Length: #{lisp_result[:quality][:response_length]} chars"
    puts "  🔧 Concepts: #{lisp_result[:quality][:concepts_found].join(', ')}"
    puts "  ✅ Has Code: #{lisp_result[:quality][:has_code] ? 'Yes' : 'No'}"
    
    # Determine winner
    ruby_score = ruby_result[:quality][:score]
    lisp_score = lisp_result[:quality][:score]
    
    if ruby_score > lisp_score
      puts "\n🏆 WINNER: Ruby (+#{(ruby_score - lisp_score).round(1)} points)"
    elsif lisp_score > ruby_score
      puts "\n🏆 WINNER: LISP (+#{(lisp_score - ruby_score).round(1)} points)"
    else
      puts "\n🤝 TIE: Both languages performed equally well"
    end
  end

  def display_deep_analysis(language, analysis)
    puts "\n#{language.upcase} ANALYSIS:"
    puts "  📏 Total Lines: #{analysis[:total_lines]}"
    puts "  💻 Code Lines: #{analysis[:code_lines]}"
    puts "  💬 Comment Lines: #{analysis[:comment_lines]}"
    puts "  📱 Blank Lines: #{analysis[:blank_lines]}"
    puts "  🔧 Functions: #{analysis[:function_count]} (#{analysis[:has_functions] ? 'Present' : 'None'})"
    puts "  📖 Examples: #{analysis[:has_examples] ? 'Yes' : 'No'}"
    puts "  🧠 Complexity: #{analysis[:complexity_indicators].join(', ')}"
    
    # Calculate code density
    code_density = analysis[:code_lines].to_f / analysis[:total_lines] * 100
    puts "  📊 Code Density: #{code_density.round(1)}%"
  end

  def generate_language_comparison_report(results)
    puts "\n" + "="*80
    puts "COMPREHENSIVE LANGUAGE COMPARISON REPORT"
    puts "="*80
    
    ruby_stats = calculate_stats(results[:ruby])
    lisp_stats = calculate_stats(results[:lisp])
    
    puts "\nOVERALL PERFORMANCE:"
    puts "-" * 40
    
    puts "RUBY:"
    puts "  📊 Average Quality: #{ruby_stats[:avg_quality]}/10"
    puts "  ⏱️  Average Time: #{ruby_stats[:avg_time]}s"
    puts "  🏆 Best Score: #{ruby_stats[:max_quality]}/10"
    puts "  📈 Consistency: #{ruby_stats[:consistency]}%"
    
    puts "\nLISP:"
    puts "  📊 Average Quality: #{lisp_stats[:avg_quality]}/10"
    puts "  ⏱️  Average Time: #{lisp_stats[:avg_time]}s"
    puts "  🏆 Best Score: #{lisp_stats[:max_quality]}/10"
    puts "  📈 Consistency: #{lisp_stats[:consistency]}%"
    
    # Determine overall preference
    puts "\n" + "="*40
    if ruby_stats[:avg_quality] > lisp_stats[:avg_quality]
      advantage = ruby_stats[:avg_quality] - lisp_stats[:avg_quality]
      puts "🏆 OVERALL WINNER: Ruby (+#{advantage.round(1)} avg quality advantage)"
    elsif lisp_stats[:avg_quality] > ruby_stats[:avg_quality]
      advantage = lisp_stats[:avg_quality] - ruby_stats[:avg_quality]
      puts "🏆 OVERALL WINNER: LISP (+#{advantage.round(1)} avg quality advantage)"
    else
      puts "🤝 OVERALL: Tie - Both languages performed equally well"
    end
    
    puts "\n📋 DETAILED INSIGHTS:"
    puts "  🚀 Faster Language: #{ruby_stats[:avg_time] < lisp_stats[:avg_time] ? 'Ruby' : 'LISP'}"
    puts "  🎯 More Consistent: #{ruby_stats[:consistency] > lisp_stats[:consistency] ? 'Ruby' : 'LISP'}"
    puts "  ⭐ Higher Peak: #{ruby_stats[:max_quality] > lisp_stats[:max_quality] ? 'Ruby' : 'LISP'}"
  end

  def calculate_stats(lang_results)
    qualities = lang_results[:qualities]
    times = lang_results[:times]
    
    {
      avg_quality: (qualities.sum.to_f / qualities.size).round(1),
      avg_time: (times.sum.to_f / times.size).round(2),
      max_quality: qualities.max,
      min_quality: qualities.min,
      consistency: (100 - ((qualities.max - qualities.min) / qualities.max.to_f * 100)).round(1)
    }
  end
end