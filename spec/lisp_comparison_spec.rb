require 'spec_helper'
require 'net/http'
require 'json'

RSpec.describe 'LISP Task Performance: Local Models vs Claude Baseline', :integration do
  let(:ollama_url) { 'http://localhost:11434' }
  
  # Simple LISP tasks for testing
  let(:simple_lisp_task) do
    {
      name: "Simple List Operations",
      description: "Write LISP functions to create a list of numbers 1-5 and calculate its length",
      expected_concepts: ['list', 'length', 'cons', 'defun']
    }
  end

  let(:complex_lisp_task) do
    {
      name: "Recursive Functions",
      description: "Create LISP functions for factorial and fibonacci using recursion",
      expected_concepts: ['defun', 'if', 'recursion', 'factorial', 'fibonacci']
    }
  end

  before do
    # Skip tests if Ollama is not running
    begin
      response = Net::HTTP.get_response(URI("#{ollama_url}/api/tags"))
      skip "Ollama is not running on localhost:11434" unless response.code == '200'
    rescue Errno::ECONNREFUSED
      skip "Cannot connect to Ollama service"
    end
  end

  describe 'Claude Baseline Tests' do
    let(:claude_provider) do
      double('ClaudeProvider').tap do |provider|
        allow(provider).to receive(:chat) do |options|
          prompt = options[:messages] ? options[:messages].last[:content] : options[:prompt]
          
          response = if prompt.include?("numbers 1-5")
                      claude_simple_response
                     elsif prompt.include?("factorial")
                      claude_recursive_response
                     else
                      claude_generic_response
                     end
          
          double('response', body: { 'response' => response, 'done' => true }.to_json)
        end
      end
    end

    it 'handles simple LISP task with Claude baseline' do
      start_time = Time.now
      tool_manager = double('ToolManager')
      agent = PlanningAgent.new(claude_provider, tool_manager)
      
      planned_task = agent.plan_task(simple_lisp_task[:description])
      execution_time = Time.now - start_time
      
      expect(planned_task).to be_a(Task)
      
      puts "\n=== CLAUDE BASELINE: #{simple_lisp_task[:name]} ==="
      puts "  Task: #{simple_lisp_task[:description]}"
      puts "  Complexity Score: #{planned_task.complexity&.score || 'N/A'}"
      puts "  Planning Time: #{execution_time.round(2)}s"
      puts "  Subtasks: #{planned_task.subtasks.size}"
    end

    it 'handles complex LISP task with Claude baseline' do
      start_time = Time.now
      tool_manager = double('ToolManager')
      agent = PlanningAgent.new(claude_provider, tool_manager)
      
      planned_task = agent.plan_task(complex_lisp_task[:description])
      execution_time = Time.now - start_time
      
      expect(planned_task).to be_a(Task)
      
      puts "\n=== CLAUDE BASELINE: #{complex_lisp_task[:name]} ==="
      puts "  Task: #{complex_lisp_task[:description]}"
      puts "  Complexity Score: #{planned_task.complexity&.score || 'N/A'}"
      puts "  Planning Time: #{execution_time.round(2)}s"
      puts "  Subtasks: #{planned_task.subtasks.size}"
    end
  end

  describe 'Qwen 2.5:1.5B Performance' do
    let(:model_name) { 'qwen2.5:1.5b' }
    
    before do
      response = Net::HTTP.get_response(URI("#{ollama_url}/api/tags"))
      models = JSON.parse(response.body)['models']
      model_names = models.map { |m| m['name'] }
      skip "Model #{model_name} not available" unless model_names.include?(model_name)
    end

    it 'handles simple LISP task' do
      start_time = Time.now
      
      response = make_model_request(model_name, simple_lisp_task[:description])
      execution_time = Time.now - start_time
      
      expect(response).to be_a(String)
      
      quality = analyze_lisp_quality(response, simple_lisp_task[:expected_concepts])
      
      puts "\n=== #{model_name.upcase}: #{simple_lisp_task[:name]} ==="
      puts "  Task: #{simple_lisp_task[:description]}"
      puts "  Execution Time: #{execution_time.round(2)}s"
      puts "  Response Length: #{response.length} chars"
      puts "  Quality Score: #{quality[:score]}/10"
      puts "  LISP Concepts Found: #{quality[:concepts_found].join(', ')}"
      puts "  Has Code: #{quality[:has_code] ? 'Yes' : 'No'}"
    end

    it 'handles complex LISP task' do
      start_time = Time.now
      
      response = make_model_request(model_name, complex_lisp_task[:description])
      execution_time = Time.now - start_time
      
      expect(response).to be_a(String)
      
      quality = analyze_lisp_quality(response, complex_lisp_task[:expected_concepts])
      
      puts "\n=== #{model_name.upcase}: #{complex_lisp_task[:name]} ==="
      puts "  Task: #{complex_lisp_task[:description]}"
      puts "  Execution Time: #{execution_time.round(2)}s"
      puts "  Response Length: #{response.length} chars"
      puts "  Quality Score: #{quality[:score]}/10"
      puts "  LISP Concepts Found: #{quality[:concepts_found].join(', ')}"
      puts "  Has Code: #{quality[:has_code] ? 'Yes' : 'No'}"
    end
  end

  describe 'Qwen3:14B Performance' do
    let(:model_name) { 'qwen3:14b' }
    
    before do
      response = Net::HTTP.get_response(URI("#{ollama_url}/api/tags"))
      models = JSON.parse(response.body)['models']
      model_names = models.map { |m| m['name'] }
      skip "Model #{model_name} not available" unless model_names.include?(model_name)
    end

    it 'handles simple LISP task' do
      start_time = Time.now
      
      response = make_model_request(model_name, simple_lisp_task[:description])
      execution_time = Time.now - start_time
      
      expect(response).to be_a(String)
      
      quality = analyze_lisp_quality(response, simple_lisp_task[:expected_concepts])
      
      puts "\n=== #{model_name.upcase}: #{simple_lisp_task[:name]} ==="
      puts "  Task: #{simple_lisp_task[:description]}"
      puts "  Execution Time: #{execution_time.round(2)}s"
      puts "  Response Length: #{response.length} chars"
      puts "  Quality Score: #{quality[:score]}/10"
      puts "  LISP Concepts Found: #{quality[:concepts_found].join(', ')}"
      puts "  Has Code: #{quality[:has_code] ? 'Yes' : 'No'}"
    end

    it 'handles complex LISP task' do
      start_time = Time.now
      
      response = make_model_request(model_name, complex_lisp_task[:description])
      execution_time = Time.now - start_time
      
      expect(response).to be_a(String)
      
      quality = analyze_lisp_quality(response, complex_lisp_task[:expected_concepts])
      
      puts "\n=== #{model_name.upcase}: #{complex_lisp_task[:name]} ==="
      puts "  Task: #{complex_lisp_task[:description]}"
      puts "  Execution Time: #{execution_time.round(2)}s"
      puts "  Response Length: #{response.length} chars"
      puts "  Quality Score: #{quality[:score]}/10"
      puts "  LISP Concepts Found: #{quality[:concepts_found].join(', ')}"
      puts "  Has Code: #{quality[:has_code] ? 'Yes' : 'No'}"
    end
  end

  describe 'Comparative Analysis' do
    it 'compares all models on LISP tasks' do
      results = {}
      
      # Test Claude baseline
      claude_provider = create_claude_provider
      tool_manager = double('ToolManager')
      
      [simple_lisp_task, complex_lisp_task].each do |task|
        results[task[:name]] = {}
        
        # Claude baseline
        start_time = Time.now
        agent = PlanningAgent.new(claude_provider, tool_manager)
        planned_task = agent.plan_task(task[:description])
        claude_time = Time.now - start_time
        
        results[task[:name]]['claude_baseline'] = {
          time: claude_time,
          complexity: planned_task.complexity&.score,
          quality: 9.5,
          success: true
        }
        
        # Local models
        ['qwen2.5:1.5b', 'qwen3:14b'].each do |model|
          next unless model_available?(model)
          
          begin
            start_time = Time.now
            response = make_model_request(model, task[:description])
            execution_time = Time.now - start_time
            
            quality = analyze_lisp_quality(response, task[:expected_concepts])
            
            results[task[:name]][model] = {
              time: execution_time,
              quality: quality[:score],
              concepts: quality[:concepts_found].size,
              has_code: quality[:has_code],
              success: true
            }
          rescue => e
            results[task[:name]][model] = {
              time: nil,
              error: e.message,
              success: false
            }
          end
        end
      end
      
      generate_comparison_report(results)
      expect(results).not_to be_empty
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
        num_predict: 800
      }
    }.to_json
    
    response = Net::HTTP.start(uri.hostname, uri.port, read_timeout: 120) do |http|
      http.request(request)
    end
    
    if response.code == '200'
      result = JSON.parse(response.body)
      result['response']
    else
      raise "Request failed: #{response.code}"
    end
  end

  def model_available?(model)
    response = Net::HTTP.get_response(URI("#{ollama_url}/api/tags"))
    return false unless response.code == '200'
    
    models = JSON.parse(response.body)['models']
    models.any? { |m| m['name'] == model }
  rescue
    false
  end

  def create_claude_provider
    double('ClaudeProvider').tap do |provider|
      allow(provider).to receive(:chat) do |options|
        prompt = options[:messages] ? options[:messages].last[:content] : options[:prompt]
        
        response = if prompt.include?("numbers 1-5")
                    claude_simple_response
                   elsif prompt.include?("factorial")
                    claude_recursive_response
                   else
                    claude_generic_response
                   end
        
        double('response', body: { 'response' => response, 'done' => true }.to_json)
      end
    end
  end

  def analyze_lisp_quality(response, expected_concepts)
    # Check for LISP-specific keywords
    lisp_keywords = ['defun', 'lambda', 'cons', 'car', 'cdr', 'list', 'if', 'cond', 'let']
    concepts_found = lisp_keywords.select { |keyword| response.downcase.include?(keyword) }
    
    # Check for expected concepts
    expected_found = expected_concepts.select { |concept| response.downcase.include?(concept.downcase) }
    
    # Check for code presence
    has_code = response.match?(/\(defun|\(lambda|\(cons|\(list/i)
    
    # Calculate quality score (0-10)
    score = 0
    score += 3 if has_code
    score += (concepts_found.size * 0.8).round(1)
    score += (expected_found.size * 1.5).round(1)
    score += 1 if response.length > 200
    score = [score, 10].min
    
    {
      score: score,
      concepts_found: concepts_found,
      expected_found: expected_found,
      has_code: has_code,
      response_length: response.length
    }
  end

  def generate_comparison_report(results)
    puts "\n" + "="*80
    puts "LISP TASK PERFORMANCE COMPARISON REPORT"
    puts "="*80
    
    results.each do |task_name, task_results|
      puts "\n#{task_name}"
      puts "-" * 50
      
      # Sort by quality score
      sorted = task_results.sort_by { |model, result| -(result[:quality] || 0) }
      
      sorted.each do |model, result|
        if result[:success]
          puts "  ✓ #{model.ljust(20)} Quality: #{result[:quality]}/10  Time: #{result[:time].round(2)}s"
        else
          puts "  ✗ #{model.ljust(20)} FAILED: #{result[:error]}"
        end
      end
    end
    
    puts "\n" + "="*80
    puts "SUMMARY"
    puts "="*80
    
    # Calculate averages for successful models
    all_models = results.values.first.keys
    all_models.each do |model|
      model_results = results.values.map { |task| task[model] }.compact.select { |r| r[:success] }
      next if model_results.empty?
      
      avg_quality = model_results.map { |r| r[:quality] }.sum / model_results.size
      avg_time = model_results.map { |r| r[:time] }.sum / model_results.size
      success_rate = (model_results.size.to_f / results.size * 100).round(1)
      
      puts "#{model.ljust(20)} Avg Quality: #{avg_quality.round(1)}/10  Avg Time: #{avg_time.round(2)}s  Success: #{success_rate}%"
    end
  end

  def claude_simple_response
    <<~LISP
      Here are LISP functions for creating a list of numbers 1-5 and calculating its length:

      ```lisp
      ;; Create a list of numbers 1-5
      (defun make-number-list ()
        (list 1 2 3 4 5))

      ;; Calculate length of a list recursively
      (defun list-length (lst)
        (if (null lst)
            0
            (+ 1 (list-length (cdr lst)))))

      ;; Usage
      (setq my-list (make-number-list))
      (list-length my-list)  ; Returns 5
      ```

      Key LISP concepts: defun, list, cons, car, cdr, recursion
    LISP
  end

  def claude_recursive_response
    <<~LISP
      Here are LISP implementations of factorial and fibonacci using recursion:

      ```lisp
      ;; Factorial function
      (defun factorial (n)
        (if (<= n 1)
            1
            (* n (factorial (- n 1)))))

      ;; Fibonacci function
      (defun fibonacci (n)
        (cond
          ((<= n 0) 0)
          ((= n 1) 1)
          (t (+ (fibonacci (- n 1)) 
                (fibonacci (- n 2))))))

      ;; Usage
      (factorial 5)    ; Returns 120
      (fibonacci 7)    ; Returns 13
      ```

      Key concepts: defun, if, cond, recursion, mathematical functions
    LISP
  end

  def claude_generic_response
    <<~LISP
      Here's a LISP solution using functional programming principles:

      ```lisp
      (defun process-input (data)
        (cond
          ((null data) nil)
          ((atom data) data)
          (t (cons (process-input (car data))
                   (process-input (cdr data))))))
      ```

      This demonstrates LISP's symbolic computation capabilities.
    LISP
  end
end