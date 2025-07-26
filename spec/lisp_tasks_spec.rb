require 'spec_helper'
require 'net/http'
require 'json'

RSpec.describe 'LISP Task Testing with Model Comparison', :integration do
  let(:ollama_url) { 'http://localhost:11434' }
  let(:local_models) { ['qwen2.5:1.5b', 'qwen2.5:3b', 'qwen3:14b', 'qwen3:32b'] }
  
  # LISP tasks of varying complexity
  let(:lisp_tasks) do
    [
      {
        name: "Simple List Operations",
        complexity: 2,
        description: "Write LISP functions to create a list of numbers 1-5 and calculate its length",
        expected_concepts: ['list', 'length', 'cons', 'car', 'cdr']
      },
      {
        name: "Basic Arithmetic Functions", 
        complexity: 3,
        description: "Create LISP functions for factorial and fibonacci using recursion",
        expected_concepts: ['defun', 'if', 'recursion', 'factorial', 'fibonacci']
      },
      {
        name: "List Processing",
        complexity: 4,
        description: "Implement map, filter, and reduce functions in LISP with lambda expressions",
        expected_concepts: ['mapcar', 'lambda', 'filter', 'reduce', 'funcall']
      },
      {
        name: "Tree Manipulation",
        complexity: 6,
        description: "Create LISP functions to build and traverse binary trees with in-order, pre-order, and post-order traversal",
        expected_concepts: ['tree', 'cons', 'traversal', 'recursion', 'binary-tree']
      },
      {
        name: "Macro Programming",
        complexity: 8,
        description: "Write LISP macros for domain-specific language constructs including when-let and thread-first operators",
        expected_concepts: ['defmacro', 'gensym', 'backquote', 'macro-expansion', 'hygiene']
      }
    ]
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

  describe 'Claude Sonnet Baseline Testing' do
    let(:claude_provider) do
      # Mock provider that simulates Claude responses for baseline
      double('ClaudeProvider').tap do |provider|
        allow(provider).to receive(:chat) do |options|
          prompt = options[:messages] ? options[:messages].last[:content] : options[:prompt]
          
          # Generate Claude-quality responses for LISP tasks
          response = case prompt.downcase
                    when /list.*numbers.*1-5.*length/
                      claude_simple_list_response
                    when /factorial.*fibonacci.*recursion/
                      claude_arithmetic_response
                    when /map.*filter.*reduce.*lambda/
                      claude_list_processing_response
                    when /binary.*tree.*traversal/
                      claude_tree_response
                    when /macro.*domain.*specific.*language/
                      claude_macro_response
                    else
                      claude_generic_lisp_response(prompt)
                    end
          
          double('response', body: { 'response' => response, 'done' => true }.to_json)
        end
      end
    end

    it "handles Simple List Operations (complexity: 2) with Claude baseline" do
      task = lisp_tasks[0]
      start_time = Time.now
      tool_manager = double('ToolManager')
      agent = PlanningAgent.new(claude_provider, tool_manager)
      
      planned_task = agent.plan_task(task[:description])
      execution_time = Time.now - start_time
      
      expect(planned_task).to be_a(Task)
      expect(planned_task.complexity).to respond_to(:score)
      
      puts "\n=== CLAUDE BASELINE: #{task[:name]} ==="
      puts "  Complexity Score: #{planned_task.complexity&.score || 'N/A'}"
      puts "  Planning Time: #{execution_time.round(2)}s"
      puts "  Subtasks Generated: #{planned_task.subtasks.size}"
      puts "  Expected Concepts: #{task[:expected_concepts].join(', ')}"
    end

    it "handles Basic Arithmetic Functions (complexity: 3) with Claude baseline" do
      task = lisp_tasks[1]
      start_time = Time.now
      tool_manager = double('ToolManager')
      agent = PlanningAgent.new(claude_provider, tool_manager)
      
      planned_task = agent.plan_task(task[:description])
      execution_time = Time.now - start_time
      
      expect(planned_task).to be_a(Task)
      expect(planned_task.complexity).to respond_to(:score)
      
      puts "\n=== CLAUDE BASELINE: #{task[:name]} ==="
      puts "  Complexity Score: #{planned_task.complexity&.score || 'N/A'}"
      puts "  Planning Time: #{execution_time.round(2)}s"
      puts "  Subtasks Generated: #{planned_task.subtasks.size}"
      puts "  Expected Concepts: #{task[:expected_concepts].join(', ')}"
    end

    it "handles List Processing (complexity: 4) with Claude baseline" do
      task = lisp_tasks[2]
      start_time = Time.now
      tool_manager = double('ToolManager')
      agent = PlanningAgent.new(claude_provider, tool_manager)
      
      planned_task = agent.plan_task(task[:description])
      execution_time = Time.now - start_time
      
      expect(planned_task).to be_a(Task)
      expect(planned_task.complexity).to respond_to(:score)
      
      puts "\n=== CLAUDE BASELINE: #{task[:name]} ==="
      puts "  Complexity Score: #{planned_task.complexity&.score || 'N/A'}"
      puts "  Planning Time: #{execution_time.round(2)}s"
      puts "  Subtasks Generated: #{planned_task.subtasks.size}"
      puts "  Expected Concepts: #{task[:expected_concepts].join(', ')}"
    end

    it "handles Tree Manipulation (complexity: 6) with Claude baseline" do
      task = lisp_tasks[3]
      start_time = Time.now
      tool_manager = double('ToolManager')
      agent = PlanningAgent.new(claude_provider, tool_manager)
      
      planned_task = agent.plan_task(task[:description])
      execution_time = Time.now - start_time
      
      expect(planned_task).to be_a(Task)
      expect(planned_task.complexity).to respond_to(:score)
      
      puts "\n=== CLAUDE BASELINE: #{task[:name]} ==="
      puts "  Complexity Score: #{planned_task.complexity&.score || 'N/A'}"
      puts "  Planning Time: #{execution_time.round(2)}s"
      puts "  Subtasks Generated: #{planned_task.subtasks.size}"
      puts "  Expected Concepts: #{task[:expected_concepts].join(', ')}"
    end

    it "handles Macro Programming (complexity: 8) with Claude baseline" do
      task = lisp_tasks[4]
      start_time = Time.now
      tool_manager = double('ToolManager')
      agent = PlanningAgent.new(claude_provider, tool_manager)
      
      planned_task = agent.plan_task(task[:description])
      execution_time = Time.now - start_time
      
      expect(planned_task).to be_a(Task)
      expect(planned_task.complexity).to respond_to(:score)
      
      puts "\n=== CLAUDE BASELINE: #{task[:name]} ==="
      puts "  Complexity Score: #{planned_task.complexity&.score || 'N/A'}"
      puts "  Planning Time: #{execution_time.round(2)}s"
      puts "  Subtasks Generated: #{planned_task.subtasks.size}"
      puts "  Expected Concepts: #{task[:expected_concepts].join(', ')}"
    end
  end

  describe 'Local Model LISP Task Performance' do
    local_models.each do |model|
      context "with #{model}" do
        let(:model_provider) do
          double('ModelProvider').tap do |provider|
            allow(provider).to receive(:chat) do |options|
              prompt = options[:messages] ? options[:messages].last[:content] : options[:prompt]
              
              # Make real API call to local model
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
              
              response = Net::HTTP.start(uri.hostname, uri.port, read_timeout: 120) do |http|
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

        before do
          # Check if model is available
          response = Net::HTTP.get_response(URI("#{ollama_url}/api/tags"))
          models = JSON.parse(response.body)['models']
          model_names = models.map { |m| m['name'] }
          skip "Model #{model} not available" unless model_names.include?(model)
        end

        lisp_tasks.each do |task|
          it "handles #{task[:name]} (complexity: #{task[:complexity]})" do
            start_time = Time.now
            tool_manager = double('ToolManager')
            agent = PlanningAgent.new(model_provider, tool_manager)
            
            begin
              planned_task = agent.plan_task(task[:description])
              execution_time = Time.now - start_time
              
              # Analyze the response quality
              quality_score = analyze_lisp_response_quality(planned_task, task)
              
              expect(planned_task).to be_a(Task)
              expect(execution_time).to be < 180 # Max 3 minutes per task
              
              puts "\n=== #{model.upcase}: #{task[:name]} ==="
              puts "  Complexity Score: #{planned_task.complexity&.score || 'N/A'}"
              puts "  Planning Time: #{execution_time.round(2)}s"
              puts "  Subtasks Generated: #{planned_task.subtasks.size}"
              puts "  Quality Score: #{quality_score[:score]}/10"
              puts "  LISP Concepts Found: #{quality_score[:concepts_found]}"
              puts "  Code Detected: #{quality_score[:has_code] ? 'Yes' : 'No'}"
              
            rescue => e
              puts "\n=== #{model.upcase}: #{task[:name]} - FAILED ==="
              puts "  Error: #{e.message}"
              puts "  Time before failure: #{(Time.now - start_time).round(2)}s"
              raise e
            end
          end
        end
      end
    end
  end

  describe 'Comparative Analysis' do
    it 'compares LISP task performance across all models' do
      results = {}
      
      # Test each task with each model
      lisp_tasks.each do |task|
        results[task[:name]] = {}
        
        # Test Claude baseline (simulated)
        start_time = Time.now
        claude_provider = create_claude_baseline_provider
        tool_manager = double('ToolManager')
        claude_agent = PlanningAgent.new(claude_provider, tool_manager)
        claude_task = claude_agent.plan_task(task[:description])
        claude_time = Time.now - start_time
        
        results[task[:name]]['claude_baseline'] = {
          time: claude_time,
          complexity: claude_task.complexity&.score,
          subtasks: claude_task.subtasks.size,
          quality: 9.5 # Assume high quality for baseline
        }
        
        # Test each local model
        local_models.each do |model|
          next unless model_available?(model)
          
          begin
            start_time = Time.now
            provider = create_model_provider(model)
            agent = PlanningAgent.new(provider, double('ToolManager'))
            planned_task = agent.plan_task(task[:description])
            execution_time = Time.now - start_time
            
            quality = analyze_lisp_response_quality(planned_task, task)
            
            results[task[:name]][model] = {
              time: execution_time,
              complexity: planned_task.complexity&.score,
              subtasks: planned_task.subtasks.size,
              quality: quality[:score],
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
      
      # Generate comprehensive report
      generate_lisp_performance_report(results)
      
      expect(results).not_to be_empty
      
      # Verify at least one model succeeded on each task
      lisp_tasks.each do |task|
        task_results = results[task[:name]]
        successful_models = task_results.select { |model, result| result[:success] }
        expect(successful_models.size).to be > 0, "No models succeeded on task: #{task[:name]}"
      end
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

  def create_model_provider(model)
    double('ModelProvider').tap do |provider|
      allow(provider).to receive(:chat) do |options|
        prompt = options[:messages] ? options[:messages].last[:content] : options[:prompt]
        
        uri = URI("#{ollama_url}/api/generate")
        request = Net::HTTP::Post.new(uri)
        request['Content-Type'] = 'application/json'
        request.body = {
          model: model,
          prompt: prompt,
          stream: false,
          options: {
            temperature: 0.1,
            num_predict: 1000
          }
        }.to_json
        
        response = Net::HTTP.start(uri.hostname, uri.port, read_timeout: 120) do |http|
          http.request(request)
        end
        
        double('response', body: response.body)
      end
    end
  end

  def create_claude_baseline_provider
    double('ClaudeProvider').tap do |provider|
      allow(provider).to receive(:chat) do |options|
        prompt = options[:messages] ? options[:messages].last[:content] : options[:prompt]
        
        response = case prompt.downcase
                  when /list.*numbers.*1-5.*length/
                    claude_simple_list_response
                  when /factorial.*fibonacci.*recursion/
                    claude_arithmetic_response
                  when /map.*filter.*reduce.*lambda/
                    claude_list_processing_response
                  when /binary.*tree.*traversal/
                    claude_tree_response
                  when /macro.*domain.*specific.*language/
                    claude_macro_response
                  else
                    claude_generic_lisp_response(prompt)
                  end
        
        double('response', body: { 'response' => response, 'done' => true }.to_json)
      end
    end
  end

  def analyze_lisp_response_quality(task, expected_task)
    response_text = task.description || ""
    
    # Check for LISP-specific concepts
    lisp_keywords = ['defun', 'lambda', 'cons', 'car', 'cdr', 'list', 'mapcar', 'if', 'cond', 'let']
    concepts_found = lisp_keywords.select { |keyword| response_text.downcase.include?(keyword) }
    
    # Check for expected concepts
    expected_found = expected_task[:expected_concepts].select { |concept| response_text.downcase.include?(concept.downcase) }
    
    # Check for code presence
    has_code = response_text.match?(/\(defun|\(lambda|\(cons|\(list/i)
    
    # Calculate quality score (0-10)
    score = 0
    score += 2 if has_code
    score += (concepts_found.size * 0.5).round(1)
    score += (expected_found.size * 1.5).round(1)
    score += 1 if response_text.length > 100
    score = [score, 10].min
    
    {
      score: score,
      concepts_found: concepts_found,
      expected_found: expected_found,
      has_code: has_code,
      response_length: response_text.length
    }
  end

  def generate_lisp_performance_report(results)
    puts "\n" + "="*80
    puts "COMPREHENSIVE LISP TASK PERFORMANCE REPORT"
    puts "="*80
    
    lisp_tasks.each do |task|
      puts "\n#{task[:name]} (Complexity: #{task[:complexity]})"
      puts "-" * 60
      
      task_results = results[task[:name]]
      
      # Sort by quality score
      sorted_results = task_results.sort_by { |model, result| -(result[:quality] || 0) }
      
      sorted_results.each do |model, result|
        if result[:success]
          status = "✓"
          time_str = "#{result[:time].round(2)}s"
          quality_str = "Quality: #{result[:quality]}/10"
          complexity_str = "Complexity: #{result[:complexity] || 'N/A'}"
          subtasks_str = "Subtasks: #{result[:subtasks]}"
          
          puts "  #{status} #{model.ljust(20)} #{time_str.ljust(8)} #{quality_str.ljust(15)} #{complexity_str.ljust(18)} #{subtasks_str}"
        else
          puts "  ✗ #{model.ljust(20)} FAILED   Error: #{result[:error]}"
        end
      end
    end
    
    puts "\n" + "="*80
    puts "SUMMARY STATISTICS"
    puts "="*80
    
    # Calculate averages
    all_models = ['claude_baseline'] + local_models.select { |m| model_available?(m) }
    
    all_models.each do |model|
      model_results = results.values.map { |task_result| task_result[model] }.compact.select { |r| r[:success] }
      next if model_results.empty?
      
      avg_time = model_results.map { |r| r[:time] }.compact.sum / model_results.size
      avg_quality = model_results.map { |r| r[:quality] }.compact.sum / model_results.size
      success_rate = (model_results.size.to_f / lisp_tasks.size * 100).round(1)
      
      puts "#{model.ljust(20)} Avg Time: #{avg_time.round(2)}s  Avg Quality: #{avg_quality.round(1)}/10  Success: #{success_rate}%"
    end
  end

  # Claude baseline response templates
  def claude_simple_list_response
    <<~LISP
      Here are LISP functions for creating a list of numbers 1-5 and calculating its length:

      ```lisp
      ;; Create a list of numbers 1-5
      (defun make-number-list ()
        (list 1 2 3 4 5))

      ;; Alternative using cons
      (defun make-list-with-cons ()
        (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil))))))

      ;; Calculate length of a list
      (defun list-length (lst)
        (if (null lst)
            0
            (+ 1 (list-length (cdr lst)))))

      ;; Usage example
      (setq my-list (make-number-list))
      (list-length my-list)  ; Returns 5
      ```

      The key LISP concepts demonstrated:
      - `list` function for creating lists
      - `cons` for building lists recursively  
      - `car` and `cdr` for list access
      - Recursive function definition with base case
    LISP
  end

  def claude_arithmetic_response
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

      ;; Tail-recursive factorial for efficiency
      (defun factorial-tail (n &optional (acc 1))
        (if (<= n 1)
            acc
            (factorial-tail (- n 1) (* n acc))))

      ;; Usage examples
      (factorial 5)    ; Returns 120
      (fibonacci 7)    ; Returns 13
      ```

      Key LISP concepts:
      - `defun` for function definition
      - `if` and `cond` for conditional logic
      - Recursive function calls
      - Optional parameters with `&optional`
    LISP
  end

  def claude_list_processing_response
    <<~LISP
      Here are LISP implementations of map, filter, and reduce with lambda expressions:

      ```lisp
      ;; Map implementation
      (defun my-map (function lst)
        (if (null lst)
            nil
            (cons (funcall function (car lst))
                  (my-map function (cdr lst)))))

      ;; Filter implementation  
      (defun my-filter (predicate lst)
        (cond
          ((null lst) nil)
          ((funcall predicate (car lst))
           (cons (car lst) (my-filter predicate (cdr lst))))
          (t (my-filter predicate (cdr lst)))))

      ;; Reduce implementation
      (defun my-reduce (function lst &optional initial)
        (if initial
            (my-reduce-helper function lst initial)
            (my-reduce-helper function (cdr lst) (car lst))))

      (defun my-reduce-helper (function lst acc)
        (if (null lst)
            acc
            (my-reduce-helper function (cdr lst) 
                              (funcall function acc (car lst)))))

      ;; Usage with lambda expressions
      (my-map (lambda (x) (* x x)) '(1 2 3 4))           ; Square each element
      (my-filter (lambda (x) (> x 2)) '(1 2 3 4 5))      ; Filter > 2
      (my-reduce (lambda (acc x) (+ acc x)) '(1 2 3 4))   ; Sum all elements
      ```

      Key concepts:
      - `lambda` for anonymous functions
      - `funcall` for function application
      - Higher-order functions
      - Accumulator pattern in reduce
    LISP
  end

  def claude_tree_response
    <<~LISP
      Here are LISP functions for binary tree creation and traversal:

      ```lisp
      ;; Tree node constructor
      (defun make-node (value left right)
        (list value left right))

      ;; Tree accessors
      (defun node-value (node) (car node))
      (defun node-left (node) (cadr node))
      (defun node-right (node) (caddr node))

      ;; In-order traversal (left, root, right)
      (defun in-order (tree)
        (if (null tree)
            nil
            (append (in-order (node-left tree))
                    (list (node-value tree))
                    (in-order (node-right tree)))))

      ;; Pre-order traversal (root, left, right)
      (defun pre-order (tree)
        (if (null tree)
            nil
            (cons (node-value tree)
                  (append (pre-order (node-left tree))
                          (pre-order (node-right tree))))))

      ;; Post-order traversal (left, right, root)
      (defun post-order (tree)
        (if (null tree)
            nil
            (append (post-order (node-left tree))
                    (post-order (node-right tree))
                    (list (node-value tree)))))

      ;; Example tree creation
      (setq sample-tree
        (make-node 4
          (make-node 2 (make-node 1 nil nil) (make-node 3 nil nil))
          (make-node 6 (make-node 5 nil nil) (make-node 7 nil nil))))

      ;; Usage
      (in-order sample-tree)   ; (1 2 3 4 5 6 7)
      (pre-order sample-tree)  ; (4 2 1 3 6 5 7)
      (post-order sample-tree) ; (1 3 2 5 7 6 4)
      ```
    LISP
  end

  def claude_macro_response
    <<~LISP
      Here are LISP macro implementations for domain-specific language constructs:

      ```lisp
      ;; when-let macro for conditional binding
      (defmacro when-let ((var expr) &body body)
        (let ((temp-var (gensym)))
          `(let ((,temp-var ,expr))
             (when ,temp-var
               (let ((,var ,temp-var))
                 ,@body)))))

      ;; Thread-first macro (similar to Clojure's ->)
      (defmacro thread-first (expr &rest forms)
        (reduce (lambda (acc form)
                  (if (listp form)
                      `(,(car form) ,acc ,@(cdr form))
                      `(,form ,acc)))
                forms
                :initial-value expr))

      ;; Unless macro (opposite of when)
      (defmacro unless (condition &body body)
        `(if (not ,condition)
             (progn ,@body)))

      ;; Syntax sugar for hash table access
      (defmacro hash-get (hash key)
        `(gethash ,key ,hash))

      ;; Usage examples:
      
      ;; when-let usage
      (when-let ((result (find-if #'oddp '(2 4 6 7 8))))
        (format t "Found odd number: ~a~%" result))

      ;; Thread-first usage  
      (thread-first 5
        (+ 3)           ; 8
        (* 2)           ; 16
        (- 1))          ; 15

      ;; Macro expansion example
      (macroexpand-1 '(when-let ((x (get-value))) (process x)))
      ```

      Key macro concepts:
      - `defmacro` for macro definition
      - `gensym` for hygenic variable generation
      - Backquote (`) and unquote (,) for template construction
      - `@` for splicing lists
      - Macro expansion vs function calls
    LISP
  end

  def claude_generic_lisp_response(prompt)
    <<~LISP
      Here's a LISP approach to: #{prompt}

      ```lisp
      ;; Basic LISP structure for the requested task
      (defun solve-problem (input)
        (cond
          ((null input) nil)
          ((atom input) (process-atom input))
          (t (mapcar #'solve-problem input))))

      (defun process-atom (atom)
        ;; Process individual atomic elements
        (cond
          ((numberp atom) (* atom 2))
          ((symbolp atom) (symbol-name atom))
          (t atom)))
      ```

      This demonstrates core LISP principles:
      - Symbolic computation
      - List processing
      - Recursive problem decomposition
      - Functional programming approach
    LISP
  end
end