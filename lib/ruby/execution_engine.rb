require 'json'
require 'tempfile'
require_relative 'auto_fixer'

class ExecutionEngine
  attr_reader :provider_manager, :tool_manager, :auto_fixer, :logger

  def initialize(provider_manager, tool_manager, options = {})
    @provider_manager = provider_manager
    @tool_manager = tool_manager
    @auto_fixer = options[:auto_fixer] || AutoFixer.new
    @logger = options[:logger] || Logger.new(STDOUT)
    @verification_enabled = options[:verify] != false
    @rollback_enabled = options[:rollback] != false
    @max_retries = options[:max_retries] || 3
  end

  # Execute a single task with verification and auto-fixing
  def execute_task(task)
    logger.info "Executing task: #{task.description}"
    
    result = ExecutionResult.new(task: task)
    checkpoint = create_checkpoint if @rollback_enabled
    
    begin
      # Generate implementation
      implementation = generate_implementation(task)
      result.implementation = implementation
      
      # Analyze the generated code
      if implementation[:type] == :code
        issues = analyze_implementation(implementation)
        result.issues = issues
        
        # Apply auto-fixes if available
        if issues.any?(&:auto_fixable)
          fixed_implementation = apply_auto_fixes(implementation, issues)
          result.fixed_implementation = fixed_implementation
          implementation = fixed_implementation
        end
      end
      
      # Execute the implementation
      execution_output = execute_implementation(implementation, task)
      result.output = execution_output
      
      # Verify the result
      if @verification_enabled
        verification = verify_execution(task, execution_output)
        result.verification = verification
        result.success = verification[:passed]
      else
        result.success = execution_output[:success]
      end
      
      # Handle failure with retries
      if !result.success && result.attempts < @max_retries
        result = retry_with_improvements(task, result)
      end
      
    rescue => e
      logger.error "Task execution failed: #{e.message}"
      logger.error e.backtrace.join("\n")
      result.error = e
      result.success = false
      
      # Rollback if enabled
      restore_checkpoint(checkpoint) if @rollback_enabled && checkpoint
    end
    
    result
  end

  private

  def generate_implementation(task)
    prompt = build_implementation_prompt(task)
    
    messages = [
      {
        role: 'system',
        content: 'You are a code implementation expert. Generate clean, working code that accomplishes the given task. Include necessary imports and handle errors appropriately.'
      },
      {
        role: 'user',
        content: prompt
      }
    ]
    
    response = @provider_manager.chat(messages, temperature: 0.2)
    parse_implementation(response, task)
  end

  def build_implementation_prompt(task)
    context_str = task.context.any? ? "\nContext: #{JSON.pretty_generate(task.context)}" : ""
    
    <<~PROMPT
      Task: #{task.description}#{context_str}
      
      Generate the implementation for this task. If it involves code, provide:
      1. The complete code with proper error handling
      2. The language (ruby, javascript, python, etc.)
      3. Any setup or dependencies needed
      
      If it involves commands, provide:
      1. The exact commands to run
      2. Expected outputs
      3. Any prerequisites
      
      Format your response as:
      ```language
      // code here
      ```
      
      Or for commands:
      COMMANDS:
      command1
      command2
      
      Be precise and include all necessary details for successful execution.
    PROMPT
  end

  def parse_implementation(response, task)
    implementation = {
      raw_response: response,
      task: task
    }
    
    # Extract code blocks
    code_match = response.match(/```(\w+)\n(.*?)```/m)
    if code_match
      implementation[:type] = :code
      implementation[:language] = code_match[1].downcase.to_sym
      implementation[:code] = code_match[2].strip
    elsif response.include?("COMMANDS:")
      implementation[:type] = :commands
      commands_section = response.split("COMMANDS:")[1]
      implementation[:commands] = commands_section.strip.split("\n").map(&:strip).reject(&:empty?)
    else
      implementation[:type] = :instructions
      implementation[:content] = response
    end
    
    implementation
  end

  def analyze_implementation(implementation)
    return [] unless implementation[:type] == :code
    
    analyzer = TaskAnalyzer.new
    analyzer.analyze_code(implementation[:code], implementation[:language])
  end

  def apply_auto_fixes(implementation, issues)
    fixed_impl = implementation.dup
    fixed_code = implementation[:code]
    
    issues.select(&:auto_fixable).each do |issue|
      fixed_code = @auto_fixer.fix_issue(fixed_code, issue, implementation[:language])
    end
    
    fixed_impl[:code] = fixed_code
    fixed_impl[:fixes_applied] = issues.select(&:auto_fixable).map(&:type)
    fixed_impl
  end

  def execute_implementation(implementation, task)
    case implementation[:type]
    when :code
      execute_code(implementation)
    when :commands
      execute_commands(implementation)
    when :instructions
      { success: true, output: "Instructions provided", implementation: implementation }
    else
      { success: false, error: "Unknown implementation type" }
    end
  end

  def execute_code(implementation)
    case implementation[:language]
    when :ruby
      execute_ruby_code(implementation[:code])
    when :javascript, :js
      execute_javascript_code(implementation[:code])
    when :python
      execute_python_code(implementation[:code])
    else
      { success: false, error: "Unsupported language: #{implementation[:language]}" }
    end
  end

  def execute_ruby_code(code)
    # Save to temp file and execute
    temp_file = Tempfile.new(['task_', '.rb'])
    temp_file.write(code)
    temp_file.close
    
    output = `ruby #{temp_file.path} 2>&1`
    success = $?.success?
    
    temp_file.unlink
    
    { success: success, output: output, language: :ruby }
  end

  def execute_javascript_code(code)
    temp_file = Tempfile.new(['task_', '.js'])
    temp_file.write(code)
    temp_file.close
    
    output = `node #{temp_file.path} 2>&1`
    success = $?.success?
    
    temp_file.unlink
    
    { success: success, output: output, language: :javascript }
  end

  def execute_python_code(code)
    temp_file = Tempfile.new(['task_', '.py'])
    temp_file.write(code)
    temp_file.close
    
    output = `python3 #{temp_file.path} 2>&1`
    success = $?.success?
    
    temp_file.unlink
    
    { success: success, output: output, language: :python }
  end

  def execute_commands(implementation)
    outputs = []
    success = true
    
    implementation[:commands].each do |command|
      output = @tool_manager.execute_tool('bash', command)
      outputs << { command: command, output: output }
      
      # Check if command failed
      if output.start_with?("Error:")
        success = false
        break
      end
    end
    
    { success: success, outputs: outputs, type: :commands }
  end

  def verify_execution(task, execution_output)
    verification = {
      passed: false,
      checks: []
    }
    
    # Basic success check
    if execution_output[:success]
      verification[:checks] << { name: "execution_success", passed: true }
    else
      verification[:checks] << { name: "execution_success", passed: false, error: execution_output[:error] }
      return verification
    end
    
    # Output validation
    if task.context[:expected_output]
      output_check = verify_output(execution_output[:output], task.context[:expected_output])
      verification[:checks] << output_check
    end
    
    # File system changes validation
    if task.context[:creates_files]
      files_check = verify_files_created(task.context[:creates_files])
      verification[:checks] << files_check
    end
    
    # All checks must pass
    verification[:passed] = verification[:checks].all? { |c| c[:passed] }
    verification
  end

  def verify_output(actual_output, expected_output)
    if expected_output.is_a?(Regexp)
      passed = actual_output.match?(expected_output)
    else
      passed = actual_output.include?(expected_output.to_s)
    end
    
    {
      name: "output_validation",
      passed: passed,
      expected: expected_output.to_s,
      actual: actual_output[0..100]
    }
  end

  def verify_files_created(expected_files)
    missing_files = expected_files.reject { |f| File.exist?(f) }
    
    {
      name: "files_created",
      passed: missing_files.empty?,
      missing: missing_files
    }
  end

  def retry_with_improvements(task, previous_result)
    logger.info "Retrying task with improvements (attempt #{previous_result.attempts + 1})"
    
    # Build improved prompt with error context
    improved_task = task.dup
    improved_task.context[:previous_error] = previous_result.error || previous_result.output
    improved_task.context[:previous_issues] = previous_result.issues.map(&:to_s) if previous_result.issues.any?
    
    # Retry execution
    new_result = execute_task(improved_task)
    new_result.attempts = previous_result.attempts + 1
    new_result
  end

  def create_checkpoint
    # Simple checkpoint: record current directory state
    {
      timestamp: Time.now,
      cwd: Dir.pwd,
      files: Dir.glob("**/*").select { |f| File.file?(f) }.map do |f|
        { path: f, mtime: File.mtime(f), size: File.size(f) }
      end
    }
  end

  def restore_checkpoint(checkpoint)
    logger.warn "Restoring checkpoint from #{checkpoint[:timestamp]}"
    # In a real implementation, this would restore file system state
    # For now, just log the action
  end
end

class ExecutionResult
  attr_accessor :task, :implementation, :fixed_implementation, :issues, 
                :output, :verification, :success, :error, :attempts

  def initialize(task:)
    @task = task
    @attempts = 1
    @success = false
    @issues = []
  end

  def success?
    @success
  end

  def to_h
    {
      task: @task.description,
      success: @success,
      attempts: @attempts,
      issues: @issues.map(&:to_s),
      output: @output,
      error: @error&.message,
      verification: @verification
    }
  end
end