require 'json'
require 'logger'

class PlanningAgent
  attr_reader :provider_manager, :tool_manager, :task_analyzer, :logger

  def initialize(provider_manager, tool_manager, options = {})
    @provider_manager = provider_manager
    @tool_manager = tool_manager
    @task_analyzer = options[:task_analyzer] || TaskAnalyzer.new
    @max_depth = options[:max_depth] || 5
    @target_success_rate = options[:target_success_rate] || 0.75
    @logger = options[:logger] || Logger.new(STDOUT)
    @task_history = []
  end

  # Main entry point for planning
  def plan_task(task_description, context = {})
    logger.info "Planning task: #{task_description}"
    
    root_task = Task.new(
      description: task_description,
      context: context,
      depth: 0,
      parent: nil
    )
    
    decompose_task(root_task)
    optimize_plan(root_task)
    
    root_task
  end

  # Execute a planned task tree
  def execute_plan(task, execution_engine = nil)
    execution_engine ||= ExecutionEngine.new(@provider_manager, @tool_manager)
    
    logger.info "Executing plan for: #{task.description}"
    execute_task_tree(task, execution_engine)
  end

  private

  # Recursively decompose tasks into subtasks
  def decompose_task(task)
    return if task.depth >= @max_depth
    
    # Check if task is simple enough
    complexity = @task_analyzer.assess_complexity(task.description)
    
    if complexity.score <= 3.0 # Simple enough for direct execution
      task.executable = true
      task.complexity = complexity
      return
    end
    
    # Decompose into subtasks
    subtasks = generate_subtasks(task)
    
    subtasks.each do |subtask_desc|
      subtask = Task.new(
        description: subtask_desc,
        context: task.context,
        depth: task.depth + 1,
        parent: task
      )
      
      task.add_subtask(subtask)
      decompose_task(subtask) # Recursive decomposition
    end
  end

  # Generate subtasks using the LLM
  def generate_subtasks(task)
    prompt = build_decomposition_prompt(task)
    
    messages = [
      {
        role: 'system',
        content: 'You are a task decomposition expert. Break down complex tasks into smaller, concrete steps that can be executed independently.'
      },
      {
        role: 'user',
        content: prompt
      }
    ]
    
    response = @provider_manager.chat(messages, temperature: 0.3)
    parse_subtasks(response)
  end

  def build_decomposition_prompt(task)
    <<~PROMPT
      Task: #{task.description}
      Context: #{task.context.to_json}
      
      Break this task down into 3-5 smaller subtasks that:
      1. Are concrete and actionable
      2. Can be executed independently
      3. Together accomplish the main task
      4. Are suitable for a local LLM to implement
      
      Format your response as a JSON array of task descriptions:
      ["subtask 1", "subtask 2", "subtask 3"]
      
      Focus on making each subtask simple enough that it has a high chance of success.
    PROMPT
  end

  def parse_subtasks(response)
    # Extract JSON array from response
    json_match = response.match(/\[.*\]/m)
    return [] unless json_match
    
    begin
      subtasks = JSON.parse(json_match[0])
      subtasks.select { |s| s.is_a?(String) && !s.empty? }
    rescue JSON::ParserError => e
      logger.error "Failed to parse subtasks: #{e.message}"
      []
    end
  end

  # Optimize the plan based on historical success rates
  def optimize_plan(task)
    # Load historical data if available
    similar_tasks = find_similar_tasks(task)
    
    if similar_tasks.any?
      avg_success_rate = similar_tasks.map(&:success_rate).sum / similar_tasks.size
      
      if avg_success_rate < @target_success_rate
        # Adjust prompts or decomposition strategy
        task.optimization_hints = generate_optimization_hints(similar_tasks)
      end
    end
  end

  def find_similar_tasks(task)
    # In a real implementation, this would query the task database
    # For now, return empty array
    []
  end

  def generate_optimization_hints(similar_tasks)
    failures = similar_tasks.select { |t| t.success_rate < @target_success_rate }
    
    hints = {
      common_errors: analyze_common_errors(failures),
      suggested_prompts: generate_improved_prompts(failures),
      decomposition_adjustments: suggest_decomposition_changes(failures)
    }
    
    hints
  end

  def analyze_common_errors(failed_tasks)
    # Analyze patterns in failed tasks
    []
  end

  def generate_improved_prompts(failed_tasks)
    # Generate suggestions for better prompts
    []
  end

  def suggest_decomposition_changes(failed_tasks)
    # Suggest different ways to break down the task
    []
  end

  # Execute a task tree recursively
  def execute_task_tree(task, execution_engine)
    if task.executable?
      # Leaf task - execute directly
      result = execution_engine.execute_task(task)
      task.execution_result = result
      @task_history << task
      result.success?
    else
      # Non-leaf task - execute subtasks
      success = true
      task.subtasks.each do |subtask|
        subtask_success = execute_task_tree(subtask, execution_engine)
        success &&= subtask_success
        
        # Stop if critical subtask fails
        break unless subtask_success && !subtask.optional?
      end
      success
    end
  end
end

# Task representation class
class Task
  attr_accessor :description, :context, :depth, :parent, :subtasks, 
                :executable, :complexity, :execution_result,
                :optimization_hints, :success_rate

  def initialize(description:, context: {}, depth: 0, parent: nil)
    @description = description
    @context = context
    @depth = depth
    @parent = parent
    @subtasks = []
    @executable = false
    @complexity = nil
    @execution_result = nil
    @optimization_hints = {}
    @success_rate = nil
  end

  def add_subtask(task)
    @subtasks << task
  end

  def optional?
    @context[:optional] || false
  end

  def to_tree_string(indent = 0)
    prefix = "  " * indent + "- "
    status = if @execution_result
               @execution_result.success? ? "✓" : "✗"
             else
               "○"
             end
    
    output = "#{prefix}[#{status}] #{@description}"
    output += " (complexity: #{@complexity.score.round(1)})" if @complexity
    output += "\n"
    
    @subtasks.each do |subtask|
      output += subtask.to_tree_string(indent + 1)
    end
    
    output
  end
end

# Task complexity assessment
class TaskComplexity
  attr_reader :score, :factors

  def initialize(score:, factors: {})
    @score = score # 1-10 scale
    @factors = factors # Hash of contributing factors
  end

  def simple?
    @score <= 3.0
  end

  def moderate?
    @score > 3.0 && @score <= 6.0
  end

  def complex?
    @score > 6.0
  end
end