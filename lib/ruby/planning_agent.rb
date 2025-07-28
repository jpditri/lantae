require 'json'

module Lantae
  class PlanningAgent
    attr_reader :provider_manager, :options
    
    def initialize(provider_manager, options = {})
      @provider_manager = provider_manager
      @options = options
    end
    
    def create_plan(prompt, context = {})
      puts "📋 Creating comprehensive plan..."
      
      planning_prompt = build_planning_prompt(prompt, context)
      
      response = @provider_manager.chat([
        { role: 'system', content: PLANNING_SYSTEM_PROMPT },
        { role: 'user', content: planning_prompt }
      ], planning_options)
      
      parse_plan(response)
    end
    
    def refine_plan(plan, feedback)
      refinement_prompt = <<~PROMPT
        Here is an existing plan:
        #{plan.to_json}
        
        User feedback: #{feedback}
        
        Please refine the plan based on this feedback. Maintain the same JSON structure.
      PROMPT
      
      response = @provider_manager.chat([
        { role: 'system', content: PLANNING_SYSTEM_PROMPT },
        { role: 'user', content: refinement_prompt }
      ], planning_options)
      
      parse_plan(response)
    end
    
    def validate_plan(plan)
      validation_prompt = <<~PROMPT
        Validate this plan for completeness and feasibility:
        #{plan.to_json}
        
        Check for:
        1. Missing steps or dependencies
        2. Unrealistic time estimates
        3. Resource conflicts
        4. Potential risks not addressed
        
        Respond with JSON: { "valid": true/false, "issues": [], "suggestions": [] }
      PROMPT
      
      response = @provider_manager.chat([
        { role: 'user', content: validation_prompt }
      ], { temperature: 0.1, max_tokens: 1000 })
      
      JSON.parse(response)
    rescue JSON::ParserError
      { "valid" => false, "issues" => ["Failed to validate plan"], "suggestions" => [] }
    end
    
    private
    
    PLANNING_SYSTEM_PROMPT = <<~PROMPT
      You are an expert planning agent. Create detailed, actionable plans that break down complex tasks.
      
      Your plans should include:
      1. Clear objectives and success criteria
      2. Step-by-step tasks with dependencies
      3. Resource requirements (tools, APIs, models)
      4. Time estimates for each step
      5. Risk assessment and mitigation strategies
      6. Parallel execution opportunities
      
      Always respond with a structured JSON plan following this schema:
      {
        "objective": "Main goal",
        "success_criteria": ["criteria1", "criteria2"],
        "estimated_duration": "time estimate",
        "phases": [
          {
            "name": "Phase name",
            "description": "What this phase accomplishes",
            "tasks": [
              {
                "id": "unique_id",
                "name": "Task name",
                "description": "Detailed description",
                "dependencies": ["task_ids"],
                "estimated_duration": "time",
                "required_capabilities": ["capability1", "capability2"],
                "agent_type": "specialist/generalist/analyzer",
                "parallel": true/false
              }
            ]
          }
        ],
        "resource_requirements": {
          "tools": ["tool1", "tool2"],
          "models": {
            "preferred": "model_name",
            "alternatives": ["alt1", "alt2"]
          },
          "external_apis": []
        },
        "risks": [
          {
            "description": "Risk description",
            "probability": "low/medium/high",
            "impact": "low/medium/high",
            "mitigation": "How to handle"
          }
        ]
      }
    PROMPT
    
    def build_planning_prompt(prompt, context)
      prompt_parts = ["Task: #{prompt}"]
      
      if context[:requirements]
        prompt_parts << "Requirements: #{context[:requirements]}"
      end
      
      if context[:constraints]
        prompt_parts << "Constraints: #{context[:constraints]}"
      end
      
      if context[:current_state]
        prompt_parts << "Current State: #{context[:current_state]}"
      end
      
      if context[:available_tools]
        prompt_parts << "Available Tools: #{context[:available_tools].join(', ')}"
      end
      
      prompt_parts.join("\n\n")
    end
    
    def planning_options
      {
        temperature: @options[:temperature] || 0.3,
        max_tokens: @options[:max_tokens] || 4000,
        response_format: { type: "json_object" }
      }.compact
    end
    
    def parse_plan(response)
      plan = JSON.parse(response)
      
      # Validate required fields
      required_fields = %w[objective phases]
      missing_fields = required_fields - plan.keys
      
      if missing_fields.any?
        raise "Invalid plan: missing fields #{missing_fields.join(', ')}"
      end
      
      # Add metadata
      plan['created_at'] = Time.now.to_s
      plan['status'] = 'pending'
      plan['version'] = 1
      
      plan
    rescue JSON::ParserError => e
      puts "Failed to parse plan: #{e.message}"
      puts "Response: #{response}"
      
      # Fallback to simple plan
      {
        'objective' => extract_objective(response),
        'phases' => extract_phases(response),
        'created_at' => Time.now.to_s,
        'status' => 'pending',
        'version' => 1
      }
    end
    
    def extract_objective(text)
      # Simple extraction logic
      if match = text.match(/objective[:\s]+([^\n]+)/i)
        match[1].strip
      else
        text.lines.first.strip
      end
    end
    
    def extract_phases(text)
      phases = []
      current_phase = nil
      
      text.lines.each do |line|
        if line.match?(/^(phase|step)\s*\d+/i)
          if current_phase
            phases << current_phase
          end
          current_phase = {
            'name' => line.strip,
            'tasks' => []
          }
        elsif current_phase && line.strip.length > 0
          current_phase['tasks'] << {
            'name' => line.strip,
            'description' => line.strip
          }
        end
      end
      
      phases << current_phase if current_phase
      phases
    end
  end
  
  # Plan execution tracker
  class PlanExecutor
    attr_reader :plan, :execution_log
    
    def initialize(plan, squad_manager)
      @plan = plan
      @squad_manager = squad_manager
      @execution_log = []
      @task_status = {}
    end
    
    def execute
      puts "🚀 Executing plan: #{@plan['objective']}"
      
      @plan['phases'].each_with_index do |phase, phase_index|
        execute_phase(phase, phase_index)
      end
      
      finalize_execution
    end
    
    def execute_phase(phase, index)
      log_event("Starting phase #{index + 1}: #{phase['name']}")
      
      # Group tasks by parallelization
      parallel_groups = group_parallel_tasks(phase['tasks'])
      
      parallel_groups.each do |group|
        if group.length == 1
          execute_task(group.first)
        else
          execute_parallel_tasks(group)
        end
      end
    end
    
    private
    
    def group_parallel_tasks(tasks)
      groups = []
      current_group = []
      
      tasks.each do |task|
        if task['parallel'] && dependencies_met?(task)
          current_group << task
        else
          groups << current_group if current_group.any?
          groups << [task]
          current_group = []
        end
      end
      
      groups << current_group if current_group.any?
      groups
    end
    
    def dependencies_met?(task)
      return true unless task['dependencies']
      
      task['dependencies'].all? do |dep_id|
        @task_status[dep_id] == :completed
      end
    end
    
    def execute_task(task)
      log_event("Executing task: #{task['name']}")
      
      begin
        # Get appropriate agent for task
        agent = @squad_manager.get_agent_for_task(task)
        
        # Execute with task-specific context
        result = agent.execute(task['description'], {
          task_id: task['id'],
          required_capabilities: task['required_capabilities']
        })
        
        @task_status[task['id']] = :completed
        log_event("Completed task: #{task['name']}", :success, result)
        
        result
      rescue => e
        @task_status[task['id']] = :failed
        log_event("Failed task: #{task['name']}", :error, e.message)
        raise if task['critical']
      end
    end
    
    def execute_parallel_tasks(tasks)
      log_event("Executing #{tasks.length} tasks in parallel")
      
      threads = tasks.map do |task|
        Thread.new { execute_task(task) }
      end
      
      threads.each(&:join)
    end
    
    def log_event(message, level = :info, data = nil)
      @execution_log << {
        timestamp: Time.now,
        level: level,
        message: message,
        data: data
      }
      
      case level
      when :success
        puts "✅ #{message}"
      when :error
        puts "❌ #{message}"
      else
        puts "ℹ️  #{message}"
      end
    end
    
    def finalize_execution
      success_count = @task_status.values.count { |s| s == :completed }
      total_count = @task_status.length
      
      @plan['execution_summary'] = {
        'completed_tasks' => success_count,
        'total_tasks' => total_count,
        'success_rate' => (success_count.to_f / total_count * 100).round(2),
        'execution_time' => calculate_execution_time,
        'status' => success_count == total_count ? 'completed' : 'partial'
      }
      
      log_event("Plan execution finished: #{success_count}/#{total_count} tasks completed")
    end
    
    def calculate_execution_time
      return 0 unless @execution_log.any?
      
      start_time = @execution_log.first[:timestamp]
      end_time = @execution_log.last[:timestamp]
      
      (end_time - start_time).round(2)
    end
  end
end