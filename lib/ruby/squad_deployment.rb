require 'json'
require 'logger'
require 'fileutils'
require_relative 'provider_manager'
require_relative 'tool_manager'
require_relative 'task_analyzer'
require_relative 'execution_engine'

module Lantae
  class SquadDeployment
    attr_reader :name, :members, :tasks, :status
    
    def initialize(name, options = {})
      @name = name
      @members = []
      @tasks = []
      @status = :initialized
      @logger = options[:logger] || Logger.new(STDOUT)
      @provider_manager = options[:provider_manager]
      @tool_manager = options[:tool_manager]
      @task_analyzer = options[:task_analyzer] || TaskAnalyzer.new
      @execution_engine = options[:execution_engine]
    end
    
    def add_member(member_config)
      member = {
        id: "member_#{@members.size + 1}",
        name: member_config[:name],
        role: member_config[:role] || :executor,
        capabilities: member_config[:capabilities] || [],
        model: member_config[:model] || 'cogito:latest',
        provider: member_config[:provider] || 'ollama',
        status: :ready
      }
      
      @members << member
      @logger.info "Added squad member: #{member[:name]} (#{member[:role]})"
      member
    end
    
    def assign_task(task_description, options = {})
      task = {
        id: "task_#{@tasks.size + 1}",
        description: task_description,
        assigned_to: options[:assigned_to],
        priority: options[:priority] || :normal,
        status: :pending,
        created_at: Time.now
      }
      
      # Auto-assign if no specific member specified
      if task[:assigned_to].nil?
        task[:assigned_to] = select_best_member_for_task(task_description)
      end
      
      @tasks << task
      @logger.info "Assigned task to #{task[:assigned_to][:name]}: #{task_description}"
      task
    end
    
    def deploy
      @status = :deploying
      @logger.info "Deploying squad: #{@name}"
      
      begin
        validate_deployment
        prepare_members
        execute_tasks
        @status = :deployed
        @logger.info "Squad deployment successful"
        true
      rescue => e
        @status = :failed
        @logger.error "Squad deployment failed: #{e.message}"
        raise
      end
    end
    
    def get_status_report
      {
        squad_name: @name,
        status: @status,
        members: @members.map { |m| { name: m[:name], role: m[:role], status: m[:status] } },
        tasks: @tasks.map { |t| { 
          id: t[:id], 
          description: t[:description][0..50] + '...', 
          assigned_to: t[:assigned_to][:name],
          status: t[:status] 
        } },
        completion_rate: calculate_completion_rate
      }
    end
    
    private
    
    def validate_deployment
      raise "No members in squad" if @members.empty?
      raise "No tasks assigned" if @tasks.empty?
      raise "Provider manager not configured" unless @provider_manager
      raise "Tool manager not configured" unless @tool_manager
    end
    
    def prepare_members
      @members.each do |member|
        # Initialize member's execution context
        member[:execution_context] = {
          provider: member[:provider],
          model: member[:model],
          capabilities: member[:capabilities]
        }
        member[:status] = :prepared
      end
    end
    
    def execute_tasks
      @tasks.each do |task|
        execute_single_task(task)
      end
    end
    
    def execute_single_task(task)
      member = task[:assigned_to]
      @logger.info "Member #{member[:name]} executing: #{task[:description]}"
      
      task[:status] = :in_progress
      member[:status] = :busy
      
      begin
        # Switch to member's preferred provider/model
        @provider_manager.switch_provider(member[:provider], member[:model])
        
        # Execute task based on member role
        case member[:role]
        when :planner
          result = plan_task(task[:description])
        when :executor
          result = execute_task(task[:description])
        when :reviewer
          result = review_task(task[:description])
        else
          result = execute_task(task[:description])
        end
        
        task[:result] = result
        task[:status] = :completed
        task[:completed_at] = Time.now
        
      rescue => e
        task[:status] = :failed
        task[:error] = e.message
        @logger.error "Task failed: #{e.message}"
      ensure
        member[:status] = :ready
      end
    end
    
    def select_best_member_for_task(task_description)
      # Analyze task to determine best member
      task_analysis = @task_analyzer.analyze_prompt(task_description)
      
      # Find member with matching capabilities
      best_member = @members.find do |member|
        # Match based on role and capabilities
        if task_analysis[:requires_planning] && member[:role] == :planner
          true
        elsif task_analysis[:technical_complexity] > 0.7 && member[:capabilities].include?(:advanced_coding)
          true
        elsif member[:status] == :ready
          true
        else
          false
        end
      end
      
      best_member || @members.first
    end
    
    def plan_task(description)
      messages = [
        { role: 'system', content: 'You are a planning specialist. Break down tasks into clear steps.' },
        { role: 'user', content: "Create a plan for: #{description}" }
      ]
      
      @provider_manager.chat(messages)
    end
    
    def execute_task(description)
      if @execution_engine
        task = OpenStruct.new(description: description, subtasks: [])
        @execution_engine.execute_task(task)
      else
        messages = [
          { role: 'system', content: 'You are an execution specialist. Complete tasks efficiently.' },
          { role: 'user', content: "Execute: #{description}" }
        ]
        
        @provider_manager.chat(messages)
      end
    end
    
    def review_task(description)
      messages = [
        { role: 'system', content: 'You are a code reviewer. Review implementations for quality and correctness.' },
        { role: 'user', content: "Review: #{description}" }
      ]
      
      @provider_manager.chat(messages)
    end
    
    def calculate_completion_rate
      return 0.0 if @tasks.empty?
      
      completed = @tasks.count { |t| t[:status] == :completed }
      (completed.to_f / @tasks.size * 100).round(2)
    end
  end
end