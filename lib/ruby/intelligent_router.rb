require 'json'
require 'yaml'

module Lantae
  class IntelligentRouter
    attr_reader :routing_config, :provider_manager
    
    PROMPT_TYPES = {
      instruction: /^(create|build|implement|write|develop|design|fix|refactor|add|remove|update|modify|generate|setup|configure|deploy)/i,
      planning_required: /^(plan|architect|design system|implement feature|build application|develop solution)/i,
      question: /^(what|how|why|when|where|who|is|are|can|could|should|explain|describe)/i,
      analysis: /^(analyze|review|evaluate|assess|check|inspect|debug|test)/i,
      simple_task: /^(show|list|display|get|find|search)/i
    }.freeze
    
    DEFAULT_ROUTING_CONFIG = {
      # Classifier stage - determines prompt type
      classifier: {
        provider: 'ollama',
        model: 'llama3:latest',  # Fast model for classification
        temperature: 0.1,
        max_tokens: 100
      },
      
      # Planning stage - creates detailed plans
      planning: {
        provider: 'anthropic',
        model: 'claude-3-5-sonnet-20241022',
        temperature: 0.3,
        max_tokens: 4000
      },
      
      # Squad coordination - manages agent teams
      squad: {
        provider: 'anthropic', 
        model: 'claude-3-5-sonnet-20241022',
        temperature: 0.2,
        max_tokens: 2000
      },
      
      # Implementation agents - do the actual work
      implementation: {
        provider: 'anthropic',
        model: 'claude-3-5-sonnet-20241022',
        temperature: 0.4,
        max_tokens: 8000
      },
      
      # Quick answers - for simple questions
      quick_answer: {
        provider: 'ollama',
        model: 'cogito:latest',  # Large context, fast model
        temperature: 0.7,
        max_tokens: 2000
      },
      
      # Analysis tasks
      analysis: {
        provider: 'openai',
        model: 'gpt-4-turbo',
        temperature: 0.2,
        max_tokens: 4000
      }
    }.freeze
    
    def initialize(provider_manager, config_file = nil)
      @provider_manager = provider_manager
      @routing_config = load_config(config_file)
      @routing_history = []
    end
    
    def route_prompt(prompt, context = {})
      # Record routing decision
      routing_decision = {
        prompt: prompt,
        timestamp: Time.now,
        context: context
      }
      
      # Classify the prompt
      prompt_type = classify_prompt(prompt)
      routing_decision[:prompt_type] = prompt_type
      
      # Determine routing path
      case prompt_type
      when :planning_instruction
        routing_decision[:path] = :full_planning_pipeline
        result = route_through_planning_pipeline(prompt, context)
        
      when :complex_instruction
        routing_decision[:path] = :squad_deployment
        result = route_through_squad(prompt, context)
        
      when :simple_instruction
        routing_decision[:path] = :direct_implementation
        result = route_to_implementation(prompt, context)
        
      when :question
        routing_decision[:path] = :quick_answer
        result = route_to_quick_answer(prompt, context)
        
      when :analysis_request
        routing_decision[:path] = :analysis
        result = route_to_analysis(prompt, context)
        
      else
        routing_decision[:path] = :default
        result = route_to_default(prompt, context)
      end
      
      routing_decision[:result] = result
      @routing_history << routing_decision
      
      result
    end
    
    def classify_prompt(prompt)
      # Quick pattern matching first
      if prompt.match?(PROMPT_TYPES[:planning_required])
        return :planning_instruction
      elsif prompt.match?(PROMPT_TYPES[:instruction]) && complex_instruction?(prompt)
        return :complex_instruction
      elsif prompt.match?(PROMPT_TYPES[:instruction])
        return :simple_instruction
      elsif prompt.match?(PROMPT_TYPES[:question])
        return :question
      elsif prompt.match?(PROMPT_TYPES[:analysis])
        return :analysis_request
      end
      
      # Use AI classifier for ambiguous cases
      classify_with_ai(prompt)
    end
    
    private
    
    def load_config(config_file)
      if config_file && File.exist?(config_file)
        config = YAML.load_file(config_file)
        DEFAULT_ROUTING_CONFIG.deep_merge(config)
      else
        DEFAULT_ROUTING_CONFIG.dup
      end
    end
    
    def complex_instruction?(prompt)
      # Heuristics for complexity
      prompt.split(/[.!?]/).length > 2 ||  # Multiple sentences
      prompt.match?(/\band\b.*\band\b/i) || # Multiple ANDs
      prompt.match?(/step|stage|phase/i) || # Multi-step indicator
      prompt.length > 200                    # Long prompts
    end
    
    def classify_with_ai(prompt)
      with_provider_config(:classifier) do
        classification_prompt = <<~PROMPT
          Classify this user prompt into ONE of these categories:
          - planning_instruction: Requires breaking down into detailed steps and architecture
          - complex_instruction: Needs multiple agents or tools but not full planning
          - simple_instruction: Can be done with a single tool or action
          - question: Asking for information, no action needed
          - analysis_request: Needs code review, debugging, or evaluation
          
          Prompt: "#{prompt}"
          
          Respond with just the category name.
        PROMPT
        
        response = @provider_manager.chat([
          { role: 'user', content: classification_prompt }
        ], { temperature: 0.1, max_tokens: 50 })
        
        response.strip.downcase.to_sym
      end
    rescue => e
      puts "Classification error: #{e.message}, defaulting to simple_instruction"
      :simple_instruction
    end
    
    def route_through_planning_pipeline(prompt, context)
      puts "🎯 Routing through full planning pipeline..."
      
      # Step 1: Generate comprehensive plan
      plan = with_provider_config(:planning) do
        planner = Lantae::PlanningAgent.new(@provider_manager)
        planner.create_plan(prompt, context)
      end
      
      # For now, return the plan as the response
      # TODO: Integrate with squad system when available
      format_plan_response(plan)
    end
    
    def format_plan_response(plan)
      response = []
      response << "## Plan: #{plan['objective']}"
      response << ""
      
      if plan['success_criteria']
        response << "### Success Criteria:"
        plan['success_criteria'].each { |c| response << "- #{c}" }
        response << ""
      end
      
      response << "### Phases:"
      plan['phases'].each_with_index do |phase, i|
        response << "\n#### Phase #{i + 1}: #{phase['name']}"
        response << phase['description'] if phase['description']
        
        if phase['tasks'] && phase['tasks'].any?
          response << "\nTasks:"
          phase['tasks'].each do |task|
            response << "- #{task['name']}"
            response << "  #{task['description']}" if task['description'] && task['description'] != task['name']
          end
        end
      end
      
      if plan['risks'] && plan['risks'].any?
        response << "\n### Risks:"
        plan['risks'].each do |risk|
          response << "- **#{risk['description']}** (#{risk['probability']} probability, #{risk['impact']} impact)"
          response << "  Mitigation: #{risk['mitigation']}" if risk['mitigation']
        end
      end
      
      response.join("\n")
    end
    
    def route_through_squad(prompt, context)
      puts "👥 Routing through squad deployment..."
      
      # For now, use the implementation provider directly
      # TODO: Integrate with squad system when available
      with_provider_config(:squad) do
        @provider_manager.chat([
          { role: 'system', content: 'You are managing a team of AI agents. Break down this task and coordinate execution.' },
          { role: 'user', content: prompt }
        ], context)
      end
    end
    
    def route_to_implementation(prompt, context)
      puts "🔧 Direct implementation routing..."
      
      with_provider_config(:implementation) do
        @provider_manager.chat([
          { role: 'system', content: 'You are an implementation agent. Execute this task directly and efficiently.' },
          { role: 'user', content: prompt }
        ], context)
      end
    end
    
    def route_to_quick_answer(prompt, context)
      puts "💬 Quick answer routing..."
      
      with_provider_config(:quick_answer) do
        @provider_manager.chat([
          { role: 'user', content: prompt }
        ], context)
      end
    end
    
    def route_to_analysis(prompt, context)
      puts "🔍 Analysis routing..."
      
      with_provider_config(:analysis) do
        @provider_manager.chat([
          { role: 'system', content: 'You are an analysis agent specializing in code review, debugging, and evaluation. Provide thorough analysis.' },
          { role: 'user', content: prompt }
        ], context)
      end
    end
    
    def route_to_default(prompt, context)
      puts "📝 Default routing..."
      
      # Use current provider settings
      @provider_manager.chat([
        { role: 'user', content: prompt }
      ], context)
    end
    
    def with_provider_config(stage)
      config = @routing_config[stage]
      return yield unless config
      
      # Save current provider state
      original_provider = @provider_manager.get_provider_info
      
      # Switch to stage-specific provider/model
      @provider_manager.switch_provider(config[:provider], config[:model])
      
      # Execute with stage-specific settings
      stage_options = {
        temperature: config[:temperature],
        max_tokens: config[:max_tokens]
      }.compact
      
      result = yield
      
      # Restore original provider
      @provider_manager.switch_provider(
        original_provider[:provider], 
        original_provider[:model]
      )
      
      result
    end
    
    def save_routing_history(file_path)
      File.write(file_path, JSON.pretty_generate(@routing_history))
    end
    
    def get_routing_stats
      stats = {
        total_routes: @routing_history.length,
        by_type: Hash.new(0),
        by_path: Hash.new(0),
        avg_response_time: 0
      }
      
      total_time = 0
      @routing_history.each do |record|
        stats[:by_type][record[:prompt_type]] += 1
        stats[:by_path][record[:path]] += 1
        if record[:response_time]
          total_time += record[:response_time]
        end
      end
      
      stats[:avg_response_time] = total_time / @routing_history.length if @routing_history.any?
      stats
    end
  end
  
  # Configuration manager for routing rules
  class RoutingConfigManager
    DEFAULT_CONFIG_PATH = File.expand_path('~/.lantae/routing_config.yml')
    
    def self.load_or_create_config
      FileUtils.mkdir_p(File.dirname(DEFAULT_CONFIG_PATH))
      
      unless File.exist?(DEFAULT_CONFIG_PATH)
        save_default_config
      end
      
      YAML.load_file(DEFAULT_CONFIG_PATH)
    end
    
    def self.save_default_config
      File.write(DEFAULT_CONFIG_PATH, YAML.dump(IntelligentRouter::DEFAULT_ROUTING_CONFIG))
      puts "Created default routing config at: #{DEFAULT_CONFIG_PATH}"
    end
    
    def self.update_stage_config(stage, provider: nil, model: nil, temperature: nil, max_tokens: nil)
      config = load_or_create_config
      
      config[stage] ||= {}
      config[stage][:provider] = provider if provider
      config[stage][:model] = model if model
      config[stage][:temperature] = temperature if temperature
      config[stage][:max_tokens] = max_tokens if max_tokens
      
      File.write(DEFAULT_CONFIG_PATH, YAML.dump(config))
      puts "Updated routing config for stage: #{stage}"
    end
    
    def self.show_config
      config = load_or_create_config
      puts "Current Routing Configuration:"
      puts "=" * 50
      
      config.each do |stage, settings|
        puts "\n#{stage.to_s.upcase}:"
        settings.each do |key, value|
          puts "  #{key}: #{value}"
        end
      end
    end
  end
end