require 'thread'
require 'readline'
require 'time'
require_relative 'ui_components'
require_relative 'side_panel_manager'
require_relative 'direct_authenticator'
require_relative 'planning_agent'
require_relative 'task_analyzer'
require_relative 'workspace_authenticator'
require_relative 'plan_visualizer'

module Lantae
  class AsyncREPL
    def initialize(provider_manager, tool_manager, options = {})
      @provider_manager = provider_manager
      @tool_manager = tool_manager
      @options = options
      @conversation = []
      
      # Threading and command management
      @command_mutex = Mutex.new
      @output_mutex = Mutex.new
      @commands = {}
      @next_command_id = 1
      @command_threads = []
      @running = true
      
      # UI settings
      @terminal_width = `tput cols`.to_i rescue 80
      @show_banner = !options[:no_banner]
      
      # Extra managers
      @extra_managers = {}
      
      # Planning components
      @task_analyzer = TaskAnalyzer.new
      @planning_agent = PlanningAgent.new(provider_manager, options)
      @planning_threshold = options[:planning_threshold] || 3.0
    end
    
    def set_extra_managers(managers)
      @extra_managers = managers
    end
    
    def start
      print_header if @show_banner
      
      # Check for API key on startup
      check_api_key_availability
      
      # Set up autocomplete
      setup_autocomplete
      
      # Main input loop
      loop do
        begin
          # Show prompt with active command count
          active_count = @commands.select { |_, cmd| cmd[:status] == :running }.size
          prompt = active_count > 0 ? "> [#{active_count} active] " : "> "
          
          input = Readline.readline(prompt, true)
          break if input.nil? || ['exit', 'quit'].include?(input.strip)
          
          input = input.strip
          next if input.empty?
          
          # Submit command for processing
          submit_command(input)
          
        rescue Interrupt
          puts "\n\nGoodbye!"
          break
        rescue => e
          puts "Error: #{e.message}"
        end
      end
      
      cleanup
    end
    
    private
    
    def check_api_key_availability
      provider_info = @provider_manager.get_provider_info
      provider = provider_info[:provider]
      
      # Skip for local providers
      return if provider == 'ollama'
      
      # Load workspace keys if in a workspace
      if @options[:workspace]
        Lantae::WorkspaceAuthenticator.load_workspace_keys(@options[:workspace])
      end
      
      # Check if API key exists
      if !has_api_key_for_provider?(provider)
        puts "\n#{"\e[33m"}No API key found for #{provider.capitalize}.#{"\e[0m"}"
        
        if @options[:workspace]
          puts "Workspace: #{@options[:workspace]}"
        end
        
        puts "Would you like to set it up now? (y/n): "
        
        response = gets&.chomp&.downcase
        if response == 'y' || response == 'yes'
          api_key = Lantae::APIKeyAuthorizer.request_api_key(provider)
          unless api_key
            puts "\n#{"\e[31m"}Warning: No API key configured. Commands will fail.#{"\e[0m"}"
            puts "You can set it up later by:"
            puts "  1. Using: /login #{provider}"
            puts "  2. Running: export #{provider.upcase}_API_KEY='your-api-key'"
            puts "  3. Or creating ~/.lantae_env with: #{provider.upcase}_API_KEY=your-api-key\n"
          end
        else
          puts "\n#{"\e[90m"}Skipping API key setup. You can set it up later.#{"\e[0m"}\n"
        end
      end
    end
    
    def print_header
      # First show the main banner
      print_banner unless @options[:no_banner]
      
      # Then show async mode info
      UIComponents.draw_panel(
        "Lantae - Async Mode",
        [
          "Multiple commands can run concurrently",
          "Commands: /status, /cancel <id>, /clear, /help",
          "",
          "Provider: #{"\e[93m"}#{@provider_manager.get_provider_info[:provider]}#{"\e[0m"} | Model: #{"\e[92m"}#{@provider_manager.get_provider_info[:model]}#{"\e[0m"}"
        ],
        width: UIComponents.terminal_width,
        color: "\e[96m"
      )
      puts
    end
    
    def print_banner
      # Show a random banner
      banner_type = rand(14) # 7 ships + 7 cats
      
      case banner_type
      when 0..6
        # Ship banners
        print_ship_banner(banner_type)
      else
        # Cat banners
        print_cat_banner(banner_type - 7)
      end
    end
    
    def print_ship_banner(type)
      puts <<~BANNER
        #{"\e[96m"}
      ╔════════════════════════════════════════════════════════════════════════════════════════════════════════════╗
      ║  #{"\e[95m"}██╗      █████╗ ███╗   ██╗████████╗ █████╗ ███████╗#{"\e[96m"}                                             ╱▔▔▔╲  ║
      ║  #{"\e[95m"}██║     ██╔══██╗████╗  ██║╚══██╔══╝██╔══██╗██╔════╝#{"\e[96m"}                                            │ ◯◯ │  ║
      ║  #{"\e[95m"}██║     ███████║██╔██╗ ██║   ██║   ███████║█████╗#{"\e[96m"}                                          ╔═══╧════╧═══╗║
      ║  #{"\e[95m"}██║     ██╔══██║██║╚██╗██║   ██║   ██╔══██║██╔══╝#{"\e[96m"}                                          ║ CONTAINER  ║║
      ║  #{"\e[95m"}███████╗██║  ██║██║ ╚████║   ██║   ██║  ██║███████╗#{"\e[96m"}                                 ═══════╬═══════════╬║
      ║  #{"\e[95m"}╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝   ╚═╝   ╚═╝  ╚═╝╚══════╝#{"\e[96m"}                                 ≈≈≈≈≈≈≈╲▄▄▄▄▄▄▄▄▄╱║
      ║                                                                                         ≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈║
      ║  #{"\e[93m"}🚀 Multi-Provider LLM Interface v1.0.0#{"\e[96m"}                                                ≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈║
      ║  #{"\e[92m"}⚡ Powered by Cogito Reasoning Model#{"\e[96m"}                                                  ≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈║
      ║  #{"\e[94m"}🔗 Ollama • OpenAI • Anthropic • Bedrock & More#{"\e[96m"}                                     ≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈║
      ╚════════════════════════════════════════════════════════════════════════════════════════════════════════════╝
        #{"\e[90m"}by thelastmerrymaker | thelastmerrymaker.com#{"\e[0m"}
      BANNER
    end
    
    def print_cat_banner(type)
      puts <<~BANNER
        #{"\e[96m"}
      ╔════════════════════════════════════════════════════════════════════════════════════════════════════════════╗
      ║  #{"\e[95m"}██╗      █████╗ ███╗   ██╗████████╗ █████╗ ███████╗#{"\e[96m"}                                        ╱╲___╱╲    ║
      ║  #{"\e[95m"}██║     ██╔══██╗████╗  ██║╚══██╔══╝██╔══██╗██╔════╝#{"\e[96m"}                                       (  o.o  )   ║
      ║  #{"\e[95m"}██║     ███████║██╔██╗ ██║   ██║   ███████║█████╗#{"\e[96m"}                                          (  >^<  )   ║
      ║  #{"\e[95m"}██║     ██╔══██║██║╚██╗██║   ██║   ██╔══██║██╔══╝#{"\e[96m"}                                         ╱|      |╲  ║
      ║  #{"\e[95m"}███████╗██║  ██║██║ ╚████║   ██║   ██║  ██║███████╗#{"\e[96m"}                                       (_|  ~~  |_) ║
      ║  #{"\e[95m"}╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝   ╚═╝   ╚═╝  ╚═╝╚══════╝#{"\e[96m"}                                                    ║
      ║                                                                                                             ║
      ║  #{"\e[93m"}🚀 Multi-Provider LLM Interface v1.0.0#{"\e[96m"}                                                                   ║
      ║  #{"\e[92m"}⚡ Powered by Cogito Reasoning Model#{"\e[96m"}                                                                      ║
      ║  #{"\e[94m"}🔗 Ollama • OpenAI • Anthropic • Bedrock & More#{"\e[96m"}                                                         ║
      ╚════════════════════════════════════════════════════════════════════════════════════════════════════════════╝
        #{"\e[90m"}by thelastmerrymaker | thelastmerrymaker.com#{"\e[0m"}
      BANNER
    end
    
    def submit_command(input)
      command_id = nil
      
      @command_mutex.synchronize do
        command_id = @next_command_id
        @next_command_id += 1
        
        # Capture current provider/model configuration
        provider_info = @provider_manager.get_provider_info
        
        @commands[command_id] = {
          id: command_id,
          input: input,
          status: :queued,
          provider: provider_info[:provider],
          model: provider_info[:model],
          submitted_at: Time.now,
          output: []
        }
      end
      
      # Show submission confirmation
      puts "\n#{"\e[94m"}[#{command_id}] Submitted:#{"\e[0m"} #{input}"
      puts "#{"\e[90m"}    Using: #{@commands[command_id][:provider]}/#{@commands[command_id][:model]}#{"\e[0m"}"
      
      # Start processing thread
      thread = Thread.new(command_id) do |cmd_id|
        process_command(cmd_id)
      end
      
      @command_threads << thread
      @command_threads.delete_if { |t| !t.alive? }
    end
    
    def process_command(command_id)
      command = nil
      
      @command_mutex.synchronize do
        command = @commands[command_id]
        command[:status] = :running
        command[:started_at] = Time.now
      end
      
      begin
        if command[:input].start_with?('/')
          process_slash_command(command_id)
        else
          process_ai_command(command_id)
        end
        
        @command_mutex.synchronize do
          command[:status] = :completed
          command[:completed_at] = Time.now
        end
        
        show_completion(command_id)
        
      rescue => e
        @command_mutex.synchronize do
          command[:status] = :failed
          command[:error] = e.message
          command[:completed_at] = Time.now
        end
        
        @output_mutex.synchronize do
          puts "\n#{"\e[31m"}[#{command_id}] Failed:#{"\e[0m"} #{e.message}"
        end
      end
    end
    
    def process_slash_command(command_id)
      command = @commands[command_id]
      input = command[:input]
      
      parts = input[1..-1].split(' ')
      cmd = parts[0]
      args = parts[1..-1].join(' ')
      
      case cmd
      when 'status'
        show_status
        
      when 'cancel'
        if args =~ /^\d+$/
          cancel_command(args.to_i)
        else
          add_command_output(command_id, "Usage: /cancel <command-id>")
        end
        
      when 'clear'
        system('clear') || system('cls')
        print_header
        
      when 'model'
        if args.empty?
          add_command_output(command_id, "Current model: #{@provider_manager.get_provider_info[:model]}")
        else
          # Update the main provider manager for future commands
          @provider_manager.current_model = args
          add_command_output(command_id, "Future commands will use: #{args}")
          
          # Update options for side panel
          @options[:model] = args
        end
        
      when 'provider'
        if args.empty?
          add_command_output(command_id, "Current provider: #{@provider_manager.get_provider_info[:provider]}")
        else
          provider, model = args.split(' ', 2)
          
          # Check if provider needs API key authentication
          if provider != 'ollama' && !has_api_key_for_provider?(provider)
            add_command_output(command_id, "⚠️  Provider '#{provider}' requires API key setup.")
            add_command_output(command_id, "Please use: /login #{provider}")
          else
            @provider_manager.switch_provider(provider, model)
            info = @provider_manager.get_provider_info
            add_command_output(command_id, "Future commands will use: #{info[:provider]} (#{info[:model]})")
            
            # Update the options to reflect the change for side panel
            @options[:provider] = info[:provider]
            @options[:model] = info[:model]
          end
        end
        
      when 'help'
        help_text = <<~HELP
          Async Mode Commands:
            /status              - Show all active and recent commands
            /cancel <id>         - Cancel a running command
            /clear               - Clear the screen
            /model <name>        - Switch model for future commands
            /provider <name>     - Switch provider for future commands
            /models              - List available models
            /side                - Toggle side panel display
            /login [provider]    - Authenticate with provider (like Claude Code)
            /workspace [name]    - Switch to workspace authentication context
            /plan <task>         - Create execution plan for complex task
            /execute <id>        - Execute a saved plan
            /plans               - List saved plans
            /graph <id>          - Show ASCII graph for a plan
            /auto                - Toggle automatic planning for complex tasks
            /help                - Show this help
            
          Regular Commands:
            - Type any question or request to send to the AI
            - Commands run in the background, prompt remains available
            - Each command uses the model configured when submitted
        HELP
        add_command_output(command_id, help_text)
        
      when 'models'
        begin
          models = @provider_manager.list_models
          output = "Available models for #{@provider_manager.get_provider_info[:provider]}:\n"
          models.each { |model| output += "  - #{model}\n" }
          add_command_output(command_id, output)
        rescue => e
          add_command_output(command_id, "Error listing models: #{e.message}")
        end
        
      when 'side'
        @options[:side_panel] = !@options[:side_panel]
        status = @options[:side_panel] ? "enabled" : "disabled"
        add_command_output(command_id, "Side panel #{status}")
        
      when 'login'
        handle_login_command(command_id, args)
        
      when 'workspace'
        handle_workspace_command(command_id, args)
        
      when 'plan'
        handle_plan_command(command_id, args)
        
      when 'execute'
        handle_execute_command(command_id, args)
        
      when 'plans'
        list_saved_plans(command_id)
        
      when 'graph'
        show_plan_graph(command_id, args)
        
      when 'auto'
        toggle_auto_planning(command_id)
        
      else
        # Command not handled by async REPL
        add_command_output(command_id, "Unknown command: /#{cmd}. Use /help for available commands.")
      end
    end
    
    def process_ai_command(command_id)
      command = @commands[command_id]
      
      # Analyze task complexity if auto-planning is enabled
      if @options[:auto_planning] && should_use_planning?(command[:input])
        @output_mutex.synchronize do
          puts "\n#{"\e[94m"}[#{command_id}] Task requires planning...#{"\e[0m"}"
        end
        process_planned_command(command_id)
        return
      end
      
      # Create conversation context for this command
      command_conversation = @conversation.dup
      command_conversation << { role: 'user', content: command[:input] }
      
      # Show processing status
      @output_mutex.synchronize do
        puts "\n#{"\e[33m"}[#{command_id}] Processing...#{"\e[0m"}"
      end
      
      # Use a clone of provider manager with captured settings
      provider_clone = create_provider_clone(command[:provider], command[:model])
      
      begin
        # Get response from AI
        response = provider_clone.chat(command_conversation, @options)
        
        # Add to conversation history
        @conversation << { role: 'user', content: command[:input] }
        @conversation << { role: 'assistant', content: response }
        
        # Format and store output
        formatted_response = Lantae::ResponseFormatter.format_response(response, 
          boxed: false,
          markdown: true
        )
        
        # Add side panel if enabled
        if @options[:side_panel]
          # Use the current command's provider and model for accurate display
          current_info = @provider_manager.get_provider_info
          side_content = Lantae::SidePanelManager.generate_side_content(
            provider: command[:provider] || current_info[:provider],
            model: command[:model] || current_info[:model],
            temperature: @options[:temperature],
            conversation: @conversation,
            tools_available: @tool_manager&.list_available_tools || []
          )
          
          formatted_response = Lantae::ResponseFormatter.with_side_panel(formatted_response, side_content)
        end
        
        add_command_output(command_id, formatted_response)
      rescue => e
        # For API key errors, provide clear guidance
        if e.message.include?('API key is required') || e.message.include?('API key found')
          @output_mutex.synchronize do
            puts "\n#{"\e[33m"}[#{command_id}] API key required for #{command[:provider].capitalize}#{"\e[0m"}"
            puts "\nTo set up your API key:"
            puts "1. Exit this session (type 'exit')"
            puts "2. Run: export #{command[:provider].upcase}_API_KEY='your-api-key'"
            puts "3. Or create ~/.lantae_env with: #{command[:provider].upcase}_API_KEY=your-api-key"
            puts "4. Restart lantae"
            puts "\nAlternatively, the browser-based setup will trigger on next startup."
          end
          
          # Mark command as failed with specific error
          @command_mutex.synchronize do
            command[:needs_api_key] = true
            command[:error] = "API key required"
          end
        else
          # Re-raise other errors
          raise
        end
      end
    end
    
    def create_provider_clone(provider, model)
      # Create a new provider manager instance with specific settings
      clone = @provider_manager.dup
      clone.switch_provider(provider, model)
      clone
    end
    
    def add_command_output(command_id, text)
      @command_mutex.synchronize do
        @commands[command_id][:output] << text
      end
    end
    
    def show_completion(command_id)
      command = @commands[command_id]
      duration = command[:completed_at] - command[:started_at]
      
      @output_mutex.synchronize do
        puts
        
        # Create status lines for the panel
        status_lines = UIComponents.draw_command_status(
          command_id,
          :completed,
          command[:input],
          provider: command[:provider],
          model: command[:model],
          elapsed: duration.round(1)
        )
        
        # Combine status and output
        panel_content = status_lines + [""] + command[:output].flat_map { |o| o.split("\n") }
        
        # Draw the panel
        UIComponents.draw_panel(
          "Command #{command_id} - Completed",
          panel_content,
          width: UIComponents.terminal_width,
          color: "\e[32m"
        )
        
        puts
      end
    end
    
    def show_status
      @output_mutex.synchronize do
        puts
        
        status_lines = []
        
        if @commands.empty?
          status_lines << "No commands submitted yet"
        else
          # Group by status
          running = @commands.select { |_, cmd| cmd[:status] == :running }
          queued = @commands.select { |_, cmd| cmd[:status] == :queued }
          recent = @commands.select { |_, cmd| [:completed, :failed].include?(cmd[:status]) }
                            .sort_by { |_, cmd| cmd[:completed_at] || Time.now }
                            .last(5)
          
          if running.any?
            status_lines << "#{"\e[33m"}Running:#{"\e[0m"}"
            running.each do |id, cmd|
              elapsed = Time.now - cmd[:started_at]
              lines = UIComponents.draw_command_status(id, :running, cmd[:input], 
                provider: cmd[:provider], model: cmd[:model], elapsed: elapsed.round)
              lines.each { |line| status_lines << "  #{line}" }
              status_lines << ""
            end
          end
          
          if queued.any?
            status_lines << "#{"\e[94m"}Queued:#{"\e[0m"}"
            queued.each do |id, cmd|
              lines = UIComponents.draw_command_status(id, :queued, cmd[:input],
                provider: cmd[:provider], model: cmd[:model])
              lines.each { |line| status_lines << "  #{line}" }
              status_lines << ""
            end
          end
          
          if recent.any?
            status_lines << "#{"\e[90m"}Recent:#{"\e[0m"}"
            recent.each do |id, cmd|
              lines = UIComponents.draw_command_status(id, cmd[:status], cmd[:input],
                provider: cmd[:provider], model: cmd[:model])
              lines.each { |line| status_lines << "  #{line}" }
              status_lines << ""
            end
          end
        end
        
        UIComponents.draw_panel(
          "Command Status",
          status_lines,
          width: UIComponents.terminal_width,
          color: "\e[96m"
        )
        
        puts
      end
    end
    
    def cancel_command(command_id)
      # TODO: Implement actual thread cancellation
      @output_mutex.synchronize do
        puts "\n#{"\e[33m"}[#{command_id}] Cancel requested (not yet implemented)#{"\e[0m"}"
      end
    end
    
    def setup_autocomplete
      # Basic autocomplete for slash commands
      commands = %w[status cancel clear model provider models side login workspace plan execute plans graph auto help]
      
      comp = proc do |input|
        if input.start_with?('/')
          cmd = input[1..-1]
          commands.select { |c| c.start_with?(cmd) }.map { |c| "/#{c}" }
        else
          []
        end
      end
      
      Readline.completion_proc = comp
      Readline.completion_append_character = ' '
    rescue
      # Ignore autocomplete setup errors
    end
    
    def handle_login_command(command_id, args)
      provider = args.strip.empty? ? 'anthropic' : args.strip.downcase
      
      # Validate provider
      unless %w[anthropic openai gemini].include?(provider)
        add_command_output(command_id, "❌ Unsupported provider '#{provider}'. Supported: anthropic, openai, gemini")
        return
      end
      
      # Check if already authenticated
      if has_api_key_for_provider?(provider)
        add_command_output(command_id, "✅ Already authenticated with #{provider.capitalize}")
        
        # Load workspace keys if in a workspace
        if @options[:workspace]
          Lantae::WorkspaceAuthenticator.load_workspace_keys(@options[:workspace])
        end
        
        @provider_manager.switch_provider(provider)
        info = @provider_manager.get_provider_info
        add_command_output(command_id, "Switched to: #{info[:provider]} (#{info[:model]})")
        return
      end
      
      add_command_output(command_id, "🔐 Starting #{provider.capitalize} authentication...")
      add_command_output(command_id, "🌐 Opening #{provider.capitalize} Console in browser...")
      add_command_output(command_id, "📋 Please create an API key and copy it")
      add_command_output(command_id, "⏳ Then paste it in the terminal when prompted")
      
      # Mark command as completed since the actual auth happens in terminal
      @command_mutex.synchronize do
        @commands[command_id][:status] = :completed
        @commands[command_id][:completed_at] = Time.now
      end
      
      # Run authentication in main thread to handle terminal input
      Thread.new do
        sleep 0.5  # Let the command output display first
        
        result = Lantae::DirectAuthenticator.login(provider, workspace: @options[:workspace])
        
        if result[:success]
          puts "\n🎉 Switching to #{provider.capitalize}..."
          @provider_manager.switch_provider(provider)
          info = @provider_manager.get_provider_info
          puts "✅ Now using: #{info[:provider]} (#{info[:model]})"
          puts "💬 You can start chatting with #{provider.capitalize}!\n"
        else
          puts "\n❌ Authentication failed: #{result[:error]}"
          puts "💡 You can try again with: /login #{provider}\n"
        end
        
        # Show the prompt again
        print "> "
        $stdout.flush
      end
    end
    
    def has_api_key_for_provider?(provider)
      env_key = "#{provider.upcase}_API_KEY"
      
      # Check workspace auth first
      if @options[:workspace]
        return Lantae::WorkspaceAuthenticator.has_key?(provider, @options[:workspace])
      end
      
      ENV[env_key] || File.exist?(File.expand_path('~/.lantae_env'))
    end
    
    def handle_workspace_command(command_id, args)
      workspace_name = args.strip
      
      if workspace_name.empty?
        # Show current workspace
        current = @options[:workspace] || "default"
        add_command_output(command_id, "Current workspace: #{current}")
        add_command_output(command_id, "Available workspaces: #{Lantae::WorkspaceAuthenticator.list_workspaces.join(', ')}")
      else
        # Switch workspace
        @options[:workspace] = workspace_name
        add_command_output(command_id, "Switched to workspace: #{workspace_name}")
        
        # Check available keys in this workspace
        available_providers = Lantae::WorkspaceAuthenticator.available_providers(workspace_name)
        if available_providers.any?
          add_command_output(command_id, "Available providers: #{available_providers.join(', ')}")
        else
          add_command_output(command_id, "No API keys configured in this workspace yet")
        end
      end
    end
    
    def handle_plan_command(command_id, args)
      if args.strip.empty?
        add_command_output(command_id, "Usage: /plan <task description>")
        return
      end
      
      # Create plan
      plan = @planning_agent.create_plan(args, {
        available_tools: @tool_manager&.list_available_tools || []
      })
      
      # Format plan output with ASCII visualization
      ascii_graph = Lantae::PlanVisualizer.generate_ascii_graph(plan)
      add_command_output(command_id, ascii_graph)
      
      # Store plan for potential execution
      @commands[command_id][:plan] = plan
      plan_id = save_plan(plan, command[:input])
      add_command_output(command_id, "\nPlan saved as ID: #{plan_id}")
      add_command_output(command_id, "To execute this plan, use: /execute #{plan_id}")
    end
    
    def toggle_auto_planning(command_id)
      @options[:auto_planning] = !@options[:auto_planning]
      status = @options[:auto_planning] ? "enabled" : "disabled"
      add_command_output(command_id, "Automatic planning for complex tasks: #{status}")
    end
    
    def should_use_planning?(input)
      complexity = @task_analyzer.assess_complexity(input)
      complexity.score >= @planning_threshold
    end
    
    def process_planned_command(command_id)
      command = @commands[command_id]
      
      # Create plan
      @output_mutex.synchronize do
        puts "#{\"\\e[93m\"}📋 Creating execution plan...#{\"\\e[0m\"}"
      end
      
      plan = @planning_agent.create_plan(command[:input], {
        available_tools: @tool_manager&.list_available_tools || [],
        provider: command[:provider],
        model: command[:model]
      })
      
      # Show plan visualization
      @output_mutex.synchronize do
        puts "#{\"\\e[92m\"}📊 Execution Plan:#{\"\\e[0m\"}"
        puts
        
        # Generate ASCII graph
        ascii_graph = Lantae::PlanVisualizer.generate_ascii_graph(plan)
        puts ascii_graph
        puts
      end
      
      # Execute plan phases
      plan['phases'].each_with_index do |phase, phase_idx|
        @output_mutex.synchronize do
          puts "#{\"\\e[94m\"}Phase #{phase_idx + 1}: #{phase['name']}#{\"\\e[0m\"}"
        end
        
        # Execute tasks in phase
        phase['tasks'].each do |task|
          execute_planned_task(command_id, task, command)
        end
      end
      
      # Mark command as completed
      @command_mutex.synchronize do
        command[:status] = :completed
        command[:completed_at] = Time.now
      end
      
      show_completion(command_id)
    end
    
    def execute_planned_task(command_id, task, command)
      @output_mutex.synchronize do
        puts "  → Executing: #{task['name']}"
      end
      
      # Create conversation for this task
      task_conversation = @conversation.dup
      task_conversation << { role: 'user', content: task['description'] }
      
      # Use appropriate provider for task
      provider_clone = create_provider_clone(command[:provider], command[:model])
      
      begin
        response = provider_clone.chat(task_conversation, @options)
        
        # Add to conversation
        @conversation << { role: 'user', content: task['description'] }
        @conversation << { role: 'assistant', content: response }
        
        @output_mutex.synchronize do
          puts "    ✓ Completed: #{task['name']}"
        end
      rescue => e
        @output_mutex.synchronize do
          puts "    ✗ Failed: #{e.message}"
        end
      end
    end
    
    def format_plan(plan)
      output = []
      output << "📋 Execution Plan"
      output << "━" * 60
      output << "Objective: #{plan['objective']}"
      output << "Duration: #{plan['estimated_duration']}"
      output << ""
      
      plan['phases'].each_with_index do |phase, idx|
        output << "Phase #{idx + 1}: #{phase['name']}"
        output << "  #{phase['description']}"
        output << ""
        
        phase['tasks'].each do |task|
          output << "  • #{task['name']}"
          output << "    #{task['description']}"
          output << "    Duration: #{task['estimated_duration']}"
          if task['dependencies'].any?
            output << "    Dependencies: #{task['dependencies'].join(', ')}"
          end
          output << ""
        end
      end
      
      if plan['risks'] && plan['risks'].any?
        output << "⚠️  Risks:"
        plan['risks'].each do |risk|
          output << "  • #{risk['description']} (#{risk['probability']}/#{risk['impact']})"
          output << "    Mitigation: #{risk['mitigation']}"
        end
      end
      
      output.join("\n")
    end
    
    def handle_execute_command(command_id, args)
      plan_id = args.strip
      
      if plan_id.empty?
        add_command_output(command_id, "Usage: /execute <plan-id>")
        add_command_output(command_id, "Use /plans to see available plans")
        return
      end
      
      plan = load_plan(plan_id)
      unless plan
        add_command_output(command_id, "Plan '#{plan_id}' not found")
        return
      end
      
      add_command_output(command_id, "🚀 Executing plan: #{plan['objective']}")
      
      # Execute plan in background thread
      Thread.new do
        begin
          execute_plan_phases(command_id, plan)
        rescue => e
          @output_mutex.synchronize do
            puts "\n❌ Plan execution failed: #{e.message}"
          end
        end
      end
    end
    
    def list_saved_plans(command_id)
      plans = get_saved_plans
      
      if plans.empty?
        add_command_output(command_id, "No saved plans found")
        return
      end
      
      output = []
      output << "📋 Saved Plans"
      output << "━" * 60
      
      plans.each do |plan_info|
        status_emoji = case plan_info[:status]
        when 'completed' then '✅'
        when 'failed' then '❌'
        when 'running' then '⚡'
        else '📋'
        end
        
        output << "#{status_emoji} #{plan_info[:id]} - #{plan_info[:objective]}"
        output << "    Created: #{plan_info[:created_at]}"
        output << "    Phases: #{plan_info[:phases_count]}"
        output << ""
      end
      
      add_command_output(command_id, output.join("\n"))
    end
    
    def show_plan_graph(command_id, args)
      plan_id = args.strip
      
      if plan_id.empty?
        add_command_output(command_id, "Usage: /graph <plan-id>")
        add_command_output(command_id, "Use /plans to see available plans")
        return
      end
      
      plan = load_plan(plan_id)
      unless plan
        add_command_output(command_id, "Plan '#{plan_id}' not found")
        return
      end
      
      # Generate and show ASCII graph
      ascii_graph = Lantae::PlanVisualizer.generate_ascii_graph(plan)
      add_command_output(command_id, ascii_graph)
    end
    
    def save_plan(plan, original_input)
      plans_dir = File.expand_path('~/.lantae/plans')
      FileUtils.mkdir_p(plans_dir) unless Dir.exist?(plans_dir)
      
      plan_id = "plan_#{Time.now.strftime('%Y%m%d_%H%M%S')}_#{rand(1000)}"
      
      plan_data = plan.merge({
        'id' => plan_id,
        'original_input' => original_input,
        'saved_at' => Time.now.to_s,
        'workspace' => @options[:workspace]
      })
      
      plan_file = File.join(plans_dir, "#{plan_id}.json")
      File.write(plan_file, JSON.pretty_generate(plan_data))
      
      plan_id
    end
    
    def load_plan(plan_id)
      plans_dir = File.expand_path('~/.lantae/plans')
      plan_file = File.join(plans_dir, "#{plan_id}.json")
      
      return nil unless File.exist?(plan_file)
      
      JSON.parse(File.read(plan_file))
    rescue JSON::ParserError
      nil
    end
    
    def get_saved_plans
      plans_dir = File.expand_path('~/.lantae/plans')
      return [] unless Dir.exist?(plans_dir)
      
      Dir.glob(File.join(plans_dir, '*.json')).map do |file|
        begin
          plan = JSON.parse(File.read(file))
          {
            id: plan['id'],
            objective: plan['objective'],
            created_at: plan['saved_at'],
            phases_count: plan['phases']&.size || 0,
            status: plan['status'] || 'pending'
          }
        rescue JSON::ParserError
          nil
        end
      end.compact.sort_by { |p| p[:created_at] }.reverse
    end
    
    def execute_plan_phases(command_id, plan)
      @output_mutex.synchronize do
        puts "\n#{\"\\e[92m\"}🚀 Executing Plan#{\"\\e[0m\"}"
        puts
        
        # Show mini visualization for execution
        puts "#{\"\\e[94m\"}📋 #{plan['objective']}#{\"\\e[0m\"}"
        puts "#{\"\\e[90m\"}Phases: #{plan['phases'].size} | Tasks: #{plan['phases'].sum { |p| p['tasks']&.size || 0 }}#{\"\\e[0m\"}"
        puts "━" * 80
        puts
      end
      
      plan['phases'].each_with_index do |phase, phase_idx|
        @output_mutex.synchronize do
          puts "#{\"\\e[94m\"}Phase #{phase_idx + 1}: #{phase['name']}#{\"\\e[0m\"}"
        end
        
        # Check if tasks can run in parallel
        parallel_tasks = phase['tasks']&.select { |t| t['parallel'] } || []
        sequential_tasks = phase['tasks']&.reject { |t| t['parallel'] } || []
        
        # Execute parallel tasks first
        if parallel_tasks.any?
          execute_parallel_tasks(command_id, parallel_tasks)
        end
        
        # Execute sequential tasks
        sequential_tasks.each do |task|
          execute_plan_task(command_id, task)
        end
      end
      
      @output_mutex.synchronize do
        puts "\n✅ Plan execution completed!"
      end
      
      # Update plan status
      update_plan_status(plan['id'], 'completed')
    end
    
    def execute_parallel_tasks(command_id, tasks)
      @output_mutex.synchronize do
        puts "  🔄 Executing #{tasks.size} tasks in parallel..."
      end
      
      threads = tasks.map do |task|
        Thread.new do
          execute_plan_task(command_id, task)
        end
      end
      
      threads.each(&:join)
    end
    
    def execute_plan_task(command_id, task)
      @output_mutex.synchronize do
        puts "  → #{task['name']}"
      end
      
      # Create conversation for this task
      task_conversation = @conversation.dup
      task_conversation << { role: 'user', content: task['description'] }
      
      # Get current provider info for this command
      command = @commands[command_id]
      provider_clone = create_provider_clone(command[:provider], command[:model])
      
      begin
        response = provider_clone.chat(task_conversation, @options)
        
        # Add to conversation
        @conversation << { role: 'user', content: task['description'] }
        @conversation << { role: 'assistant', content: response }
        
        @output_mutex.synchronize do
          puts "    ✓ #{task['name']}"
        end
      rescue => e
        @output_mutex.synchronize do
          puts "    ✗ #{task['name']}: #{e.message}"
        end
      end
    end
    
    def update_plan_status(plan_id, status)
      plans_dir = File.expand_path('~/.lantae/plans')
      plan_file = File.join(plans_dir, "#{plan_id}.json")
      
      return unless File.exist?(plan_file)
      
      begin
        plan = JSON.parse(File.read(plan_file))
        plan['status'] = status
        plan['updated_at'] = Time.now.to_s
        File.write(plan_file, JSON.pretty_generate(plan))
      rescue JSON::ParserError
        # Ignore
      end
    end
    
    def cleanup
      @running = false
      
      # Wait for active commands to complete
      active_count = @commands.select { |_, cmd| cmd[:status] == :running }.size
      if active_count > 0
        puts "\nWaiting for #{active_count} active commands to complete..."
        @command_threads.each(&:join)
      end
      
      # Restore readline state to prevent interfering with other commands
      begin
        Readline.completion_proc = nil
        Readline.completion_append_character = nil
      rescue
        # Ignore cleanup errors
      end
    end
  end
end

# String truncate helper
class String
  def truncate(max_length)
    if length > max_length
      self[0, max_length - 3] + "..."
    else
      self
    end
  end
end