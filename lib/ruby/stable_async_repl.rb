require 'thread'
require 'readline'
require 'time'
require_relative 'stable_ui'
require_relative 'side_panel_manager'
require_relative 'direct_authenticator'
require_relative 'planning_agent'
require_relative 'task_analyzer'
require_relative 'workspace_authenticator'

module Lantae
  class StableAsyncREPL
    def initialize(provider_manager, tool_manager, options = {})
      @provider_manager = provider_manager
      @tool_manager = tool_manager
      @options = options
      @conversation = []
      
      # UI components
      @ui = StableUI.new
      @show_side_panel = !options[:no_side_panel]
      
      # Threading and command management
      @command_mutex = Mutex.new
      @commands = {}
      @next_command_id = 1
      @command_threads = []
      @running = true
      
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
      # Set up the screen once
      setup_initial_ui
      
      # Check for API key on startup
      check_api_key_availability
      
      # Set up autocomplete
      setup_autocomplete
      
      # Start side panel update thread if enabled
      start_side_panel_updater if @show_side_panel
      
      # Main input loop
      loop do
        begin
          # Update status line with active commands
          update_status_line
          
          # Get input with minimal redraw
          input = Readline.readline("> ", true)
          break if input.nil? || ['exit', 'quit'].include?(input.strip)
          
          input = input.strip
          next if input.empty?
          
          # Clear the input line for output
          @ui.clear_input_line
          
          # Submit command
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
    
    def setup_initial_ui
      # Create header with provider info
      header = create_header
      @ui.setup_screen(header)
    end
    
    def create_header
      provider_info = @provider_manager.get_provider_info
      
      "\e[96m╔══════════════════════════════════════════════════════════════════════════════╗\e[0m\n" +
      "\e[96m║                            Lantae - Stable UI Mode                           ║\e[0m\n" +
      "\e[96m╠══════════════════════════════════════════════════════════════════════════════╣\e[0m\n" +
      "\e[96m║\e[0m Provider: \e[93m#{provider_info[:provider]}\e[0m | Model: \e[92m#{provider_info[:model]}\e[0m".ljust(87) + "\e[96m║\e[0m\n" +
      "\e[96m║\e[0m Commands: /help, /login, /workspace, /plan, /auto, /side, /clear            \e[96m║\e[0m\n" +
      "\e[96m╚══════════════════════════════════════════════════════════════════════════════╝\e[0m"
    end
    
    def start_side_panel_updater
      Thread.new do
        loop do
          sleep 2  # Update every 2 seconds
          
          if @show_side_panel
            provider_info = @provider_manager.get_provider_info
            side_content = SidePanelManager.generate_side_content(
              provider: provider_info[:provider],
              model: provider_info[:model],
              temperature: @options[:temperature],
              conversation: @conversation,
              tools_available: @tool_manager&.list_available_tools || []
            )
            
            @ui.update_side_panel(side_content)
          end
        end
      end
    end
    
    def update_status_line
      active_count = @commands.select { |_, cmd| cmd[:status] == :running }.size
      if active_count > 0
        @ui.update_status_line("\e[33m⚡ #{active_count} active command(s)\e[0m")
      else
        @ui.update_status_line("")
      end
    end
    
    def submit_command(input)
      command_id = nil
      
      @command_mutex.synchronize do
        command_id = @next_command_id
        @next_command_id += 1
        
        provider_info = @provider_manager.get_provider_info
        
        @commands[command_id] = {
          id: command_id,
          input: input,
          status: :queued,
          provider: provider_info[:provider],
          model: provider_info[:model],
          submitted_at: Time.now
        }
      end
      
      # Show submission with minimal UI update
      @ui.append_output("\e[94m→\e[0m [#{command_id}] #{input}")
      
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
        
      rescue => e
        @command_mutex.synchronize do
          command[:status] = :failed
          command[:error] = e.message
          command[:completed_at] = Time.now
        end
        
        @ui.show_command_result(command_id, :failed, e.message)
      end
    end
    
    def process_slash_command(command_id)
      command = @commands[command_id]
      input = command[:input]
      
      parts = input[1..-1].split(' ')
      cmd = parts[0]
      args = parts[1..-1].join(' ')
      
      case cmd
      when 'clear'
        setup_initial_ui
        
      when 'side'
        @show_side_panel = !@show_side_panel
        status = @show_side_panel ? "enabled" : "disabled"
        @ui.append_output("Side panel #{status}")
        
      when 'login'
        handle_login_command(command_id, args)
        
      when 'provider'
        if args.empty?
          @ui.append_output("Current: #{@provider_manager.get_provider_info[:provider]}")
        else
          handle_provider_switch(args)
        end
        
      when 'model'
        if args.empty?
          @ui.append_output("Current: #{@provider_manager.get_provider_info[:model]}")
        else
          @provider_manager.current_model = args
          @ui.append_output("Model set to: #{args}")
        end
        
      when 'help'
        show_help
        
      else
        @ui.append_output("Unknown command: /#{cmd}")
      end
    end
    
    def process_ai_command(command_id)
      command = @commands[command_id]
      
      # Create conversation for this command
      command_conversation = @conversation.dup
      command_conversation << { role: 'user', content: command[:input] }
      
      # Get response
      provider_clone = create_provider_clone(command[:provider], command[:model])
      response = provider_clone.chat(command_conversation, @options)
      
      # Add to conversation
      @conversation << { role: 'user', content: command[:input] }
      @conversation << { role: 'assistant', content: response }
      
      # Format response with side panel if enabled
      if @show_side_panel
        side_content = SidePanelManager.generate_side_content(
          provider: command[:provider],
          model: command[:model],
          temperature: @options[:temperature],
          conversation: @conversation,
          tools_available: @tool_manager&.list_available_tools || []
        )
        
        output = @ui.with_side_panel(response, side_content)
      else
        output = response
      end
      
      # Show result
      @ui.show_command_result(command_id, :completed, response.lines.first || "")
      @ui.append_output(output)
    end
    
    def handle_login_command(command_id, args)
      provider = args.strip.empty? ? 'anthropic' : args.strip.downcase
      
      unless %w[anthropic openai gemini].include?(provider)
        @ui.append_output("❌ Unsupported provider: #{provider}")
        return
      end
      
      Thread.new do
        result = DirectAuthenticator.login(provider)
        
        if result[:success]
          @provider_manager.switch_provider(provider)
          @ui.append_output("✅ Authenticated with #{provider.capitalize}")
          
          # Update header to show new provider
          setup_initial_ui
        else
          @ui.append_output("❌ Authentication failed")
        end
      end
    end
    
    def handle_provider_switch(provider_spec)
      provider, model = provider_spec.split(' ', 2)
      
      # Check API key first
      if provider != 'ollama' && !has_api_key?(provider)
        @ui.append_output("⚠️  #{provider} requires authentication. Use: /login #{provider}")
        return
      end
      
      @provider_manager.switch_provider(provider, model)
      @ui.append_output("Switched to: #{provider}")
      
      # Update header
      setup_initial_ui
    end
    
    def has_api_key?(provider)
      ENV["#{provider.upcase}_API_KEY"] || File.exist?(File.expand_path('~/.lantae_env'))
    end
    
    def create_provider_clone(provider, model)
      clone = @provider_manager.dup
      clone.switch_provider(provider, model)
      clone
    end
    
    def show_help
      help = <<~HELP
        Commands:
          /help              - Show this help
          /login [provider]  - Authenticate with provider
          /provider <name>   - Switch provider
          /model <name>      - Switch model
          /workspace [name]  - Switch workspace
          /plan <task>       - Create execution plan
          /auto              - Toggle auto-planning
          /side              - Toggle side panel
          /clear             - Clear screen
          exit/quit          - Exit
      HELP
      @ui.append_output(help)
    end
    
    def check_api_key_availability
      # Similar to original but with minimal UI updates
      provider_info = @provider_manager.get_provider_info
      provider = provider_info[:provider]
      
      return if provider == 'ollama'
      
      env_key = "#{provider.upcase}_API_KEY"
      
      if ENV[env_key].nil? && !File.exist?(File.expand_path('~/.lantae_env'))
        @ui.append_output("\e[33m⚠️  No API key for #{provider}. Use: /login #{provider}\e[0m")
      end
    end
    
    def setup_autocomplete
      commands = %w[help login provider model side clear]
      
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
      # Ignore errors
    end
    
    def cleanup
      @running = false
      @command_threads.each(&:join)
      
      # Restore terminal
      print "\e[?25h"  # Show cursor
      puts
    end
  end
end