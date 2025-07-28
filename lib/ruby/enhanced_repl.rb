require_relative 'split_screen_ui'
require 'io/console'

module Lantae
  # Completion provider for the enhanced UI
  class CompletionProvider
    def initialize(provider_manager, tool_manager)
      @provider_manager = provider_manager
      @tool_manager = tool_manager
    end
    
    def get_providers
      %w[ollama openai anthropic claude bedrock gemini mistral perplexity]
    end
    
    def get_models
      begin
        @provider_manager.list_models || []
      rescue => e
        # Fallback to common models if listing fails
        case @provider_manager.get_provider_info[:provider]
        when 'ollama'
          %w[cogito:latest llama3:latest codellama:latest mistral:latest]
        when 'claude'
          %w[claude-3-5-sonnet-20241022 claude-3-5-haiku-20241022 claude-3-opus-20240229]
        when 'openai'
          %w[gpt-4 gpt-4-turbo gpt-3.5-turbo]
        else
          []
        end
      end
    end
    
    def get_tools
      @tool_manager.list_tools rescue []
    end
  end
  
  class EnhancedREPL
    def initialize(provider_manager, tool_manager, options = {})
      @provider_manager = provider_manager
      @tool_manager = tool_manager
      @options = options
      @conversation = []
      @ui = SplitScreenUI.new(split_ratio: 0.65)
      @running = false
      @command_threads = []
      @active_commands = {}
      @command_mutex = Mutex.new
      @next_command_id = 1
      
      # Set up completion provider
      @completion_provider = CompletionProvider.new(provider_manager, tool_manager)
      @ui.completion_provider = @completion_provider
    end
    
    def start
      @running = true
      @ui.start
      
      # Start command processing thread
      @processor_thread = Thread.new { process_commands }
      
      # Start input handling
      handle_input_loop
    ensure
      stop
    end
    
    def stop
      @running = false
      @processor_thread&.join
      @command_threads.each(&:join)
      @ui.stop
    end
    
    private
    
    def handle_input_loop
      STDIN.raw do |io|
        while @running
          begin
            char = io.read_nonblock(1) rescue nil
            
            if char
              command = @ui.handle_input(char)
              
              if command
                # Check for exit commands
                if command == 'exit' || command == 'quit'
                  @running = false
                  break
                end
                
                # Add command to queue
                @ui.queue_command(command)
                @ui.add_output("Queued: #{command}", :info)
              end
            else
              sleep 0.01
            end
          rescue Interrupt
            @running = false
            break
          end
        end
      end
    end
    
    def process_commands
      while @running
        command = @ui.get_next_command
        
        if command
          # Create a new thread for each command to allow parallel processing
          thread = Thread.new(command) do |cmd|
            command_id = nil
            
            @command_mutex.synchronize do
              command_id = @next_command_id
              @next_command_id += 1
              @active_commands[command_id] = { command: cmd, status: :running }
            end
            
            begin
              @ui.add_output("[#{command_id}] Starting: #{cmd}", :info)
              
              if cmd.start_with?('/')
                handle_slash_command(cmd, command_id)
              else
                handle_ai_command(cmd, command_id)
              end
              
              @command_mutex.synchronize do
                @active_commands[command_id][:status] = :completed
              end
              
              @ui.add_output("[#{command_id}] Completed: #{cmd}", :success)
            rescue => e
              @command_mutex.synchronize do
                @active_commands[command_id][:status] = :failed
              end
              
              @ui.add_output("[#{command_id}] Error: #{e.message}", :error)
            ensure
              # Clean up completed threads
              @command_mutex.synchronize do
                @active_commands.delete(command_id)
              end
            end
          end
          
          @command_threads << thread
          @command_threads.delete_if { |t| !t.alive? }
        else
          sleep 0.1
        end
      end
    end
    
    def handle_slash_command(command, command_id)
      parts = command[1..-1].split(' ')
      cmd = parts[0]
      args = parts[1..-1].join(' ')
      
      case cmd
      when 'help'
        help_text = <<~HELP
          Available commands:
            /model <name>      - Switch model
            /provider <name>   - Switch provider
            /models           - List available models
            /status           - Show active commands
            /clear            - Clear output
            /cancel <id>      - Cancel a command
        HELP
        help_text.lines.each { |line| @ui.add_output(line.chomp, :info) }
        
      when 'models'
        models = @provider_manager.list_models
        @ui.add_output("Available models:", :info)
        models.each { |model| @ui.add_output("  - #{model}", :info) }
        
      when 'provider'
        if args.empty?
          @ui.add_output("Current provider: #{@provider_manager.get_provider_info[:provider]}", :info)
        else
          provider, model = args.split(' ', 2)
          @provider_manager.switch_provider(provider, model)
          info = @provider_manager.get_provider_info
          @ui.add_output("Switched to: #{info[:provider]} (#{info[:model]})", :success)
        end
        
      when 'model'
        if args.empty?
          @ui.add_output("Current model: #{@provider_manager.get_provider_info[:model]}", :info)
        else
          @provider_manager.switch_model(args)
          @ui.add_output("Switched to model: #{args}", :success)
        end
        
      when 'status'
        @command_mutex.synchronize do
          if @active_commands.empty?
            @ui.add_output("No active commands", :info)
          else
            @ui.add_output("Active commands:", :info)
            @active_commands.each do |id, info|
              @ui.add_output("  [#{id}] #{info[:status]}: #{info[:command]}", :info)
            end
          end
        end
        
      when 'clear'
        @ui.output_buffer.clear
        @ui.draw_ui
        
      when 'cancel'
        if args =~ /^\d+$/
          id = args.to_i
          # TODO: Implement command cancellation
          @ui.add_output("Cancel not yet implemented", :warning)
        else
          @ui.add_output("Usage: /cancel <command-id>", :warning)
        end
        
      else
        @ui.add_output("Unknown command: /#{cmd}", :error)
      end
    end
    
    def handle_ai_command(input, command_id)
      @conversation << { role: 'user', content: input }
      
      # Show streaming response
      @ui.add_output("[#{command_id}] AI processing...", :info)
      
      begin
        # Simulate streaming for now
        response = @provider_manager.chat(@conversation, @options)
        
        # Add response to conversation
        @conversation << { role: 'assistant', content: response }
        
        # Show response in output
        response.lines.each do |line|
          @ui.add_output("[#{command_id}] #{line.chomp}", :info)
        end
      rescue => e
        @ui.add_output("[#{command_id}] AI Error: #{e.message}", :error)
        raise
      end
    end
  end
  
  # Monkey patch to add enhanced REPL mode
  class ::ProviderManager
    def switch_model(model)
      @current_model = model
    end
  end
end