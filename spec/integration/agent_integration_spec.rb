require 'spec_helper'
require 'tempfile'

RSpec.describe 'Agent System Integration' do
  let(:provider_manager) { double('ProviderManager') }
  let(:agent) { PlanningAgent.new(provider_manager) }
  let(:db_file) { Tempfile.new(['integration_test', '.db']) }
  
  before do
    # Set up default provider responses
    allow(provider_manager).to receive(:chat).and_return(
      mock_provider_response("Simple implementation")
    )
  end

  after do
    db_file.close
    db_file.unlink
  end

  describe 'end-to-end task execution' do
    context 'with simple task' do
      let(:task_description) { "Create a function that adds two numbers" }

      before do
        allow(provider_manager).to receive(:chat)
          .with(hash_including(messages: include(hash_including(content: include("add")))))
          .and_return(mock_provider_response('def add(a, b); a + b; end'))
      end

      it 'plans and executes successfully' do
        # Plan
        task = agent.plan_task(task_description)
        expect(task.subtasks).to be_empty # Simple task, no decomposition

        # Execute
        result = agent.execute_plan(task)
        
        expect(result).to be_success
        expect(result.output).to include('def add')
        expect(task.status).to eq(:completed)
      end
    end

    context 'with complex task requiring decomposition' do
      let(:task_description) { "Build a web scraper with error handling and rate limiting" }

      before do
        # Mock decomposition
        allow(provider_manager).to receive(:chat)
          .with(hash_including(messages: include(hash_including(content: include("Break down")))))
          .and_return(mock_provider_response(<<~JSON
            {
              "subtasks": [
                {"description": "Set up HTTP client with headers", "complexity": 2},
                {"description": "Implement scraping logic", "complexity": 4},
                {"description": "Add error handling", "complexity": 3},
                {"description": "Implement rate limiting", "complexity": 3}
              ]
            }
          JSON
          ))

        # Mock individual subtask executions
        allow(provider_manager).to receive(:chat)
          .with(hash_including(messages: include(hash_including(content: include("HTTP client")))))
          .and_return(mock_provider_response('require "net/http"'))
        
        allow(provider_manager).to receive(:chat)
          .with(hash_including(messages: include(hash_including(content: include("scraping logic")))))
          .and_return(mock_provider_response('def scrape(url); end'))
        
        allow(provider_manager).to receive(:chat)
          .with(hash_including(messages: include(hash_including(content: include("error handling")))))
          .and_return(mock_provider_response('rescue => e; puts e; end'))
        
        allow(provider_manager).to receive(:chat)
          .with(hash_including(messages: include(hash_including(content: include("rate limiting")))))
          .and_return(mock_provider_response('sleep(1)'))
      end

      it 'decomposes and executes all subtasks' do
        # Plan
        task = agent.plan_task(task_description)
        
        expect(task.subtasks.size).to eq(4)
        expect(task.subtasks.map(&:description)).to include("Set up HTTP client with headers")

        # Execute
        result = agent.execute_plan(task)
        
        expect(result).to be_success
        expect(task.subtasks.all? { |st| st.status == :completed }).to be true
      end
    end

    context 'with code requiring auto-fixes' do
      let(:task_description) { "Print a greeting message" }

      before do
        # Return code with issues
        allow(provider_manager).to receive(:chat)
          .and_return(mock_provider_response('puts "Hello World'))
      end

      it 'automatically fixes and executes' do
        task = agent.plan_task(task_description)
        result = agent.execute_plan(task)
        
        expect(result).to be_success
        expect(result.output).to eq('puts "Hello World"')
        expect(result.fixes_applied).not_to be_empty
        expect(result.fixes_applied.first[:type]).to eq(:unclosed_string)
      end
    end

    context 'with task history optimization' do
      let(:database) { TaskDatabase.new(db_file.path) }
      let(:engine) do
        ExecutionEngine.new(
          provider_manager: provider_manager,
          database: database
        )
      end

      before do
        # Record similar successful execution
        database.record_execution(
          task_description: "Create an addition function",
          complexity_score: 2.0,
          success: true,
          output: "def add(x, y); x + y; end",
          model_used: "cogito:latest"
        )

        # Configure engine to use history
        allow(ExecutionEngine).to receive(:new).and_return(engine)
      end

      it 'uses similar task history for optimization' do
        expect(provider_manager).to receive(:chat)
          .with(hash_including(messages: include(hash_including(content: include("similar task")))))
          .and_return(mock_provider_response('def add(a, b); a + b; end'))

        task = agent.plan_task("Make a function to add numbers")
        result = agent.execute_plan(task)
        
        expect(result).to be_success
      end
    end
  end

  describe 'error handling and recovery' do
    context 'with execution failures' do
      let(:task_description) { "Complex task that might fail" }

      before do
        call_count = 0
        allow(provider_manager).to receive(:chat) do
          call_count += 1
          if call_count == 1
            raise StandardError, "Network error"
          else
            mock_provider_response('puts "Recovered"')
          end
        end
      end

      it 'retries and recovers from transient errors' do
        task = agent.plan_task(task_description)
        result = agent.execute_plan(task, max_retries: 2)
        
        expect(result).to be_success
        expect(result.retry_count).to eq(1)
      end
    end

    context 'with persistent failures' do
      before do
        allow(provider_manager).to receive(:chat)
          .and_raise(StandardError, "Persistent error")
      end

      it 'records failure and provides error details' do
        task = agent.plan_task("Failing task")
        result = agent.execute_plan(task)
        
        expect(result).not_to be_success
        expect(result.error).to include("Persistent error")
        expect(task.status).to eq(:failed)
      end
    end
  end

  describe 'performance tracking' do
    let(:database) { TaskDatabase.new(db_file.path) }

    before do
      allow(ExecutionEngine).to receive(:new).and_return(
        ExecutionEngine.new(
          provider_manager: provider_manager,
          database: database
        )
      )
    end

    it 'tracks execution metrics' do
      task = agent.plan_task("Simple task")
      
      start_time = Time.now
      result = agent.execute_plan(task)
      end_time = Time.now
      
      expect(result).to be_success
      
      # Check database record
      executions = database.instance_variable_get(:@db).execute(
        "SELECT * FROM task_executions WHERE task_description = ?",
        ["Simple task"]
      )
      
      expect(executions).not_to be_empty
      execution = executions.first
      expect(execution['success']).to eq(1)
      expect(execution['execution_time']).to be_between(0, (end_time - start_time))
    end

    it 'generates optimization reports' do
      # Execute several tasks
      5.times do |i|
        task = agent.plan_task("Task #{i}")
        agent.execute_plan(task)
      end

      report = database.generate_optimization_report
      
      expect(report[:summary][:total_executions]).to eq(5)
      expect(report[:summary][:overall_success_rate]).to eq(1.0)
    end
  end

  describe 'multi-language support' do
    %w[ruby javascript python].each do |language|
      context "with #{language} tasks" do
        let(:task_description) { "Create a hello world in #{language}" }

        before do
          code = case language
                 when 'ruby' then 'puts "Hello World"'
                 when 'javascript' then 'console.log("Hello World");'
                 when 'python' then 'print("Hello World")'
                 end

          allow(provider_manager).to receive(:chat)
            .and_return(mock_provider_response(code))
        end

        it "handles #{language} code correctly" do
          task = agent.plan_task(task_description)
          result = agent.execute_plan(task)
          
          expect(result).to be_success
          expect(result.output).to include(
            language == 'ruby' ? 'puts' : 
            language == 'javascript' ? 'console.log' : 
            'print'
          )
        end
      end
    end
  end

  describe 'concurrent execution' do
    it 'handles multiple tasks in parallel' do
      tasks = []
      results = []
      
      # Create multiple tasks
      3.times do |i|
        tasks << agent.plan_task("Concurrent task #{i}")
      end

      # Execute in threads
      threads = tasks.map do |task|
        Thread.new do
          results << agent.execute_plan(task)
        end
      end

      threads.each(&:join)
      
      expect(results.size).to eq(3)
      expect(results.all?(&:success?)).to be true
    end
  end

  describe 'rollback functionality' do
    let(:engine) do
      ExecutionEngine.new(
        provider_manager: provider_manager,
        rollback_enabled: true
      )
    end

    before do
      allow(ExecutionEngine).to receive(:new).and_return(engine)
    end

    it 'creates checkpoints and can rollback' do
      # Mock a task that modifies environment
      allow(provider_manager).to receive(:chat) do
        # Simulate environment change
        ENV['TEST_VAR'] = 'modified'
        raise StandardError, "Execution failed"
      end

      original_env = ENV['TEST_VAR']
      
      task = agent.plan_task("Modifying task")
      result = agent.execute_plan(task)
      
      expect(result).not_to be_success
      # Rollback should have restored environment
      expect(ENV['TEST_VAR']).to eq(original_env)
    end
  end
end