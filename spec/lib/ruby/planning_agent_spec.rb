require 'spec_helper'

RSpec.describe PlanningAgent do
  let(:provider_manager) { double('ProviderManager') }
  let(:tool_manager) { double('ToolManager') }
  let(:agent) { described_class.new(provider_manager, tool_manager) }
  let(:simple_task) { "Print hello world" }
  let(:complex_task) { "Build a web application with user authentication, database integration, and REST API" }

  before do
    allow(provider_manager).to receive(:chat).and_return(
      mock_provider_response("Simple task that can be executed directly")
    )
  end

  describe '#plan_task' do
    context 'with a simple task' do
      it 'creates a single root task without decomposition' do
        task = agent.plan_task(simple_task)
        
        expect(task).to be_a(Task)
        expect(task.description).to eq(simple_task)
        expect(task.subtasks).to be_empty
        expect(task.depth).to eq(0)
      end
    end

    context 'with a complex task' do
      before do
        # Mock decomposition responses
        allow(provider_manager).to receive(:chat).and_return(
          mock_provider_response(<<~JSON
            {
              "subtasks": [
                {"description": "Set up project structure", "complexity": 2},
                {"description": "Implement user authentication", "complexity": 6},
                {"description": "Design database schema", "complexity": 4},
                {"description": "Create REST API endpoints", "complexity": 5}
              ]
            }
          JSON
          )
        )
      end

      it 'decomposes task into subtasks' do
        task = agent.plan_task(complex_task)
        
        expect(task.subtasks).not_to be_empty
        expect(task.subtasks.size).to eq(4)
        expect(task.subtasks.map(&:description)).to include("Set up project structure")
      end

      it 'respects maximum decomposition depth' do
        # Mock recursive decomposition
        call_count = 0
        allow(provider_manager).to receive(:chat) do
          call_count += 1
          if call_count > 10 # Force early termination
            mock_provider_response('{"subtasks": []}')
          else
            mock_provider_response('{"subtasks": [{"description": "Subtask", "complexity": 5}]}')
          end
        end

        task = agent.plan_task(complex_task)
        max_depth = agent.send(:calculate_max_depth, task)
        
        expect(max_depth).to be <= 5
      end
    end

    context 'with invalid responses' do
      it 'handles malformed JSON gracefully' do
        allow(provider_manager).to receive(:chat).and_return(
          mock_provider_response("Not valid JSON")
        )

        task = agent.plan_task(simple_task)
        expect(task.subtasks).to be_empty
      end

      it 'handles missing subtasks key' do
        allow(provider_manager).to receive(:chat).and_return(
          mock_provider_response('{"other_key": "value"}')
        )

        task = agent.plan_task(simple_task)
        expect(task.subtasks).to be_empty
      end
    end
  end

  describe '#execute_plan' do
    let(:task) { agent.plan_task(simple_task) }
    let(:execution_engine) { double('ExecutionEngine') }

    before do
      allow(ExecutionEngine).to receive(:new).and_return(execution_engine)
      allow(execution_engine).to receive(:execute_task).and_return(
        double('ExecutionResult', success?: true, output: "Hello World")
      )
    end

    it 'executes a simple task' do
      result = agent.execute_plan(task)
      
      expect(result).to be_success
      expect(result.output).to eq("Hello World")
    end

    it 'executes subtasks in order' do
      # Create task with subtasks
      task.subtasks << Task.new(
        description: "Subtask 1",
        context: {},
        depth: 1,
        parent: task
      )
      task.subtasks << Task.new(
        description: "Subtask 2", 
        context: {},
        depth: 1,
        parent: task
      )

      execution_order = []
      allow(execution_engine).to receive(:execute_task) do |t|
        execution_order << t.description
        double('ExecutionResult', success?: true, output: "Done")
      end

      agent.execute_plan(task)
      
      expect(execution_order).to eq(["Subtask 1", "Subtask 2", simple_task])
    end

    it 'propagates subtask failures' do
      task.subtasks << Task.new(
        description: "Failing subtask",
        context: {},
        depth: 1,
        parent: task
      )

      allow(execution_engine).to receive(:execute_task).and_return(
        double('ExecutionResult', success?: false, output: "Error", error: "Failed")
      )

      result = agent.execute_plan(task)
      
      expect(result).not_to be_success
      expect(result.error).to include("Failed")
    end
  end

  describe '#optimize_plan' do
    let(:task) { agent.plan_task(simple_task) }

    before do
      # Add some subtasks
      3.times do |i|
        task.subtasks << Task.new(
          description: "Subtask #{i}",
          context: {},
          depth: 1,
          parent: task,
          complexity_score: i + 1
        )
      end
    end

    it 'removes redundant tasks' do
      # Add duplicate task
      task.subtasks << Task.new(
        description: "Subtask 1", # Duplicate
        context: {},
        depth: 1,
        parent: task
      )

      initial_count = task.subtasks.size
      agent.send(:optimize_plan, task)
      
      expect(task.subtasks.size).to be < initial_count
    end

    it 'reorders tasks by complexity' do
      agent.send(:optimize_plan, task)
      
      complexities = task.subtasks.map(&:complexity_score).compact
      expect(complexities).to eq(complexities.sort)
    end
  end

  describe 'task tree helpers' do
    let(:task) { agent.plan_task(simple_task) }

    describe '#calculate_max_depth' do
      it 'returns 0 for leaf task' do
        depth = agent.send(:calculate_max_depth, task)
        expect(depth).to eq(0)
      end

      it 'calculates correct depth for nested tasks' do
        subtask = Task.new(
          description: "Subtask",
          context: {},
          depth: 1,
          parent: task
        )
        task.subtasks << subtask
        
        subsubtask = Task.new(
          description: "Sub-subtask",
          context: {},
          depth: 2,
          parent: subtask
        )
        subtask.subtasks << subsubtask

        depth = agent.send(:calculate_max_depth, task)
        expect(depth).to eq(2)
      end
    end

    describe '#count_tasks' do
      it 'counts single task' do
        count = agent.send(:count_tasks, task)
        expect(count).to eq(1)
      end

      it 'counts all tasks in tree' do
        2.times do |i|
          subtask = Task.new(
            description: "Subtask #{i}",
            context: {},
            depth: 1,
            parent: task
          )
          task.subtasks << subtask
          
          # Add sub-subtask to first subtask
          if i == 0
            subtask.subtasks << Task.new(
              description: "Sub-subtask",
              context: {},
              depth: 2,
              parent: subtask
            )
          end
        end

        count = agent.send(:count_tasks, task)
        expect(count).to eq(4) # root + 2 subtasks + 1 sub-subtask
      end
    end
  end

  describe '#format_plan' do
    let(:task) { agent.plan_task(simple_task) }

    it 'formats simple task' do
      formatted = agent.format_plan(task)
      
      expect(formatted).to include(simple_task)
      expect(formatted).to include("[○]")
    end

    it 'formats task tree with indentation' do
      task.subtasks << Task.new(
        description: "Subtask 1",
        context: {},
        depth: 1,
        parent: task,
        complexity_score: 3.5
      )

      formatted = agent.format_plan(task)
      
      expect(formatted).to include("  - [○] Subtask 1")
      expect(formatted).to include("complexity: 3.5")
    end

    it 'shows execution status' do
      task.status = :completed
      task.subtasks << Task.new(
        description: "Failed task",
        context: {},
        depth: 1,
        parent: task,
        status: :failed
      )

      formatted = agent.format_plan(task)
      
      expect(formatted).to include("[✓]")
      expect(formatted).to include("[✗]")
    end
  end

  describe 'edge cases' do
    it 'handles nil task description' do
      expect { agent.plan_task(nil) }.not_to raise_error
    end

    it 'handles empty task description' do
      task = agent.plan_task("")
      expect(task.description).to eq("")
    end

    it 'handles circular dependencies' do
      task1 = Task.new(
        description: "Task 1",
        context: {},
        depth: 0,
        parent: nil
      )
      
      task2 = Task.new(
        description: "Task 2",
        context: {},
        depth: 1,
        parent: task1
      )
      
      # Create circular reference
      task1.parent = task2
      task1.subtasks << task2
      task2.subtasks << task1

      # Should not cause infinite loop
      expect { agent.send(:calculate_max_depth, task1) }.not_to raise_error
    end
  end
end