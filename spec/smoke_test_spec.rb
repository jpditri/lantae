require 'spec_helper'

RSpec.describe 'Lantae Planning Agent System - Smoke Tests' do
  describe 'Core Class Loading' do
    it 'loads PlanningAgent class' do
      expect { PlanningAgent }.not_to raise_error
    end

    it 'loads TaskAnalyzer class' do
      expect { TaskAnalyzer }.not_to raise_error
    end

    it 'loads TaskDatabase class' do
      expect { TaskDatabase }.not_to raise_error
    end

    it 'loads AutoFixer class' do
      expect { AutoFixer }.not_to raise_error
    end

    it 'loads Task class' do
      expect { Task }.not_to raise_error
    end
  end

  describe 'Basic Functionality' do
    let(:provider_manager) { double('ProviderManager') }
    let(:tool_manager) { double('ToolManager') }

    before do
      allow(provider_manager).to receive(:chat).and_return(
        double('response', body: '{"response": "test response", "done": true}')
      )
    end

    it 'creates PlanningAgent instance' do
      agent = PlanningAgent.new(provider_manager, tool_manager)
      expect(agent).to be_a(PlanningAgent)
      expect(agent.provider_manager).to eq(provider_manager)
    end

    it 'creates TaskAnalyzer instance' do
      analyzer = TaskAnalyzer.new
      expect(analyzer).to be_a(TaskAnalyzer)
    end

    it 'creates Task instance' do
      task = Task.new(
        description: "Test task",
        context: {},
        depth: 0,
        parent: nil
      )
      expect(task).to be_a(Task)
      expect(task.description).to eq("Test task")
    end

    it 'creates TaskDatabase instance' do
      db_file = Tempfile.new(['test', '.db'])
      database = TaskDatabase.new(db_file.path)
      expect(database).to be_a(TaskDatabase)
      db_file.close
      db_file.unlink
    end

    it 'creates AutoFixer instance' do
      fixer = AutoFixer.new
      expect(fixer).to be_a(AutoFixer)
    end
  end

  describe 'Basic Task Planning' do
    let(:provider_manager) { double('ProviderManager') }
    let(:tool_manager) { double('ToolManager') }
    let(:agent) { PlanningAgent.new(provider_manager, tool_manager) }

    before do
      allow(provider_manager).to receive(:chat).and_return(
        double('response', body: '{"response": "Simple task implementation", "done": true}')
      )
    end

    it 'can plan a simple task' do
      task = agent.plan_task("Print hello world")
      expect(task).to be_a(Task)
      expect(task.description).to eq("Print hello world")
    end
  end

  describe 'Task Complexity Assessment' do
    let(:analyzer) { TaskAnalyzer.new }

    it 'assesses simple task complexity' do
      result = analyzer.assess_complexity("Print hello world")
      expect(result).to respond_to(:score)
      expect(result.score).to be_a(Numeric)
    end

    it 'assesses complex task complexity' do
      complex_task = "Build a distributed microservices architecture with OAuth2 authentication"
      result = analyzer.assess_complexity(complex_task)
      expect(result).to respond_to(:score)
      expect(result.score).to be_a(Numeric)
    end
  end

  describe 'Database Operations' do
    let(:db_file) { Tempfile.new(['test', '.db']) }
    let(:database) { TaskDatabase.new(db_file.path) }

    after do
      db_file.close
      db_file.unlink
    end

    it 'initializes database successfully' do
      expect(database).to be_a(TaskDatabase)
      expect(File.exist?(db_file.path)).to be true
    end

    it 'has required methods' do
      expect(database).to respond_to(:record_task_execution)
    end
  end

  describe 'Code Analysis' do
    let(:analyzer) { TaskAnalyzer.new }

    it 'analyzes Ruby code' do
      ruby_code = 'def hello; puts "Hello"; end'
      result = analyzer.analyze_code(ruby_code, :ruby)
      expect(result).to be_an(Array)
    end

    it 'analyzes JavaScript code' do
      js_code = 'function hello() { console.log("Hello"); }'
      result = analyzer.analyze_code(js_code, :javascript)
      expect(result).to be_an(Array)
    end
  end

  describe 'Auto-Fixing' do
    let(:fixer) { AutoFixer.new }

    it 'has fix methods available' do
      expect(fixer).to respond_to(:fix_issue)
      expect(fixer).to respond_to(:apply_fixes)
    end
  end
end