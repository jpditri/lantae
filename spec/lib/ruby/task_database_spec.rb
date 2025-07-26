require 'spec_helper'

RSpec.describe TaskDatabase do
  let(:db_file) { Tempfile.new(['test_tasks', '.db']) }
  let(:database) { described_class.new(db_file.path) }

  after do
    db_file.close
    db_file.unlink
  end

  describe '#initialize' do
    it 'creates database file' do
      expect(File.exist?(db_file.path)).to be true
    end

    it 'creates required tables' do
      result = database.instance_variable_get(:@db).execute(
        "SELECT name FROM sqlite_master WHERE type='table'"
      )
      
      table_names = result.map { |row| row['name'] }
      expect(table_names).to include('task_executions', 'optimization_reports')
    end
  end

  describe '#record_execution' do
    let(:execution_data) do
      {
        task_description: "Create a hello world function",
        complexity_score: 2.5,
        model_used: "cogito:latest",
        execution_time: 1.234,
        success: true,
        output: "def hello_world; puts 'Hello, World!'; end",
        error: nil,
        fixes_applied: [{ type: :missing_end, description: "Added missing end" }],
        verification_passed: true,
        retry_count: 0
      }
    end

    it 'saves execution record' do
      database.record_execution(execution_data)
      
      result = database.instance_variable_get(:@db).execute(
        "SELECT * FROM task_executions WHERE task_description = ?",
        ["Create a hello world function"]
      )
      
      expect(result.first['success']).to eq(1)
      expect(result.first['complexity_score']).to eq(2.5)
      expect(result.first['model_used']).to eq("cogito:latest")
    end

    it 'serializes JSON fields' do
      database.record_execution(execution_data)
      
      result = database.instance_variable_get(:@db).execute(
        "SELECT fixes_applied FROM task_executions"
      ).first
      
      fixes = JSON.parse(result['fixes_applied'])
      expect(fixes).to be_an(Array)
      expect(fixes.first).to include('type' => 'missing_end')
    end

    it 'handles nil values' do
      data = execution_data.merge(
        output: nil,
        error: "Execution failed",
        fixes_applied: nil
      )
      
      expect { database.record_execution(data) }.not_to raise_error
    end
  end

  describe '#find_similar_tasks' do
    before do
      # Record some sample tasks
      database.record_execution(
        task_description: "Create a hello world function",
        complexity_score: 2.0,
        success: true,
        output: "def hello_world; puts 'Hello!'; end"
      )
      
      database.record_execution(
        task_description: "Make a hello world method",
        complexity_score: 2.1,
        success: true,
        output: "def hello_world; puts 'Hello World!'; end"
      )
      
      database.record_execution(
        task_description: "Build a REST API server",
        complexity_score: 7.5,
        success: false,
        error: "Task too complex"
      )
    end

    it 'finds similar tasks by description' do
      similar = database.find_similar_tasks("Create hello world function")
      
      expect(similar.size).to be >= 2
      expect(similar.first['task_description']).to include('hello world')
    end

    it 'respects similarity threshold' do
      similar_high = database.find_similar_tasks("hello world", threshold: 0.9)
      similar_low = database.find_similar_tasks("hello world", threshold: 0.3)
      
      expect(similar_low.size).to be >= similar_high.size
    end

    it 'orders by similarity score' do
      similar = database.find_similar_tasks("Create a hello world function")
      
      if similar.size > 1
        first_score = database.send(:calculate_similarity, 
          "Create a hello world function", 
          similar.first['task_description']
        )
        second_score = database.send(:calculate_similarity,
          "Create a hello world function",
          similar[1]['task_description']
        )
        
        expect(first_score).to be >= second_score
      end
    end

    it 'returns empty array for no matches' do
      similar = database.find_similar_tasks("Quantum computing algorithm")
      expect(similar).to be_empty
    end
  end

  describe '#get_success_rate' do
    before do
      # Create execution records
      3.times do
        database.record_execution(
          task_description: "Task with 75% success",
          complexity_score: 3.0,
          success: true
        )
      end
      
      database.record_execution(
        task_description: "Task with 75% success",
        complexity_score: 3.0,
        success: false
      )
    end

    it 'calculates overall success rate' do
      rate = database.get_success_rate
      expect(rate).to eq(0.75)
    end

    it 'calculates success rate by complexity range' do
      # Add more varied data
      database.record_execution(
        task_description: "Simple task",
        complexity_score: 1.0,
        success: true
      )
      
      database.record_execution(
        task_description: "Complex task",
        complexity_score: 8.0,
        success: false
      )

      rate_simple = database.get_success_rate(min_complexity: 0, max_complexity: 2)
      rate_complex = database.get_success_rate(min_complexity: 7, max_complexity: 10)
      
      expect(rate_simple).to eq(1.0)
      expect(rate_complex).to eq(0.0)
    end

    it 'calculates success rate by model' do
      database.record_execution(
        task_description: "GPT task",
        model_used: "gpt-4",
        success: true
      )
      
      rate_gpt = database.get_success_rate(model: "gpt-4")
      rate_other = database.get_success_rate(model: "cogito:latest")
      
      expect(rate_gpt).to eq(1.0)
      expect(rate_other).to eq(0.75) # From setup data
    end

    it 'returns 0 for no executions' do
      empty_db = described_class.new(Tempfile.new(['empty', '.db']).path)
      rate = empty_db.get_success_rate
      
      expect(rate).to eq(0.0)
    end
  end

  describe '#generate_optimization_report' do
    before do
      # Create diverse execution data
      5.times do |i|
        database.record_execution(
          task_description: "Simple task #{i}",
          complexity_score: 1.5 + (i * 0.1),
          model_used: "cogito:latest",
          execution_time: 0.5 + (i * 0.1),
          success: true,
          fixes_applied: i.even? ? [{ type: :unclosed_string }] : []
        )
      end
      
      3.times do |i|
        database.record_execution(
          task_description: "Complex task #{i}",
          complexity_score: 7.0 + i,
          model_used: "gpt-4",
          execution_time: 2.0 + i,
          success: i != 1, # Middle one fails
          error: i == 1 ? "Timeout" : nil,
          retry_count: i
        )
      end
    end

    it 'generates comprehensive report' do
      report = database.generate_optimization_report
      
      expect(report).to include(:summary, :by_complexity, :by_model, :common_errors, :recommendations)
    end

    it 'calculates summary statistics' do
      report = database.generate_optimization_report
      summary = report[:summary]
      
      expect(summary[:total_executions]).to eq(8)
      expect(summary[:overall_success_rate]).to be_between(0.8, 0.9)
      expect(summary[:average_execution_time]).to be > 0
      expect(summary[:average_complexity]).to be_between(3, 5)
    end

    it 'groups by complexity ranges' do
      report = database.generate_optimization_report
      by_complexity = report[:by_complexity]
      
      expect(by_complexity).to have_key("1.0-3.0")
      expect(by_complexity).to have_key("7.0-10.0")
      expect(by_complexity["1.0-3.0"][:success_rate]).to eq(1.0)
    end

    it 'groups by model' do
      report = database.generate_optimization_report
      by_model = report[:by_model]
      
      expect(by_model).to have_key("cogito:latest")
      expect(by_model).to have_key("gpt-4")
      expect(by_model["cogito:latest"][:count]).to eq(5)
    end

    it 'identifies common errors' do
      report = database.generate_optimization_report
      errors = report[:common_errors]
      
      expect(errors).to include(
        hash_including(
          error: "Timeout",
          count: 1
        )
      )
    end

    it 'provides recommendations' do
      report = database.generate_optimization_report
      recommendations = report[:recommendations]
      
      expect(recommendations).to be_an(Array)
      expect(recommendations).not_to be_empty
    end

    it 'saves report to database' do
      report = database.generate_optimization_report
      
      saved = database.instance_variable_get(:@db).execute(
        "SELECT * FROM optimization_reports ORDER BY created_at DESC LIMIT 1"
      ).first
      
      expect(saved).not_to be_nil
      expect(JSON.parse(saved['report_data'])).to eq(report)
    end
  end

  describe '#cleanup_old_records' do
    before do
      # Create old records
      5.times do |i|
        database.instance_variable_get(:@db).execute(
          "INSERT INTO task_executions (task_description, created_at) VALUES (?, ?)",
          ["Old task #{i}", (Time.now - (40 + i) * 24 * 60 * 60).to_s]
        )
      end
      
      # Create recent records
      3.times do |i|
        database.record_execution(
          task_description: "Recent task #{i}",
          success: true
        )
      end
    end

    it 'removes records older than specified days' do
      initial_count = database.instance_variable_get(:@db).execute(
        "SELECT COUNT(*) as count FROM task_executions"
      ).first['count']
      
      database.cleanup_old_records(days: 30)
      
      final_count = database.instance_variable_get(:@db).execute(
        "SELECT COUNT(*) as count FROM task_executions"
      ).first['count']
      
      expect(final_count).to eq(3)
      expect(final_count).to be < initial_count
    end

    it 'preserves recent records' do
      database.cleanup_old_records(days: 30)
      
      remaining = database.instance_variable_get(:@db).execute(
        "SELECT task_description FROM task_executions"
      ).map { |r| r['task_description'] }
      
      expect(remaining).to include("Recent task 0", "Recent task 1", "Recent task 2")
      expect(remaining).not_to include("Old task 0")
    end
  end

  describe 'similarity calculation' do
    it 'calculates exact match as 1.0' do
      score = database.send(:calculate_similarity, "hello world", "hello world")
      expect(score).to eq(1.0)
    end

    it 'calculates no match as 0.0' do
      score = database.send(:calculate_similarity, "hello world", "xyz abc")
      expect(score).to be < 0.1
    end

    it 'calculates partial matches correctly' do
      score = database.send(:calculate_similarity, 
        "Create a hello world function",
        "Make a hello world method"
      )
      
      expect(score).to be_between(0.5, 0.8)
    end

    it 'is case insensitive' do
      score1 = database.send(:calculate_similarity, "Hello World", "hello world")
      score2 = database.send(:calculate_similarity, "HELLO WORLD", "hello world")
      
      expect(score1).to eq(score2)
    end
  end

  describe 'edge cases' do
    it 'handles corrupt database gracefully' do
      # Corrupt the database file
      File.write(db_file.path, "corrupted data")
      
      expect { described_class.new(db_file.path) }.to raise_error(SQLite3::Exception)
    end

    it 'handles very long task descriptions' do
      long_description = "A" * 10000
      
      expect {
        database.record_execution(
          task_description: long_description,
          success: true
        )
      }.not_to raise_error
    end

    it 'handles concurrent access' do
      threads = []
      
      5.times do |i|
        threads << Thread.new do
          database.record_execution(
            task_description: "Concurrent task #{i}",
            success: true
          )
        end
      end
      
      threads.each(&:join)
      
      count = database.instance_variable_get(:@db).execute(
        "SELECT COUNT(*) as count FROM task_executions WHERE task_description LIKE 'Concurrent%'"
      ).first['count']
      
      expect(count).to eq(5)
    end
  end
end