require 'sqlite3'
require 'json'

class TaskDatabase
  attr_reader :db_path

  def initialize(db_path = nil)
    @db_path = db_path || File.join(Dir.home, '.lantae', 'tasks.db')
    ensure_directory
    setup_database
  end

  def record_task_execution(task, execution_result)
    @db.execute(<<-SQL, 
      task.description,
      task.complexity&.score || 0,
      JSON.generate(task.context),
      execution_result.success? ? 1 : 0,
      execution_result.attempts,
      JSON.generate(execution_result.issues.map(&:type)),
      execution_result.output.to_s[0..1000], # Limit output size
      Time.now.to_i
    )
      INSERT INTO task_executions 
      (description, complexity_score, context, success, attempts, issues, output, timestamp)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    SQL
  end

  def get_similar_tasks(task_description, limit = 10)
    # Simple similarity based on common words
    words = task_description.downcase.split(/\W+/).reject(&:empty?)
    
    return [] if words.empty?
    
    # Build LIKE conditions for each word
    like_conditions = words.map { "LOWER(description) LIKE ?" }.join(" OR ")
    like_params = words.map { |w| "%#{w}%" }
    
    results = @db.execute(<<-SQL, *like_params)
      SELECT description, complexity_score, success, attempts, issues, 
             COUNT(*) as execution_count,
             AVG(CAST(success AS FLOAT)) as success_rate
      FROM task_executions
      WHERE #{like_conditions}
      GROUP BY description
      ORDER BY execution_count DESC
      LIMIT #{limit}
    SQL
    
    results.map do |row|
      TaskHistory.new(
        description: row[0],
        complexity_score: row[1],
        execution_count: row[5],
        success_rate: row[6],
        common_issues: JSON.parse(row[4] || '[]')
      )
    end
  end

  def get_task_success_rate(task_description)
    result = @db.execute(<<-SQL, task_description)
      SELECT AVG(CAST(success AS FLOAT)) as success_rate,
             COUNT(*) as execution_count
      FROM task_executions
      WHERE description = ?
    SQL
    
    return nil if result.empty? || result[0][1] == 0
    
    {
      success_rate: result[0][0],
      execution_count: result[0][1]
    }
  end

  def get_complexity_analysis
    results = @db.execute(<<-SQL)
      SELECT 
        CASE 
          WHEN complexity_score <= 3 THEN 'Simple'
          WHEN complexity_score <= 6 THEN 'Moderate'
          ELSE 'Complex'
        END as complexity_level,
        COUNT(*) as task_count,
        AVG(CAST(success AS FLOAT)) as avg_success_rate,
        AVG(attempts) as avg_attempts
      FROM task_executions
      GROUP BY complexity_level
      ORDER BY 
        CASE complexity_level
          WHEN 'Simple' THEN 1
          WHEN 'Moderate' THEN 2
          WHEN 'Complex' THEN 3
        END
    SQL
    
    results.map do |row|
      {
        complexity_level: row[0],
        task_count: row[1],
        avg_success_rate: row[2],
        avg_attempts: row[3]
      }
    end
  end

  def get_common_failures(limit = 10)
    results = @db.execute(<<-SQL)
      SELECT description, 
             COUNT(*) as failure_count,
             GROUP_CONCAT(DISTINCT issues) as all_issues
      FROM task_executions
      WHERE success = 0
      GROUP BY description
      ORDER BY failure_count DESC
      LIMIT #{limit}
    SQL
    
    results.map do |row|
      issues = row[2] ? row[2].split(',').flat_map { |i| JSON.parse(i) rescue [] }.uniq : []
      
      {
        description: row[0],
        failure_count: row[1],
        common_issues: issues
      }
    end
  end

  def generate_optimization_report
    report = "Task Execution Optimization Report\n"
    report += "=" * 50 + "\n\n"
    
    # Overall statistics
    overall = @db.execute(<<-SQL)[0]
      SELECT COUNT(*) as total_tasks,
             AVG(CAST(success AS FLOAT)) as overall_success_rate,
             AVG(attempts) as avg_attempts
      FROM task_executions
    SQL
    
    report += "Overall Statistics:\n"
    report += "  Total executions: #{overall[0]}\n"
    report += "  Overall success rate: #{(overall[1] * 100).round(1)}%\n"
    report += "  Average attempts: #{overall[2].round(1)}\n\n"
    
    # Complexity analysis
    report += "Success Rate by Complexity:\n"
    complexity_analysis.each do |level|
      report += "  #{level[:complexity_level]}: #{(level[:avg_success_rate] * 100).round(1)}% "
      report += "(#{level[:task_count]} tasks, avg #{level[:avg_attempts].round(1)} attempts)\n"
    end
    report += "\n"
    
    # Common failures
    report += "Most Common Failures:\n"
    get_common_failures(5).each_with_index do |failure, i|
      report += "  #{i + 1}. #{failure[:description][0..50]}#{'...' if failure[:description].length > 50}\n"
      report += "     Failures: #{failure[:failure_count]}, Issues: #{failure[:common_issues].join(', ')}\n"
    end
    
    report
  end

  def suggest_prompt_improvements(task_description)
    similar_tasks = get_similar_tasks(task_description, 20)
    
    return [] if similar_tasks.empty?
    
    # Analyze successful vs failed tasks
    successful = similar_tasks.select { |t| t.success_rate >= 0.75 }
    failed = similar_tasks.select { |t| t.success_rate < 0.5 }
    
    suggestions = []
    
    # Look for patterns in successful tasks
    if successful.any?
      successful_patterns = analyze_description_patterns(successful.map(&:description))
      suggestions << {
        type: :include_patterns,
        patterns: successful_patterns,
        reason: "These patterns appear in successful similar tasks"
      }
    end
    
    # Look for patterns to avoid from failed tasks
    if failed.any?
      failed_patterns = analyze_description_patterns(failed.map(&:description))
      suggestions << {
        type: :avoid_patterns,
        patterns: failed_patterns,
        reason: "These patterns appear in failed similar tasks"
      }
    end
    
    # Suggest complexity reduction if needed
    avg_complexity = similar_tasks.map(&:complexity_score).sum / similar_tasks.size.to_f
    if avg_complexity > 6.0
      suggestions << {
        type: :reduce_complexity,
        recommendation: "Break this task into smaller subtasks",
        reason: "Similar tasks have high complexity (#{avg_complexity.round(1)})"
      }
    end
    
    suggestions
  end

  private

  def ensure_directory
    dir = File.dirname(@db_path)
    FileUtils.mkdir_p(dir) unless File.exist?(dir)
  end

  def setup_database
    @db = SQLite3::Database.new(@db_path)
    
    @db.execute(<<-SQL)
      CREATE TABLE IF NOT EXISTS task_executions (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        description TEXT NOT NULL,
        complexity_score REAL,
        context TEXT,
        success INTEGER NOT NULL,
        attempts INTEGER NOT NULL,
        issues TEXT,
        output TEXT,
        timestamp INTEGER NOT NULL
      )
    SQL
    
    # Create indexes for better query performance
    @db.execute("CREATE INDEX IF NOT EXISTS idx_description ON task_executions(description)")
    @db.execute("CREATE INDEX IF NOT EXISTS idx_success ON task_executions(success)")
    @db.execute("CREATE INDEX IF NOT EXISTS idx_timestamp ON task_executions(timestamp)")
  end

  def analyze_description_patterns(descriptions)
    # Extract common words and phrases
    word_frequency = Hash.new(0)
    
    descriptions.each do |desc|
      words = desc.downcase.split(/\W+/).reject(&:empty?)
      words.each { |word| word_frequency[word] += 1 }
    end
    
    # Return words that appear in at least 30% of descriptions
    threshold = descriptions.size * 0.3
    word_frequency.select { |_, count| count >= threshold }.keys.sort
  end
end

class TaskHistory
  attr_reader :description, :complexity_score, :execution_count, 
              :success_rate, :common_issues

  def initialize(description:, complexity_score:, execution_count:, 
                 success_rate:, common_issues: [])
    @description = description
    @complexity_score = complexity_score
    @execution_count = execution_count
    @success_rate = success_rate
    @common_issues = common_issues
  end
end