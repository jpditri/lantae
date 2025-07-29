module Lantae
  class TaskComplexity
    attr_reader :score, :factors
    
    def initialize(score:, factors:)
      @score = score
      @factors = factors
    end
    
    def complex?
      @score >= 3.0
    end
    
    def simple?
      @score < 2.0
    end
    
    def medium?
      @score >= 2.0 && @score < 3.0
    end
    
    def to_s
      "TaskComplexity(score: #{@score.round(2)}, level: #{complexity_level})"
    end
    
    def complexity_level
      case @score
      when 0...2.0 then :simple
      when 2.0...3.0 then :medium
      else :complex
      end
    end
    
    def summary
      {
        score: @score.round(2),
        level: complexity_level,
        factors: @factors
      }
    end
  end
  
  class CodeIssue
    attr_reader :type, :message, :severity, :line
    
    def initialize(type:, message:, severity:, line: nil)
      @type = type
      @message = message
      @severity = severity
      @line = line
    end
    
    def critical?
      @severity == :critical
    end
    
    def error?
      @severity == :error
    end
    
    def warning?
      @severity == :warning
    end
    
    def to_s
      line_info = @line ? " (line #{@line})" : ""
      "[#{@severity.upcase}] #{@type}: #{@message}#{line_info}"
    end
  end
end