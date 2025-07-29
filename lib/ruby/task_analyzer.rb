# Silence parser version compatibility warning
original_verbose = $VERBOSE
$VERBOSE = nil
require 'parser/current'
$VERBOSE = original_verbose

require 'json'
require_relative 'task_complexity'

class TaskAnalyzer
  attr_reader :patterns, :complexity_factors

  def initialize
    @patterns = load_patterns
    @complexity_factors = load_complexity_factors
  end

  # Assess the complexity of a task description
  def assess_complexity(task_description)
    factors = {}
    score = 0.0

    # Analyze task description
    factors[:length] = analyze_length(task_description)
    factors[:keywords] = analyze_keywords(task_description)
    factors[:technical_terms] = analyze_technical_terms(task_description)
    factors[:dependencies] = analyze_dependencies(task_description)
    
    # Calculate weighted score
    score = calculate_complexity_score(factors)
    
    Lantae::TaskComplexity.new(score: score, factors: factors)
  end

  # Analyze generated code for common issues
  def analyze_code(code, language = :ruby)
    issues = []
    
    case language
    when :ruby
      issues.concat(analyze_ruby_code(code))
    when :javascript
      issues.concat(analyze_javascript_code(code))
    when :python
      issues.concat(analyze_python_code(code))
    else
      issues << Lantae::Lantae::CodeIssue.new(
        type: :unsupported_language,
        message: "Language #{language} not supported for analysis",
        severity: :warning
      )
    end
    
    issues
  end

  # Check if code meets success criteria
  def meets_success_criteria?(code, language = :ruby)
    issues = analyze_code(code, language)
    
    # No critical issues
    critical_issues = issues.select { |i| i.severity == :critical }
    return false if critical_issues.any?
    
    # Limited number of errors
    error_issues = issues.select { |i| i.severity == :error }
    return false if error_issues.size > 2
    
    true
  end

  private

  def load_patterns
    {
      # Common error patterns
      eof_marker: /<<\s*EOF\s*$/,
      unclosed_string: /["'](?:[^"'\\]|\\.)*$/,
      unclosed_bracket: /[\[\{]\s*$/,
      syntax_error: /SyntaxError|unexpected|unterminated/i,
      
      # Code quality patterns
      long_line: /.{120,}/,
      complex_method: /def\s+\w+.*\n(?:.*\n){20,}.*end/m,
      nested_conditions: /if.*\n.*if.*\n.*if/m
    }
  end

  def load_complexity_factors
    {
      # Task description factors
      length: { weight: 0.1, threshold: 100 },
      technical_keywords: { weight: 0.3, keywords: %w[api authentication database deployment microservice distributed] },
      action_verbs: { weight: 0.2, keywords: %w[implement create build design develop integrate] },
      dependencies: { weight: 0.4, keywords: %w[with using requires depends needs] }
    }
  end

  def analyze_length(description)
    length = description.split.size
    base_score = [length / 20.0, 5.0].min
    { score: base_score, word_count: length }
  end

  def analyze_keywords(description)
    keywords = description.downcase.split(/\W+/)
    technical_count = keywords.count { |k| @complexity_factors[:technical_keywords][:keywords].include?(k) }
    action_count = keywords.count { |k| @complexity_factors[:action_verbs][:keywords].include?(k) }
    
    {
      score: (technical_count * 0.5 + action_count * 0.3).clamp(0, 5),
      technical_count: technical_count,
      action_count: action_count
    }
  end

  def analyze_technical_terms(description)
    terms = %w[
      async concurrent parallel thread process
      encryption security oauth jwt
      cache redis elasticsearch mongodb
      kubernetes docker container
      websocket grpc graphql rest
      machine learning ml ai neural
    ]
    
    term_count = terms.count { |term| description.downcase.include?(term) }
    
    {
      score: [term_count * 0.8, 5.0].min,
      term_count: term_count
    }
  end

  def analyze_dependencies(description)
    dependency_indicators = %w[
      integrate with
      connect to
      interface with
      depends on
      requires
      using external
      third-party
    ]
    
    dep_count = dependency_indicators.count { |indicator| description.downcase.include?(indicator) }
    
    {
      score: [dep_count * 1.2, 5.0].min,
      dependency_count: dep_count
    }
  end

  def calculate_complexity_score(factors)
    total_score = 0.0
    
    factors.each do |factor_name, factor_data|
      next unless factor_data.is_a?(Hash) && factor_data[:score]
      
      weight = @complexity_factors[factor_name]&.[](:weight) || 0.25
      total_score += factor_data[:score] * weight
    end
    
    # Normalize to 1-10 scale
    [total_score * 2, 10.0].min
  end

  def analyze_ruby_code(code)
    issues = []
    
    # Check for syntax errors
    begin
      Parser::CurrentRuby.parse(code)
    rescue Parser::SyntaxError => e
      issues << Lantae::CodeIssue.new(
        type: :syntax_error,
        message: e.message,
        line: e.diagnostic.location.line,
        severity: :critical
      )
    end
    
    # Check for common patterns
    check_eof_markers(code, issues)
    check_unclosed_strings(code, issues)
    check_method_length(code, issues)
    
    issues
  end

  def analyze_javascript_code(code)
    issues = []
    
    # Basic pattern matching for JS
    check_semicolons(code, issues)
    check_arrow_functions(code, issues)
    check_async_await(code, issues)
    
    issues
  end

  def analyze_python_code(code)
    issues = []
    
    # Basic pattern matching for Python
    check_indentation(code, issues)
    check_colons(code, issues)
    
    issues
  end

  def check_eof_markers(code, issues)
    if code.match?(@patterns[:eof_marker])
      issues << Lantae::CodeIssue.new(
        type: :eof_marker,
        message: "Code contains EOF marker at end",
        severity: :warning,
        auto_fixable: true
      )
    end
  end

  def check_unclosed_strings(code, issues)
    lines = code.split("\n")
    lines.each_with_index do |line, index|
      if line.match?(@patterns[:unclosed_string])
        issues << Lantae::CodeIssue.new(
          type: :unclosed_string,
          message: "Possible unclosed string",
          line: index + 1,
          severity: :error,
          auto_fixable: true
        )
      end
    end
  end

  def check_method_length(code, issues)
    methods = code.scan(/def\s+(\w+).*?\nend/m)
    methods.each do |method|
      lines = method[0].split("\n").size
      if lines > 20
        issues << Lantae::CodeIssue.new(
          type: :long_method,
          message: "Method is too long (#{lines} lines)",
          severity: :warning
        )
      end
    end
  end

  def check_semicolons(code, issues)
    lines = code.split("\n")
    lines.each_with_index do |line, index|
      if line.strip.end_with?(';') && !line.include?('for')
        issues << Lantae::CodeIssue.new(
          type: :unnecessary_semicolon,
          message: "Unnecessary semicolon",
          line: index + 1,
          severity: :style,
          auto_fixable: true
        )
      end
    end
  end

  def check_arrow_functions(code, issues)
    # Check for incorrect arrow function syntax
    if code.match?(/=>\s*{[^}]*}\s*;/)
      issues << Lantae::CodeIssue.new(
        type: :arrow_function_syntax,
        message: "Arrow function might need parentheses",
        severity: :warning
      )
    end
  end

  def check_async_await(code, issues)
    # Check for missing await
    if code.match?(/async.*function.*{[^}]*fetch\([^)]*\)[^}]*}/) && !code.match?(/await\s+fetch/)
      issues << Lantae::CodeIssue.new(
        type: :missing_await,
        message: "Async function might be missing await",
        severity: :warning
      )
    end
  end

  def check_indentation(code, issues)
    lines = code.split("\n")
    indent_stack = [0]
    
    lines.each_with_index do |line, index|
      stripped = line.lstrip
      next if stripped.empty? || stripped.start_with?('#')
      
      indent = line.size - stripped.size
      
      if stripped.match?(/^(if|for|while|def|class|try|with)/)
        indent_stack.push(indent + 4)
      elsif stripped.match?(/^(else|elif|except|finally)/)
        expected = indent_stack[-2] || 0
        if indent != expected
          issues << Lantae::CodeIssue.new(
            type: :indentation_error,
            message: "Incorrect indentation",
            line: index + 1,
            severity: :error
          )
        end
      end
    end
  end

  def check_colons(code, issues)
    lines = code.split("\n")
    lines.each_with_index do |line, index|
      if line.match?(/^\s*(if|for|while|def|class|try|with|else|elif|except|finally)\s+.*[^:]$/)
        issues << Lantae::CodeIssue.new(
          type: :missing_colon,
          message: "Missing colon at end of statement",
          line: index + 1,
          severity: :error,
          auto_fixable: true
        )
      end
    end
  end
end

class CodeIssue
  attr_reader :type, :message, :line, :severity, :auto_fixable

  def initialize(type:, message:, line: nil, severity: :warning, auto_fixable: false)
    @type = type
    @message = message
    @line = line
    @severity = severity # :critical, :error, :warning, :style
    @auto_fixable = auto_fixable
  end

  def to_s
    location = @line ? " (line #{@line})" : ""
    "[#{@severity.to_s.upcase}] #{@message}#{location}"
  end
end