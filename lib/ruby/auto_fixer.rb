class AutoFixer
  def initialize
    @fixes = {
      eof_marker: method(:fix_eof_marker),
      unclosed_string: method(:fix_unclosed_string),
      missing_colon: method(:fix_missing_colon),
      unnecessary_semicolon: method(:fix_unnecessary_semicolon),
      indentation_error: method(:fix_indentation),
      syntax_error: method(:fix_common_syntax_errors)
    }
  end

  def fix_issue(code, issue, language)
    fix_method = @fixes[issue.type]
    return code unless fix_method
    
    fix_method.call(code, issue, language)
  rescue => e
    # If fix fails, return original code
    puts "Auto-fix failed for #{issue.type}: #{e.message}"
    code
  end

  def apply_fixes(code, issues, language)
    fixed_code = code
    
    # Sort issues by line number (reverse) to avoid offset problems
    sorted_issues = issues.select(&:auto_fixable).sort_by { |i| -(i.line || 0) }
    
    sorted_issues.each do |issue|
      fixed_code = fix_issue(fixed_code, issue, language)
    end
    
    fixed_code
  end

  private

  def fix_eof_marker(code, issue, language)
    # Remove EOF markers that LLMs sometimes append
    code.gsub(/\s*<<\s*EOF\s*$/, '')
  end

  def fix_unclosed_string(code, issue, language)
    return code unless issue.line
    
    lines = code.split("\n")
    line_index = issue.line - 1
    return code if line_index >= lines.size
    
    line = lines[line_index]
    
    # Detect the quote type and close it
    if line.match?(/["'][^"']*$/)
      quote_char = line.match(/(['"])/)[1]
      lines[line_index] = line + quote_char
    end
    
    lines.join("\n")
  end

  def fix_missing_colon(code, issue, language)
    return code unless language == :python && issue.line
    
    lines = code.split("\n")
    line_index = issue.line - 1
    return code if line_index >= lines.size
    
    lines[line_index] = lines[line_index].rstrip + ":"
    lines.join("\n")
  end

  def fix_unnecessary_semicolon(code, issue, language)
    return code unless [:javascript, :ruby].include?(language) && issue.line
    
    lines = code.split("\n")
    line_index = issue.line - 1
    return code if line_index >= lines.size
    
    lines[line_index] = lines[line_index].rstrip.chomp(';')
    lines.join("\n")
  end

  def fix_indentation(code, issue, language)
    return code unless language == :python
    
    # Simple indentation fix - ensure consistent 4-space indentation
    lines = code.split("\n")
    fixed_lines = []
    indent_level = 0
    
    lines.each do |line|
      stripped = line.lstrip
      
      # Decrease indent for dedent keywords
      if stripped.match?(/^(return|break|continue|pass)/) ||
         stripped.match?(/^(else|elif|except|finally):/)
        indent_level = [indent_level - 1, 0].max
      end
      
      # Apply indentation
      if stripped.empty?
        fixed_lines << line
      else
        fixed_lines << ("    " * indent_level) + stripped
      end
      
      # Increase indent after colon
      if stripped.match?(/:$/) && !stripped.start_with?('#')
        indent_level += 1
      end
      
      # Decrease after return/break/continue/pass
      if stripped.match?(/^(return|break|continue|pass)/)
        indent_level = [indent_level - 1, 0].max
      end
    end
    
    fixed_lines.join("\n")
  end

  def fix_common_syntax_errors(code, issue, language)
    case language
    when :ruby
      fix_ruby_syntax_errors(code)
    when :javascript
      fix_javascript_syntax_errors(code)
    when :python
      fix_python_syntax_errors(code)
    else
      code
    end
  end

  def fix_ruby_syntax_errors(code)
    fixed = code
    
    # Fix missing 'end' keywords
    def_count = fixed.scan(/^\s*def\s+/).size
    end_count = fixed.scan(/^\s*end\s*$/).size
    
    if def_count > end_count
      (def_count - end_count).times do
        fixed += "\nend"
      end
    end
    
    # Fix missing 'do' in blocks
    fixed = fixed.gsub(/(\.|^)\s*each\s*{\s*\|/, '\1each do |')
    fixed = fixed.gsub(/}\s*$/, 'end') if fixed.match?(/\.\s*each\s+do/)
    
    fixed
  end

  def fix_javascript_syntax_errors(code)
    fixed = code
    
    # Balance braces
    open_braces = fixed.count('{')
    close_braces = fixed.count('}')
    
    if open_braces > close_braces
      (open_braces - close_braces).times do
        fixed += "\n}"
      end
    end
    
    # Fix arrow function syntax
    fixed = fixed.gsub(/=>\s*{\s*([^}]+)\s*}\s*;/, '=> { \1 }')
    
    # Add missing semicolons for variable declarations
    fixed = fixed.gsub(/^(\s*(?:let|const|var)\s+\w+\s*=\s*[^;]+)$/m, '\1;')
    
    fixed
  end

  def fix_python_syntax_errors(code)
    fixed = code
    
    # Fix common f-string errors
    fixed = fixed.gsub(/f"([^"]*){([^}]*)}"/, 'f"\1{\2}"')
    
    # Ensure proper indentation after function definitions
    lines = fixed.split("\n")
    fixed_lines = []
    
    lines.each_with_index do |line, i|
      fixed_lines << line
      
      # If this line defines a function/class and next line isn't indented
      if line.match?(/^(def|class)\s+\w+.*:$/) && 
         lines[i + 1] && !lines[i + 1].match?(/^\s+/)
        # Add a pass statement if needed
        fixed_lines << "    pass"
      end
    end
    
    fixed_lines.join("\n")
  end
end

# Module for tracking fix effectiveness
module FixEffectiveness
  class Tracker
    def initialize
      @fix_stats = Hash.new { |h, k| h[k] = { attempts: 0, successes: 0 } }
    end

    def record_fix_attempt(issue_type, success)
      @fix_stats[issue_type][:attempts] += 1
      @fix_stats[issue_type][:successes] += 1 if success
    end

    def effectiveness_rate(issue_type)
      stats = @fix_stats[issue_type]
      return 0.0 if stats[:attempts] == 0
      
      stats[:successes].to_f / stats[:attempts]
    end

    def report
      report = "Auto-Fix Effectiveness Report:\n"
      report += "-" * 40 + "\n"
      
      @fix_stats.each do |issue_type, stats|
        rate = effectiveness_rate(issue_type)
        report += "#{issue_type}: #{(rate * 100).round(1)}% (#{stats[:successes]}/#{stats[:attempts]})\n"
      end
      
      report
    end
  end
end