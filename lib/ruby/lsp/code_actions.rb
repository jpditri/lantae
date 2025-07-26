require_relative '../provider_manager'
require_relative '../planning_agent'

module Lantae
  module LSP
    class CodeActionProvider
      def initialize(provider_manager, tool_manager)
        @provider_manager = provider_manager
        @tool_manager = tool_manager
        @planning_agent = PlanningAgent.new(provider_manager, tool_manager)
      end

      def get_code_actions(document, range, context)
        actions = []
        
        # Quick fixes for diagnostics
        if context['diagnostics']
          context['diagnostics'].each do |diagnostic|
            actions.concat(get_quick_fixes(document, diagnostic))
          end
        end
        
        # Refactoring actions
        actions.concat(get_refactor_actions(document, range))
        
        # AI-powered actions for Lantae files
        if lantae_generated?(document)
          actions.concat(get_ai_actions(document, range))
        end
        
        actions
      end

      # Made public for testing
      def execute_command(command, arguments)
        case command
        when 'lantae.refactorCode'
          refactor_with_ai(arguments[0], arguments[1])
        when 'lantae.optimizeCode'
          optimize_with_ai(arguments[0], arguments[1])
        when 'lantae.generateTests'
          generate_tests_with_ai(arguments[0])
        when 'lantae.addDocumentation'
          add_documentation_with_ai(arguments[0], arguments[1])
        when 'lantae.analyzeComplexity'
          analyze_complexity_with_ai(arguments[0], arguments[1])
        when 'lantae.extractMethod'
          extract_method(arguments[0], arguments[1])
        when 'lantae.extractVariable'
          extract_variable(arguments[0], arguments[1])
        else
          nil
        end
      end

      private

      def get_quick_fixes(document, diagnostic)
        fixes = []
        
        case diagnostic['code']
        when 'missing_end'
          fixes << {
            title: 'Add missing end keyword',
            kind: 'quickfix',
            diagnostics: [diagnostic],
            edit: {
              changes: {
                document[:uri] => [{
                  range: diagnostic['range'],
                  newText: "#{get_text_at_range(document, diagnostic['range'])}\nend"
                }]
              }
            }
          }
        when 'undefined_variable'
          var_name = extract_variable_name(diagnostic['message'])
          fixes << {
            title: "Define variable '#{var_name}'",
            kind: 'quickfix',
            diagnostics: [diagnostic],
            edit: {
              changes: {
                document[:uri] => [{
                  range: {
                    start: { line: diagnostic['range']['start']['line'], character: 0 },
                    end: { line: diagnostic['range']['start']['line'], character: 0 }
                  },
                  newText: "#{var_name} = nil\n"
                }]
              }
            }
          }
        end
        
        fixes
      end

      def extract_variable_name(message)
        # Extract variable name from error message
        # e.g., "Undefined variable 'foo'" -> "foo"
        if message =~ /['"](\w+)['"]/
          $1
        else
          'variable'
        end
      end

      def get_refactor_actions(document, range)
        actions = []
        
        # Extract method
        if range_spans_multiple_lines?(range)
          actions << {
            title: 'Extract method',
            kind: 'refactor.extract',
            command: {
              title: 'Extract method',
              command: 'lantae.extractMethod',
              arguments: [document[:uri], range]
            }
          }
        end
        
        # Extract variable
        if range_on_single_line?(range)
          actions << {
            title: 'Extract variable',
            kind: 'refactor.extract',
            command: {
              title: 'Extract variable',
              command: 'lantae.extractVariable',
              arguments: [document[:uri], range]
            }
          }
        end
        
        actions
      end

      def get_ai_actions(document, range)
        [
          {
            title: '🤖 Refactor with AI',
            kind: 'refactor.rewrite',
            command: {
              title: 'Refactor with AI',
              command: 'lantae.refactorCode',
              arguments: [document[:uri], range]
            }
          },
          {
            title: '🚀 Optimize performance',
            kind: 'source.optimize',
            command: {
              title: 'Optimize performance',
              command: 'lantae.optimizeCode',
              arguments: [document[:uri], range]
            }
          },
          {
            title: '🧪 Generate tests',
            kind: 'source.generateTests',
            command: {
              title: 'Generate tests',
              command: 'lantae.generateTests',
              arguments: [document[:uri], range]
            }
          },
          {
            title: '📝 Add documentation',
            kind: 'source.addDocumentation',
            command: {
              title: 'Add documentation',
              command: 'lantae.addDocumentation',
              arguments: [document[:uri], range]
            }
          },
          {
            title: '🔍 Analyze complexity',
            kind: 'source.analyze',
            command: {
              title: 'Analyze complexity',
              command: 'lantae.analyzeComplexity',
              arguments: [document[:uri], range]
            }
          }
        ]
      end

      private

      def refactor_with_ai(uri, range)
        document = get_document(uri)
        code = get_text_at_range(document, range)
        
        prompt = <<~PROMPT
          Refactor the following #{document[:language_id]} code to improve readability and maintainability:
          
          ```#{document[:language_id]}
          #{code}
          ```
          
          Provide the refactored code with explanations for the changes.
        PROMPT
        
        response = @provider_manager.chat(prompt)
        
        # Extract code from response
        refactored_code = extract_code_from_response(response)
        
        {
          changes: {
            uri => [{
              range: range,
              newText: refactored_code
            }]
          }
        }
      end

      def optimize_with_ai(uri, range)
        document = get_document(uri)
        code = get_text_at_range(document, range)
        
        prompt = <<~PROMPT
          Optimize the following #{document[:language_id]} code for better performance:
          
          ```#{document[:language_id]}
          #{code}
          ```
          
          Focus on algorithmic improvements and efficient data structures.
        PROMPT
        
        response = @provider_manager.chat(prompt)
        optimized_code = extract_code_from_response(response)
        
        {
          changes: {
            uri => [{
              range: range,
              newText: optimized_code
            }]
          }
        }
      end

      def generate_tests_with_ai(uri)
        document = get_document(uri)
        
        prompt = <<~PROMPT
          Generate comprehensive test cases for the following #{document[:language_id]} code:
          
          ```#{document[:language_id]}
          #{document[:content]}
          ```
          
          Use the appropriate testing framework for #{document[:language_id]}.
        PROMPT
        
        response = @provider_manager.chat(prompt)
        test_code = extract_code_from_response(response)
        
        # Create test file
        test_file_path = generate_test_file_path(uri)
        
        {
          documentChanges: [{
            kind: 'create',
            uri: test_file_path,
            options: { overwrite: false, ignoreIfExists: true }
          }, {
            textDocument: { uri: test_file_path, version: 1 },
            edits: [{
              range: { start: { line: 0, character: 0 }, end: { line: 0, character: 0 } },
              newText: test_code
            }]
          }]
        }
      end

      def add_documentation_with_ai(uri, range)
        document = get_document(uri)
        code = get_text_at_range(document, range)
        
        prompt = <<~PROMPT
          Add comprehensive documentation to the following #{document[:language_id]} code:
          
          ```#{document[:language_id]}
          #{code}
          ```
          
          Use the appropriate documentation style for #{document[:language_id]}.
        PROMPT
        
        response = @provider_manager.chat(prompt)
        documented_code = extract_code_from_response(response)
        
        {
          changes: {
            uri => [{
              range: range,
              newText: documented_code
            }]
          }
        }
      end

      def analyze_complexity_with_ai(uri, range)
        document = get_document(uri)
        code = get_text_at_range(document, range)
        
        prompt = <<~PROMPT
          Analyze the complexity of the following #{document[:language_id]} code:
          
          ```#{document[:language_id]}
          #{code}
          ```
          
          Provide cyclomatic complexity, time complexity, and space complexity analysis.
        PROMPT
        
        response = @provider_manager.chat(prompt)
        
        # Return as a comment at the top of the selection
        analysis_comment = case document[:language_id]
        when 'ruby', 'python'
          "# #{response.gsub("\n", "\n# ")}\n\n"
        when 'javascript', 'typescript', 'java', 'go', 'rust'
          "// #{response.gsub("\n", "\n// ")}\n\n"
        else
          "#{response}\n\n"
        end
        
        {
          changes: {
            uri => [{
              range: {
                start: range['start'],
                end: range['start']
              },
              newText: analysis_comment
            }]
          }
        }
      end

      def extract_method(uri, range)
        # Placeholder for extract method refactoring
        # In a real implementation, this would analyze the code and extract it
        nil
      end

      def extract_variable(uri, range)
        # Placeholder for extract variable refactoring
        # In a real implementation, this would analyze the expression and extract it
        nil
      end

      def lantae_generated?(document)
        document[:content].include?('Generated by Lantae AI') ||
          document[:content].include?('_lantae_metadata')
      end

      def range_spans_multiple_lines?(range)
        range['start']['line'] != range['end']['line']
      end

      def range_on_single_line?(range)
        range['start']['line'] == range['end']['line']
      end

      def get_text_at_range(document, range)
        lines = document[:content].split("\n")
        
        if range['start']['line'] == range['end']['line']
          line = lines[range['start']['line']]
          line[range['start']['character']...range['end']['character']]
        else
          # Multi-line selection
          result = []
          (range['start']['line']..range['end']['line']).each do |line_num|
            if line_num == range['start']['line']
              result << lines[line_num][range['start']['character']..-1]
            elsif line_num == range['end']['line']
              result << lines[line_num][0...range['end']['character']]
            else
              result << lines[line_num]
            end
          end
          result.join("\n")
        end
      end

      def extract_code_from_response(response)
        # Extract code blocks from AI response
        if response =~ /```\w*\n(.*?)\n```/m
          $1
        else
          response
        end
      end

      def generate_test_file_path(uri)
        path = uri.sub('file://', '')
        dir = File.dirname(path)
        basename = File.basename(path, '.*')
        ext = File.extname(path)
        
        # Generate test file name based on language conventions
        case ext
        when '.rb'
          "file://#{dir}/#{basename}_spec.rb"
        when '.py'
          "file://#{dir}/test_#{basename}.py"
        when '.js', '.ts'
          "file://#{dir}/#{basename}.test#{ext}"
        when '.go'
          "file://#{dir}/#{basename}_test.go"
        else
          "file://#{dir}/#{basename}_test#{ext}"
        end
      end

      def get_document(uri)
        # This would normally get the document from the LSP server's document store
        # For now, we'll read from disk
        path = uri.sub('file://', '')
        {
          uri: uri,
          content: File.read(path),
          language_id: detect_language(path)
        }
      end

      def detect_language(path)
        case File.extname(path).downcase
        when '.rb' then 'ruby'
        when '.py' then 'python'
        when '.js' then 'javascript'
        when '.ts' then 'typescript'
        when '.go' then 'go'
        when '.rs' then 'rust'
        else 'plaintext'
        end
      end
    end
  end
end