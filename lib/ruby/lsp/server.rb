require 'json'
require 'logger'
require 'socket'
require 'stringio'
require_relative '../provider_manager'
require_relative '../tool_manager'
require_relative '../planning_agent'
require_relative 'code_actions'

module Lantae
  module LSP
    class Server
      METHODS = {
        'initialize' => :handle_initialize,
        'initialized' => :handle_initialized,
        'textDocument/didOpen' => :handle_did_open,
        'textDocument/didChange' => :handle_did_change,
        'textDocument/completion' => :handle_completion,
        'textDocument/hover' => :handle_hover,
        'textDocument/definition' => :handle_definition,
        'textDocument/codeAction' => :handle_code_action,
        'textDocument/formatting' => :handle_formatting,
        'textDocument/diagnostics' => :handle_diagnostics,
        '$/cancelRequest' => :handle_cancel,
        'shutdown' => :handle_shutdown,
        'exit' => :handle_exit
      }.freeze

      attr_reader :logger, :documents, :capabilities, :provider_manager

      def initialize(input = STDIN, output = STDOUT, error = STDERR)
        @input = input
        @output = output
        @error = error
        @logger = Logger.new(error)
        @documents = {}
        @capabilities = {}
        @running = false
        @shutdown_requested = false
        
        # Initialize AI providers for enhanced features
        @provider_manager = ProviderManager.new
        @provider_manager.ensure_provider(:ollama, model: 'cogito:latest')
        
        # Tool manager for code actions
        @tool_manager = ToolManager.new
        
        # Planning agent for complex refactoring
        @planning_agent = PlanningAgent.new(@provider_manager, @tool_manager)
        
        # Code action provider for AI features
        @code_action_provider = CodeActionProvider.new(@provider_manager, @tool_manager)
        
        logger.info "Lantae LSP Server initialized"
      end

      def run
        @running = true
        logger.info "Starting LSP server..."
        
        while @running && !@input.eof?
          begin
            handle_message
          rescue => e
            logger.error "Error handling message: #{e.message}"
            logger.error e.backtrace.join("\n")
          end
        end
        
        logger.info "LSP server stopped"
      end

      private

      def handle_message
        # Read Content-Length header
        headers = {}
        while (line = @input.gets)
          line = line.strip
          break if line.empty?
          
          if line =~ /^([^:]+):\s*(.+)$/
            headers[$1] = $2
          end
        end
        
        return unless headers['Content-Length']
        
        # Read message content
        content_length = headers['Content-Length'].to_i
        content = @input.read(content_length)
        
        # Parse JSON-RPC message
        begin
          message = JSON.parse(content)
        rescue JSON::ParserError => e
          logger.error "Failed to parse JSON: #{e.message}"
          return
        end
        
        logger.debug "Received: #{message['method'] || 'response'}"
        
        if message['method']
          handle_request(message)
        else
          # Handle response (if needed)
        end
      end

      def handle_request(message)
        method = message['method']
        handler = METHODS[method]
        
        if handler
          result = send(handler, message['params'] || {})
          
          if message['id']
            send_response(message['id'], result)
          end
        else
          logger.warn "Unknown method: #{method}"
          if message['id']
            send_error(message['id'], -32601, "Method not found: #{method}")
          end
        end
      end

      def send_response(id, result)
        response = {
          jsonrpc: '2.0',
          id: id,
          result: result
        }
        send_message(response)
      end

      def send_error(id, code, message, data = nil)
        response = {
          jsonrpc: '2.0',
          id: id,
          error: {
            code: code,
            message: message
          }
        }
        response[:error][:data] = data if data
        send_message(response)
      end

      def send_notification(method, params)
        notification = {
          jsonrpc: '2.0',
          method: method,
          params: params
        }
        send_message(notification)
      end

      def send_message(message)
        content = JSON.generate(message)
        headers = "Content-Length: #{content.bytesize}\r\n\r\n"
        
        @output.write(headers)
        @output.write(content)
        @output.flush
        
        logger.debug "Sent: #{message[:method] || 'response'}"
      end

      # LSP Method Handlers

      def handle_initialize(params)
        @capabilities = params['capabilities'] || {}
        
        # Server capabilities
        {
          capabilities: {
            textDocumentSync: {
              openClose: true,
              change: 2, # Incremental
              save: true
            },
            completionProvider: {
              resolveProvider: true,
              triggerCharacters: ['.', ':', '@', '#', '$', '/', '\\']
            },
            hoverProvider: true,
            definitionProvider: true,
            codeActionProvider: {
              codeActionKinds: [
                'quickfix',
                'refactor',
                'refactor.extract',
                'refactor.inline',
                'refactor.rewrite',
                'source.organize'
              ]
            },
            documentFormattingProvider: true,
            documentSymbolProvider: true,
            workspaceSymbolProvider: true
          },
          serverInfo: {
            name: 'Lantae LSP Server',
            version: '1.0.0'
          }
        }
      end

      def handle_initialized(params)
        logger.info "Client initialized"
        
        # Register dynamic capabilities if supported
        if @capabilities.dig('workspace', 'didChangeConfiguration', 'dynamicRegistration')
          # Register for configuration changes
        end
      end

      def handle_did_open(params)
        doc = params['textDocument']
        uri = doc['uri']
        
        @documents[uri] = {
          uri: uri,
          language_id: doc['languageId'],
          version: doc['version'],
          content: doc['text']
        }
        
        logger.info "Opened document: #{uri}"
        
        # Analyze document and send diagnostics
        analyze_document(uri)
      end

      def handle_did_change(params)
        uri = params['textDocument']['uri']
        version = params['textDocument']['version']
        changes = params['contentChanges']
        
        if @documents[uri]
          @documents[uri][:version] = version
          
          changes.each do |change|
            if change['range']
              # Incremental change
              apply_incremental_change(@documents[uri], change)
            else
              # Full content update
              @documents[uri][:content] = change['text']
            end
          end
          
          # Re-analyze document
          analyze_document(uri)
        end
      end

      def handle_completion(params)
        uri = params['textDocument']['uri']
        position = params['position']
        context = params['context']
        
        doc = @documents[uri]
        return { isIncomplete: false, items: [] } unless doc
        
        # Get completions based on context
        items = []
        
        # AI-powered completions for Lantae-generated files
        if lantae_generated?(uri, doc[:content])
          items.concat(ai_completions(doc, position))
        end
        
        # Language-specific completions
        items.concat(language_completions(doc, position))
        
        {
          isIncomplete: false,
          items: items
        }
      end

      def handle_hover(params)
        uri = params['textDocument']['uri']
        position = params['position']
        
        doc = @documents[uri]
        return nil unless doc
        
        # Get word at position
        word = word_at_position(doc[:content], position)
        return nil unless word
        
        contents = []
        
        # Add Lantae metadata if applicable
        if lantae_generated?(uri, doc[:content])
          contents << {
            kind: 'markdown',
            value: "**Generated by Lantae AI** 🤖\n\nThis code was automatically generated."
          }
        end
        
        # Add language-specific information
        case doc[:language_id]
        when 'ruby'
          contents.concat(ruby_hover_info(word))
        when 'python'
          contents.concat(python_hover_info(word))
        when 'javascript', 'typescript'
          contents.concat(js_hover_info(word))
        end
        
        return nil if contents.empty?
        
        {
          contents: contents,
          range: word_range_at_position(doc[:content], position)
        }
      end

      def handle_definition(params)
        uri = params['textDocument']['uri']
        position = params['position']
        
        doc = @documents[uri]
        return nil unless doc
        
        # Find definition locations
        definitions = find_definitions(doc, position)
        
        definitions.empty? ? nil : definitions
      end

      def handle_code_action(params)
        uri = params['textDocument']['uri']
        range = params['range']
        context = params['context']
        
        doc = @documents[uri]
        return [] unless doc
        
        # Use the code action provider
        @code_action_provider.get_code_actions(doc, range, context)
      end

      def handle_formatting(params)
        uri = params['textDocument']['uri']
        options = params['options']
        
        doc = @documents[uri]
        return [] unless doc
        
        # Format document based on language
        formatted = format_document(doc, options)
        
        return [] if formatted == doc[:content]
        
        [{
          range: {
            start: { line: 0, character: 0 },
            end: position_at_end(doc[:content])
          },
          newText: formatted
        }]
      end

      def format_document(doc, options)
        # Simple formatting based on language
        # In a real implementation, this would use language-specific formatters
        case doc[:language_id]
        when 'ruby'
          # Basic Ruby formatting
          doc[:content].gsub(/\s+$/, '') # Remove trailing whitespace
        when 'python'
          # Basic Python formatting
          doc[:content].gsub(/\s+$/, '')
        else
          doc[:content]
        end
      end

      def handle_diagnostics(params)
        uri = params['textDocument']['uri']
        
        doc = @documents[uri]
        return [] unless doc
        
        analyze_document(uri)
      end

      def handle_cancel(params)
        # Cancel in-progress request
        request_id = params['id']
        logger.info "Cancelling request: #{request_id}"
      end

      def handle_shutdown(params)
        @shutdown_requested = true
        logger.info "Shutdown requested"
        nil
      end

      def handle_exit(params)
        exit_code = @shutdown_requested ? 0 : 1
        @running = false
        logger.info "Exiting with code: #{exit_code}"
        exit(exit_code)
      end

      # Helper methods

      def lantae_generated?(uri, content)
        # Check if file has Lantae metadata
        content.include?('Generated by Lantae AI') ||
          content.include?('_lantae_metadata') ||
          content.match?(/Context:.*lantae/i) ||
          File.exist?(File.join(File.dirname(uri.sub('file://', '')), '.lantae-generated.json'))
      end

      def ai_completions(doc, position)
        [
          {
            label: '🤖 Lantae: Refactor this code',
            kind: 1, # Text
            detail: 'Use AI to refactor the selected code',
            insertText: '',
            command: {
              title: 'Refactor with AI',
              command: 'lantae.refactorCode'
            }
          },
          {
            label: '🤖 Lantae: Optimize performance',
            kind: 1,
            detail: 'Get AI suggestions for performance improvements',
            insertText: '',
            command: {
              title: 'Optimize with AI',
              command: 'lantae.optimizeCode'
            }
          },
          {
            label: '🤖 Lantae: Generate tests',
            kind: 1,
            detail: 'Generate comprehensive test cases',
            insertText: '',
            command: {
              title: 'Generate tests',
              command: 'lantae.generateTests'
            }
          },
          {
            label: '🤖 Lantae: Add documentation',
            kind: 1,
            detail: 'Generate documentation for this code',
            insertText: '',
            command: {
              title: 'Add documentation',
              command: 'lantae.addDocumentation'
            }
          }
        ]
      end

      def language_completions(doc, position)
        case doc[:language_id]
        when 'ruby'
          ruby_completions(doc, position)
        when 'python'
          python_completions(doc, position)
        when 'javascript', 'typescript'
          js_completions(doc, position)
        else
          []
        end
      end

      def ruby_completions(doc, position)
        # Basic Ruby completions
        [
          { label: 'def', kind: 14, insertText: "def ${1:method_name}\n  $0\nend" },
          { label: 'class', kind: 7, insertText: "class ${1:ClassName}\n  $0\nend" },
          { label: 'module', kind: 9, insertText: "module ${1:ModuleName}\n  $0\nend" },
          { label: 'if', kind: 14, insertText: "if ${1:condition}\n  $0\nend" },
          { label: 'unless', kind: 14, insertText: "unless ${1:condition}\n  $0\nend" },
          { label: 'each', kind: 2, insertText: "each do |${1:item}|\n  $0\nend" },
          { label: 'map', kind: 2, insertText: "map { |${1:item}| $0 }" }
        ]
      end

      def python_completions(doc, position)
        # Basic Python completions
        [
          { label: 'def', kind: 14, insertText: "def ${1:function_name}(${2:args}):\n    $0" },
          { label: 'class', kind: 7, insertText: "class ${1:ClassName}:\n    $0" },
          { label: 'if', kind: 14, insertText: "if ${1:condition}:\n    $0" },
          { label: 'for', kind: 14, insertText: "for ${1:item} in ${2:items}:\n    $0" },
          { label: 'import', kind: 9, insertText: "import ${1:module}" }
        ]
      end

      def js_completions(doc, position)
        # Basic JavaScript completions
        [
          { label: 'function', kind: 14, insertText: "function ${1:name}(${2:params}) {\n  $0\n}" },
          { label: 'const', kind: 14, insertText: "const ${1:name} = $0" },
          { label: 'let', kind: 14, insertText: "let ${1:name} = $0" },
          { label: 'if', kind: 14, insertText: "if (${1:condition}) {\n  $0\n}" },
          { label: 'for', kind: 14, insertText: "for (${1:init}; ${2:condition}; ${3:increment}) {\n  $0\n}" }
        ]
      end

      def ruby_hover_info(word)
        # Basic Ruby hover information
        case word
        when 'def'
          [{ kind: 'markdown', value: '**Ruby Method Definition**\n\nDefines a new method.' }]
        when 'class'
          [{ kind: 'markdown', value: '**Ruby Class Definition**\n\nDefines a new class.' }]
        when 'module'
          [{ kind: 'markdown', value: '**Ruby Module Definition**\n\nDefines a new module.' }]
        else
          # For any other word, provide generic hover info
          [{ kind: 'markdown', value: "**#{word}**\n\nSymbol in Ruby code." }]
        end
      end

      def python_hover_info(word)
        # Basic Python hover information
        case word
        when 'def'
          [{ kind: 'markdown', value: '**Python Function Definition**\n\nDefines a new function.' }]
        when 'class'
          [{ kind: 'markdown', value: '**Python Class Definition**\n\nDefines a new class.' }]
        else
          []
        end
      end

      def js_hover_info(word)
        # Basic JavaScript hover information
        case word
        when 'function'
          [{ kind: 'markdown', value: '**JavaScript Function**\n\nDefines a new function.' }]
        when 'const'
          [{ kind: 'markdown', value: '**JavaScript Constant**\n\nDeclares a block-scoped constant.' }]
        else
          []
        end
      end

      def analyze_document(uri)
        doc = @documents[uri]
        return unless doc
        
        diagnostics = []
        
        # Run language-specific analysis
        case doc[:language_id]
        when 'ruby'
          diagnostics.concat(analyze_ruby(doc))
        when 'python'
          diagnostics.concat(analyze_python(doc))
        when 'javascript', 'typescript'
          diagnostics.concat(analyze_javascript(doc))
        end
        
        # Send diagnostics
        send_notification('textDocument/publishDiagnostics', {
          uri: uri,
          diagnostics: diagnostics
        })
      end

      def analyze_ruby(doc)
        diagnostics = []
        lines = doc[:content].split("\n")
        
        lines.each_with_index do |line, index|
          # Check for common Ruby issues
          if line.match?(/\bputs\s+.*\bpassword\b/i)
            diagnostics << {
              range: {
                start: { line: index, character: 0 },
                end: { line: index, character: line.length }
              },
              severity: 1, # Error
              code: 'security',
              source: 'lantae',
              message: 'Potential security issue: logging sensitive information'
            }
          end
          
          # Check for missing 'end' keywords
          if line.match?(/^\s*(def|class|module|if|unless|while|for)\s+/)
            # Simple heuristic - would need proper parsing for accuracy
          end
        end
        
        diagnostics
      end

      def analyze_python(doc)
        diagnostics = []
        lines = doc[:content].split("\n")
        
        lines.each_with_index do |line, index|
          # Check for common Python issues
          if line.match?(/\bprint\s*\(.*password/i)
            diagnostics << {
              range: {
                start: { line: index, character: 0 },
                end: { line: index, character: line.length }
              },
              severity: 1, # Error
              code: 'security',
              source: 'lantae',
              message: 'Potential security issue: logging sensitive information'
            }
          end
        end
        
        diagnostics
      end

      def analyze_javascript(doc)
        diagnostics = []
        lines = doc[:content].split("\n")
        
        lines.each_with_index do |line, index|
          # Check for common JavaScript issues
          if line.match?(/console\.(log|info|warn|error)\s*\(.*password/i)
            diagnostics << {
              range: {
                start: { line: index, character: 0 },
                end: { line: index, character: line.length }
              },
              severity: 1, # Error
              code: 'security',
              source: 'lantae',
              message: 'Potential security issue: logging sensitive information'
            }
          end
        end
        
        diagnostics
      end

      def word_at_position(content, position)
        lines = content.split("\n")
        return nil if position['line'] >= lines.length
        
        line = lines[position['line']]
        char = position['character']
        
        # Find word boundaries
        start = char
        while start > 0 && line[start - 1] =~ /\w/
          start -= 1
        end
        
        finish = char
        while finish < line.length && line[finish] =~ /\w/
          finish += 1
        end
        
        return nil if start == finish
        
        line[start...finish]
      end

      def position_at_end(content)
        lines = content.split("\n")
        {
          line: lines.length - 1,
          character: lines.last&.length || 0
        }
      end

      def find_definitions(doc, position)
        # Simple definition finding - in a real implementation, this would parse the AST
        word = word_at_position(doc[:content], position)
        return [] unless word
        
        definitions = []
        lines = doc[:content].split("\n")
        
        lines.each_with_index do |line, index|
          # Look for method/function definitions
          case doc[:language_id]
          when 'ruby'
            if line =~ /def\s+#{Regexp.escape(word)}\b/
              definitions << {
                uri: doc[:uri],
                range: {
                  start: { line: index, character: line.index(word) },
                  end: { line: index, character: line.index(word) + word.length }
                }
              }
            end
          when 'python'
            if line =~ /def\s+#{Regexp.escape(word)}\s*\(/
              definitions << {
                uri: doc[:uri],
                range: {
                  start: { line: index, character: line.index(word) },
                  end: { line: index, character: line.index(word) + word.length }
                }
              }
            end
          end
        end
        
        definitions
      end

      def word_range_at_position(content, position)
        lines = content.split("\n")
        return nil if position['line'] >= lines.length
        
        line = lines[position['line']]
        char = position['character']
        
        # Find word boundaries
        start = char
        while start > 0 && line[start - 1] =~ /\w/
          start -= 1
        end
        
        finish = char
        while finish < line.length && line[finish] =~ /\w/
          finish += 1
        end
        
        return nil if start == finish
        
        {
          start: { line: position['line'], character: start },
          end: { line: position['line'], character: finish }
        }
      end

      def apply_incremental_change(doc, change)
        # Apply text change to document
        # This is a simplified implementation
        range = change['range']
        text = change['text']
        
        lines = doc[:content].split("\n")
        
        # Convert to string positions
        start_line = range['start']['line']
        start_char = range['start']['character']
        end_line = range['end']['line']
        end_char = range['end']['character']
        
        # Apply change (simplified)
        if start_line == end_line
          line = lines[start_line] || ""
          lines[start_line] = line[0...start_char].to_s + text + (line[end_char..-1] || "")
        else
          # Multi-line change - more complex
          # For now, just replace with the new text
          lines[start_line] = (lines[start_line] || "")[0...start_char].to_s + text
        end
        
        doc[:content] = lines.join("\n")
      end
    end
  end
end