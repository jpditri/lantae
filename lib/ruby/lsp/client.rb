require 'json'
require 'uri'
require 'open3'
require 'logger'

module Lantae
  module LSP
    class Client
      attr_reader :server_process, :logger, :capabilities

      def initialize(server_command = nil, logger: nil)
        @server_command = server_command || default_server_command
        @logger = logger || Logger.new(STDERR)
        @request_id = 0
        @pending_requests = {}
        @capabilities = {}
        @documents = {}
        @diagnostics_received = {}
        @running = false
        
        @logger.info "LSP Client initialized with command: #{@server_command}"
      end

      def start
        return if @running
        
        @logger.info "Starting LSP server..."
        
        @stdin, @stdout, @stderr, @server_process = Open3.popen3(*@server_command)
        @running = true
        
        # Start reader thread
        @reader_thread = Thread.new { read_messages }
        
        # Initialize connection
        initialize_connection
        
        true
      rescue => e
        @logger.error "Failed to start LSP server: #{e.message}"
        false
      end

      def stop
        return unless @running
        
        @logger.info "Stopping LSP server..."
        
        # Send shutdown request
        shutdown_id = send_request('shutdown', {})
        
        # Wait for response (with timeout)
        wait_for_response(shutdown_id, timeout: 5)
        
        # Send exit notification
        send_notification('exit', {})
        
        # Clean up
        @running = false
        @stdin&.close
        @stdout&.close
        @stderr&.close
        if @server_process
          @server_process.terminate
          @server_process.wait rescue nil
          @server_process = nil
        end
        @reader_thread&.join(5)
        
        @logger.info "LSP server stopped"
      end

      def open_file(file_path, content, language_id = nil)
        uri = file_uri(file_path)
        language_id ||= detect_language(file_path)
        
        # Track document locally
        @documents[uri] = {
          content: content,
          language: language_id,
          version: 1
        }
        
        send_notification('textDocument/didOpen', {
          textDocument: {
            uri: uri,
            languageId: language_id,
            version: 1,
            text: content
          }
        })
      end

      def change_file(file_path, changes, version)
        uri = file_uri(file_path)
        
        send_notification('textDocument/didChange', {
          textDocument: {
            uri: uri,
            version: version
          },
          contentChanges: changes
        })
      end

      def close_file(file_path)
        uri = file_uri(file_path)
        
        send_notification('textDocument/didClose', {
          textDocument: {
            uri: uri
          }
        })
      end

      def get_completions(file_path, line, character)
        uri = file_uri(file_path)
        
        response = send_request_sync('textDocument/completion', {
          textDocument: { uri: uri },
          position: { line: line, character: character }
        })
        
        response&.dig('result', 'items') || []
      end

      def get_hover(file_path, line, character)
        uri = file_uri(file_path)
        
        response = send_request_sync('textDocument/hover', {
          textDocument: { uri: uri },
          position: { line: line, character: character }
        })
        
        response&.dig('result')
      end

      def get_definition(file_path, line, character)
        uri = file_uri(file_path)
        
        response = send_request_sync('textDocument/definition', {
          textDocument: { uri: uri },
          position: { line: line, character: character }
        })
        
        response&.dig('result')
      end

      def get_code_actions(file_path, start_line, start_char, end_line, end_char, diagnostics = [])
        uri = file_uri(file_path)
        
        response = send_request_sync('textDocument/codeAction', {
          textDocument: { uri: uri },
          range: {
            start: { line: start_line, character: start_char },
            end: { line: end_line, character: end_char }
          },
          context: {
            diagnostics: diagnostics
          }
        })
        
        response&.dig('result') || []
      end

      def format_document(file_path, tab_size: 2, insert_spaces: true)
        uri = file_uri(file_path)
        
        response = send_request_sync('textDocument/formatting', {
          textDocument: { uri: uri },
          options: {
            tabSize: tab_size,
            insertSpaces: insert_spaces
          }
        })
        
        response&.dig('result') || []
      end

      def analyze_file(file_path)
        content = File.read(file_path)
        language = detect_language(file_path)
        uri = file_uri(file_path)
        
        # Open the file temporarily
        open_file(file_path, content, language)
        
        # Wait for diagnostics
        sleep 0.1
        
        # Get latest diagnostics
        diagnostics = @diagnostics_received&.dig(uri) || []
        
        {
          uri: uri,
          language: language,
          diagnostics: diagnostics
        }
      end

      def execute_command(command, arguments = [])
        send_request_sync('workspace/executeCommand', {
          command: command,
          arguments: arguments
        })
      end

      # AI-powered features for Lantae
      
      def refactor_code(file_path, range)
        execute_command('lantae.refactorCode', [file_path, range])
      end

      def optimize_code(file_path, range)
        execute_command('lantae.optimizeCode', [file_path, range])
      end

      def generate_tests(file_path)
        execute_command('lantae.generateTests', [file_path])
      end

      def add_documentation(file_path, range)
        execute_command('lantae.addDocumentation', [file_path, range])
      end

      private

      def default_server_command
        # Try to find the LSP server
        if File.exist?('bin/lantae-lsp')
          ['ruby', 'bin/lantae-lsp']
        else
          ['ruby', File.join(__dir__, 'server_runner.rb')]
        end
      end

      def initialize_connection
        response = send_request_sync('initialize', {
          processId: Process.pid,
          clientInfo: {
            name: 'Lantae CLI',
            version: '1.0.0'
          },
          capabilities: {
            textDocument: {
              synchronization: {
                dynamicRegistration: false,
                willSave: false,
                willSaveWaitUntil: false,
                didSave: true
              },
              completion: {
                dynamicRegistration: false,
                completionItem: {
                  snippetSupport: true,
                  commitCharactersSupport: true,
                  documentationFormat: ['markdown', 'plaintext']
                }
              },
              hover: {
                dynamicRegistration: false,
                contentFormat: ['markdown', 'plaintext']
              },
              definition: {
                dynamicRegistration: false
              },
              codeAction: {
                dynamicRegistration: false,
                codeActionLiteralSupport: {
                  codeActionKind: {
                    valueSet: ['quickfix', 'refactor', 'refactor.extract', 'refactor.inline', 'refactor.rewrite', 'source']
                  }
                }
              },
              formatting: {
                dynamicRegistration: false
              }
            },
            workspace: {
              applyEdit: true,
              workspaceEdit: {
                documentChanges: true
              },
              symbol: {
                dynamicRegistration: false
              },
              executeCommand: {
                dynamicRegistration: false
              }
            }
          },
          rootUri: file_uri(Dir.pwd),
          workspaceFolders: [{
            uri: file_uri(Dir.pwd),
            name: File.basename(Dir.pwd)
          }]
        })
        
        @capabilities = response&.dig('result', 'capabilities') || {}
        
        # Send initialized notification
        send_notification('initialized', {})
        
        @logger.info "LSP connection initialized with capabilities: #{@capabilities.keys.join(', ')}"
      end

      def send_request(method, params)
        request_id = next_request_id
        
        message = {
          jsonrpc: '2.0',
          id: request_id,
          method: method,
          params: params
        }
        
        send_message(message)
        request_id
      end

      def send_request_sync(method, params, timeout: 10)
        request_id = send_request(method, params)
        wait_for_response(request_id, timeout: timeout)
      end

      def send_notification(method, params)
        message = {
          jsonrpc: '2.0',
          method: method,
          params: params
        }
        
        send_message(message)
      end

      def send_message(message)
        content = JSON.generate(message)
        headers = "Content-Length: #{content.bytesize}\r\n\r\n"
        
        @stdin.write(headers)
        @stdin.write(content)
        @stdin.flush
        
        @logger.debug "Sent: #{message[:method] || "response to #{message[:id]}"}"
      rescue => e
        @logger.error "Failed to send message: #{e.message}"
      end

      def read_messages
        buffer = ""
        
        while @running
          begin
            # Read headers
            headers = {}
            while (line = @stdout.gets)
              line = line.strip
              break if line.empty?
              
              if line =~ /^([^:]+):\s*(.+)$/
                headers[$1] = $2
              end
            end
            
            next unless headers['Content-Length']
            
            # Read content
            content_length = headers['Content-Length'].to_i
            content = @stdout.read(content_length)
            
            # Parse message
            message = JSON.parse(content)
            handle_message(message)
            
          rescue EOFError
            @logger.info "LSP server closed connection"
            break
          rescue => e
            @logger.error "Error reading message: #{e.message}"
          end
        end
      end

      def handle_message(message)
        @logger.debug "Received: #{message['method'] || "response to #{message['id']}"}"
        
        if message['id'] && !message['method']
          # Response to our request
          if (callback = @pending_requests.delete(message['id']))
            callback.call(message)
          end
        elsif message['method']
          # Request or notification from server
          handle_server_message(message)
        end
      end

      def handle_server_message(message)
        case message['method']
        when 'textDocument/publishDiagnostics'
          # Handle diagnostics
          uri = message.dig('params', 'uri')
          diagnostics = message.dig('params', 'diagnostics') || []
          @diagnostics_received[uri] = diagnostics
          @logger.info "Diagnostics for #{uri}: #{diagnostics.size} issues"
        when 'window/showMessage'
          # Handle messages
          type = message.dig('params', 'type')
          text = message.dig('params', 'message')
          @logger.info "Server message: #{text}"
        when 'window/logMessage'
          # Handle log messages
          type = message.dig('params', 'type')
          text = message.dig('params', 'message')
          @logger.debug "Server log: #{text}"
        end
      end

      def wait_for_response(request_id, timeout: 10)
        response = nil
        mutex = Mutex.new
        condition = ConditionVariable.new
        
        @pending_requests[request_id] = lambda do |msg|
          mutex.synchronize do
            response = msg
            condition.signal
          end
        end
        
        mutex.synchronize do
          condition.wait(mutex, timeout)
        end
        
        @pending_requests.delete(request_id)
        response
      end

      def next_request_id
        @request_id += 1
      end

      def file_uri(path)
        "file://#{File.expand_path(path)}"
      end

      def detect_language(file_path)
        case File.extname(file_path).downcase
        when '.rb' then 'ruby'
        when '.py' then 'python'
        when '.js' then 'javascript'
        when '.ts' then 'typescript'
        when '.go' then 'go'
        when '.rs' then 'rust'
        when '.java' then 'java'
        when '.cpp', '.cc', '.cxx' then 'cpp'
        when '.c' then 'c'
        when '.h', '.hpp' then 'cpp'
        when '.cs' then 'csharp'
        when '.php' then 'php'
        when '.sql' then 'sql'
        when '.sh', '.bash' then 'shellscript'
        when '.yaml', '.yml' then 'yaml'
        when '.json' then 'json'
        when '.xml' then 'xml'
        when '.html', '.htm' then 'html'
        when '.css' then 'css'
        when '.md' then 'markdown'
        when '.lisp', '.el', '.scm' then 'lisp'
        else 'plaintext'
        end
      end
    end
  end
end