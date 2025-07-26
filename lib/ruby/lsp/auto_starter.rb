module Lantae
  module LSP
    # Smart auto-starter for LSP server based on context
    class AutoStarter
      class << self
        def should_start_for_context?(context = {})
          # Auto-start if:
          # 1. User is working with code files
          # 2. In a development project
          # 3. Using code-related tools
          # 4. IDE extensions are connected
          
          return false if ENV['LANTAE_NO_AUTO_LSP']
          
          # Check for code file activity
          return true if code_files_present?
          
          # Check for development project indicators
          return true if in_development_project?
          
          # Check for code-related tool usage
          return true if using_code_tools?(context)
          
          # Check for IDE extension connection
          return true if ide_extension_connected?(context)
          
          false
        end

        def auto_start_if_beneficial(context = {})
          return false unless should_start_for_context?(context)
          
          unless ServerManager.running?
            puts "🚀 Auto-starting LSP server for enhanced code intelligence..."
            return ServerManager.start(background: true)
          end
          
          true
        end

        def smart_port_selection
          # Try default port first, then find available port
          port = ServerManager::DEFAULT_PORT
          
          return port unless port_in_use?(port)
          
          # Find next available port
          (port + 1..port + 100).each do |candidate_port|
            return candidate_port unless port_in_use?(candidate_port)
          end
          
          # Fallback to random high port
          rand(8000..9999)
        end

        def start_with_smart_detection(options = {})
          context = {
            current_directory: Dir.pwd,
            tools_used: options[:tools_used] || [],
            files_accessed: options[:files_accessed] || [],
            ide_client: options[:ide_client]
          }
          
          if should_start_for_context?(context)
            port = options[:port] || smart_port_selection
            
            puts "🔍 Detected beneficial LSP context, starting server..."
            puts "📁 Project: #{File.basename(Dir.pwd)}"
            puts "🔧 Tools: #{context[:tools_used].join(', ')}" unless context[:tools_used].empty?
            
            ServerManager.start(port: port, background: true)
          else
            false
          end
        end

        private

        def code_files_present?
          # Check for common code file extensions in current directory
          code_extensions = %w[.rb .py .js .ts .jsx .tsx .go .rs .java .cpp .c .h .php .cs .swift .kt]
          
          Dir.glob('**/*').any? do |file|
            next false if File.directory?(file)
            code_extensions.any? { |ext| file.end_with?(ext) }
          end
        end

        def in_development_project?
          # Check for development project indicators
          indicators = [
            '.git',
            '.gitignore',
            'package.json',
            'Gemfile',
            'requirements.txt',
            'go.mod',
            'Cargo.toml',
            'pom.xml',
            'build.gradle',
            'composer.json',
            '.eslintrc',
            '.prettierrc',
            'tsconfig.json',
            'Makefile',
            'CMakeLists.txt',
            '.nvmrc',
            '.ruby-version',
            '.python-version'
          ]
          
          indicators.any? { |indicator| File.exist?(indicator) }
        end

        def using_code_tools?(context)
          code_tools = %w[
            git ruby python node npm yarn bundle pip cargo go
            write_file edit_file create_file cat ls find
            ruby_script python_script bash
          ]
          
          tools_used = context[:tools_used] || []
          tools_used.any? { |tool| code_tools.include?(tool) }
        end

        def ide_extension_connected?(context)
          # Check if VS Code, Vim, or other IDE extensions are connected
          return true if context[:ide_client]
          
          # Check for VS Code workspace
          return true if File.exist?('.vscode/settings.json')
          
          # Check for Vim/Neovim config in project
          return true if File.exist?('.vim') || File.exist?('.nvim')
          
          # Check environment variables that might indicate IDE usage
          return true if ENV['VSCODE_PID'] || ENV['NVIM_LISTEN_ADDRESS']
          
          false
        end

        def port_in_use?(port)
          require 'socket'
          
          begin
            server = TCPServer.new('localhost', port)
            server.close
            false
          rescue Errno::EADDRINUSE
            true
          rescue => e
            # If we can't determine, assume it's available
            false
          end
        end
      end
    end
  end
end