module Lantae
  module LSP
    # Manages LSP server lifecycle
    class ServerManager
      DEFAULT_PORT = 7777
      PIDFILE_PATH = File.expand_path('~/.lantae/lsp.pid')
      LOGFILE_PATH = File.expand_path('~/.lantae/lsp.log')

      class << self
        def start(port: DEFAULT_PORT, background: true)
          if running?
            current_port = get_running_port
            if current_port == port
              puts "LSP server already running on port #{port}"
              return true
            else
              puts "LSP server running on different port (#{current_port}), stopping first..."
              stop
              sleep 1
            end
          end

          ensure_config_directory

          if background
            start_background(port)
          else
            start_foreground(port)
          end
        end

        def stop
          unless running?
            puts "LSP server is not running"
            return true
          end

          pid = get_pid
          begin
            Process.kill('TERM', pid)
            
            # Wait for graceful shutdown
            timeout = 10
            while timeout > 0 && process_exists?(pid)
              sleep 0.5
              timeout -= 0.5
            end
            
            # Force kill if still running
            if process_exists?(pid)
              Process.kill('KILL', pid)
              sleep 0.5
            end
            
            cleanup_pid_file
            puts "LSP server stopped"
            true
          rescue Errno::ESRCH
            # Process doesn't exist
            cleanup_pid_file
            true
          rescue => e
            puts "Error stopping LSP server: #{e.message}"
            false
          end
        end

        def restart(port: DEFAULT_PORT)
          stop
          sleep 1
          start(port: port)
        end

        def status
          if running?
            pid = get_pid
            port = get_running_port
            puts "LSP server is running (PID: #{pid}, Port: #{port})"
            
            # Test connection
            if test_connection(port)
              puts "Server is responding to requests ✅"
            else
              puts "Server is not responding ⚠️"
            end
          else
            puts "LSP server is not running"
          end
        end

        def running?
          return false unless File.exist?(PIDFILE_PATH)
          
          pid = get_pid
          return false unless pid
          
          process_exists?(pid)
        end

        def get_running_port
          return nil unless running?
          
          # Try to read port from process command line or config
          pid = get_pid
          begin
            # Read process command line on Linux/macOS
            if File.exist?("/proc/#{pid}/cmdline")
              cmdline = File.read("/proc/#{pid}/cmdline").split("\0")
            else
              # Fallback for macOS
              cmdline = `ps -p #{pid} -o command=`.strip.split
            end
            
            port_index = cmdline.index('--port') || cmdline.index('-p')
            if port_index && cmdline[port_index + 1]
              cmdline[port_index + 1].to_i
            else
              DEFAULT_PORT
            end
          rescue
            DEFAULT_PORT
          end
        end

        def ensure_running(port: DEFAULT_PORT)
          unless running?
            puts "Starting LSP server..."
            start(port: port)
          end
        end

        def auto_start_if_needed(port: DEFAULT_PORT)
          # Auto-start LSP server if clients are trying to connect
          unless running?
            if should_auto_start?
              puts "Auto-starting LSP server for client connection..."
              start(port: port, background: true)
              
              # Wait a moment for server to start
              sleep 2
              
              unless running?
                puts "Failed to auto-start LSP server"
                return false
              end
            end
          end
          true
        end

        private

        def start_background(port)
          ensure_config_directory

          # Build command
          cmd = build_server_command(port)
          
          # Start in background with proper daemonization
          pid = Process.spawn(
            *cmd,
            out: LOGFILE_PATH,
            err: LOGFILE_PATH,
            pgroup: true
          )
          
          Process.detach(pid)
          write_pid_file(pid, port)
          
          # Wait a moment and check if it started successfully
          sleep 1
          
          if process_exists?(pid)
            puts "LSP server started (PID: #{pid}, Port: #{port})"
            puts "Logs: #{LOGFILE_PATH}"
            true
          else
            cleanup_pid_file
            puts "Failed to start LSP server"
            puts "Check logs: #{LOGFILE_PATH}"
            false
          end
        end

        def start_foreground(port)
          cmd = build_server_command(port)
          
          puts "Starting LSP server on port #{port}..."
          puts "Command: #{cmd.join(' ')}"
          
          # Use exec to replace current process
          exec(*cmd)
        end

        def build_server_command(port)
          # Get the current Ruby executable and Lantae path
          ruby_exe = RbConfig.ruby
          lantae_lib = File.expand_path('../../', __dir__)
          
          [
            ruby_exe,
            '-I', lantae_lib,
            '-r', 'lantae/lsp/server',
            '-e', "Lantae::LSP::Server.new(port: #{port}).start"
          ]
        end

        def ensure_config_directory
          config_dir = File.dirname(PIDFILE_PATH)
          FileUtils.mkdir_p(config_dir) unless Dir.exist?(config_dir)
        end

        def write_pid_file(pid, port)
          File.write(PIDFILE_PATH, "#{pid}\n#{port}")
        end

        def cleanup_pid_file
          File.delete(PIDFILE_PATH) if File.exist?(PIDFILE_PATH)
        end

        def get_pid
          return nil unless File.exist?(PIDFILE_PATH)
          
          content = File.read(PIDFILE_PATH).strip
          lines = content.split("\n")
          lines.first&.to_i
        end

        def process_exists?(pid)
          return false unless pid
          
          begin
            Process.kill(0, pid)
            true
          rescue Errno::ESRCH
            false
          rescue Errno::EPERM
            true # Process exists but we don't have permission to signal it
          end
        end

        def test_connection(port)
          begin
            require 'net/http'
            require 'json'
            
            uri = URI("http://localhost:#{port}")
            http = Net::HTTP.new(uri.host, uri.port)
            http.read_timeout = 2
            
            # Try a simple LSP initialize request
            request = Net::HTTP::Post.new('/')
            request['Content-Type'] = 'application/vscode-jsonrpc; charset=utf-8'
            request.body = build_lsp_initialize_request
            
            response = http.request(request)
            response.code == '200'
          rescue => e
            false
          end
        end

        def build_lsp_initialize_request
          JSON.dump({
            jsonrpc: '2.0',
            id: 1,
            method: 'initialize',
            params: {
              processId: Process.pid,
              clientInfo: { name: 'lantae-cli', version: '1.0.0' },
              capabilities: {}
            }
          })
        end

        def should_auto_start?
          # Auto-start if:
          # 1. No explicit disable flag is set
          # 2. We're in a development environment
          # 3. Config allows auto-start
          
          return false if ENV['LANTAE_NO_AUTO_LSP']
          
          # Check if we're in a project that might benefit from LSP
          project_indicators = [
            '.git',
            'package.json',
            'Gemfile',
            'requirements.txt',
            'go.mod',
            'Cargo.toml',
            '.lantae'
          ]
          
          project_indicators.any? { |indicator| File.exist?(indicator) }
        end
      end
    end
  end
end