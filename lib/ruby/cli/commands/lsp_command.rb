module Lantae
  module CLI
    module Commands
      class LspCommand < BaseCommand
        def initialize
          super(
            'lsp',
            'Manage Lantae LSP server',
            'lsp [start|stop|restart|status] [--port PORT] [--foreground]'
          )
        end

        def execute(args, context = {})
          action = args.first || 'status'
          options = parse_options(args[1..])

          case action.downcase
          when 'start'
            start_server(options)
          when 'stop'
            stop_server
          when 'restart'
            restart_server(options)
          when 'status'
            show_status
          when 'auto-start'
            auto_start(options)
          else
            error("Unknown LSP action: #{action}")
            show_usage
            false
          end
        end

        def complete(args, context = {})
          if args.length <= 1
            ['start', 'stop', 'restart', 'status', 'auto-start']
          elsif args.length == 2
            case args.first
            when 'start', 'restart', 'auto-start'
              ['--port', '--foreground', '--background']
            else
              []
            end
          else
            []
          end
        end

        private

        def start_server(options)
          port = options[:port] || Lantae::LSP::ServerManager::DEFAULT_PORT
          background = !options[:foreground]

          if Lantae::LSP::ServerManager.start(port: port, background: background)
            success("LSP server started on port #{port}")
            if background
              info("Use 'lantae lsp status' to check server status")
              info("Logs: #{Lantae::LSP::ServerManager::LOGFILE_PATH}")
            end
            true
          else
            error("Failed to start LSP server")
            false
          end
        end

        def stop_server
          if Lantae::LSP::ServerManager.stop
            success("LSP server stopped")
            true
          else
            error("Failed to stop LSP server")
            false
          end
        end

        def restart_server(options)
          port = options[:port] || Lantae::LSP::ServerManager::DEFAULT_PORT
          
          info("Restarting LSP server...")
          if Lantae::LSP::ServerManager.restart(port: port)
            success("LSP server restarted on port #{port}")
            true
          else
            error("Failed to restart LSP server")
            false
          end
        end

        def show_status
          Lantae::LSP::ServerManager.status
          true
        end

        def auto_start(options)
          port = options[:port] || Lantae::LSP::ServerManager::DEFAULT_PORT
          
          if Lantae::LSP::ServerManager.auto_start_if_needed(port: port)
            success("LSP server is available")
            true
          else
            error("Failed to ensure LSP server availability")
            false
          end
        end

        def parse_options(args)
          options = {}
          i = 0
          
          while i < args.length
            case args[i]
            when '--port', '-p'
              if i + 1 < args.length
                options[:port] = args[i + 1].to_i
                i += 2
              else
                error("--port requires a value")
                i += 1
              end
            when '--foreground', '-f'
              options[:foreground] = true
              i += 1
            when '--background', '-b'
              options[:foreground] = false
              i += 1
            else
              warning("Unknown option: #{args[i]}")
              i += 1
            end
          end
          
          options
        end

        def show_usage
          puts <<~USAGE
            Usage: lantae lsp <action> [options]
            
            Actions:
              start     Start the LSP server
              stop      Stop the LSP server  
              restart   Restart the LSP server
              status    Show server status
              auto-start Auto-start server if needed
            
            Options:
              --port, -p PORT       Specify port number (default: 7777)
              --foreground, -f      Run in foreground (don't daemonize)
              --background, -b      Run in background (default)
            
            Examples:
              lantae lsp start                    # Start server on default port
              lantae lsp start --port 8888        # Start on custom port
              lantae lsp start --foreground       # Start in foreground
              lantae lsp status                   # Check server status
              lantae lsp stop                     # Stop server
          USAGE
        end
      end
    end
  end
end