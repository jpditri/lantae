require 'yaml'
require 'fileutils'

module Lantae
  class PluginSystem
    DEFAULT_DIR = File.expand_path('~/.lantae/plugins')
    
    def initialize(plugin_dir = DEFAULT_DIR)
      @plugin_dir = plugin_dir
      @plugins = {}
      @hooks = {
        before_prompt: [],
        after_response: [],
        on_command: [],
        on_error: [],
        on_startup: [],
        on_shutdown: []
      }
      
      FileUtils.mkdir_p(@plugin_dir)
      load_plugins
    end
    
    def load_plugins
      # Load built-in plugins
      load_builtin_plugins
      
      # Load user plugins
      Dir.glob(File.join(@plugin_dir, '*.rb')).each do |plugin_file|
        load_plugin(plugin_file)
      end
      
      # Run startup hooks
      execute_hook(:on_startup)
    end
    
    def load_plugin(plugin_file)
      begin
        plugin_name = File.basename(plugin_file, '.rb')
        
        # Create isolated context for plugin
        plugin_module = Module.new
        plugin_module.module_eval(File.read(plugin_file), plugin_file)
        
        # Look for plugin class
        plugin_class = plugin_module.constants
          .map { |c| plugin_module.const_get(c) }
          .find { |c| c.is_a?(Class) && c.ancestors.include?(Plugin) }
        
        if plugin_class
          plugin_instance = plugin_class.new
          @plugins[plugin_name] = plugin_instance
          
          # Register hooks
          register_plugin_hooks(plugin_instance)
          
          puts "✅ Loaded plugin: #{plugin_name}" if ENV['DEBUG']
        else
          puts "⚠️  No valid plugin class found in #{plugin_file}"
        end
        
      rescue => e
        puts "❌ Failed to load plugin #{plugin_file}: #{e.message}"
      end
    end
    
    def register_plugin_hooks(plugin)
      @hooks.each_key do |hook_name|
        if plugin.respond_to?(hook_name)
          @hooks[hook_name] << plugin
        end
      end
    end
    
    def execute_hook(hook_name, *args)
      results = []
      
      @hooks[hook_name].each do |plugin|
        begin
          result = plugin.send(hook_name, *args)
          results << result if result
        rescue => e
          puts "Plugin error in #{hook_name}: #{e.message}" if ENV['DEBUG']
        end
      end
      
      results
    end
    
    def list_plugins
      @plugins.map do |name, plugin|
        {
          name: name,
          version: plugin.version,
          description: plugin.description,
          author: plugin.author,
          hooks: @hooks.select { |_, plugins| plugins.include?(plugin) }.keys
        }
      end
    end
    
    def enable_plugin(name)
      plugin_file = File.join(@plugin_dir, "#{name}.rb")
      
      if File.exist?(plugin_file)
        load_plugin(plugin_file)
        true
      else
        false
      end
    end
    
    def disable_plugin(name)
      if plugin = @plugins[name]
        # Remove from hooks
        @hooks.each_value { |plugins| plugins.delete(plugin) }
        
        # Call cleanup if available
        plugin.cleanup if plugin.respond_to?(:cleanup)
        
        # Remove from loaded plugins
        @plugins.delete(name)
        true
      else
        false
      end
    end
    
    def install_plugin(source)
      # Support different plugin sources
      case source
      when /^https?:\/\//
        install_from_url(source)
      when /^[^\/]+\/[^\/]+$/ # GitHub repo format
        install_from_github(source)
      else
        install_from_file(source)
      end
    end
    
    private
    
    def load_builtin_plugins
      # Load git integration plugin
      create_git_plugin
      
      # Load documentation generator plugin  
      create_doc_plugin
      
      # Load test generator plugin
      create_test_plugin
    end
    
    def create_git_plugin
      plugin_code = <<~RUBY
        class GitIntegrationPlugin < Lantae::Plugin
          def initialize
            @name = "Git Integration"
            @version = "1.0.0"
            @description = "Automated git commit messages and PR descriptions"
            @author = "Lantae Team"
          end
          
          def on_command(command, args, context)
            case command
            when 'git-commit'
              generate_commit_message(context)
            when 'git-pr'
              generate_pr_description(context)
            end
          end
          
          def after_response(response, context)
            # Detect if response contains code changes
            if response.include?("```") && context[:git_auto_suggest]
              suggest_commit(response)
            end
          end
          
          private
          
          def generate_commit_message(context)
            # Get git diff
            diff = `git diff --cached`
            return nil if diff.empty?
            
            # Use AI to generate commit message
            prompt = "Generate a concise commit message for these changes:\\n\#{diff[0..1000]}"
            
            {
              action: :prompt,
              content: prompt
            }
          end
          
          def generate_pr_description(context)
            # Get commits since main
            commits = `git log main..HEAD --oneline`
            diff_stat = `git diff main...HEAD --stat`
            
            prompt = <<~PROMPT
              Generate a pull request description for these changes:
              
              Commits:
              \#{commits}
              
              Stats:
              \#{diff_stat}
              
              Include: Summary, changes made, testing done, and checklist.
            PROMPT
            
            {
              action: :prompt,
              content: prompt
            }
          end
          
          def suggest_commit(response)
            puts "\\n💡 Tip: Use /git-commit to generate a commit message for these changes"
          end
        end
      RUBY
      
      temp_file = File.join(@plugin_dir, 'git_integration.rb')
      File.write(temp_file, plugin_code)
      load_plugin(temp_file)
    end
    
    def create_doc_plugin
      plugin_code = <<~RUBY
        class DocumentationPlugin < Lantae::Plugin
          def initialize
            @name = "Documentation Generator"
            @version = "1.0.0"
            @description = "Automatic documentation generation"
            @author = "Lantae Team"
          end
          
          def on_command(command, args, context)
            case command
            when 'doc-gen'
              generate_documentation(args, context)
            when 'doc-update'
              update_documentation(args, context)
            end
          end
          
          private
          
          def generate_documentation(args, context)
            file_path = args.first
            return nil unless file_path && File.exist?(file_path)
            
            code = File.read(file_path)
            language = detect_language(file_path)
            
            prompt = <<~PROMPT
              Generate comprehensive documentation for this \#{language} code:
              
              ```\#{language}
              \#{code}
              ```
              
              Include:
              1. Overview
              2. Class/Function descriptions
              3. Parameters and return values
              4. Usage examples
              5. Any important notes
            PROMPT
            
            {
              action: :prompt,
              content: prompt,
              callback: -> (response) { save_documentation(file_path, response) }
            }
          end
          
          def detect_language(file_path)
            ext = File.extname(file_path)
            {
              '.rb' => 'ruby',
              '.py' => 'python',
              '.js' => 'javascript',
              '.go' => 'go',
              '.rs' => 'rust'
            }[ext] || 'text'
          end
          
          def save_documentation(source_file, documentation)
            doc_file = source_file.sub(/\\.[^.]+$/, '.md')
            File.write(doc_file, documentation)
            puts "\\n📝 Documentation saved to: \#{doc_file}"
          end
        end
      RUBY
      
      temp_file = File.join(@plugin_dir, 'documentation.rb')
      File.write(temp_file, plugin_code)
      load_plugin(temp_file)
    end
    
    def create_test_plugin
      plugin_code = <<~RUBY
        class TestGeneratorPlugin < Lantae::Plugin
          def initialize
            @name = "Test Generator"
            @version = "1.0.0"
            @description = "Automatic test generation"
            @author = "Lantae Team"
          end
          
          def on_command(command, args, context)
            case command
            when 'test-gen'
              generate_tests(args, context)
            end
          end
          
          def after_response(response, context)
            # Detect if response contains new functions
            if response.include?("def ") || response.include?("function ")
              if context[:auto_test_suggest]
                puts "\\n🧪 Tip: Use /test-gen to generate tests for this code"
              end
            end
          end
          
          private
          
          def generate_tests(args, context)
            file_path = args.first
            return nil unless file_path && File.exist?(file_path)
            
            code = File.read(file_path)
            language = detect_language(file_path)
            framework = detect_test_framework(language)
            
            prompt = <<~PROMPT
              Generate comprehensive tests for this \#{language} code using \#{framework}:
              
              ```\#{language}
              \#{code}
              ```
              
              Include:
              1. Unit tests for all public methods
              2. Edge cases
              3. Error handling tests
              4. Integration tests if applicable
            PROMPT
            
            {
              action: :prompt,
              content: prompt,
              callback: -> (response) { save_tests(file_path, response, language) }
            }
          end
          
          def detect_language(file_path)
            File.extname(file_path).delete('.')
          end
          
          def detect_test_framework(language)
            {
              'rb' => 'RSpec',
              'py' => 'pytest',
              'js' => 'Jest',
              'go' => 'testing package',
              'rs' => 'Rust test framework'
            }[language] || 'appropriate test framework'
          end
          
          def save_tests(source_file, tests, language)
            test_file = generate_test_filename(source_file, language)
            File.write(test_file, tests)
            puts "\\n✅ Tests saved to: \#{test_file}"
          end
          
          def generate_test_filename(source_file, language)
            dir = File.dirname(source_file)
            base = File.basename(source_file, '.*')
            
            case language
            when 'rb'
              File.join(dir, 'spec', "\#{base}_spec.rb")
            when 'py'
              File.join(dir, "test_\#{base}.py")
            when 'js'
              File.join(dir, "\#{base}.test.js")
            else
              File.join(dir, "\#{base}_test.\#{language}")
            end
          end
        end
      RUBY
      
      temp_file = File.join(@plugin_dir, 'test_generator.rb')
      File.write(temp_file, plugin_code)
      load_plugin(temp_file)
    end
    
    def install_from_url(url)
      require 'net/http'
      require 'uri'
      
      uri = URI(url)
      response = Net::HTTP.get_response(uri)
      
      if response.code == '200'
        filename = File.basename(uri.path)
        plugin_path = File.join(@plugin_dir, filename)
        File.write(plugin_path, response.body)
        load_plugin(plugin_path)
        true
      else
        raise "Failed to download plugin: HTTP #{response.code}"
      end
    end
    
    def install_from_github(repo)
      # Format: username/repo or username/repo/path/to/plugin.rb
      parts = repo.split('/')
      
      if parts.length >= 2
        raw_url = "https://raw.githubusercontent.com/#{repo}/main/#{parts[2..-1].join('/')}"
        raw_url += '.rb' unless raw_url.end_with?('.rb')
        install_from_url(raw_url)
      else
        raise "Invalid GitHub repository format"
      end
    end
    
    def install_from_file(file_path)
      if File.exist?(file_path)
        filename = File.basename(file_path)
        dest_path = File.join(@plugin_dir, filename)
        FileUtils.cp(file_path, dest_path)
        load_plugin(dest_path)
        true
      else
        raise "Plugin file not found: #{file_path}"
      end
    end
  end
  
  # Base plugin class
  class Plugin
    attr_reader :name, :version, :description, :author
    
    def initialize
      @name = "Unnamed Plugin"
      @version = "1.0.0"
      @description = "No description"
      @author = "Unknown"
    end
    
    # Hook methods (override in subclasses)
    def before_prompt(prompt, context)
      # Called before prompt is sent
      # Return modified prompt or nil
    end
    
    def after_response(response, context)
      # Called after response is received
      # Can modify response or trigger actions
    end
    
    def on_command(command, args, context)
      # Called when a command is executed
      # Return { action: :prompt/:execute, content: "..." }
    end
    
    def on_error(error, context)
      # Called when an error occurs
    end
    
    def on_startup
      # Called when plugin system starts
    end
    
    def on_shutdown
      # Called when plugin system shuts down
    end
    
    def cleanup
      # Called when plugin is disabled
    end
  end
end