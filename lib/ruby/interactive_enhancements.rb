require 'readline'
require 'io/console'
require 'tempfile'

module Lantae
  class InteractiveEnhancements
    def initialize(options = {})
      @multiline_mode = false
      @multiline_buffer = []
      @history_search_mode = false
      @editor = ENV['EDITOR'] || 'vim'
      setup_key_bindings
    end
    
    def setup_key_bindings
      # Ctrl+M for multiline mode
      Readline.bind_key("\C-m") { toggle_multiline_mode }
      
      # Ctrl+E to open in editor
      Readline.bind_key("\C-e") { open_in_editor }
      
      # Ctrl+R for history search
      Readline.bind_key("\C-r") { history_search }
      
      # Ctrl+K to clear screen
      Readline.bind_key("\C-k") { clear_screen }
    end
    
    def read_input(prompt = '> ')
      if @multiline_mode
        read_multiline_input(prompt)
      else
        Readline.readline(prompt, true)
      end
    end
    
    def read_multiline_input(prompt)
      puts "#{prompt}[Multiline mode - Type '###' to submit, Ctrl+C to cancel]"
      buffer = []
      
      loop do
        line = Readline.readline('... ', false)
        
        if line.nil? || line.strip == '###'
          break
        else
          buffer << line
        end
      end
      
      result = buffer.join("\n")
      Readline::HISTORY.push(result) unless result.empty?
      result
    end
    
    def toggle_multiline_mode
      @multiline_mode = !@multiline_mode
      status = @multiline_mode ? "ON" : "OFF"
      puts "\nMultiline mode: #{status}"
    end
    
    def open_in_editor
      temp_file = Tempfile.new(['lantae_input', '.md'])
      
      begin
        # Pre-populate with current line
        current_line = Readline.line_buffer
        temp_file.write(current_line)
        temp_file.write("\n\n# Enter your prompt above. Lines starting with # are ignored.")
        temp_file.close
        
        # Open in editor
        system("#{@editor} #{temp_file.path}")
        
        # Read back the content
        content = File.read(temp_file.path)
        
        # Remove comments and empty lines
        cleaned = content.lines
                        .reject { |line| line.strip.start_with?('#') }
                        .join
                        .strip
        
        # Replace current line buffer
        Readline.line_buffer = cleaned
        Readline.refresh_line
        
      ensure
        temp_file.unlink
      end
    end
    
    def history_search
      print "\n(reverse-i-search)`': "
      search_term = ""
      matches = []
      current_match_index = 0
      
      loop do
        char = STDIN.getch
        
        case char
        when "\x7F", "\b" # Backspace
          search_term = search_term[0...-1]
        when "\r", "\n" # Enter
          if matches.any? && matches[current_match_index]
            Readline.line_buffer = matches[current_match_index]
            Readline.refresh_line
          end
          break
        when "\e" # Escape
          break
        when "\x10" # Ctrl+P (previous)
          current_match_index = [current_match_index - 1, 0].max
        when "\x0E" # Ctrl+N (next)
          current_match_index = [current_match_index + 1, matches.length - 1].min
        else
          search_term += char if char =~ /[[:print:]]/
        end
        
        # Find matches
        matches = Readline::HISTORY.to_a.reverse.select do |item|
          item.downcase.include?(search_term.downcase)
        end
        
        # Display current match
        print "\r(reverse-i-search)`#{search_term}': "
        if matches.any? && matches[current_match_index]
          print matches[current_match_index]
        end
      end
      
      print "\n"
    end
    
    def clear_screen
      system('clear') || system('cls')
    end
    
    # Auto-completion context manager
    class CompletionContext
      def initialize
        @contexts = {
          files: -> { Dir.glob("*").select { |f| File.file?(f) } },
          directories: -> { Dir.glob("*/").map { |d| d.chomp('/') } },
          commands: -> { %w[help model provider lsp squad task clear info env] },
          models: -> { [] }, # Will be populated dynamically
          providers: -> { %w[ollama openai anthropic bedrock gemini mistral perplexity] }
        }
      end
      
      def complete(text, context_type = :auto)
        if context_type == :auto
          context_type = detect_context(text)
        end
        
        candidates = @contexts[context_type]&.call || []
        candidates.select { |c| c.start_with?(text) }
      end
      
      def add_context(name, generator)
        @contexts[name] = generator
      end
      
      private
      
      def detect_context(text)
        case text
        when /^\/\w*$/
          :commands
        when /\.(rb|py|js|go|rs)$/
          :files
        when /^\//
          :directories
        else
          :general
        end
      end
    end
    
    # Smart paste detection
    class PasteDetector
      def initialize
        @paste_threshold = 0.05 # 50ms between characters
        @last_char_time = Time.now
        @buffer = ""
        @in_paste = false
      end
      
      def detect_paste(char)
        current_time = Time.now
        time_diff = current_time - @last_char_time
        
        if time_diff < @paste_threshold
          @in_paste = true
          @buffer += char
        elsif @in_paste
          # Paste ended
          result = @buffer + char
          @buffer = ""
          @in_paste = false
          return result
        end
        
        @last_char_time = current_time
        @in_paste ? nil : char
      end
      
      def in_paste?
        @in_paste
      end
    end
  end
  
  # Code block extractor
  class CodeBlockExtractor
    def self.extract_blocks(text)
      blocks = []
      
      text.scan(/```(\w+)?\n(.*?)```/m) do |language, code|
        blocks << {
          language: language || 'text',
          code: code.strip
        }
      end
      
      blocks
    end
    
    def self.save_blocks(text, base_dir = '.')
      blocks = extract_blocks(text)
      saved_files = []
      
      blocks.each_with_index do |block, index|
        extension = language_to_extension(block[:language])
        filename = "extracted_#{index + 1}#{extension}"
        filepath = File.join(base_dir, filename)
        
        File.write(filepath, block[:code])
        saved_files << filepath
      end
      
      saved_files
    end
    
    def self.copy_to_clipboard(text)
      # Try different clipboard commands based on OS
      if RUBY_PLATFORM =~ /darwin/
        IO.popen('pbcopy', 'w') { |io| io.write(text) }
      elsif RUBY_PLATFORM =~ /linux/
        if system('which xclip > /dev/null 2>&1')
          IO.popen('xclip -selection clipboard', 'w') { |io| io.write(text) }
        elsif system('which xsel > /dev/null 2>&1')
          IO.popen('xsel --clipboard --input', 'w') { |io| io.write(text) }
        end
      elsif RUBY_PLATFORM =~ /mswin|mingw/
        IO.popen('clip', 'w') { |io| io.write(text) }
      end
    end
    
    private
    
    def self.language_to_extension(language)
      extensions = {
        'ruby' => '.rb',
        'python' => '.py',
        'javascript' => '.js',
        'typescript' => '.ts',
        'go' => '.go',
        'rust' => '.rs',
        'java' => '.java',
        'cpp' => '.cpp',
        'c' => '.c',
        'shell' => '.sh',
        'bash' => '.sh',
        'sql' => '.sql',
        'html' => '.html',
        'css' => '.css',
        'json' => '.json',
        'yaml' => '.yaml',
        'xml' => '.xml',
        'markdown' => '.md'
      }
      
      extensions[language.downcase] || '.txt'
    end
  end
  
  # Auto-save manager
  class AutoSaveManager
    def initialize(conversation_manager, interval = 60)
      @conversation_manager = conversation_manager
      @interval = interval
      @thread = nil
      @last_save = Time.now
      @enabled = true
    end
    
    def start
      @thread = Thread.new do
        while @enabled
          sleep @interval
          
          if should_save?
            @conversation_manager.auto_save(get_current_conversation)
            @last_save = Time.now
          end
        end
      end
    end
    
    def stop
      @enabled = false
      @thread&.join
    end
    
    def force_save
      @conversation_manager.auto_save(get_current_conversation)
      @last_save = Time.now
    end
    
    private
    
    def should_save?
      # Save if there have been changes since last save
      Time.now - @last_save > @interval
    end
    
    def get_current_conversation
      # This will be injected from the main REPL
      Thread.current[:current_conversation] || []
    end
  end
end