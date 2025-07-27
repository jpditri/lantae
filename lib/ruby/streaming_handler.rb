require 'json'
require 'net/http'
require 'stringio'

module Lantae
  class StreamingHandler
    def initialize(options = {})
      @on_token = options[:on_token] || ->(token) { print token }
      @on_complete = options[:on_complete] || ->(content) {}
      @on_error = options[:on_error] || ->(error) { puts "\nStreaming error: #{error}" }
      @buffer = StringIO.new
      @syntax_highlighter = SyntaxHighlighter.new
    end
    
    def stream_response(uri, request, options = {})
      http = Net::HTTP.new(uri.host, uri.port)
      http.use_ssl = uri.scheme == 'https'
      http.read_timeout = 300
      
      # Modify request for streaming
      request_body = JSON.parse(request.body)
      request_body['stream'] = true
      request.body = request_body.to_json
      
      accumulated_content = ""
      in_code_block = false
      code_language = nil
      code_buffer = ""
      
      begin
        http.request(request) do |response|
          response.read_body do |chunk|
            chunk.each_line do |line|
              next if line.strip.empty?
              
              begin
                # Parse SSE format
                if line.start_with?("data: ")
                  data = line[6..-1].strip
                  next if data == "[DONE]"
                  
                  json = JSON.parse(data)
                  token = extract_token(json, options[:provider])
                  
                  if token
                    accumulated_content += token
                    
                    # Handle code block detection and syntax highlighting
                    process_token_display(token, accumulated_content, 
                                        in_code_block, code_language, code_buffer)
                  end
                end
              rescue JSON::ParserError => e
                # Skip malformed JSON
              end
            end
          end
        end
        
        @on_complete.call(accumulated_content)
        accumulated_content
        
      rescue => e
        @on_error.call(e.message)
        raise
      end
    end
    
    def stream_ollama_response(uri, request)
      http = Net::HTTP.new(uri.host, uri.port)
      http.read_timeout = 300
      
      # Ensure streaming is enabled
      request_body = JSON.parse(request.body)
      request_body['stream'] = true
      request.body = request_body.to_json
      
      accumulated_content = ""
      
      begin
        http.request(request) do |response|
          response.read_body do |chunk|
            chunk.each_line do |line|
              next if line.strip.empty?
              
              begin
                json = JSON.parse(line)
                
                if json['message'] && json['message']['content']
                  token = json['message']['content']
                  accumulated_content += token
                  @on_token.call(token)
                end
                
                # Check if response is complete
                if json['done']
                  @on_complete.call(accumulated_content)
                  break
                end
              rescue JSON::ParserError
                # Skip malformed JSON
              end
            end
          end
        end
        
        accumulated_content
      rescue => e
        @on_error.call(e.message)
        raise
      end
    end
    
    private
    
    def extract_token(json, provider)
      case provider
      when 'openai'
        json.dig('choices', 0, 'delta', 'content')
      when 'anthropic'
        json.dig('delta', 'text')
      when 'gemini'
        json.dig('candidates', 0, 'content', 'parts', 0, 'text')
      else
        json.dig('message', 'content') || json.dig('response')
      end
    end
    
    def process_token_display(token, accumulated_content, in_code_block, code_language, code_buffer)
      # Simple code block detection
      if token.include?('```')
        if !in_code_block && accumulated_content =~ /```(\w+)?\s*$/
          in_code_block = true
          code_language = $1
          code_buffer = ""
          @on_token.call(token)
        elsif in_code_block && token.include?('```')
          # End of code block - apply syntax highlighting
          if code_buffer && !code_buffer.empty?
            highlighted = @syntax_highlighter.highlight(code_buffer, code_language)
            print "\r" + " " * code_buffer.length + "\r" # Clear unhighlighted code
            print highlighted
          end
          in_code_block = false
          code_language = nil
          code_buffer = ""
          @on_token.call(token)
        else
          @on_token.call(token)
        end
      elsif in_code_block
        code_buffer += token
        @on_token.call(token) # Show unhighlighted for now
      else
        @on_token.call(token)
      end
    end
  end
  
  class SyntaxHighlighter
    COLORS = {
      keyword: "\e[35m",    # Magenta
      string: "\e[32m",     # Green
      comment: "\e[90m",    # Gray
      number: "\e[36m",     # Cyan
      function: "\e[33m",   # Yellow
      reset: "\e[0m"
    }
    
    LANGUAGE_KEYWORDS = {
      'ruby' => %w[def end class module if else elsif unless while for do begin rescue ensure require],
      'python' => %w[def class if else elif while for import from return try except finally with as],
      'javascript' => %w[function const let var if else while for return try catch finally class extends],
      'go' => %w[func package import type struct interface if else for range return defer go],
    }
    
    def highlight(code, language)
      return code unless language && LANGUAGE_KEYWORDS[language.downcase]
      
      keywords = LANGUAGE_KEYWORDS[language.downcase]
      highlighted = code.dup
      
      # Highlight keywords
      keywords.each do |keyword|
        highlighted.gsub!(/\b#{keyword}\b/, "#{COLORS[:keyword]}#{keyword}#{COLORS[:reset]}")
      end
      
      # Highlight strings (simple approach)
      highlighted.gsub!(/"([^"]*)"/, "#{COLORS[:string]}\"\\1\"#{COLORS[:reset]}")
      highlighted.gsub!(/'([^']*)'/, "#{COLORS[:string]}'\\1'#{COLORS[:reset]}")
      
      # Highlight comments
      case language.downcase
      when 'ruby', 'python'
        highlighted.gsub!(/#(.*)$/, "#{COLORS[:comment]}#\\1#{COLORS[:reset]}")
      when 'javascript', 'go'
        highlighted.gsub!(/\/\/(.*)$/, "#{COLORS[:comment]}//\\1#{COLORS[:reset]}")
      end
      
      # Highlight numbers
      highlighted.gsub!(/\b\d+\b/, "#{COLORS[:number]}\\0#{COLORS[:reset]}")
      
      highlighted
    end
  end
  
  # Progress indicator for long operations
  class ProgressIndicator
    def initialize(options = {})
      @style = options[:style] || :spinner
      @message = options[:message] || "Processing"
      @running = false
      @thread = nil
    end
    
    def start
      @running = true
      
      @thread = Thread.new do
        case @style
        when :spinner
          show_spinner
        when :dots
          show_dots
        when :bar
          show_progress_bar
        end
      end
    end
    
    def stop
      @running = false
      @thread&.join
      print "\r" + " " * 80 + "\r" # Clear line
    end
    
    def update_message(message)
      @message = message
    end
    
    private
    
    def show_spinner
      frames = %w[⠋ ⠙ ⠹ ⠸ ⠼ ⠴ ⠦ ⠧ ⠇ ⠏]
      i = 0
      
      while @running
        print "\r#{frames[i % frames.length]} #{@message}..."
        sleep 0.1
        i += 1
      end
    end
    
    def show_dots
      dots = 0
      
      while @running
        print "\r#{@message}#{'.' * (dots % 4)}   "
        sleep 0.5
        dots += 1
      end
    end
    
    def show_progress_bar
      width = 30
      progress = 0
      
      while @running
        filled = (progress * width / 100).to_i
        bar = "█" * filled + "░" * (width - filled)
        print "\r#{@message} [#{bar}] #{progress}%"
        sleep 0.1
        progress = (progress + 1) % 101
      end
    end
  end
end