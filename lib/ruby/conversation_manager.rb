require 'json'
require 'fileutils'
require 'time'
require 'digest'

module Lantae
  class ConversationManager
    DEFAULT_DIR = File.expand_path('~/.lantae/conversations')
    
    def initialize(base_dir = DEFAULT_DIR)
      @base_dir = base_dir
      FileUtils.mkdir_p(@base_dir)
      @current_session = nil
      @auto_save_enabled = true
      @auto_save_interval = 10 # messages
      @message_count = 0
    end
    
    def save_conversation(name, conversation, metadata = {})
      session_data = {
        name: name,
        timestamp: Time.now.iso8601,
        messages: conversation,
        metadata: metadata.merge(
          message_count: conversation.size,
          providers_used: extract_providers(conversation),
          total_tokens: calculate_tokens(conversation)
        ),
        version: '1.0'
      }
      
      file_path = session_file_path(name)
      File.write(file_path, JSON.pretty_generate(session_data))
      
      # Update index
      update_index(name, session_data[:metadata])
      
      file_path
    end
    
    def load_conversation(name)
      file_path = session_file_path(name)
      
      unless File.exist?(file_path)
        raise "Conversation '#{name}' not found"
      end
      
      session_data = JSON.parse(File.read(file_path), symbolize_names: true)
      @current_session = name
      
      {
        conversation: session_data[:messages],
        metadata: session_data[:metadata]
      }
    end
    
    def list_conversations
      index_file = File.join(@base_dir, 'index.json')
      return [] unless File.exist?(index_file)
      
      index = JSON.parse(File.read(index_file), symbolize_names: true)
      index[:sessions] || []
    end
    
    def search_conversations(query)
      conversations = list_conversations
      
      conversations.select do |conv|
        conv[:name].downcase.include?(query.downcase) ||
        conv[:summary]&.downcase&.include?(query.downcase) ||
        conv[:tags]&.any? { |tag| tag.downcase.include?(query.downcase) }
      end
    end
    
    def delete_conversation(name)
      file_path = session_file_path(name)
      
      if File.exist?(file_path)
        File.delete(file_path)
        remove_from_index(name)
        true
      else
        false
      end
    end
    
    def auto_save(conversation)
      return unless @auto_save_enabled
      return unless @current_session
      
      @message_count += 1
      
      if @message_count >= @auto_save_interval
        save_conversation(@current_session, conversation, { auto_saved: true })
        @message_count = 0
      end
    end
    
    def export_conversation(name, format = :markdown)
      session = load_conversation(name)
      
      case format
      when :markdown
        export_to_markdown(session[:conversation], session[:metadata])
      when :json
        JSON.pretty_generate(session)
      when :html
        export_to_html(session[:conversation], session[:metadata])
      else
        raise "Unsupported export format: #{format}"
      end
    end
    
    # Conversation branching
    def create_branch(base_name, branch_name, conversation, branch_point = nil)
      base_session = load_conversation(base_name)
      base_conversation = base_session[:conversation]
      
      # Branch from specific point or end
      branch_point ||= base_conversation.size
      branched_conversation = base_conversation[0...branch_point] + conversation
      
      metadata = {
        branched_from: base_name,
        branch_point: branch_point,
        branch_created: Time.now.iso8601
      }
      
      save_conversation(branch_name, branched_conversation, metadata)
    end
    
    def get_conversation_stats(name)
      session = load_conversation(name)
      conversation = session[:conversation]
      
      {
        message_count: conversation.size,
        user_messages: conversation.count { |m| m[:role] == 'user' },
        assistant_messages: conversation.count { |m| m[:role] == 'assistant' },
        total_length: conversation.sum { |m| m[:content].length },
        average_message_length: conversation.sum { |m| m[:content].length } / conversation.size.to_f,
        providers_used: extract_providers(conversation),
        created_at: session[:metadata][:timestamp],
        last_modified: File.mtime(session_file_path(name))
      }
    end
    
    def get_context_info(conversation, provider = nil)
      return nil unless provider
      
      {
        tokens_used: provider.calculate_context_usage(conversation),
        context_window: provider.context_window,
        remaining_tokens: provider.remaining_context(conversation),
        percentage_used: provider.context_percentage_used(conversation)
      }
    end
    
    def format_context_display(context_info)
      return "" unless context_info
      
      used = context_info[:tokens_used]
      total = context_info[:context_window]
      remaining = context_info[:remaining_tokens]
      percentage = context_info[:percentage_used]
      
      # Color coding based on usage
      color = case percentage
              when 0..50
                "\033[32m"  # Green
              when 51..80
                "\033[33m"  # Yellow
              else
                "\033[31m"  # Red
              end
      
      "#{color}Context: #{used}/#{total} tokens (#{percentage}% used, #{remaining} remaining)\033[0m"
    end
    
    private
    
    def session_file_path(name)
      safe_name = name.gsub(/[^a-zA-Z0-9_-]/, '_')
      File.join(@base_dir, "#{safe_name}.json")
    end
    
    def update_index(name, metadata)
      index_file = File.join(@base_dir, 'index.json')
      
      index = if File.exist?(index_file)
        JSON.parse(File.read(index_file), symbolize_names: true)
      else
        { sessions: [] }
      end
      
      # Remove existing entry if updating
      index[:sessions].reject! { |s| s[:name] == name }
      
      # Add new entry
      index[:sessions] << {
        name: name,
        timestamp: metadata[:timestamp] || Time.now.iso8601,
        message_count: metadata[:message_count],
        summary: generate_summary(metadata),
        tags: extract_tags(metadata)
      }
      
      # Sort by timestamp
      index[:sessions].sort_by! { |s| s[:timestamp] }.reverse!
      
      File.write(index_file, JSON.pretty_generate(index))
    end
    
    def remove_from_index(name)
      index_file = File.join(@base_dir, 'index.json')
      return unless File.exist?(index_file)
      
      index = JSON.parse(File.read(index_file), symbolize_names: true)
      index[:sessions].reject! { |s| s[:name] == name }
      
      File.write(index_file, JSON.pretty_generate(index))
    end
    
    def extract_providers(conversation)
      providers = []
      conversation.each do |msg|
        if msg[:metadata] && msg[:metadata][:provider]
          providers << msg[:metadata][:provider]
        end
      end
      providers.uniq
    end
    
    def calculate_tokens(conversation)
      # Simple approximation: ~4 characters per token
      conversation.sum { |m| m[:content].length / 4 }
    end
    
    def generate_summary(metadata)
      # Generate a brief summary for the index
      if metadata[:message_count]
        "#{metadata[:message_count]} messages"
      else
        "Conversation session"
      end
    end
    
    def extract_tags(metadata)
      tags = []
      tags << "auto-saved" if metadata[:auto_saved]
      tags << "branched" if metadata[:branched_from]
      tags += metadata[:providers_used] if metadata[:providers_used]
      tags
    end
    
    def export_to_markdown(conversation, metadata)
      output = []
      output << "# Conversation Export"
      output << "\n**Date**: #{metadata[:timestamp]}"
      output << "**Messages**: #{conversation.size}"
      output << "\n---\n"
      
      conversation.each do |msg|
        role = msg[:role].capitalize
        output << "### #{role}:"
        output << msg[:content]
        output << "\n"
      end
      
      output.join("\n")
    end
    
    def export_to_html(conversation, metadata)
      html = <<~HTML
        <!DOCTYPE html>
        <html>
        <head>
          <title>Lantae Conversation - #{metadata[:timestamp]}</title>
          <style>
            body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }
            .message { margin: 20px 0; padding: 15px; border-radius: 8px; }
            .user { background: #e3f2fd; }
            .assistant { background: #f5f5f5; }
            .role { font-weight: bold; margin-bottom: 10px; }
            pre { background: #263238; color: #aed581; padding: 10px; border-radius: 4px; overflow-x: auto; }
            code { background: #eceff1; padding: 2px 4px; border-radius: 3px; }
          </style>
        </head>
        <body>
          <h1>Conversation Export</h1>
          <p><strong>Date:</strong> #{metadata[:timestamp]}</p>
          <p><strong>Messages:</strong> #{conversation.size}</p>
          <hr>
      HTML
      
      conversation.each do |msg|
        role_class = msg[:role]
        content = msg[:content]
          .gsub(/```(\w+)?\n(.*?)```/m) { |match| "<pre><code>#{$2}</code></pre>" }
          .gsub(/`([^`]+)`/) { |match| "<code>#{$1}</code>" }
          .gsub(/\n/, '<br>')
        
        html += <<~MSG
          <div class="message #{role_class}">
            <div class="role">#{msg[:role].capitalize}:</div>
            <div class="content">#{content}</div>
          </div>
        MSG
      end
      
      html += "</body></html>"
      html
    end
  end
end