require 'yaml'
require 'erb'
require 'fileutils'

module Lantae
  class PromptTemplateManager
    DEFAULT_DIR = File.expand_path('~/.lantae/templates')
    
    def initialize(base_dir = DEFAULT_DIR)
      @base_dir = base_dir
      FileUtils.mkdir_p(@base_dir)
      @templates = {}
      @snippets = {}
      load_templates
      load_snippets
      create_default_templates if @templates.empty?
    end
    
    def save_template(name, template, metadata = {})
      template_data = {
        name: name,
        template: template,
        variables: extract_variables(template),
        metadata: metadata.merge(
          created_at: Time.now.iso8601,
          usage_count: 0
        )
      }
      
      @templates[name] = template_data
      save_templates_to_disk
      name
    end
    
    def get_template(name)
      template = @templates[name]
      raise "Template '#{name}' not found" unless template
      template
    end
    
    def render_template(name, variables = {})
      template_data = get_template(name)
      template_text = template_data[:template]
      
      # Update usage count
      template_data[:metadata][:usage_count] += 1
      template_data[:metadata][:last_used] = Time.now.iso8601
      save_templates_to_disk
      
      # Render with ERB
      erb = ERB.new(template_text)
      erb.result_with_hash(variables)
    end
    
    def list_templates
      @templates.map do |name, data|
        {
          name: name,
          variables: data[:variables],
          usage_count: data[:metadata][:usage_count],
          created_at: data[:metadata][:created_at]
        }
      end
    end
    
    def delete_template(name)
      @templates.delete(name)
      save_templates_to_disk
    end
    
    # Snippets management
    def save_snippet(name, content, metadata = {})
      @snippets[name] = {
        content: content,
        metadata: metadata.merge(
          created_at: Time.now.iso8601,
          usage_count: 0
        )
      }
      save_snippets_to_disk
    end
    
    def get_snippet(name)
      snippet = @snippets[name]
      raise "Snippet '#{name}' not found" unless snippet
      
      # Update usage count
      snippet[:metadata][:usage_count] += 1
      snippet[:metadata][:last_used] = Time.now.iso8601
      save_snippets_to_disk
      
      snippet[:content]
    end
    
    def list_snippets
      @snippets.map do |name, data|
        {
          name: name,
          preview: data[:content][0..50] + (data[:content].length > 50 ? '...' : ''),
          usage_count: data[:metadata][:usage_count],
          created_at: data[:metadata][:created_at]
        }
      end
    end
    
    def delete_snippet(name)
      @snippets.delete(name)
      save_snippets_to_disk
    end
    
    # Template suggestions based on context
    def suggest_templates(context)
      suggestions = []
      
      @templates.each do |name, data|
        score = calculate_relevance_score(data, context)
        suggestions << { name: name, score: score } if score > 0.3
      end
      
      suggestions.sort_by { |s| -s[:score] }.first(5)
    end
    
    private
    
    def load_templates
      template_file = File.join(@base_dir, 'templates.yaml')
      return unless File.exist?(template_file)
      
      @templates = YAML.load_file(template_file) || {}
    end
    
    def save_templates_to_disk
      template_file = File.join(@base_dir, 'templates.yaml')
      File.write(template_file, YAML.dump(@templates))
    end
    
    def load_snippets
      snippet_file = File.join(@base_dir, 'snippets.yaml')
      return unless File.exist?(snippet_file)
      
      @snippets = YAML.load_file(snippet_file) || {}
    end
    
    def save_snippets_to_disk
      snippet_file = File.join(@base_dir, 'snippets.yaml')
      File.write(snippet_file, YAML.dump(@snippets))
    end
    
    def extract_variables(template)
      # Find ERB variables in template
      template.scan(/<%=\s*(\w+)\s*%>/).flatten.uniq
    end
    
    def calculate_relevance_score(template_data, context)
      score = 0.0
      
      # Check if context keywords match template
      if context[:keywords]
        template_keywords = template_data[:metadata][:keywords] || []
        matching = (context[:keywords] & template_keywords).size
        score += matching * 0.2
      end
      
      # Boost recently used templates
      if template_data[:metadata][:last_used]
        days_ago = (Time.now - Time.parse(template_data[:metadata][:last_used])) / 86400
        score += (1.0 / (days_ago + 1)) * 0.3
      end
      
      # Boost frequently used templates
      usage_count = template_data[:metadata][:usage_count] || 0
      score += [usage_count * 0.05, 0.5].min
      
      score
    end
    
    def create_default_templates
      # Code Review Template
      save_template('code_review', <<~TEMPLATE, keywords: ['review', 'code', 'quality'])
        Please review the following code for:
        1. Code quality and best practices
        2. Potential bugs or issues
        3. Performance considerations
        4. Security concerns
        5. Suggestions for improvement
        
        Code to review:
        ```<%= language %>
        <%= code %>
        ```
        
        Additional context: <%= context if defined?(context) %>
      TEMPLATE
      
      # Debugging Template
      save_template('debug', <<~TEMPLATE, keywords: ['debug', 'error', 'fix'])
        I'm encountering an error and need help debugging.
        
        Error message:
        ```
        <%= error_message %>
        ```
        
        Code causing the error:
        ```<%= language %>
        <%= code %>
        ```
        
        What I've tried: <%= attempts if defined?(attempts) %>
        Expected behavior: <%= expected if defined?(expected) %>
      TEMPLATE
      
      # Feature Implementation Template
      save_template('implement_feature', <<~TEMPLATE, keywords: ['implement', 'feature', 'add'])
        I need to implement a new feature with the following requirements:
        
        Feature: <%= feature_name %>
        Description: <%= description %>
        
        Current code structure:
        ```<%= language %>
        <%= current_code if defined?(current_code) %>
        ```
        
        Constraints:
        - <%= constraints if defined?(constraints) %>
        
        Please provide a clean implementation with tests.
      TEMPLATE
      
      # Explanation Template
      save_template('explain', <<~TEMPLATE, keywords: ['explain', 'understand', 'how'])
        Please explain how this code works:
        
        ```<%= language %>
        <%= code %>
        ```
        
        Specifically, I'd like to understand:
        <%= specific_questions if defined?(specific_questions) %>
        
        Please use simple language and provide examples if helpful.
      TEMPLATE
      
      # Optimization Template
      save_template('optimize', <<~TEMPLATE, keywords: ['optimize', 'performance', 'speed'])
        Please optimize this code for better performance:
        
        ```<%= language %>
        <%= code %>
        ```
        
        Current performance issues:
        <%= issues if defined?(issues) %>
        
        Constraints:
        - <%= constraints if defined?(constraints) %>
        
        Please explain the optimizations and their impact.
      TEMPLATE
    end
  end
end