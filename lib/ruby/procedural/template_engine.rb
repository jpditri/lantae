require 'yaml'

module Procedural
  class TemplateEngine
    attr_reader :templates, :word_banks, :campaign_context
    
    def initialize(campaign_path: nil)
      @templates = {}
      @word_banks = {}
      @campaign_context = load_campaign_context(campaign_path)
      @variable_pattern = /\[([A-Z_]+)(?::([a-z_]+))?\]/
    end
    
    def load_template(name, content)
      @templates[name] = content
      self
    end
    
    def load_templates_from_file(file_path)
      data = YAML.load_file(file_path)
      data.each { |name, template| load_template(name, template) }
      self
    end
    
    def load_word_bank(category, words)
      @word_banks[category] = words
      self
    end
    
    def load_word_banks_from_file(file_path)
      data = YAML.load_file(file_path)
      data.each { |category, words| load_word_bank(category, words) }
      self
    end
    
    def generate(template_name, overrides: {}, campaign_specific: true)
      template = @templates[template_name]
      raise "Template '#{template_name}' not found" unless template
      
      result = template.dup
      
      result.scan(@variable_pattern).uniq.each do |match|
        category = match[0]
        modifier = match[1]
        placeholder = "[#{category}#{modifier ? ':' + modifier : ''}]"
        
        replacement = if overrides.key?(category)
                        overrides[category]
                      elsif campaign_specific && @campaign_context[category.downcase]
                        select_from_campaign(category.downcase, modifier)
                      else
                        select_from_bank(category, modifier)
                      end
        
        result.gsub!(placeholder, replacement.to_s)
      end
      
      result
    end
    
    def generate_batch(template_name, count, options = {})
      count.times.map { generate(template_name, **options) }
    end
    
    def add_campaign_content(category, items)
      @campaign_context[category] ||= []
      @campaign_context[category].concat(Array(items))
      self
    end
    
    private
    
    def load_campaign_context(campaign_path)
      return {} unless campaign_path && File.exist?(campaign_path)
      
      context = {}
      
      if File.directory?(campaign_path)
        Dir.glob(File.join(campaign_path, '*.yml')).each do |file|
          data = YAML.load_file(file)
          context.merge!(data) if data.is_a?(Hash)
        end
      else
        data = YAML.load_file(campaign_path)
        context = data if data.is_a?(Hash)
      end
      
      context
    end
    
    def select_from_campaign(category, modifier)
      items = @campaign_context[category]
      return select_from_bank(category.upcase, modifier) if items.nil? || items.empty?
      
      if modifier
        filtered = items.select { |item| matches_modifier?(item, modifier) }
        filtered.empty? ? items.sample : filtered.sample
      else
        items.sample
      end
    end
    
    def select_from_bank(category, modifier)
      bank = @word_banks[category]
      raise "Word bank '#{category}' not found" unless bank
      
      if modifier && bank.is_a?(Hash) && bank[modifier]
        bank[modifier].sample
      elsif bank.is_a?(Array)
        bank.sample
      elsif bank.is_a?(Hash)
        bank.values.flatten.sample
      else
        bank.to_s
      end
    end
    
    def matches_modifier?(item, modifier)
      case item
      when Hash
        item[:type] == modifier || item[:tags]&.include?(modifier)
      when String
        false
      else
        false
      end
    end
  end
end