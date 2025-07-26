module Procedural
  class SmartRandomizer
    attr_reader :items, :recent, :memory_size
    
    def initialize(items, memory_size: 10, weights: nil)
      @items = items.dup
      @memory_size = memory_size
      @recent = []
      @weights = weights || Hash.new(1.0)
      @usage_count = Hash.new(0)
    end
    
    def next(context: {})
      available = filter_by_context(@items - @recent, context)
      available = filter_by_context(@items, context) if available.empty?
      
      choice = weighted_sample(available)
      
      @recent << choice
      @recent.shift if @recent.size > @memory_size
      @usage_count[choice] += 1
      
      choice
    end
    
    def next_batch(count, context: {})
      results = []
      count.times { results << self.next(context: context) }
      results
    end
    
    def reset_memory
      @recent.clear
      self
    end
    
    def stats
      {
        total_items: @items.size,
        recent_items: @recent.size,
        usage_counts: @usage_count.sort_by { |_, v| -v }.to_h
      }
    end
    
    private
    
    def filter_by_context(items, context)
      return items if context.empty?
      
      items.select do |item|
        item_context = extract_context(item)
        context.all? do |key, value|
          item_context[key].nil? || item_context[key] == value
        end
      end
    end
    
    def extract_context(item)
      case item
      when Hash
        item[:context] || {}
      else
        {}
      end
    end
    
    def weighted_sample(items)
      return items.sample if @weights.empty?
      
      total_weight = items.sum { |item| item_weight(item) }
      random = rand * total_weight
      
      cumulative = 0
      items.each do |item|
        cumulative += item_weight(item)
        return item if cumulative >= random
      end
      
      items.last
    end
    
    def item_weight(item)
      base_weight = case item
                    when Hash
                      @weights[item[:id]] || @weights[item[:name]] || 1.0
                    else
                      @weights[item] || 1.0
                    end
      
      frequency_penalty = 1.0 / (1 + @usage_count[item] * 0.1)
      base_weight * frequency_penalty
    end
  end
end