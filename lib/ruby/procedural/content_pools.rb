require 'yaml'
require 'json'

module Procedural
  class ContentPools
    attr_reader :pools, :campaign_pools
    
    def initialize(campaign_path: nil)
      @pools = {}
      @campaign_pools = {}
      @usage_stats = Hash.new { |h, k| h[k] = Hash.new(0) }
      load_campaign_pools(campaign_path) if campaign_path
    end
    
    def add_pool(name, items, tags: [])
      @pools[name] = {
        items: items,
        tags: tags,
        created_at: Time.now
      }
      self
    end
    
    def add_campaign_pool(name, items, tags: [])
      @campaign_pools[name] = {
        items: items,
        tags: tags,
        created_at: Time.now
      }
      self
    end
    
    def get(pool_name, count: 1, unique: true, campaign_priority: true)
      items = gather_items(pool_name, campaign_priority)
      
      raise "Pool '#{pool_name}' not found" if items.empty?
      
      @usage_stats[pool_name][:total_requests] += 1
      
      if unique && count > 1
        selected = items.sample(count)
      else
        selected = count.times.map { items.sample }
      end
      
      selected.each { |item| @usage_stats[pool_name][item] += 1 }
      
      count == 1 ? selected.first : selected
    end
    
    def get_filtered(pool_name, filter:, count: 1, campaign_priority: true)
      items = gather_items(pool_name, campaign_priority)
      
      filtered = items.select do |item|
        case filter
        when Hash
          filter.all? { |k, v| item_matches?(item, k, v) }
        when Proc
          filter.call(item)
        else
          true
        end
      end
      
      raise "No items match filter in pool '#{pool_name}'" if filtered.empty?
      
      count == 1 ? filtered.sample : filtered.sample(count)
    end
    
    def merge_pools(*pool_names, name: nil)
      merged_items = []
      merged_tags = []
      
      pool_names.each do |pool_name|
        pool = @pools[pool_name] || @campaign_pools[pool_name]
        next unless pool
        
        merged_items.concat(pool[:items])
        merged_tags.concat(pool[:tags])
      end
      
      merged_tags.uniq!
      
      if name
        add_pool(name, merged_items, tags: merged_tags)
      else
        merged_items
      end
    end
    
    def load_from_file(file_path, campaign: false)
      data = case File.extname(file_path)
             when '.yml', '.yaml'
               YAML.load_file(file_path)
             when '.json'
               JSON.parse(File.read(file_path))
             else
               raise "Unsupported file format: #{file_path}"
             end
      
      data.each do |pool_name, pool_data|
        items = pool_data['items'] || pool_data
        tags = pool_data['tags'] || []
        
        if campaign
          add_campaign_pool(pool_name, items, tags: tags)
        else
          add_pool(pool_name, items, tags: tags)
        end
      end
      
      self
    end
    
    def stats(pool_name = nil)
      if pool_name
        {
          pool: pool_name,
          size: pool_size(pool_name),
          usage: @usage_stats[pool_name]
        }
      else
        {
          pools: @pools.keys,
          campaign_pools: @campaign_pools.keys,
          total_usage: @usage_stats
        }
      end
    end
    
    def clear_usage_stats
      @usage_stats.clear
      self
    end
    
    private
    
    def load_campaign_pools(campaign_path)
      return unless File.exist?(campaign_path)
      
      if File.directory?(campaign_path)
        Dir.glob(File.join(campaign_path, 'pools', '*.yml')).each do |file|
          load_from_file(file, campaign: true)
        end
      end
    end
    
    def gather_items(pool_name, campaign_priority)
      items = []
      
      if campaign_priority && @campaign_pools[pool_name]
        items.concat(@campaign_pools[pool_name][:items])
      end
      
      if @pools[pool_name]
        items.concat(@pools[pool_name][:items])
      elsif items.empty?
        items.concat(@campaign_pools[pool_name][:items]) if @campaign_pools[pool_name]
      end
      
      items
    end
    
    def pool_size(pool_name)
      size = 0
      size += @pools[pool_name][:items].size if @pools[pool_name]
      size += @campaign_pools[pool_name][:items].size if @campaign_pools[pool_name]
      size
    end
    
    def item_matches?(item, key, value)
      case item
      when Hash
        item[key] == value || item[key.to_s] == value
      when String
        key == :text && item.include?(value.to_s)
      else
        false
      end
    end
  end
end