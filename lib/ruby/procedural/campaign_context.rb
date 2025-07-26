require 'yaml'
require 'json'

module Procedural
  class CampaignContext
    attr_reader :data, :relationships, :themes
    
    def initialize(campaign_path: nil)
      @data = {
        npcs: {},
        locations: {},
        factions: {},
        items: {},
        events: [],
        themes: [],
        relationships: {},
        ontology: {}
      }
      @campaign_path = campaign_path
      load_campaign_data if campaign_path
    end
    
    def add_npc(name, details)
      @data[:npcs][name] = enrich_with_ontology(details, :npc)
      self
    end
    
    def add_location(name, details)
      @data[:locations][name] = enrich_with_ontology(details, :location)
      self
    end
    
    def add_faction(name, details)
      @data[:factions][name] = enrich_with_ontology(details, :faction)
      self
    end
    
    def add_relationship(entity1, entity2, relationship_type, details = {})
      key = [entity1, entity2].sort.join('-')
      @data[:relationships][key] = {
        entities: [entity1, entity2],
        type: relationship_type,
        details: details,
        created_at: Time.now
      }
      self
    end
    
    def add_event(event)
      @data[:events] << {
        description: event[:description],
        participants: event[:participants] || [],
        location: event[:location],
        timestamp: event[:timestamp] || Time.now,
        consequences: event[:consequences] || []
      }
      self
    end
    
    def get_context_for(category, filters: {})
      items = @data[category] || {}
      
      if filters.any?
        items.select do |name, details|
          filters.all? do |key, value|
            case value
            when Array
              value.any? { |v| matches_value?(details[key], v) }
            else
              matches_value?(details[key], value)
            end
          end
        end
      else
        items
      end
    end
    
    def get_related_entities(entity_name, relationship_type: nil)
      related = []
      
      @data[:relationships].each do |_, rel|
        if rel[:entities].include?(entity_name)
          if relationship_type.nil? || rel[:type] == relationship_type
            other_entity = rel[:entities].find { |e| e != entity_name }
            related << {
              entity: other_entity,
              relationship: rel[:type],
              details: rel[:details]
            }
          end
        end
      end
      
      related
    end
    
    def suggest_connections(entity_name, entity_type)
      suggestions = []
      entity = @data[entity_type.to_s.pluralize.to_sym][entity_name]
      return suggestions unless entity
      
      @data[:npcs].each do |npc_name, npc|
        next if npc_name == entity_name
        
        if shared_themes?(entity, npc) || compatible_traits?(entity, npc)
          suggestions << {
            entity: npc_name,
            type: :npc,
            reason: analyze_connection_reason(entity, npc)
          }
        end
      end
      
      @data[:locations].each do |loc_name, location|
        if location_relevant?(entity, location)
          suggestions << {
            entity: loc_name,
            type: :location,
            reason: analyze_location_relevance(entity, location)
          }
        end
      end
      
      suggestions
    end
    
    def generate_contextual_detail(template_category, base_content)
      enriched = base_content.dup
      
      case template_category
      when :npc
        enriched[:connections] = suggest_connections(base_content[:name], :npc)
        enriched[:motivations] = derive_motivations(base_content)
        enriched[:secrets] = generate_secrets(base_content)
      when :quest
        enriched[:stakeholders] = find_stakeholders(base_content)
        enriched[:complications] = generate_complications(base_content)
      when :rumor
        enriched[:sources] = find_rumor_sources(base_content)
        enriched[:truth_level] = calculate_truth_level(base_content)
      end
      
      enriched
    end
    
    def export_for_tables
      {
        npc_names: @data[:npcs].keys,
        location_names: @data[:locations].keys,
        faction_names: @data[:factions].keys,
        recent_events: @data[:events].last(10).map { |e| e[:description] },
        active_themes: @data[:themes],
        relationship_types: @data[:relationships].values.map { |r| r[:type] }.uniq
      }
    end
    
    private
    
    def load_campaign_data
      return unless File.exist?(@campaign_path)
      
      if File.directory?(@campaign_path)
        load_from_directory(@campaign_path)
      else
        load_from_file(@campaign_path)
      end
    end
    
    def load_from_directory(dir)
      Dir.glob(File.join(dir, '**/*.{yml,yaml,json}')).each do |file|
        next if file.include?('/tables/') || file.include?('/templates/')
        
        data = load_file_data(file)
        merge_campaign_data(data) if data
      end
    end
    
    def load_from_file(file)
      data = load_file_data(file)
      merge_campaign_data(data) if data
    end
    
    def load_file_data(file)
      case File.extname(file)
      when '.yml', '.yaml'
        YAML.load_file(file)
      when '.json'
        JSON.parse(File.read(file), symbolize_names: true)
      end
    rescue => e
      puts "Error loading #{file}: #{e.message}"
      nil
    end
    
    def merge_campaign_data(data)
      data.each do |key, value|
        if @data.key?(key.to_sym)
          case @data[key.to_sym]
          when Hash
            @data[key.to_sym].merge!(value)
          when Array
            @data[key.to_sym].concat(value)
          else
            @data[key.to_sym] = value
          end
        end
      end
    end
    
    def enrich_with_ontology(details, entity_type)
      enriched = details.dup
      
      enriched[:ontology] = {
        themes: extract_themes(details),
        traits: extract_traits(details),
        connections: [],
        potential_conflicts: [],
        roleplay_hooks: generate_roleplay_hooks(details, entity_type)
      }
      
      enriched
    end
    
    def extract_themes(details)
      themes = []
      
      text = [details[:description], details[:background], details[:goals]].compact.join(' ')
      
      theme_keywords = {
        power: %w[control authority rule command dominate],
        redemption: %w[forgive atone redeem mistake past],
        revenge: %w[vengeance payback retribution avenge],
        love: %w[romance affection passion heart beloved],
        duty: %w[obligation responsibility honor bound sworn],
        freedom: %w[liberty independence escape chains free],
        knowledge: %w[wisdom learn discover understand truth],
        survival: %w[survive endure persist struggle hardship]
      }
      
      theme_keywords.each do |theme, keywords|
        if keywords.any? { |keyword| text.downcase.include?(keyword) }
          themes << theme
        end
      end
      
      themes
    end
    
    def extract_traits(details)
      traits = []
      traits.concat(details[:traits]) if details[:traits]
      traits.concat(details[:personality]) if details[:personality]
      traits.concat(details[:tags]) if details[:tags]
      traits.uniq
    end
    
    def generate_roleplay_hooks(details, entity_type)
      hooks = []
      
      case entity_type
      when :npc
        hooks << "Ask about their #{details[:occupation]}" if details[:occupation]
        hooks << "Mention #{details[:homeland]}" if details[:homeland]
        hooks << "Appeal to their sense of #{details[:values].first}" if details[:values]&.any?
      when :location
        hooks << "Investigate the #{details[:notable_features].first}" if details[:notable_features]&.any?
        hooks << "Speak with the #{details[:inhabitants]}" if details[:inhabitants]
      when :faction
        hooks << "Discuss their #{details[:goals].first}" if details[:goals]&.any?
        hooks << "Challenge their #{details[:methods]}" if details[:methods]
      end
      
      hooks
    end
    
    def shared_themes?(entity1, entity2)
      themes1 = entity1[:ontology]&.dig(:themes) || []
      themes2 = entity2[:ontology]&.dig(:themes) || []
      
      (themes1 & themes2).any?
    end
    
    def compatible_traits?(entity1, entity2)
      traits1 = entity1[:ontology]&.dig(:traits) || []
      traits2 = entity2[:ontology]&.dig(:traits) || []
      
      compatibility_pairs = [
        %w[brave cowardly], %w[honest deceptive], %w[kind cruel],
        %w[patient impatient], %w[generous greedy], %w[humble proud]
      ]
      
      compatibility_pairs.any? do |pair|
        (traits1.include?(pair[0]) && traits2.include?(pair[1])) ||
        (traits1.include?(pair[1]) && traits2.include?(pair[0]))
      end
    end
    
    def analyze_connection_reason(entity1, entity2)
      reasons = []
      
      shared = shared_themes?(entity1, entity2)
      reasons << "share thematic connection" if shared
      
      compatible = compatible_traits?(entity1, entity2)
      reasons << "have conflicting personalities" if compatible
      
      reasons.join(" and ")
    end
    
    def location_relevant?(entity, location)
      return true if entity[:location] == location[:name]
      return true if location[:themes]&.any? { |theme| entity[:themes]&.include?(theme) }
      false
    end
    
    def analyze_location_relevance(entity, location)
      return "is their home location" if entity[:location] == location[:name]
      "shares thematic elements"
    end
    
    def derive_motivations(npc_data)
      motivations = []
      
      if npc_data[:goals]
        motivations.concat(npc_data[:goals])
      end
      
      if npc_data[:fears]
        motivations << "Avoid #{npc_data[:fears].first}"
      end
      
      if npc_data[:desires]
        motivations << "Obtain #{npc_data[:desires].first}"
      end
      
      motivations
    end
    
    def generate_secrets(npc_data)
      secrets = []
      
      if npc_data[:background]
        secrets << "Hidden past involving #{npc_data[:background]}"
      end
      
      if npc_data[:relationships]
        secrets << "Secret connection to #{npc_data[:relationships].keys.sample}"
      end
      
      secrets
    end
    
    def find_stakeholders(quest_data)
      stakeholders = []
      
      @data[:npcs].each do |name, npc|
        if quest_data[:description]&.include?(name) ||
           npc[:goals]&.any? { |goal| quest_data[:description]&.include?(goal) }
          stakeholders << name
        end
      end
      
      stakeholders
    end
    
    def generate_complications(quest_data)
      complications = []
      
      if quest_data[:location] && @data[:locations][quest_data[:location]]
        location = @data[:locations][quest_data[:location]]
        complications << "#{location[:hazards].first} at location" if location[:hazards]
      end
      
      if quest_data[:factions]
        complications << "Faction interference from #{quest_data[:factions].first}"
      end
      
      complications
    end
    
    def find_rumor_sources(rumor_data)
      potential_sources = []
      
      @data[:npcs].each do |name, npc|
        if npc[:occupation]&.include?('merchant') || 
           npc[:occupation]&.include?('tavern') ||
           npc[:traits]&.include?('gossipy')
          potential_sources << name
        end
      end
      
      potential_sources.sample(2)
    end
    
    def calculate_truth_level(rumor_data)
      if rumor_data[:based_on_event]
        "mostly true"
      elsif rumor_data[:exaggerated]
        "partially true"
      else
        "questionable"
      end
    end
    
    def matches_value?(value, filter)
      case value
      when Array
        value.include?(filter)
      when String
        value.downcase.include?(filter.to_s.downcase)
      else
        value == filter
      end
    end
  end
end