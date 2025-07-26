# Non-AI Content Generation - Development Guide

## 🎯 Purpose
This guide provides step-by-step instructions for implementing the remaining components of the non-AI content generation system.

---

## 🔗 Dependency Graph

```
SmartRandomizer ─┐
TemplateEngine ──┼─→ CampaignContext ─┬─→ Relationship Web Generator
ContentPools ────┘                     ├─→ Consequence Engine ─→ Event Cascade
                                      │
                                      ├─→ Rumor Mill System
                                      │
SmartRandomizer ─────────────────────→ Oracle System
```

---

## 📋 Implementation Order

Based on dependencies and value delivery:

1. **Relationship Web Generator** (HIGH priority, enables social dynamics)
2. **Rumor Mill System** (HIGH priority, adds emergent storytelling)
3. **Oracle System** (MEDIUM priority, core GM tool)
4. **Consequence Engine** (MEDIUM priority, enables persistence)
5. **Event Cascade System** (LOW priority, advanced feature)

---

## 🔨 Component Implementation Guides

### 1. Relationship Web Generator

#### Purpose
Automatically generate a web of relationships between NPCs based on their traits, goals, and campaign context.

#### Implementation Steps

1. **Create Core Class Structure**
```ruby
# lib/ruby/procedural/relationship_web_generator.rb
module Procedural
  class RelationshipWebGenerator
    def initialize(campaign_context)
      @context = campaign_context
      @relationships = []
    end
    
    def generate_web(options = {})
      entities = gather_entities(options)
      calculate_all_relationships(entities)
      apply_campaign_events
      format_output(options[:format])
    end
  end
end
```

2. **Define Relationship Types**
```yaml
# data/tables/relationships/relationship_types.yml
positive:
  - type: friend
    strength: [casual, close, best]
    mutual_probability: 0.8
  - type: mentor
    strength: [former, current, devoted]
    mutual_probability: 0.3
  - type: family
    strength: [distant, close, immediate]
    mutual_probability: 1.0

negative:
  - type: rival
    strength: [professional, personal, bitter]
    mutual_probability: 0.9
  - type: enemy
    strength: [minor, serious, mortal]
    mutual_probability: 0.95

complex:
  - type: complicated
    strength: [history, tension, unresolved]
    mutual_probability: 0.7
```

3. **Implement Compatibility Scoring**
```ruby
def calculate_compatibility(entity1, entity2)
  score = 0
  
  # Shared themes increase compatibility
  shared_themes = entity1[:themes] & entity2[:themes]
  score += shared_themes.length * 10
  
  # Opposing traits can create interesting dynamics
  if opposing_traits?(entity1[:traits], entity2[:traits])
    score += 5  # Conflict creates story
  end
  
  # Professional overlap
  if related_professions?(entity1[:profession], entity2[:profession])
    score += 15
  end
  
  # Location proximity
  if entity1[:location] == entity2[:location]
    score += 20
  end
  
  score
end
```

4. **Add Visualization Support**
```ruby
def to_graphviz
  graph = "digraph relationships {\n"
  @relationships.each do |rel|
    color = relationship_color(rel[:type])
    graph += "  \"#{rel[:from]}\" -> \"#{rel[:to]}\" "
    graph += "[label=\"#{rel[:type]}\", color=\"#{color}\"];\n"
  end
  graph += "}"
end
```

#### Testing Checklist
- [ ] Generates relationships for 10+ NPCs without duplicates
- [ ] Respects campaign context and existing relationships
- [ ] Produces valid Graphviz output
- [ ] Performance under 100ms for 50 NPCs

---

### 2. Rumor Mill System

#### Purpose
Generate dynamic rumors based on campaign events, NPC actions, and world state.

#### Implementation Steps

1. **Create Rumor Generator**
```ruby
# lib/ruby/procedural/rumor_mill.rb
module Procedural
  class RumorMill
    def initialize(campaign_context, template_engine)
      @context = campaign_context
      @templates = template_engine
      @distortion_levels = [:accurate, :exaggerated, :twisted, :false]
    end
    
    def generate_rumor(options = {})
      source_type = options[:source_type] || random_source_type
      base_event = select_base_event(source_type)
      distorted_event = apply_distortion(base_event)
      add_source_attribution(distorted_event)
    end
  end
end
```

2. **Define Rumor Templates**
```yaml
# data/templates/rumors/rumor_templates.yml
event_based:
  witnessed:
    template: "[SOURCE] swears they saw [WHAT] at [WHERE] [WHEN]"
    distortion_prone: true
  heard_about:
    template: "Word is that [EVENT] happened because [SPECULATION]"
    distortion_prone: true

npc_based:
  suspicious_activity:
    template: "[NPC] has been [ACTIVITY] when they think no one's looking"
    distortion_prone: false
  secret_revealed:
    template: "Did you know [NPC] is actually [SECRET]?"
    distortion_prone: true

speculation:
  pattern_noticed:
    template: "Strange how [EVENT1] happened right after [EVENT2], don't you think?"
    distortion_prone: false
```

3. **Implement Distortion System**
```ruby
def apply_distortion(event, level = nil)
  level ||= weighted_random_distortion_level
  
  case level
  when :accurate
    event  # No change
  when :exaggerated
    exaggerate_details(event)
  when :twisted
    swap_key_details(event)
  when :false
    generate_plausible_falsehood(event)
  end
end

def exaggerate_details(event)
  # "fought off bandits" → "single-handedly defeated an army"
  # "found some gold" → "discovered a dragon's hoard"
end
```

#### Testing Checklist
- [ ] Generates varied rumors from same event
- [ ] Distortion levels create believable variations
- [ ] Sources are appropriate to rumor type
- [ ] No modern anachronisms in output

---

### 3. Oracle System

#### Purpose
Provide probability-based answers to yes/no questions with contextual modifiers.

#### Implementation Steps

1. **Create Oracle Engine**
```ruby
# lib/ruby/procedural/oracle_system.rb
module Procedural
  class OracleSystem
    BASE_PROBABILITIES = {
      yes: 35,
      no: 35,
      maybe: 20,
      complication: 10
    }
    
    def initialize
      @modifiers = load_modifiers
      @complication_table = load_complications
    end
    
    def ask(question, context = {})
      probabilities = calculate_probabilities(context)
      result = weighted_roll(probabilities)
      enhance_result(result, context)
    end
  end
end
```

2. **Define Modifier System**
```yaml
# data/tables/oracle/modifiers.yml
likelihood_modifiers:
  very_likely:
    yes: +25
    no: -20
  likely:
    yes: +15
    no: -10
  unlikely:
    yes: -15
    no: +10
  very_unlikely:
    yes: -25
    no: +20

context_modifiers:
  chaotic_situation:
    complication: +15
    maybe: +5
  tense_situation:
    no: +10
    complication: +5
  favorable_situation:
    yes: +10
    complication: -5
```

3. **Add Complication Generation**
```ruby
def generate_complication(context)
  base_complication = @complication_table.sample
  contextualized = apply_context_to_complication(base_complication, context)
  
  {
    answer: "Yes, but...",
    complication: contextualized,
    severity: assess_severity(contextualized)
  }
end
```

#### Testing Checklist
- [ ] Probability distribution matches design
- [ ] Modifiers correctly adjust chances
- [ ] Complications are contextually appropriate
- [ ] Can handle 1000+ rolls without bias

---

### 4. Consequence Engine

#### Purpose
Track character actions and generate appropriate world reactions.

#### Implementation Steps

1. **Create Action Tracking System**
```ruby
# lib/ruby/procedural/consequence_engine.rb
module Procedural
  class ConsequenceEngine
    def initialize(campaign_context)
      @context = campaign_context
      @action_log = []
      @pending_consequences = []
    end
    
    def record_action(action)
      categorized = categorize_action(action)
      @action_log << categorized
      generate_immediate_consequences(categorized)
      schedule_delayed_consequences(categorized)
    end
  end
end
```

2. **Define Action Categories**
```yaml
# data/tables/consequences/action_categories.yml
violence:
  subcategories:
    - murder
    - assault  
    - self_defense
    - mass_violence
  typical_consequences:
    - authority_response
    - witness_trauma
    - reputation_change
    - retaliation

altruism:
  subcategories:
    - charity
    - rescue
    - healing
    - protection
  typical_consequences:
    - gratitude
    - reputation_boost
    - reciprocal_aid
    - inspiration_spreading
```

3. **Implement Ripple Effects**
```ruby
def calculate_ripple_effects(action)
  affected_entities = find_affected_entities(action)
  
  ripples = affected_entities.map do |entity|
    {
      entity: entity,
      impact: calculate_impact(action, entity),
      reaction: generate_reaction(entity, action),
      timeline: determine_reaction_timeline(entity, action)
    }
  end
  
  chain_reactions = detect_chain_reactions(ripples)
  ripples + chain_reactions
end
```

#### Testing Checklist
- [ ] Actions correctly categorized
- [ ] Consequences scale with action severity
- [ ] Delayed consequences trigger appropriately
- [ ] Faction reactions are consistent

---

## 🧪 Testing Strategy

### Unit Testing Requirements
Each component must have tests for:
- Core functionality
- Edge cases
- Error handling
- Performance benchmarks

### Integration Testing
- Components work together correctly
- Campaign context flows through system
- No memory leaks between components
- Consistent data formats

### Variety Testing
Run 1000+ generations and measure:
- Unique output percentage
- Pattern detection
- Memory system effectiveness
- Context utilization

---

## 📝 Documentation Requirements

For each component:
1. **API Documentation** - Method signatures and usage
2. **Data Format Specs** - Input/output structures
3. **Usage Examples** - Common scenarios
4. **Integration Guide** - How to connect with other tools
5. **Troubleshooting** - Common issues and solutions

---

## 🚀 Quick Start Templates

### New Generator Template
```ruby
#!/usr/bin/env ruby

require 'optparse'
require_relative '../lib/ruby/procedural/smart_randomizer'
require_relative '../lib/ruby/procedural/campaign_context'

class MyGenerator
  def initialize(campaign_path: nil)
    @context = Procedural::CampaignContext.new(campaign_path: campaign_path)
    # Initialize other components
  end
  
  def generate(options = {})
    # Core generation logic
  end
  
  private
  
  def format_output(data, format)
    case format
    when 'json'
      JSON.pretty_generate(data)
    when 'yaml'  
      data.to_yaml
    when 'text'
      format_text_output(data)
    end
  end
end

# Option parsing boilerplate
options = {}
OptionParser.new do |opts|
  # Define options
end.parse!

generator = MyGenerator.new(campaign_path: options[:campaign_path])
result = generator.generate(options)
puts generator.send(:format_output, result, options[:format] || 'text')
```

---

## 🎓 Learning Resources

### Design Patterns Used
- **Strategy Pattern** - Output formatters
- **Factory Pattern** - Content generators
- **Observer Pattern** - Consequence system
- **Memento Pattern** - State tracking

### Key Algorithms
- **Weighted Random Selection** - Fair distribution with memory
- **Graph Traversal** - Relationship networks
- **Markov Chains** - Name/text generation
- **Cellular Automata** - Dungeon generation (future)

### Ruby Best Practices
- Use symbols for internal identifiers
- Lazy load large data sets
- Memoize expensive calculations
- Keep methods under 10 lines when possible