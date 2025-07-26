# Non-AI Content Generation - Quick Reference

## 🚀 Component Status & Commands

### ✅ Available Now

#### NPC Generator
```bash
# Basic NPC
./bin/npc-generator-tables

# Detailed NPC with connections
./bin/npc-generator-tables --detailed --connections --secret

# Multiple NPCs with campaign context
./bin/npc-generator-tables -n 5 -c campaign/ --format json

# Specific profession type
./bin/npc-generator-tables --type rare --detailed
```

#### Quest Weaver
```bash
# Basic quest
./bin/quest-weaver-templates

# Quest with subquests and DM notes
./bin/quest-weaver-templates --subquests --format dm_notes

# Linked quest chain
./bin/quest-weaver-templates -n 3 --linked -c campaign/

# Specific quest type
./bin/quest-weaver-templates --type investigation_quest --subquests
```

### 🔄 Coming Soon

#### Relationship Web Generator
```bash
# Generate relationships for all NPCs
./bin/relationship-web-generator -c campaign/

# Visualize as graph
./bin/relationship-web-generator -c campaign/ --format graphviz > relationships.dot

# Focus on specific NPC
./bin/relationship-web-generator -c campaign/ --focus "Aldric Ironforge"
```

#### Rumor Mill
```bash
# Generate random rumors
./bin/rumor-mill -c campaign/ -n 5

# Event-based rumors
./bin/rumor-mill -c campaign/ --source event --distortion high

# NPC-specific rumors
./bin/rumor-mill -c campaign/ --about "Vera Nightfall"
```

#### Oracle System
```bash
# Simple yes/no question
./bin/oracle-system "Will the guards let us pass?"

# With likelihood modifier
./bin/oracle-system "Will the ritual succeed?" --likelihood unlikely

# With context
./bin/oracle-system "Does the NPC trust us?" --context tense_situation
```

#### Consequence Engine
```bash
# Record an action
./bin/consequence-engine record "Party killed the mayor" -c campaign/

# Check pending consequences
./bin/consequence-engine check -c campaign/

# Generate consequence report
./bin/consequence-engine report -c campaign/ --timeline week
```

---

## 📁 File Structure

```
ollama-code/
├── lib/ruby/procedural/
│   ├── smart_randomizer.rb      # Memory-aware randomization
│   ├── template_engine.rb       # Mad Libs templates
│   ├── content_pools.rb         # Content management
│   └── campaign_context.rb      # Campaign integration
├── data/
│   ├── tables/
│   │   ├── npc/                 # NPC generation tables
│   │   ├── relationships/       # Relationship types
│   │   └── consequences/        # Action categories
│   ├── templates/
│   │   ├── quests/             # Quest templates
│   │   └── rumors/             # Rumor templates
│   └── word-banks/             # Variable substitutions
└── bin/
    ├── npc-generator-tables     # ✅ Complete
    ├── quest-weaver-templates   # ✅ Complete
    ├── relationship-web-*       # 🔄 In Progress
    ├── rumor-mill              # 🔄 In Progress
    ├── oracle-system           # 📋 Planned
    └── consequence-engine      # 📋 Planned
```

---

## 🔧 Core Classes

### SmartRandomizer
```ruby
# Create with memory
rand = Procedural::SmartRandomizer.new(items, memory_size: 10)

# Get next item
item = rand.next

# Get with context
item = rand.next(context: { rarity: 'common' })

# Get batch
items = rand.next_batch(5)
```

### TemplateEngine
```ruby
# Initialize
engine = Procedural::TemplateEngine.new(campaign_path: 'campaign/')

# Load template
engine.load_template('greeting', 'The [ADJECTIVE] [NPC_TYPE] says hello')

# Generate
text = engine.generate('greeting')

# With overrides
text = engine.generate('greeting', overrides: { 'NPC_TYPE' => 'wizard' })
```

### ContentPools
```ruby
# Initialize
pools = Procedural::ContentPools.new(campaign_path: 'campaign/')

# Add pool
pools.add_pool('monsters', ['goblin', 'orc', 'troll'])

# Get items
monster = pools.get('monsters')
monsters = pools.get('monsters', count: 3, unique: true)

# Filter
evil_monsters = pools.get_filtered('monsters', 
  filter: { alignment: 'evil' })
```

### CampaignContext
```ruby
# Initialize
context = Procedural::CampaignContext.new(campaign_path: 'campaign/')

# Add entities
context.add_npc('Aldric', { profession: 'blacksmith', traits: ['gruff'] })
context.add_location('Ironhold', { type: 'city', population: 5000 })

# Query
npcs = context.get_context_for(:npcs, filters: { profession: 'merchant' })
related = context.get_related_entities('Aldric')

# Generate connections
suggestions = context.suggest_connections('Aldric', :npc)
```

---

## 🎲 Common Patterns

### Campaign Integration
```ruby
# All generators accept campaign path
generator = MyGenerator.new(campaign_path: 'path/to/campaign')

# Use campaign data when available
options[:use_campaign] = true
result = generator.generate(options)
```

### Output Formats
```ruby
# All generators support multiple formats
--format text      # Human readable (default)
--format json      # Machine parseable
--format yaml      # Configuration friendly
--format dm_notes  # Extended info for DMs
```

### Batch Generation
```ruby
# Most generators support batch mode
-n 10              # Generate 10 items
--linked           # Create connections between items
```

---

## 📊 Performance Guidelines

| Operation | Target | Actual |
|-----------|--------|--------|
| Single Generation | <50ms | ✅ 12-18ms |
| Batch (100) | <1s | ✅ 0.8s |
| Memory per Generator | <10MB | ✅ 2-3MB |
| Variety Score | >90% | ✅ 91-94% |

---

## 🐛 Troubleshooting

### "Word bank not found"
Add missing word bank to `/data/word-banks/`

### Repetitive outputs
Increase memory_size in SmartRandomizer

### Slow generation
Check for N+1 queries in campaign context

### Memory leaks
Ensure randomizers are reused, not recreated

---

## 🔗 Integration Points

### Adding to Existing Tools
```ruby
# In existing generator
if options[:no_ai]
  # Use procedural generation
  require_relative '../lib/ruby/procedural/template_engine'
  generator = Procedural::TemplateEngine.new
else
  # Use AI generation
end
```

### Sharing Data
```ruby
# Export campaign data for tables
data = campaign_context.export_for_tables

# Import into generator
generator = NPCGenerator.new
generator.use_campaign_data(data)
```

---

## 📚 See Also

- [Full IMF Document](./non-ai-content-generation.md)
- [Implementation Tracker](./implementation-tracker.md)
- [Development Guide](./development-guide.md)
- [Testing Strategy](./development-guide.md#testing-strategy)