# Review and Follow-up Commands Guide

This guide covers the new commands for reviewing results and sending follow-up queries in Lantae's async mode.

## Overview

When working with multiple queries, especially with slow local models, you need ways to:
- Review previous results without scrolling
- See your command history at a glance
- Send contextual follow-up questions
- Export results for later reference

## Commands

### 📜 `/history [n]` - View Command History

Shows recent commands with their status and timing.

**Usage:**
```
/history      # Shows last 10 commands
/history 20   # Shows last 20 commands (max: 50)
```

**Example Output:**
```
📜 Command History (Last 10)
────────────────────────────────────────────────────────────
[5] 14:32:15 ✓ completed What is machine learning? (12s)
[4] 14:31:45 ✓ completed Explain neural networks... (8s)
[3] 14:31:30 ▶ running How does backpropagation work?
[2] 14:31:20 ⏸ queued List gradient descent types...
[1] 14:30:55 ✓ completed What is Python? (5s)

Use /review <id> to see full details of any command
```

### 📋 `/review [id]` - Review Command Results

Shows the full details and response of a command.

**Usage:**
```
/review      # Reviews the last completed command
/review 3    # Reviews command with ID 3
```

**Example Output:**
```
📋 Review of Command [3]
──────────────────────────────────────────────────
Query: What is machine learning?
Status: ✓ completed
Provider: ollama/qwen2.5:1.5b
Duration: 12s
Cost: $0.0002

Response:
──────────────────────────────────────────────────
Machine learning is a subset of artificial intelligence...
[Full response content here]

Tip: Use /follow 3 <your follow-up> to ask a follow-up question
```

### 🔗 `/follow <id> <text>` - Send Follow-up Query

Sends a contextual follow-up question based on a previous command.

**Usage:**
```
/follow 3 Can you give me a practical example?
/follow 5 What are the main challenges with this approach?
```

**How it works:**
- Takes the original query and response as context
- Sends your follow-up with full context to the AI
- Creates a new command that references the original

**Example:**
```
> /follow 3 Can you give me a practical example?
🔗 Sending follow-up to command [3]

[6] Submitted: Based on our previous exchange...
```

### 📁 `/export <id> [filename]` - Export Command Result

Exports a command's query and response to a markdown file.

**Usage:**
```
/export 3                    # Auto-generates filename
/export 3 ml_explanation     # Saves as ml_explanation.md
/export 3 results/ml_notes   # Saves to specific path
```

**Export Format:**
- Clean markdown without ANSI codes
- Includes metadata (date, provider, duration, cost)
- Formatted for easy reading and sharing

**Example Output:**
```
✅ Exported command [3] to: lantae_export_3_20240803_143256.md
```

## Workflow Examples

### Example 1: Research Workflow
```bash
# Submit multiple research queries
> What is quantum computing?
> Explain superposition in quantum physics
> How do quantum gates work?

# Check progress
> /history

# Review the first response
> /review 1

# Ask for more details
> /follow 1 Can you explain this with a real-world analogy?

# Export for notes
> /export 1 quantum_computing_intro
```

### Example 2: Code Learning Workflow
```bash
# Ask about programming concepts
> Explain Python decorators
> Show me an example of a Python context manager
> What are Python generators?

# Review decorator explanation
> /review 1

# Ask for code example
> /follow 1 Show me a practical decorator for timing functions

# Export all for reference
> /export 1 python_decorators
> /export 2 python_context_managers
> /export 3 python_generators
```

## Tips

1. **Efficient Review**: Use `/history` first to see command IDs, then `/review` specific ones
2. **Context Preservation**: The `/follow` command maintains full context from the original query
3. **Batch Processing**: Submit multiple queries, then review/follow-up as they complete
4. **Documentation**: Use `/export` to build your own knowledge base from AI responses

## Benefits for Slow Local Models

When using local Ollama models that may take 10-30+ seconds:

- **Queue Multiple Questions**: Submit all your questions at once
- **Review at Leisure**: Come back later to review all responses
- **Contextual Follow-ups**: Ask clarifying questions without repeating context
- **Build Knowledge Base**: Export useful responses for future reference

## Integration with Side Panel

The side panel (`/side`) works perfectly with these commands:
- See pending queries in the queue
- Monitor which commands are running
- Track completion status
- Then use review/follow commands to dig deeper

## Keyboard Shortcuts (Future Enhancement)

Planned shortcuts:
- `Ctrl+R` - Review last command
- `Ctrl+H` - Show history
- `Ctrl+F` - Follow-up on last command