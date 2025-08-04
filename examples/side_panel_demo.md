# Side Panel Query Queue Demo

This demonstrates the enhanced side panel that shows pending queries - particularly useful when using slow local models.

## Features

### 📋 Query Queue Display
- **Running Queries**: Shows active queries with elapsed time
- **Queued Queries**: Lists pending queries waiting to be processed  
- **Recent Completions**: Shows last few completed queries
- **Summary Stats**: Total pending count and average runtime

### Example Display

```
📋 Query Queue
──────────────────────────────
▶ Running (2):
  [1] Tell me a joke...
      ⏱ 12s
  [3] Explain quantum physics...
      ⏱ 3s

⏸ Queued (3):
  [4] List 3 programming lan...
  [5] What's the weather like...
  [6] Calculate fibonacci seq...

✓ Recent:
  [2] What's 2+2?

📊 Summary:
  Total pending: 5
  Avg runtime: 7s
```

## Usage

1. **Enable Side Panel**:
   ```
   /side
   ```

2. **Submit Multiple Queries**:
   When using slow local models, you can queue multiple queries:
   ```
   > What is machine learning?
   > Explain neural networks
   > How does backpropagation work?
   ```

3. **Monitor Progress**:
   The side panel will show:
   - Which queries are actively being processed
   - How long each query has been running
   - Which queries are waiting in the queue

## Benefits for Local Models

When using local Ollama models that may take 10-30+ seconds per query:
- **Queue Management**: See all pending work at a glance
- **Time Estimation**: Track how long queries typically take
- **Parallel Submission**: Submit multiple queries without waiting
- **Progress Tracking**: Know which query is currently being processed

## Commands

- `/side` - Toggle side panel display
- `/status` - Show detailed command status
- `/cancel <id>` - Cancel a specific queued command