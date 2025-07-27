# Lantae Node.js Implementation

TypeScript/Node.js implementation of Lantae focused on web integration and JavaScript ecosystem compatibility.

## 🟨 Node.js Features

### Implementation Status
- 🔄 **In Development** - See [Feature Parity Status](#feature-parity-status)
- 🌐 **Web Integration** - Built for web services and API integration
- 📦 **NPM Ecosystem** - Leverage the rich JavaScript package ecosystem
- ⚡ **Async/Await** - Modern asynchronous programming patterns

### Node.js-Specific Advantages
- **Rich Ecosystem** - Access to thousands of NPM packages
- **Web Integration** - Easy integration with web frameworks
- **JSON Native** - Natural JSON handling for APIs
- **Event-Driven** - Excellent for streaming and real-time features
- **Cross-Platform** - Runs on any platform with Node.js

## 🚀 Quick Start

### Prerequisites
- **Node.js** 18+ (install via [nodejs.org](https://nodejs.org/))
- **npm** or **yarn** (included with Node.js)

### Installation
```bash
# Clone the Node.js implementation
git clone -b nodejs-implementation https://github.com/jpditri/lantae-cli.git
cd lantae-cli/nodejs-lantae

# Install dependencies
npm install

# Build TypeScript
npm run build

# Run Lantae
npm start
```

### Development Setup
```bash
# Install dependencies
npm install

# Run in development mode (with TypeScript)
npm run dev

# Watch mode for development
npm run build:watch

# Run tests
npm test

# Lint and format
npm run lint
npm run format
```

## 📖 Usage

### Command Line Interface
```bash
# Interactive mode
npx lantae

# Single prompt
npx lantae "Explain async/await in JavaScript"

# Specify provider and model
npx lantae --provider openai --model gpt-4o "Write Node.js code"

# Help
npx lantae --help
```

### Programmatic API
```typescript
import { Lantae } from 'lantae';

const client = new Lantae({
  provider: 'openai',
  apiKey: process.env.OPENAI_API_KEY,
});

const response = await client.chat('Hello, world!');
console.log(response);
```

### Web Service Integration
```typescript
import express from 'express';
import { Lantae } from 'lantae';

const app = express();
const lantae = new Lantae();

app.post('/chat', async (req, res) => {
  const { message } = req.body;
  const response = await lantae.chat(message);
  res.json({ response });
});
```

## 🔧 Configuration

### Configuration File
```json
{
  "default": {
    "provider": "ollama",
    "model": "cogito:latest"
  },
  "providers": {
    "ollama": {
      "host": "http://localhost:11434",
      "timeout": 30000
    },
    "openai": {
      "apiKeyEnv": "OPENAI_API_KEY",
      "baseUrl": "https://api.openai.com/v1",
      "timeout": 60000
    },
    "anthropic": {
      "apiKeyEnv": "ANTHROPIC_API_KEY",
      "baseUrl": "https://api.anthropic.com",
      "timeout": 60000
    }
  },
  "ui": {
    "colors": true,
    "progressBars": true,
    "streaming": true
  }
}
```

### Environment Variables
```bash
# API Keys
export OPENAI_API_KEY="your-openai-key"
export ANTHROPIC_API_KEY="your-anthropic-key"
export GEMINI_API_KEY="your-gemini-key"

# Configuration
export LANTAE_CONFIG_PATH="./config.json"
export LANTAE_LOG_LEVEL="info"
```

## 🏗️ Architecture

### Project Structure
```
nodejs-lantae/
├── package.json            # Project configuration
├── tsconfig.json           # TypeScript configuration
├── src/
│   ├── index.ts            # Library entry point
│   ├── cli.ts              # CLI entry point
│   ├── cli/
│   │   ├── commands.ts     # Command definitions
│   │   ├── repl.ts         # REPL implementation
│   │   └── args.ts         # Argument parsing
│   ├── providers/
│   │   ├── base.ts         # Provider interface
│   │   ├── ollama.ts       # Ollama implementation
│   │   ├── openai.ts       # OpenAI implementation
│   │   └── anthropic.ts    # Anthropic implementation
│   ├── config/
│   │   ├── manager.ts      # Configuration management
│   │   └── types.ts        # Configuration types
│   ├── tools/
│   │   ├── executor.ts     # Tool execution
│   │   └── manager.ts      # Tool management
│   ├── agent/
│   │   ├── planner.ts      # Task planning
│   │   └── executor.ts     # Agent execution
│   └── utils/
│       ├── logger.ts       # Logging utilities
│       ├── errors.ts       # Error types
│       └── helpers.ts      # Helper functions
├── dist/                   # Compiled JavaScript
├── tests/                  # Test files
└── docs/                   # Documentation
```

### Core Modules
- **CLI**: Command-line interface and REPL
- **Providers**: Multi-provider LLM interface
- **Config**: Configuration management
- **Tools**: Local tool integration
- **Agent**: Planning and task management
- **Utils**: Logging, errors, utilities

## 🔄 Feature Parity Status

See the main [Feature Parity Document](../docs/FEATURE_PARITY.md) for detailed status compared to other implementations.

### Node.js Implementation Roadmap

#### Phase 1: Core TypeScript Setup
- [ ] **Project Structure** - TypeScript, build system
- [ ] **CLI Framework** - Command parsing, help system
- [ ] **Configuration** - JSON/YAML config management
- [ ] **Basic Provider** - Ollama integration
- [ ] **REPL** - Interactive command interface

#### Phase 2: Provider Ecosystem
- [ ] **OpenAI Provider** - GPT model support
- [ ] **Anthropic Provider** - Claude integration
- [ ] **Gemini Provider** - Google AI support
- [ ] **Provider Management** - Switching and detection
- [ ] **Streaming** - Real-time response handling

#### Phase 3: Web Integration
- [ ] **Express Middleware** - Web framework integration
- [ ] **REST API** - HTTP API endpoints
- [ ] **WebSocket Support** - Real-time communication
- [ ] **Authentication** - API key management
- [ ] **Rate Limiting** - Request throttling

#### Phase 4: Advanced Features
- [ ] **Tool Integration** - Local command execution
- [ ] **MCP Support** - Protocol implementation
- [ ] **Planning Agent** - Task decomposition
- [ ] **NPM Package** - Published package
- [ ] **Docker Container** - Containerized deployment

## 🛠️ Development

### Building
```bash
# TypeScript compilation
npm run build

# Watch mode
npm run build:watch

# Clean build
npm run clean && npm run build
```

### Testing
```bash
# Run all tests
npm test

# Watch mode
npm run test:watch

# Coverage report
npm run test:coverage

# Specific test file
npm test -- --testNamePattern="provider"
```

### Code Quality
```bash
# Lint TypeScript
npm run lint

# Fix linting issues
npm run lint:fix

# Format code
npm run format

# Type checking
npx tsc --noEmit
```

### Package Management
```bash
# Install new dependency
npm install axios

# Install dev dependency
npm install --save-dev @types/node

# Update dependencies
npm update

# Audit security
npm audit
```

## 📦 Distribution

### NPM Package
```bash
# Build for publishing
npm run prepublishOnly

# Publish to NPM
npm publish

# Install globally
npm install -g lantae
```

### Docker Container
```dockerfile
FROM node:18-alpine
WORKDIR /app
COPY package*.json ./
RUN npm ci --only=production
COPY dist ./dist
CMD ["node", "dist/cli.js"]
```

## 🌐 Web Integration Examples

### Express.js Middleware
```typescript
import { lantaeMiddleware } from 'lantae/express';

app.use('/api/ai', lantaeMiddleware({
  provider: 'openai',
  apiKey: process.env.OPENAI_API_KEY,
}));
```

### Next.js API Route
```typescript
// pages/api/chat.ts
import { Lantae } from 'lantae';

export default async function handler(req, res) {
  const lantae = new Lantae();
  const response = await lantae.chat(req.body.message);
  res.json({ response });
}
```

### WebSocket Server
```typescript
import WebSocket from 'ws';
import { Lantae } from 'lantae';

const wss = new WebSocket.Server({ port: 8080 });
const lantae = new Lantae();

wss.on('connection', (ws) => {
  ws.on('message', async (data) => {
    const message = JSON.parse(data.toString());
    const response = await lantae.chat(message.text);
    ws.send(JSON.stringify({ response }));
  });
});
```

## 🤝 Contributing

### Node.js-Specific Guidelines
1. **Follow TypeScript best practices** - Strict typing, interfaces
2. **Write comprehensive tests** - Jest with good coverage
3. **Use modern JavaScript** - Async/await, ES modules
4. **Document APIs** - TSDoc comments for public methods
5. **Update feature parity** - Keep documentation current

### Code Style
- Use Prettier for formatting
- Follow ESLint rules
- Prefer async/await over Promises
- Use meaningful variable names
- Write descriptive commit messages

## 📚 Resources

- [Node.js Documentation](https://nodejs.org/docs/)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/)
- [NPM Documentation](https://docs.npmjs.com/)
- [Jest Testing Framework](https://jestjs.io/docs/getting-started)

---

*This Node.js implementation focuses on web integration and JavaScript ecosystem compatibility while maintaining feature parity with the Ruby reference implementation.*