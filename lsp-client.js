#!/usr/bin/env node
/**
 * LSP Client for testing and development
 * This can be used to test the LSP server functionality
 */

const { spawn } = require('child_process');
const path = require('path');

class LSPClient {
  constructor() {
    this.server = null;
    this.messageId = 1;
    this.responses = new Map();
  }

  start() {
    return new Promise((resolve, reject) => {
      const serverPath = path.join(__dirname, 'lsp-server.js');
      this.server = spawn('node', [serverPath, '--stdio'], {
        stdio: ['pipe', 'pipe', 'pipe']
      });

      this.server.on('error', reject);
      this.server.on('spawn', () => {
        console.log('LSP Server started');
        this.setupMessageHandling();
        resolve();
      });

      this.server.stderr.on('data', (data) => {
        console.error('Server Error:', data.toString());
      });
    });
  }

  setupMessageHandling() {
    let buffer = '';
    
    this.server.stdout.on('data', (data) => {
      buffer += data.toString();
      
      while (true) {
        const headerEnd = buffer.indexOf('\r\n\r\n');
        if (headerEnd === -1) break;
        
        const header = buffer.substring(0, headerEnd);
        const contentLengthMatch = header.match(/Content-Length: (\d+)/);
        
        if (!contentLengthMatch) {
          buffer = buffer.substring(headerEnd + 4);
          continue;
        }
        
        const contentLength = parseInt(contentLengthMatch[1]);
        const messageStart = headerEnd + 4;
        
        if (buffer.length < messageStart + contentLength) break;
        
        const messageBody = buffer.substring(messageStart, messageStart + contentLength);
        buffer = buffer.substring(messageStart + contentLength);
        
        try {
          const message = JSON.parse(messageBody);
          this.handleMessage(message);
        } catch (error) {
          console.error('Failed to parse message:', error);
        }
      }
    });
  }

  handleMessage(message) {
    console.log('Received:', JSON.stringify(message, null, 2));
    
    if (message.id && this.responses.has(message.id)) {
      const resolver = this.responses.get(message.id);
      this.responses.delete(message.id);
      resolver(message);
    }
  }

  sendMessage(message) {
    const jsonMessage = JSON.stringify(message);
    const header = `Content-Length: ${Buffer.byteLength(jsonMessage)}\r\n\r\n`;
    const fullMessage = header + jsonMessage;
    
    console.log('Sending:', JSON.stringify(message, null, 2));
    this.server.stdin.write(fullMessage);
  }

  sendRequest(method, params = {}) {
    return new Promise((resolve) => {
      const id = this.messageId++;
      this.responses.set(id, resolve);
      
      this.sendMessage({
        jsonrpc: '2.0',
        id,
        method,
        params
      });
    });
  }

  sendNotification(method, params = {}) {
    this.sendMessage({
      jsonrpc: '2.0',
      method,
      params
    });
  }

  async initialize() {
    const response = await this.sendRequest('initialize', {
      processId: process.pid,
      clientInfo: {
        name: 'Lantae LSP Test Client',
        version: '1.0.0'
      },
      capabilities: {
        textDocument: {
          synchronization: {
            dynamicRegistration: true,
            willSave: true,
            willSaveWaitUntil: true,
            didSave: true
          },
          completion: {
            dynamicRegistration: true,
            completionItem: {
              snippetSupport: true,
              commitCharactersSupport: true,
              documentationFormat: ['markdown', 'plaintext']
            }
          },
          hover: {
            dynamicRegistration: true,
            contentFormat: ['markdown', 'plaintext']
          },
          definition: {
            dynamicRegistration: true
          }
        },
        workspace: {
          configuration: true,
          workspaceFolders: true
        }
      },
      workspaceFolders: [{
        uri: `file://${process.cwd()}`,
        name: 'Lantae Project'
      }]
    });

    this.sendNotification('initialized', {});
    return response;
  }

  async testCompletion() {
    console.log('\n=== Testing Completion ===');
    
    // First, open a document
    this.sendNotification('textDocument/didOpen', {
      textDocument: {
        uri: 'file:///test.js',
        languageId: 'javascript',
        version: 1,
        text: 'console.'
      }
    });

    // Request completion
    const completion = await this.sendRequest('textDocument/completion', {
      textDocument: { uri: 'file:///test.js' },
      position: { line: 0, character: 8 }
    });

    console.log('Completion result:', completion);
  }

  async testHover() {
    console.log('\n=== Testing Hover ===');
    
    this.sendNotification('textDocument/didOpen', {
      textDocument: {
        uri: 'file:///test2.js',
        languageId: 'javascript',
        version: 1,
        text: 'const require = function() {};\nrequire("fs");'
      }
    });

    const hover = await this.sendRequest('textDocument/hover', {
      textDocument: { uri: 'file:///test2.js' },
      position: { line: 1, character: 0 }
    });

    console.log('Hover result:', hover);
  }

  stop() {
    if (this.server) {
      this.server.kill();
    }
  }
}

// Test the LSP server
async function main() {
  const client = new LSPClient();
  
  try {
    await client.start();
    await client.initialize();
    await client.testCompletion();
    await client.testHover();
    
    console.log('\n=== All tests completed ===');
  } catch (error) {
    console.error('Test failed:', error);
  } finally {
    setTimeout(() => {
      client.stop();
      process.exit(0);
    }, 1000);
  }
}

if (require.main === module) {
  main();
}

module.exports = LSPClient;