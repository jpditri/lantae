#!/usr/bin/env node
/**
 * Language Server Protocol implementation for Lantae AI CLI
 * Provides intelligent features for JavaScript/Node.js development
 */

const {
  createConnection,
  TextDocuments,
  Diagnostic,
  DiagnosticSeverity,
  ProposedFeatures,
  InitializeParams,
  DidChangeConfigurationNotification,
  CompletionItem,
  CompletionItemKind,
  TextDocumentPositionParams,
  TextDocumentSyncKind,
  InitializeResult,
  DocumentDiagnosticReportKind,
} = require('vscode-languageserver/node');

const { TextDocument } = require('vscode-languageserver-textdocument');
const { URI } = require('vscode-uri');
const fs = require('fs-extra');
const path = require('path');

// Create a connection for the server, using stdio for communication
const connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager
const documents = new TextDocuments(TextDocument);

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;
let hasDiagnosticRelatedInformationCapability = false;

// Cache for file analysis
const fileCache = new Map();
const projectStructure = new Map();
const lantaeGeneratedFiles = new Map(); // Track Lantae-generated files
const languageSupport = new Map(); // Dynamic language support

connection.onInitialize((params) => {
  const capabilities = params.capabilities;

  hasConfigurationCapability = !!(
    capabilities.workspace && !!capabilities.workspace.configuration
  );
  hasWorkspaceFolderCapability = !!(
    capabilities.workspace && !!capabilities.workspace.workspaceFolders
  );
  hasDiagnosticRelatedInformationCapability = !!(
    capabilities.textDocument &&
    capabilities.textDocument.publishDiagnostics &&
    capabilities.textDocument.publishDiagnostics.relatedInformation
  );

  const result = {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      completionProvider: {
        resolveProvider: true,
        triggerCharacters: ['.', '/', '"', "'"]
      },
      hoverProvider: true,
      definitionProvider: true,
      referencesProvider: true,
      documentSymbolProvider: true,
      workspaceSymbolProvider: true,
      codeActionProvider: true,
      codeLensProvider: {
        resolveProvider: false
      },
      documentFormattingProvider: true,
      documentRangeFormattingProvider: true,
      renameProvider: { prepareProvider: true },
      foldingRangeProvider: true,
      executeCommandProvider: {
        commands: [
          'lantae.analyzeFile',
          'lantae.optimizeCode',
          'lantae.generateTests',
          'lantae.refactorCode'
        ]
      }
    }
  };

  if (hasWorkspaceFolderCapability) {
    result.capabilities.workspace = {
      workspaceFolders: {
        supported: true
      }
    };
  }

  return result;
});

connection.onInitialized(() => {
  if (hasConfigurationCapability) {
    connection.client.register(DidChangeConfigurationNotification.type, undefined);
  }
  if (hasWorkspaceFolderCapability) {
    connection.workspace.onDidChangeWorkspaceFolders(_event => {
      connection.console.log('Workspace folder change event received.');
    });
  }

  // Initialize project structure analysis
  analyzeProjectStructure();
  
  // Load Lantae-generated files metadata
  loadLantaeMetadata();
  
  // Register dynamic language support
  registerLanguageSupport();
});

// Settings configuration

const defaultSettings = {
  maxNumberOfProblems: 1000,
  aiProvider: 'ollama',
  aiModel: 'cogito:latest',
  enableIntelliSense: true,
  enableCodeAnalysis: true
};

let globalSettings = defaultSettings;
const documentSettings = new Map();

connection.onDidChangeConfiguration(change => {
  if (hasConfigurationCapability) {
    documentSettings.clear();
  } else {
    globalSettings = (change.settings.lantae || defaultSettings);
  }
  documents.all().forEach(validateTextDocument);
});

function getDocumentSettings(resource) {
  if (!hasConfigurationCapability) {
    return Promise.resolve(globalSettings);
  }
  let result = documentSettings.get(resource);
  if (!result) {
    result = connection.workspace.getConfiguration({
      scopeUri: resource,
      section: 'lantae'
    });
    documentSettings.set(resource, result);
  }
  return result;
}

documents.onDidClose(e => {
  documentSettings.delete(e.document.uri);
  fileCache.delete(e.document.uri);
});

documents.onDidChangeContent(change => {
  validateTextDocument(change.document);
});

// Load Lantae metadata file
async function loadLantaeMetadata() {
  try {
    const workspaceFolders = await connection.workspace.getWorkspaceFolders();
    if (!workspaceFolders || workspaceFolders.length === 0) return;

    const rootPath = URI.parse(workspaceFolders[0].uri).fsPath;
    const metadataPath = path.join(rootPath, '.lantae-generated.json');
    
    if (await fs.pathExists(metadataPath)) {
      const content = await fs.readFile(metadataPath, 'utf8');
      const metadata = JSON.parse(content);
      
      // Track Lantae-generated files
      for (const filePath of metadata.files || []) {
        const uri = URI.file(path.resolve(rootPath, filePath)).toString();
        lantaeGeneratedFiles.set(uri, metadata.snippets[filePath] || {});
      }
      
      connection.console.log(`Loaded ${lantaeGeneratedFiles.size} Lantae-generated files`);
    }
  } catch (error) {
    connection.console.error(`Error loading Lantae metadata: ${error.message}`);
  }
}

// Register dynamic language support for various file types
async function registerLanguageSupport() {
  const languages = {
    'python': { extensions: ['.py'], commentStyle: '#' },
    'go': { extensions: ['.go'], commentStyle: '//' },
    'rust': { extensions: ['.rs'], commentStyle: '//' },
    'java': { extensions: ['.java'], commentStyle: '//' },
    'cpp': { extensions: ['.cpp', '.cc', '.cxx'], commentStyle: '//' },
    'c': { extensions: ['.c', '.h'], commentStyle: '//' },
    'html': { extensions: ['.html', '.htm'], commentStyle: '<!--' },
    'css': { extensions: ['.css'], commentStyle: '/*' },
    'sql': { extensions: ['.sql'], commentStyle: '--' },
    'shell': { extensions: ['.sh', '.bash'], commentStyle: '#' },
    'yaml': { extensions: ['.yaml', '.yml'], commentStyle: '#' },
    'ruby': { extensions: ['.rb'], commentStyle: '#' },
    'php': { extensions: ['.php'], commentStyle: '//' }
  };
  
  for (const [lang, config] of Object.entries(languages)) {
    languageSupport.set(lang, config);
  }
  
  connection.console.log(`Registered language support for ${languageSupport.size} languages`);
}

// Check if a file is Lantae-generated
function isLantaeGenerated(content, uri) {
  // Check metadata first
  if (lantaeGeneratedFiles.has(uri)) {
    return true;
  }
  
  // Check content patterns
  const patterns = [
    /Generated by Lantae AI/,
    /_lantae_metadata/,
    /Context:.*lantae/i,
    /🤖.*Generated with.*Lantae/i
  ];
  
  return patterns.some(pattern => pattern.test(content));
}

// Get enhanced features for Lantae-generated code
function getLantaeEnhancedFeatures(uri, content) {
  const metadata = lantaeGeneratedFiles.get(uri) || {};
  const isGenerated = isLantaeGenerated(content, uri);
  
  if (!isGenerated) return null;
  
  return {
    isLantaeGenerated: true,
    metadata,
    features: {
      showGenerationInfo: true,
      highlightAICode: true,
      provideAIContext: true,
      enableSmartRefactoring: true
    }
  };
}

// Analyze project structure on startup
async function analyzeProjectStructure() {
  try {
    const workspaceFolders = await connection.workspace.getWorkspaceFolders();
    if (!workspaceFolders || workspaceFolders.length === 0) return;

    const rootPath = URI.parse(workspaceFolders[0].uri).fsPath;
    await scanDirectory(rootPath);
    
    connection.console.log(`Analyzed project structure: ${projectStructure.size} files`);
  } catch (error) {
    connection.console.error(`Error analyzing project: ${error.message}`);
  }
}

async function scanDirectory(dirPath, maxDepth = 5, currentDepth = 0) {
  if (currentDepth >= maxDepth) return;

  try {
    const items = await fs.readdir(dirPath);
    for (const item of items) {
      if (item.startsWith('.') && item !== '.env') continue;
      if (item === 'node_modules') continue;

      const fullPath = path.join(dirPath, item);
      const stat = await fs.stat(fullPath);

      if (stat.isDirectory()) {
        await scanDirectory(fullPath, maxDepth, currentDepth + 1);
      } else if (item.match(/\.(js|ts|json|md)$/)) {
        const uri = URI.file(fullPath).toString();
        try {
          const content = await fs.readFile(fullPath, 'utf8');
          projectStructure.set(uri, {
            path: fullPath,
            type: path.extname(item),
            size: stat.size,
            lastModified: stat.mtime,
            symbols: extractSymbols(content, path.extname(item))
          });
        } catch (readError) {
          // Skip files that can't be read
        }
      }
    }
  } catch (error) {
    // Skip directories that can't be read
  }
}

function extractSymbols(content, fileType) {
  const symbols = [];
  
  if (fileType === '.js') {
    // Extract functions
    const functionRegex = /(?:function\s+(\w+)|(?:const|let|var)\s+(\w+)\s*=\s*(?:function|\([^)]*\)\s*=>)|class\s+(\w+))/g;
    let match;
    while ((match = functionRegex.exec(content)) !== null) {
      const name = match[1] || match[2] || match[3];
      if (name) {
        symbols.push({
          name,
          type: match[3] ? 'class' : 'function',
          line: content.substring(0, match.index).split('\n').length
        });
      }
    }

    // Extract requires/imports
    const requireRegex = /(?:require\(['"`]([^'"`]+)['"`]\)|import.*from\s+['"`]([^'"`]+)['"`])/g;
    while ((match = requireRegex.exec(content)) !== null) {
      const module = match[1] || match[2];
      if (module) {
        symbols.push({
          name: module,
          type: 'import',
          line: content.substring(0, match.index).split('\n').length
        });
      }
    }
  }

  return symbols;
}

async function validateTextDocument(textDocument) {
  const settings = await getDocumentSettings(textDocument.uri);
  const text = textDocument.getText();
  const diagnostics = [];

  if (!settings.enableCodeAnalysis) {
    connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
    return;
  }

  // Cache analysis results
  const cached = fileCache.get(textDocument.uri);
  if (cached && cached.version === textDocument.version) {
    connection.sendDiagnostics({ uri: textDocument.uri, diagnostics: cached.diagnostics });
    return;
  }

  // Basic JavaScript/Node.js analysis
  if (textDocument.uri.endsWith('.js')) {
    // Check for common issues
    await analyzeJavaScript(text, diagnostics, settings);
  }

  // Cache results
  fileCache.set(textDocument.uri, {
    version: textDocument.version,
    diagnostics,
    symbols: extractSymbols(text, path.extname(URI.parse(textDocument.uri).fsPath))
  });

  connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
}

async function analyzeJavaScript(text, diagnostics, settings) {
  const lines = text.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    
    // Check for console.log in production code
    if (line.includes('console.log') && !line.includes('//')) {
      diagnostics.push({
        severity: DiagnosticSeverity.Warning,
        range: {
          start: { line: i, character: line.indexOf('console.log') },
          end: { line: i, character: line.indexOf('console.log') + 11 }
        },
        message: 'Consider removing console.log statements in production code',
        source: 'lantae-lsp',
        code: 'console-log-warning'
      });
    }

    // Check for potential SQL injection
    if (line.match(/query\s*\(\s*['"`][^'"`]*\$\{.*\}/)) {
      diagnostics.push({
        severity: DiagnosticSeverity.Error,
        range: {
          start: { line: i, character: 0 },
          end: { line: i, character: line.length }
        },
        message: 'Potential SQL injection vulnerability - use parameterized queries',
        source: 'lantae-lsp',
        code: 'sql-injection'
      });
    }

    // Check for unused variables (basic check)
    const varMatch = line.match(/(?:const|let|var)\s+(\w+)/);
    if (varMatch) {
      const varName = varMatch[1];
      const remainingText = text.substring(text.indexOf(line) + line.length);
      if (!remainingText.includes(varName) && varName !== 'require' && varName !== 'module') {
        diagnostics.push({
          severity: DiagnosticSeverity.Information,
          range: {
            start: { line: i, character: line.indexOf(varName) },
            end: { line: i, character: line.indexOf(varName) + varName.length }
          },
          message: `Variable '${varName}' is declared but never used`,
          source: 'lantae-lsp',
          code: 'unused-variable'
        });
      }
    }
  }
}

// Completion provider
connection.onCompletion((textDocumentPosition) => {
  const document = documents.get(textDocumentPosition.textDocument.uri);
  if (!document) return [];

  const completions = [];
  const uri = textDocumentPosition.textDocument.uri;
  const content = document.getText();
  
  // Check if this is a Lantae-generated file
  const lantaeFeatures = getLantaeEnhancedFeatures(uri, content);
  
  // Get file language from extension
  const filePath = URI.parse(uri).fsPath;
  const ext = path.extname(filePath).toLowerCase();
  const language = getLanguageFromExtension(ext);

  // Add language-specific completions
  addLanguageCompletions(completions, language);
  
  // Add Lantae-specific completions if it's generated code
  if (lantaeFeatures) {
    addLantaeSpecificCompletions(completions, lantaeFeatures);
  }

  // Add project-specific completions
  for (const [projUri, info] of projectStructure) {
    if (info.symbols) {
      for (const symbol of info.symbols) {
        if (symbol.type === 'function' || symbol.type === 'class') {
          const isLantaeSymbol = lantaeGeneratedFiles.has(projUri);
          completions.push({
            label: symbol.name,
            kind: symbol.type === 'class' ? CompletionItemKind.Class : CompletionItemKind.Function,
            detail: `From ${path.basename(info.path)}${isLantaeSymbol ? ' (Lantae AI)' : ''}`,
            data: { uri: projUri, symbol, isLantaeGenerated: isLantaeSymbol }
          });
        }
      }
    }
  }

  return completions;
});

function getLanguageFromExtension(ext) {
  const languageMap = {
    '.js': 'javascript',
    '.ts': 'typescript',
    '.py': 'python',
    '.go': 'go',
    '.rs': 'rust',
    '.java': 'java',
    '.cpp': 'cpp',
    '.c': 'c',
    '.html': 'html',
    '.css': 'css',
    '.sql': 'sql',
    '.sh': 'shell',
    '.yaml': 'yaml',
    '.yml': 'yaml',
    '.rb': 'ruby',
    '.php': 'php'
  };
  
  return languageMap[ext] || 'text';
}

function addLanguageCompletions(completions, language) {
  const languageCompletions = {
    javascript: [
      { label: 'console.log', kind: CompletionItemKind.Function, detail: 'Log to console' },
      { label: 'require', kind: CompletionItemKind.Function, detail: 'Require module' },
      { label: 'module.exports', kind: CompletionItemKind.Property, detail: 'Export module' },
      { label: 'process.env', kind: CompletionItemKind.Property, detail: 'Environment variables' },
      { label: 'async function', kind: CompletionItemKind.Snippet, detail: 'Async function declaration' },
      { label: 'try/catch', kind: CompletionItemKind.Snippet, detail: 'Try-catch block' }
    ],
    python: [
      { label: 'print', kind: CompletionItemKind.Function, detail: 'Print to console' },
      { label: 'import', kind: CompletionItemKind.Keyword, detail: 'Import module' },
      { label: 'def', kind: CompletionItemKind.Keyword, detail: 'Define function' },
      { label: 'class', kind: CompletionItemKind.Keyword, detail: 'Define class' },
      { label: 'if __name__ == "__main__":', kind: CompletionItemKind.Snippet, detail: 'Main block' }
    ],
    go: [
      { label: 'fmt.Println', kind: CompletionItemKind.Function, detail: 'Print to console' },
      { label: 'func', kind: CompletionItemKind.Keyword, detail: 'Define function' },
      { label: 'package', kind: CompletionItemKind.Keyword, detail: 'Package declaration' },
      { label: 'import', kind: CompletionItemKind.Keyword, detail: 'Import package' }
    ],
    rust: [
      { label: 'println!', kind: CompletionItemKind.Function, detail: 'Print to console' },
      { label: 'fn', kind: CompletionItemKind.Keyword, detail: 'Define function' },
      { label: 'use', kind: CompletionItemKind.Keyword, detail: 'Use declaration' },
      { label: 'struct', kind: CompletionItemKind.Keyword, detail: 'Define struct' }
    ]
  };

  const langCompletions = languageCompletions[language] || [];
  completions.push(...langCompletions);
}

function addLantaeSpecificCompletions(completions, features) {
  // Add AI-specific completions for Lantae-generated code
  completions.push(
    {
      label: '🤖 Lantae: Refactor this code',
      kind: CompletionItemKind.Text,
      detail: 'AI-powered code refactoring',
      insertText: '// Request Lantae AI refactoring',
      data: { isLantaeCommand: true, command: 'refactor' }
    },
    {
      label: '🤖 Lantae: Optimize performance',
      kind: CompletionItemKind.Text,
      detail: 'AI-powered performance optimization',
      insertText: '// Request Lantae AI optimization',
      data: { isLantaeCommand: true, command: 'optimize' }
    },
    {
      label: '🤖 Lantae: Generate tests',
      kind: CompletionItemKind.Text,
      detail: 'AI-powered test generation',
      insertText: '// Request Lantae AI test generation',
      data: { isLantaeCommand: true, command: 'test' }
    },
    {
      label: '🤖 Lantae: Add documentation',
      kind: CompletionItemKind.Text,
      detail: 'AI-powered documentation generation',
      insertText: '// Request Lantae AI documentation',
      data: { isLantaeCommand: true, command: 'document' }
    }
  );
  
  // Add generation context info
  if (features.metadata.timestamp) {
    completions.push({
      label: `📅 Generated: ${new Date(features.metadata.timestamp).toLocaleDateString()}`,
      kind: CompletionItemKind.Text,
      detail: 'Lantae generation timestamp',
      insertText: `// Generated by Lantae AI on ${new Date(features.metadata.timestamp).toLocaleDateString()}`,
      data: { isLantaeInfo: true }
    });
  }
}

// Completion resolve
connection.onCompletionResolve((item) => {
  if (item.data && item.data.symbol) {
    item.documentation = `${item.data.symbol.type} from line ${item.data.symbol.line}`;
  }
  return item;
});

// Hover provider
connection.onHover(async (params) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return null;

  const position = params.position;
  const text = document.getText();
  const lines = text.split('\n');
  const line = lines[position.line];
  const uri = params.textDocument.uri;
  
  // Check if this is Lantae-generated code
  const lantaeFeatures = getLantaeEnhancedFeatures(uri, text);
  
  // Get word at position
  const wordRange = getWordRangeAtPosition(line, position.character);
  if (!wordRange) {
    // If no word, but it's Lantae code, show file info
    if (lantaeFeatures) {
      return {
        contents: {
          kind: 'markdown',
          value: getLantaeFileInfo(lantaeFeatures)
        }
      };
    }
    return null;
  }

  const word = line.substring(wordRange.start, wordRange.end);

  // Look up in project symbols
  for (const [projUri, info] of projectStructure) {
    if (info.symbols) {
      const symbol = info.symbols.find(s => s.name === word);
      if (symbol) {
        const isLantaeSymbol = lantaeGeneratedFiles.has(projUri);
        let hoverText = `**${symbol.name}** (${symbol.type})\n\nDefined in: ${path.basename(info.path)}:${symbol.line}`;
        
        if (isLantaeSymbol) {
          hoverText += `\n\n🤖 **Generated by Lantae AI**`;
          const metadata = lantaeGeneratedFiles.get(projUri);
          if (metadata.timestamp) {
            hoverText += `\n📅 Created: ${new Date(metadata.timestamp).toLocaleString()}`;
          }
          if (metadata.tool) {
            hoverText += `\n🔧 Tool: ${metadata.tool}`;
          }
        }
        
        return {
          contents: {
            kind: 'markdown',
            value: hoverText
          }
        };
      }
    }
  }

  // Get language-specific built-ins
  const filePath = URI.parse(uri).fsPath;
  const ext = path.extname(filePath).toLowerCase();
  const language = getLanguageFromExtension(ext);
  
  const builtinInfo = getLanguageBuiltins(language, word);
  if (builtinInfo) {
    let hoverText = `**${word}** (${language} built-in)\n\n${builtinInfo}`;
    
    if (lantaeFeatures) {
      hoverText += `\n\n---\n🤖 **This file was generated by Lantae AI**`;
    }
    
    return {
      contents: {
        kind: 'markdown',
        value: hoverText
      }
    };
  }

  // If it's Lantae code but no specific symbol found, show context
  if (lantaeFeatures) {
    return {
      contents: {
        kind: 'markdown',
        value: `🤖 **Lantae AI Generated Code**\n\n${getLantaeFileInfo(lantaeFeatures)}`
      }
    };
  }

  return null;
});

function getLanguageBuiltins(language, word) {
  const builtins = {
    javascript: {
      'require': 'Load a module - `require(id)`',
      'console': 'Console utilities for debugging and logging',
      'process': 'Global process object with environment and process info',
      'Buffer': 'Binary data handling utilities',
      '__dirname': 'Directory name of the current module',
      '__filename': 'File name of the current module'
    },
    python: {
      'print': 'Print objects to stdout - `print(*values, sep=" ", end="\\n")`',
      'len': 'Return the length of an object - `len(obj)`',
      'range': 'Generate a sequence of numbers - `range(start, stop, step)`',
      'str': 'String type constructor - `str(object="")`',
      'int': 'Integer type constructor - `int(x, base=10)`',
      'list': 'List type constructor - `list(iterable=())`'
    },
    go: {
      'fmt': 'Package for formatted I/O operations',
      'Println': 'Print to stdout with newline - `fmt.Println(a ...interface{})`',
      'Printf': 'Formatted print to stdout - `fmt.Printf(format string, a ...interface{})`',
      'make': 'Allocate and initialize slice, map, or channel - `make(Type, size)`',
      'len': 'Return length of slice, array, string, map, or channel - `len(v Type)`'
    },
    rust: {
      'println': 'Print to stdout with newline - `println!(format, args...)`',
      'print': 'Print to stdout - `print!(format, args...)`',
      'vec': 'Create a new vector - `vec![elements...]`',
      'String': 'Owned string type - `String::new()`',
      'Option': 'Nullable type - `Option<T>`'
    }
  };
  
  return builtins[language]?.[word];
}

function getLantaeFileInfo(features) {
  let info = `This code was generated by **Lantae AI v${features.metadata.version || '1.0.0'}**`;
  
  if (features.metadata.timestamp) {
    info += `\n📅 **Generated:** ${new Date(features.metadata.timestamp).toLocaleString()}`;
  }
  
  if (features.metadata.tool) {
    info += `\n🔧 **Tool:** ${features.metadata.tool}`;
  }
  
  if (features.metadata.language) {
    info += `\n💾 **Language:** ${features.metadata.language}`;
  }
  
  info += `\n\n**Available AI Actions:**`;
  info += `\n• Refactor this code`;
  info += `\n• Optimize performance`;  
  info += `\n• Generate tests`;
  info += `\n• Add documentation`;
  
  return info;
}

function getWordRangeAtPosition(line, character) {
  let start = character;
  let end = character;

  // Move start backwards to find word start
  while (start > 0 && /\w/.test(line[start - 1])) {
    start--;
  }

  // Move end forwards to find word end
  while (end < line.length && /\w/.test(line[end])) {
    end++;
  }

  if (start === end) return null;

  return { start, end };
}

// Definition provider
connection.onDefinition(async (params) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return null;

  const position = params.position;
  const text = document.getText();
  const lines = text.split('\n');
  const line = lines[position.line];
  
  const wordRange = getWordRangeAtPosition(line, position.character);
  if (!wordRange) return null;

  const word = line.substring(wordRange.start, wordRange.end);

  // Search for definition in project files
  for (const [uri, info] of projectStructure) {
    if (info.symbols) {
      const symbol = info.symbols.find(s => s.name === word && (s.type === 'function' || s.type === 'class'));
      if (symbol) {
        return {
          uri,
          range: {
            start: { line: symbol.line - 1, character: 0 },
            end: { line: symbol.line - 1, character: word.length }
          }
        };
      }
    }
  }

  return null;
});

// Execute command
connection.onExecuteCommand(async (params) => {
  const command = params.command;
  const args = params.arguments || [];

  switch (command) {
    case 'lantae.analyzeFile':
      if (args[0]) {
        const uri = args[0];
        const document = documents.get(uri);
        if (document) {
          await validateTextDocument(document);
          return { message: 'File analysis completed' };
        }
      }
      break;

    case 'lantae.optimizeCode':
      // Placeholder for AI-powered code optimization
      return { message: 'Code optimization feature coming soon' };

    case 'lantae.generateTests':
      // Placeholder for AI-powered test generation
      return { message: 'Test generation feature coming soon' };

    case 'lantae.refactorCode':
      // Placeholder for AI-powered refactoring
      return { message: 'Code refactoring feature coming soon' };
  }

  return { message: 'Command executed' };
});

// Document formatting
connection.onDocumentFormatting(async (params) => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return null;

  // Basic JavaScript formatting (placeholder)
  // In a real implementation, you'd use a proper formatter like Prettier
  return [];
});

// Make the text document manager listen on the connection
documents.listen(connection);

// Listen on the connection
connection.listen();

connection.console.log('Lantae LSP Server started');