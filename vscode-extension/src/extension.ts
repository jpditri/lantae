import * as path from 'path';
import { workspace, ExtensionContext, commands } from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  // The server is implemented in node
  const serverModule = context.asAbsolutePath(
    path.join('..', 'lsp-server.js')
  );
  
  // The debug options for the server
  const debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  const serverOptions: ServerOptions = {
    run: { module: serverModule, transport: TransportKind.ipc },
    debug: {
      module: serverModule,
      transport: TransportKind.ipc,
      options: debugOptions
    }
  };

  // Options to control the language client
  const clientOptions: LanguageClientOptions = {
    // Register the server for multiple language documents
    documentSelector: [
      { scheme: 'file', language: 'javascript' },
      { scheme: 'file', language: 'typescript' },
      { scheme: 'file', language: 'python' },
      { scheme: 'file', language: 'go' },
      { scheme: 'file', language: 'rust' },
      { scheme: 'file', language: 'java' },
      { scheme: 'file', language: 'cpp' },
      { scheme: 'file', language: 'c' },
      { scheme: 'file', language: 'html' },
      { scheme: 'file', language: 'css' },
      { scheme: 'file', language: 'sql' },
      { scheme: 'file', language: 'shellscript' },
      { scheme: 'file', language: 'yaml' },
      { scheme: 'file', language: 'ruby' },
      { scheme: 'file', language: 'php' }
    ],
    synchronize: {
      // Notify the server about file changes to Lantae metadata files
      fileEvents: workspace.createFileSystemWatcher('**/.lantae-generated.json')
    }
  };

  // Create the language client and start the client.
  client = new LanguageClient(
    'lantaeLSP',
    'Lantae Language Server',
    serverOptions,
    clientOptions
  );

  // Register commands
  const analyzeFileCommand = commands.registerCommand('lantae.analyzeFile', async () => {
    const editor = workspace.activeTextEditor;
    if (editor) {
      await client.sendRequest('workspace/executeCommand', {
        command: 'lantae.analyzeFile',
        arguments: [editor.document.uri.toString()]
      });
    }
  });

  const optimizeCodeCommand = commands.registerCommand('lantae.optimizeCode', async () => {
    const editor = workspace.activeTextEditor;
    if (editor) {
      await client.sendRequest('workspace/executeCommand', {
        command: 'lantae.optimizeCode',
        arguments: [editor.document.uri.toString()]
      });
    }
  });

  const generateTestsCommand = commands.registerCommand('lantae.generateTests', async () => {
    const editor = workspace.activeTextEditor;
    if (editor) {
      await client.sendRequest('workspace/executeCommand', {
        command: 'lantae.generateTests',
        arguments: [editor.document.uri.toString()]  
      });
    }
  });

  const refactorCodeCommand = commands.registerCommand('lantae.refactorCode', async () => {
    const editor = workspace.activeTextEditor;
    if (editor) {
      await client.sendRequest('workspace/executeCommand', {
        command: 'lantae.refactorCode',
        arguments: [editor.document.uri.toString()]
      });
    }
  });

  context.subscriptions.push(
    analyzeFileCommand,
    optimizeCodeCommand, 
    generateTestsCommand,
    refactorCodeCommand
  );

  // Start the client. This will also launch the server
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}