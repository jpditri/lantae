import * as vscode from 'vscode';
import { LantaeProvider } from './provider';

export class LantaeDiagnostics {
    private _diagnosticCollection: vscode.DiagnosticCollection;
    private _provider?: LantaeProvider;

    constructor(private readonly _context: vscode.ExtensionContext) {
        this._diagnosticCollection = vscode.languages.createDiagnosticCollection('lantae');
        this._context.subscriptions.push(this._diagnosticCollection);

        // Register document change listener
        vscode.workspace.onDidChangeTextDocument(
            this.onDocumentChange.bind(this),
            null,
            this._context.subscriptions
        );

        // Register document open listener
        vscode.workspace.onDidOpenTextDocument(
            this.analyzeDocument.bind(this),
            null,
            this._context.subscriptions
        );
    }

    public setProvider(provider: LantaeProvider): void {
        this._provider = provider;
    }

    private async onDocumentChange(event: vscode.TextDocumentChangeEvent): Promise<void> {
        const config = vscode.workspace.getConfiguration('lantae');
        
        if (!config.get('enableDiagnostics')) {
            return;
        }

        // Debounce document changes
        setTimeout(() => {
            this.analyzeDocument(event.document);
        }, 1000);
    }

    private async analyzeDocument(document: vscode.TextDocument): Promise<void> {
        const config = vscode.workspace.getConfiguration('lantae');
        
        if (!config.get('enableDiagnostics') || !this._provider) {
            return;
        }

        // Only analyze certain file types
        const supportedLanguages = [
            'typescript', 'javascript', 'python', 'ruby', 'go', 'rust', 'java', 'cpp', 'c'
        ];

        if (!supportedLanguages.includes(document.languageId)) {
            return;
        }

        try {
            const diagnostics = await this.generateDiagnostics(document);
            this._diagnosticCollection.set(document.uri, diagnostics);
        } catch (error) {
            console.error('Error generating diagnostics:', error);
        }
    }

    private async generateDiagnostics(document: vscode.TextDocument): Promise<vscode.Diagnostic[]> {
        const diagnostics: vscode.Diagnostic[] = [];
        const text = document.getText();
        const lines = text.split('\n');

        // Analyze the code for potential issues
        const issues = await this.analyzeCodeIssues(text, document.languageId);

        for (const issue of issues) {
            const range = this.createRange(issue.line, issue.column, issue.length);
            const diagnostic = new vscode.Diagnostic(
                range,
                issue.message,
                this.getSeverity(issue.severity)
            );

            diagnostic.source = 'Lantae AI';
            diagnostic.code = issue.code;

            // Add quick fix if available
            if (issue.quickFix) {
                diagnostic.tags = [vscode.DiagnosticTag.Unnecessary];
            }

            diagnostics.push(diagnostic);
        }

        return diagnostics;
    }

    private async analyzeCodeIssues(code: string, language: string): Promise<CodeIssue[]> {
        if (!this._provider) {
            return [];
        }

        const prompt = `Analyze this ${language} code for potential issues, bugs, and improvements. 
        Return a JSON array of issues with the following format:
        [
          {
            "line": 0,
            "column": 0,
            "length": 10,
            "message": "Description of the issue",
            "severity": "error|warning|info",
            "code": "issue_code",
            "quickFix": "suggested fix if available"
          }
        ]

        Code to analyze:
        \`\`\`${language}
        ${code}
        \`\`\`

        Only return the JSON array, no other text.`;

        try {
            const response = await this._provider.sendMessage(prompt);
            
            // Parse JSON response
            const jsonMatch = response.match(/\[[\s\S]*\]/);
            if (jsonMatch) {
                const issues = JSON.parse(jsonMatch[0]);
                return issues.filter((issue: any) => this.isValidIssue(issue));
            }
        } catch (error) {
            console.error('Error parsing diagnostics response:', error);
        }

        return [];
    }

    private isValidIssue(issue: any): boolean {
        return (
            typeof issue.line === 'number' &&
            typeof issue.column === 'number' &&
            typeof issue.message === 'string' &&
            ['error', 'warning', 'info'].includes(issue.severity)
        );
    }

    private createRange(line: number, column: number, length: number): vscode.Range {
        const start = new vscode.Position(Math.max(0, line), Math.max(0, column));
        const end = new vscode.Position(Math.max(0, line), Math.max(0, column + (length || 1)));
        return new vscode.Range(start, end);
    }

    private getSeverity(severity: string): vscode.DiagnosticSeverity {
        switch (severity.toLowerCase()) {
            case 'error':
                return vscode.DiagnosticSeverity.Error;
            case 'warning':
                return vscode.DiagnosticSeverity.Warning;
            case 'info':
                return vscode.DiagnosticSeverity.Information;
            default:
                return vscode.DiagnosticSeverity.Hint;
        }
    }

    public clearDiagnostics(): void {
        this._diagnosticCollection.clear();
    }
}

interface CodeIssue {
    line: number;
    column: number;
    length?: number;
    message: string;
    severity: 'error' | 'warning' | 'info';
    code?: string;
    quickFix?: string;
}