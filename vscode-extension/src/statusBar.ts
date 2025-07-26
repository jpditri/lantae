import * as vscode from 'vscode';

export class StatusBarManager {
    private _statusBarItem: vscode.StatusBarItem;

    constructor(private readonly _context: vscode.ExtensionContext) {
        // Create status bar item
        this._statusBarItem = vscode.window.createStatusBarItem(
            vscode.StatusBarAlignment.Right,
            100
        );
        
        this._statusBarItem.command = 'lantae.showStatus';
        this._statusBarItem.tooltip = 'Click to show Lantae status';
        this._statusBarItem.show();

        // Add to subscriptions for cleanup
        this._context.subscriptions.push(this._statusBarItem);

        // Initialize with default text
        this.update('ollama', 'cogito:latest');
    }

    public update(provider: string, model: string): void {
        this._statusBarItem.text = `$(robot) ${provider}:${model}`;
    }

    public setConnected(connected: boolean): void {
        const icon = connected ? '$(check)' : '$(error)';
        const currentText = this._statusBarItem.text;
        
        // Replace or add status icon
        if (currentText.includes('$(check)') || currentText.includes('$(error)')) {
            this._statusBarItem.text = currentText.replace(/\$\(check\)|\$\(error\)/, icon);
        } else {
            this._statusBarItem.text = `${icon} ${currentText}`;
        }
    }

    public setError(error: string): void {
        this._statusBarItem.text = `$(error) Lantae Error`;
        this._statusBarItem.tooltip = `Lantae Error: ${error}`;
    }

    public dispose(): void {
        this._statusBarItem.dispose();
    }
}