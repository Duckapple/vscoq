import * as vscode from 'vscode';

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions
} from 'vscode-languageclient/node';

import {decorationsManual, decorationsContinuous} from './Decorations';

export default class Client extends LanguageClient {

	private _channel: any = vscode.window.createOutputChannel('vscoq');
    private _decorations: Map<String, vscode.Range[]> = new Map<String, vscode.Range[]>();

	constructor(
        serverOptions: ServerOptions,
        clientOptions: LanguageClientOptions,
	) {
        super(
		    'vscoq-language-server',
		    'Coq Language Server',
		    serverOptions,
		    clientOptions
        );
		this._channel.appendLine("vscoq initialised");
	}

    dispose(): void {

    };

    public writeToChannel(message: string) {
        this._channel.appendLine(message);
    };

    public saveHighlights(uri: String, parsedRange: vscode.Range[], processingRange: vscode.Range[], processedRange: vscode.Range[]) {
        this._decorations.set(uri, processedRange);
    }

    public updateHightlights() {
        for(let entry of this._decorations.entries()) {
            this.updateDocumentEditors(entry[0], entry[1]);
        }
    };

    private getDocumentEditors(uri: String) {
        return vscode.window.visibleTextEditors.filter(editor => {
            return editor.document.uri.toString() === uri;
        });
    }

    private updateDocumentEditors(uri: String, ranges: vscode.Range[]) {
        const config = vscode.workspace.getConfiguration('vscoq.proof');
        const editors = this.getDocumentEditors(uri);
        editors.map(editor => {
            if(config.mode === 0) {
                editor.setDecorations(decorationsManual.processed, ranges);
            } else {
                editor.setDecorations(decorationsContinuous.processed, ranges);
            }
        });
    }

}
