import { Disposable, Webview, WebviewPanel, window, workspace, Uri, ViewColumn, TextEditor, Position } from "vscode";
import { UpdateProofViewRequest, UpdateProofViewResponse } from '../protocol/types';
import {
  RequestType,
  VersionedTextDocumentIdentifier,
} from "vscode-languageclient";
import { LanguageClient } from "vscode-languageclient/node";
import { getUri } from "../utilities/getUri";
import { getNonce } from "../utilities/getNonce";

// /////////////////////////////////////////////////////////////////////////////
// GOAL VIEW PANEL CODE
// /////////////////////////////////////////////////////////////////////////////

/**
 * This class manages the state and behavior of Goal webview panels.
 *
 * It contains all the data and methods for:
 *
 * - Creating and rendering Goal webview panels
 * - Properly cleaning up and disposing of webview resources when the panel is closed
 * - Setting the HTML (and by proxy CSS/JavaScript) content of the webview panel
 * - Setting message listeners so data can be passed between the webview and extension
 */
export default class GoalPanel {

  public static currentPanel: GoalPanel | undefined;
  private readonly _panel: WebviewPanel;
  private _disposables: Disposable[] = [];
  private static _channel: any = window.createOutputChannel('vscoq-goal-panel');


  /**
   * The GoalPanel class private constructor (called only from the render method).
   *
   * @param panel A reference to the webview panel
   * @param extensionUri The URI of the directory containing the extension
   */
  private constructor(panel: WebviewPanel, extensionUri: Uri) {
    this._panel = panel;

    // Set an event listener to listen for when the panel is disposed (i.e. when the user closes
    // the panel or when the panel is closed programmatically)
    this._panel.onDidDispose(() => this.dispose(), null, this._disposables);

    // Set the HTML content for the webview panel
    this._panel.webview.html = this._getWebviewContent(this._panel.webview, extensionUri);

    // Set an event listener to listen for messages passed from the webview context
    this._setWebviewMessageListener(this._panel.webview);
  }

  /**
   * Renders the current webview panel if it exists otherwise a new webview panel
   * will be created and displayed.
   *
   * @param extensionUri The URI of the directory containing the extension.
   */
  public static render(editor: TextEditor, extensionUri: Uri) {

    //Get the correct view column
    let column = editor && editor.viewColumn ? editor.viewColumn + 1 : ViewColumn.Two;
    if (column === 4) { column = ViewColumn.Three; }

    if (GoalPanel.currentPanel) {
      // If the webview panel already exists reveal it
      GoalPanel.currentPanel._panel.reveal(column);
    } else {
      // If a webview panel does not already exist create and show a new one
      const panel = window.createWebviewPanel(
        // Panel view type
        "coq",
        // Panel title
        "Coq Goals",
        // The editor column the panel should be displayed in
        {preserveFocus: true, viewColumn: column },
        // Extra panel configurations
        {
          // Enable JavaScript in the webview
          enableScripts: true,
          // Restrict the webview to only load resources from the `out` and `webview-ui/build` directories
          localResourceRoots: [Uri.joinPath(extensionUri, "out"), Uri.joinPath(extensionUri, "goal-view-ui/build")],

          retainContextWhenHidden: true,
        }
      );

      GoalPanel.currentPanel = new GoalPanel(panel, extensionUri);
      GoalPanel._initWebAppSettings();

    }
  }

  /**
   * Cleans up and disposes of webview resources when the webview panel is closed.
   */
  public dispose() {
    GoalPanel.currentPanel = undefined;

    // Dispose of the current webview panel
    this._panel.dispose();

    // Dispose of all disposables (i.e. commands) for the current webview panel
    while (this._disposables.length) {
      const disposable = this._disposables.pop();
      if (disposable) {
        disposable.dispose();
      }
    }
  }


  // /////////////////////////////////////////////////////////////////////////////
  // Send a request to the server to update the current goals
  // /////////////////////////////////////////////////////////////////////////////
  public static sendProofViewRequest(client: LanguageClient, uri: Uri, version: number, position: Position) {
    const req = new RequestType<UpdateProofViewRequest, UpdateProofViewResponse, void>("vscoq/updateProofView");
    let textDocument = VersionedTextDocumentIdentifier.create(
      uri.toString(),
      version
    );
    const params: UpdateProofViewRequest = { textDocument, position };
    client.sendRequest(req, params).then(
      (response : UpdateProofViewResponse) => {
        GoalPanel.currentPanel?._panel.webview.postMessage({ "command": "renderProofView", "proofView": response });
      }
    );
  }

  private static _initWebAppSettings() {
    const config = workspace.getConfiguration('vscoq.goals');
    GoalPanel.currentPanel?._panel.webview.postMessage({ "command": "initAppSettings", "text": config.display });
  };

  
  /**
   * Defines and returns the HTML that should be rendered within the webview panel.
   *
   * @remarks This is also the place where references to the React webview build files
   * are created and inserted into the webview HTML.
   *
   * @param webview A reference to the extension webview
   * @param extensionUri The URI of the directory containing the extension
   * @returns A template string literal containing the HTML that should be
   * rendered within the webview panel
   */
  private _getWebviewContent(webview: Webview, extensionUri: Uri) {
    // The CSS file from the React build output
    const stylesUri = getUri(webview, extensionUri, ["goal-view-ui", "build", "assets", "index.css"]);
    // The JS file from the React build output
    const scriptUri = getUri(webview, extensionUri, ["goal-view-ui", "build", "assets", "index.js"]);

    const nonce = getNonce();

    // Tip: Install the es6-string-html VS Code extension to enable code highlighting below
    return /*html*/ `
      <!DOCTYPE html>
      <html lang="en">
        <head>
          <meta charset="UTF-8" />
          <meta name="viewport" content="width=device-width, initial-scale=1.0" />
          <meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src ${webview.cspSource}; script-src 'nonce-${nonce}';">
          <link rel="stylesheet" type="text/css" nonce="${nonce}" href="${stylesUri}">
          <title>Goal View</title>
        </head>
        <body>
          <div id="root"></div>
          <script type="module" nonce="${nonce}" src="${scriptUri}"></script>
        </body>
      </html>
    `;
  }

  /**
   * Sets up an event listener to listen for messages passed from the webview context and
   * executes code based on the message that is recieved.
   *
   * @param webview A reference to the extension webview
   * @param context A reference to the extension context
   */
  private _setWebviewMessageListener(webview: Webview) {
    webview.onDidReceiveMessage(
      (message: any) => {
        const command = message.command;
        const text = message.text;

        switch (command) {
            // Add more switch case statements here as more webview message commands
            // are created within the webview context (i.e. inside media/main.js)
            case "coqSearch":
                window.showInformationMessage(text);
                return;
                
        }
      },
      undefined,
      this._disposables
    );
  }
}
