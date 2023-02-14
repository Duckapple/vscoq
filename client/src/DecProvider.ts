import { pathToFileURL } from "url";
import { CancellationToken, Declaration, DeclarationProvider, Location, Position, ProviderResult, TextDocument, Uri } from "vscode";
import { RequestType, VersionedTextDocumentIdentifier } from "vscode-languageclient";
import Client from "./client";
import { CompletionItemCoq, CompletionItemCoqRequest, CompletionItemCoqResponse, DeclarationLocationCoqRequest, DeclarationLocationCoqResponse } from "./protocol/types";
import * as fs from 'fs';

export default class DecProvider implements DeclarationProvider {
    client: Client;
    coqlibPath: URL | undefined;
    
    constructor(client: Client, coqlibPath : string) {
        this.client = client;
        this.coqlibPath = coqlibPath ? pathToFileURL(coqlibPath) : undefined; 
    }

    async provideDeclaration(document: TextDocument, position: Position, token: CancellationToken): Promise<Declaration> {
        if (!this.coqlibPath) {
            throw new Error("No path set to CoqLib");
        }
        let coqIdentRegex = /(\.?(\w|\_)(\w|\d|\_|\')*)+/;
        let wordRange = document.getWordRangeAtPosition(position, coqIdentRegex);
        let requestedDeclaration = document.getText(wordRange);
        let response = await this.sendDeclarationLocationRequest(document.uri, document.version, position, requestedDeclaration);
        let path = response.path.replace(/\.vo$/, ".v");
        if (path && fs.existsSync(path)) {
            return new Location(Uri.parse(path), new Position(0, 0));
        }
        throw new Error(`Could not find path to ${requestedDeclaration}`);
    }

    private sendDeclarationLocationRequest(uri: Uri, version: number, position: Position, requestedDeclaration: string): Promise<DeclarationLocationCoqResponse> {
        const req = new RequestType<DeclarationLocationCoqRequest, DeclarationLocationCoqResponse, void>("vscoq/declarationLocation");
        let textDocument = VersionedTextDocumentIdentifier.create(
            uri.toString(),
            version
        );
        const params: DeclarationLocationCoqRequest = { textDocument, position, requestedDeclaration };
        return this.client.sendRequest(req, params).then(
            (response: DeclarationLocationCoqResponse) => {
                return response;
            }
        );
    }
}