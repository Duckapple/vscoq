import React, {useState, useCallback, useEffect, KeyboardEventHandler, ChangeEventHandler, ChangeEvent} from 'react';
import { vscode } from "./utilities/vscode";
import "./App.css";
import { DidChangeWorkspaceFoldersNotification } from 'vscode-languageclient';
import { PropertyStyleSheetBehavior } from '@microsoft/fast-foundation';
import SearchField from './components/molecules/SearchField';
import SearchPage from './components/templates/SearchPage';

type SearchResult = {
    id: string, 
    name: string,
    statement: string
};

const app = () => {
    
    const [searchString, setSearchString] = useState("");
    const [searchResults, setSearchResults] = useState<SearchResult[]>([]);

    const handleMessage = useCallback ((msg: any) => {
        switch (msg.data.command) {
            case 'renderResult':
                const result = msg.data.text;
                setSearchResults(searchResults => searchResults.concat([result]));
                break;
            case 'searching': 
                //TODO: Add UI elements to show user the searching state
                break;
            
        }
      }, []);
    
    useEffect(() => {
        window.addEventListener("message", handleMessage);
        return () => {
            window.removeEventListener("message", handleMessage);
        };
    }, [handleMessage]);
                
    
    const searchFieldInputHandler: ChangeEventHandler<HTMLInputElement> = (e) => {
        setSearchString(e.target.value);
    };

    const searchFieldEnterHandler: KeyboardEventHandler<HTMLInputElement> = (e) => {
            
        if(e.code === "Enter") {
            
            vscode.postMessage({
                command: "coqSearch",
                text: searchString,
            });

        }

    };

    const copyNameToClipboard = (name: string) => {
        vscode.postMessage({
            command: "copySearchResult",
            text: name
        });
    };
    

    return (
        <main>
            <SearchPage
                value={searchString} 
                onTextInput={searchFieldInputHandler} 
                handleSearch={searchFieldEnterHandler} 
                copyNameHandler={copyNameToClipboard}
                results={searchResults}
            />
        </main>
    );
};

export default app;