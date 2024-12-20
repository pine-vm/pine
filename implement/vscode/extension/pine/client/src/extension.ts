import * as path from 'path';
import { workspace, ExtensionContext, window, OutputChannel } from 'vscode';

import {
    CloseAction,
    ErrorAction,
    ErrorHandlerResult,
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;


const langServerExecutablePathDefault: string =
    "pine";


const logDir: string =
    "./ls-log/";

const pineOutputChannel: OutputChannel =
    window.createOutputChannel("Pine Logs");

function buildServerOptions(context: ExtensionContext): ServerOptions {

    const config =
        workspace.getConfiguration('pineLanguageServer');

    const customPath =
        sanitizePathFromSettings(
            config.get<string>('pathToPineBinary')?.trim() || '');

    const langServerExecutablePath =
        customPath !== ''
            ? customPath
            : langServerExecutablePathDefault;

    pineOutputChannel.appendLine("lang-server executable path: " + langServerExecutablePath);

    return {
        run: {
            command: langServerExecutablePath,
            args: ["lsp"],
            transport: TransportKind.stdio
        },
        debug: {
            command: langServerExecutablePath,
            args: ["lsp"],
            transport: TransportKind.stdio
        }
    };
}

function sanitizePathFromSettings(input: string): string {
    /*
    Remove leading and trailing quotes,
    as could be present when using "Copy as path" in Windows Explorer
    */
    const withoutQuotes = input.replace(/^"|"$/g, '');

    return withoutQuotes;
}

export function activate(context: ExtensionContext) {

    pineOutputChannel.appendLine("Pine extension activated.");

    const serverOptions: ServerOptions = buildServerOptions(context);

    // Options to control the language client
    const clientOptions: LanguageClientOptions = {
        // Register the server for plain text documents
        documentSelector: [{ scheme: 'file', language: 'elm' }],
        synchronize: {
            // Notify the server about file changes
            fileEvents: [
                workspace.createFileSystemWatcher('**/.elm'),
                workspace.createFileSystemWatcher('**/elm.json')
            ]
        },
        initializationFailedHandler: (error) => {

            console.error("Init ls failed: ", error);

            return true;
        },

        /*

        export type ErrorHandlerResult = {
            /**
             * The action to take.
             *
            action: ErrorAction;
            /**
             * An optional message to be presented to the user.
             *
            message?: string;
            /**
             * If set to true the client assumes that the corresponding
             * error handler has presented an appropriate message to the
             * user and the message will only be log to the client's
             * output channel.
             *
            handled?: boolean;
        };
        */
        errorHandler: {

            error: (error, message, count) => {

                console.error("errorHandler.error: ", error, message, count);

                const errorHandlerResult: ErrorHandlerResult = {
                    action: ErrorAction.Continue,
                    message: "Failed message to Pine language server: " + error.name + ": " + error.message,
                    handled: null
                };

                return errorHandlerResult;
            },

            closed: () => {
                console.error("closed");

                return {
                    action: CloseAction.DoNotRestart,
                    message: "closed connection to Pine language server"
                };
            }
        }
    };

    // Create the language client and start the client.
    client = new LanguageClient(
        'pineLanguageServer',
        'Pine Language Server Client',
        serverOptions,
        clientOptions
    );

    console.log("client start...");

    // Start the client. This will also launch the server
    client.start();
    console.log("client started");

    // listen for configuration changes
    workspace.onDidChangeConfiguration(e => {
        if (e.affectsConfiguration('pineLanguageServer.trace.server')) {

        }
    });
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
