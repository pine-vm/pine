import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';

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


const langServerCommand: string =
	"pine";


const logDir: string =
	"./ls-log/";

function buildServerOptions(context: ExtensionContext): ServerOptions {

	/*
	https://github.com/denoland/vscode_deno/blob/df2633e58a95554158065ac5e6fc9b87e6c02c7a/client/src/commands.ts#L167-L179
	*/
	return {
		run: {
			command: langServerCommand
			// , args: ["lsp", "--log-dir=" + logDir]
			, args: ["lsp"]
			, transport: TransportKind.stdio
		},
		debug: {
			command: langServerCommand
			// , args: ["lsp", "--log-dir=" + logDir]
			, args: ["lsp"]
			, transport: TransportKind.stdio
		},
	};
}

export function activate(context: ExtensionContext) {

	console.info("Begin activate...");

	const serverOptions: ServerOptions =
		buildServerOptions(context);


	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [{ scheme: 'file', language: 'elm' }],
		synchronize: {
			// Notify the server about file changes
			fileEvents: [
				workspace.createFileSystemWatcher('**/.elm')
				, workspace.createFileSystemWatcher('**/elm.json')]
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
		'pine-ls-client',
		'Pine Language Server Client',
		serverOptions,
		clientOptions
	);


	console.log("client start...");


	// Start the client. This will also launch the server
	client.start();


	console.log("client started");
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
