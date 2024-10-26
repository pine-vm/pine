import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';

import {
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
