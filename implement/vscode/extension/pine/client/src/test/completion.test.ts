/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as vscode from 'vscode';
import * as assert from 'assert';
import { getDocUri, activate, sleep } from './helper';

suite('Reference CodeLens', () => {
	const docUri = getDocUri('src/Main.elm');

	test('Shows usage count and includes the declaration in navigation', async () => {
		await activate(docUri);

		const lenses = await waitForResolvedCodeLenses(docUri);
		const helperLens =
			lenses.find(lens => lens.command?.title === '2 references');

		assert.ok(helperLens?.command);
		assert.equal(helperLens.command.command, 'pine.client.peekReferences');

		const references = await vscode.commands.executeCommand<vscode.Location[]>(
			'vscode.executeReferenceProvider',
			docUri,
			new vscode.Position(4, 0));

		assert.equal(references.length, 3);
		assert.deepEqual(references[0].range, new vscode.Range(4, 0, 4, 6));
	});
});

async function waitForResolvedCodeLenses(docUri: vscode.Uri): Promise<vscode.CodeLens[]> {
	for (let attempt = 0; attempt < 30; attempt++) {
		const lenses = await vscode.commands.executeCommand<vscode.CodeLens[]>(
			'vscode.executeCodeLensProvider',
			docUri,
			20);

		if (lenses?.some(lens => lens.command)) {
			return lenses;
		}

		await sleep(500);
	}

	throw new Error('Timed out waiting for resolved CodeLens results.');
}
