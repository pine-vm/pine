/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as vscode from 'vscode';
import * as assert from 'assert';
import { getDocUri, activate, sleep } from './helper';

suite('Module-level CodeLens', () => {
	const docUri = getDocUri('src/Main.elm');

	test('Does not create a lens for a local declaration', async () => {
		await activate(docUri);

		let lenses: vscode.CodeLens[] = [];

		for (let attempt = 0; attempt < 30; attempt++) {
			lenses = await vscode.commands.executeCommand<vscode.CodeLens[]>(
				'vscode.executeCodeLensProvider',
				docUri) ?? [];

			if (lenses.length > 0) {
				break;
			}

			await sleep(500);
		}

		assert.equal(lenses.length, 2);
		assert.ok(lenses.every(lens => lens.range.start.line !== 10));
	});
});