/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';

import { runTests } from '@vscode/test-electron';

async function main() {
	try {
		// The folder containing the Extension Manifest package.json
		// Passed to `--extensionDevelopmentPath`
		const extensionDevelopmentPath = path.resolve(__dirname, '../../../');

		// The path to test runner
		// Passed to --extensionTestsPath
		const extensionTestsPath = path.resolve(__dirname, './index');
		const currentServerWorkspace = path.resolve(__dirname, '../../testFixture');
		const oldServerWorkspace = path.resolve(__dirname, '../../testFixture-old');
		const oldServerRequestLog = path.join(
			os.tmpdir(),
			`pine-old-language-server-${process.pid}.log`);

		// Download VS Code, unzip it and run the integration test
		await runTests({
			extensionDevelopmentPath,
			extensionTestsPath,
			launchArgs: [currentServerWorkspace]
		});

		fs.writeFileSync(oldServerRequestLog, '');

		await runTests({
			extensionDevelopmentPath,
			extensionTestsPath,
			launchArgs: [oldServerWorkspace],
			extensionTestsEnv: {
				...process.env,
				CODE_TESTS_WORKSPACE: oldServerWorkspace,
				OLD_SERVER_REQUEST_LOG: oldServerRequestLog,
				PATH: `${oldServerWorkspace}${path.delimiter}${process.env.PATH ?? ''}`,
				PINE_TEST_MODE: 'old-server'
			}
		});

		fs.rmSync(oldServerRequestLog, { force: true });
	} catch (err) {
		console.error('Failed to run tests');
		process.exit(1);
	}
}

main();
