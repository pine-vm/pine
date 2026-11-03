/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';

import { runTests } from '@vscode/test-electron';

async function main() {
	const userDataRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'pine-vscode-'));
	const oldServerRequestLog = path.join(os.tmpdir(), `pine-old-language-server-${process.pid}.log`);
	try {
		// The folder containing the Extension Manifest package.json
		// Passed to `--extensionDevelopmentPath`
		const extensionDevelopmentPath = path.resolve(__dirname, '../../../');

		// The path to test runner
		// Passed to --extensionTestsPath
		const extensionTestsPath = path.resolve(__dirname, './index');
		const currentServerWorkspace = path.resolve(__dirname, '../../testFixture');
		const oldServerWorkspace = path.resolve(__dirname, '../../testFixture-old');
		const vscodeExecutablePath = process.env.VSCODE_EXECUTABLE_PATH || undefined;
		const sandboxArgs = process.platform === 'linux' && process.getuid?.() === 0 ? ['--no-sandbox'] : [];

		// Download VS Code, unzip it and run the integration test
		await runTests({
			extensionDevelopmentPath,
			extensionTestsPath,
			vscodeExecutablePath,
			launchArgs: [currentServerWorkspace, `--user-data-dir=${path.join(userDataRoot, 'current')}`, ...sandboxArgs]
		});

		fs.writeFileSync(oldServerRequestLog, '');

		await runTests({
			extensionDevelopmentPath,
			extensionTestsPath,
			vscodeExecutablePath,
			launchArgs: [oldServerWorkspace, `--user-data-dir=${path.join(userDataRoot, 'old')}`, ...sandboxArgs],
			extensionTestsEnv: {
				...process.env,
				CODE_TESTS_WORKSPACE: oldServerWorkspace,
				OLD_SERVER_REQUEST_LOG: oldServerRequestLog,
				PATH: `${oldServerWorkspace}${path.delimiter}${process.env.PATH ?? ''}`,
				PINE_TEST_MODE: 'old-server'
			}
		});

	} catch (err) {
		console.error('Failed to run tests', err);
		process.exitCode = 1;
	} finally {
		fs.rmSync(oldServerRequestLog, { force: true });
		fs.rmSync(userDataRoot, { recursive: true, force: true });
	}
}

main();
