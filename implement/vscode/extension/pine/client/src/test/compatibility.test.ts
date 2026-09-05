import * as assert from 'assert';
import * as fs from 'fs';
import * as path from 'path';
import * as vscode from 'vscode';
import { activate, sleep } from './helper';

suite('Older language-server compatibility', () => {
    test('Activates without registering or requesting CodeLens', async () => {
        const workspacePath = process.env.CODE_TESTS_WORKSPACE;
        const requestLog = process.env.OLD_SERVER_REQUEST_LOG;

        assert.ok(workspacePath);
        assert.ok(requestLog);

        const docUri = vscode.Uri.file(path.join(workspacePath, 'src', 'Main.elm'));

        await activate(docUri);
        await sleep(1000);

        const references = await vscode.commands.executeCommand<vscode.Location[]>(
            'vscode.executeReferenceProvider',
            docUri,
            new vscode.Position(3, 0));

        assert.deepEqual(references, []);

        const lenses = await vscode.commands.executeCommand<vscode.CodeLens[]>(
            'vscode.executeCodeLensProvider',
            docUri);

        assert.ok(!lenses || lenses.length === 0);

        await vscode.commands.executeCommand('pine.client.peekReferences');

        const methods = fs.readFileSync(requestLog, 'utf8').split('\n');

        assert.ok(methods.includes('initialize'));
        assert.ok(methods.includes('textDocument/references'));
        assert.ok(!methods.includes('textDocument/codeLens'));
        assert.ok(!methods.includes('codeLens/resolve'));
    });
});
