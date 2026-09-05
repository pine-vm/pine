import * as assert from 'assert';
import * as vscode from 'vscode';
import { showReferences } from '../extension';

suite('Reference CodeLens command', () => {
    test('Safely ignores invalid arguments from missing or older servers', async () => {
        let invocationCount = 0;

        await showReferences(
            undefined,
            undefined,
            async () => {
                invocationCount++;
                return undefined;
            });

        await showReferences(
            'file:///workspace/Main.elm',
            { line: -1, character: 0 },
            async () => {
                invocationCount++;
                return undefined;
            });

        assert.equal(invocationCount, 0);
    });

    test('Queries references and opens the peek view', async () => {
        const calls: { command: string; args: any[] }[] = [];
        const locations =
            [
                new vscode.Location(
                    vscode.Uri.parse('file:///workspace/Main.elm'),
                    new vscode.Position(1, 0))
            ];

        await showReferences(
            'file:///workspace/Main.elm',
            { line: 3, character: 2 },
            async (command, ...args) => {
                calls.push({ command, args });

                return command === 'vscode.executeReferenceProvider'
                    ? locations
                    : undefined;
            });

        assert.equal(calls.length, 2);
        assert.equal(calls[0].command, 'vscode.executeReferenceProvider');
        assert.deepEqual(calls[0].args[0], vscode.Uri.parse('file:///workspace/Main.elm'));
        assert.deepEqual(calls[0].args[1], new vscode.Position(3, 2));
        assert.equal(calls[1].command, 'editor.action.showReferences');
        assert.deepEqual(calls[1].args[2], locations);
    });

    test('Contains reference-provider failures', async () => {
        await showReferences(
            'file:///workspace/Main.elm',
            { line: 3, character: 2 },
            async () => {
                throw new Error('Expected test failure');
            });
    });
});
