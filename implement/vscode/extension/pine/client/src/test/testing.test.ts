import * as assert from 'assert';
import { randomUUID } from 'crypto';
import * as vscode from 'vscode';
import { PineTestController, testMessages } from '../testing';
import {
    ElmTestEvent, ElmTestItem, ElmTestResult, ElmTestRunParams, ElmTestRunResult,
    TestInventory, TestingTransport, selectedLeafIds, supportsTesting
} from '../testingProtocol';
import { sleep } from './helper';

const versionA = 'file-tree-' + 'a'.repeat(64);
const versionB = 'file-tree-' + 'b'.repeat(64);
const location = {
    uri: vscode.Uri.joinPath(vscode.workspace.workspaceFolders[0].uri, 'tests', 'Tests.elm').toString(),
    range: { start: { line: 4, character: 7 }, end: { line: 4, character: 23 } }
};

function item(id: string, label: string, parentId?: string, kind: ElmTestItem['kind'] = 'Unit'): ElmTestItem {
    return {
        id, label, parentId, kind, filePath: 'tests/Tests.elm',
        moduleName: 'Tests', declarationName: 'suite',
        descriptionPath: parentId ? ['suite'] : [], selection: 'Included',
        isOnly: false, isEmptyGroup: false, sourcePath: 'tests/Tests.elm', location
    };
}

class Transport implements TestingTransport {
    readonly events = new vscode.EventEmitter<ElmTestEvent>();
    readonly changes = new vscode.EventEmitter<{ workspaceRevision: number }>();
    readonly requests: ElmTestRunParams[] = [];
    readonly sourceRequests: { projectUri: string; version: string; path: string }[] = [];
    discoverCount = 0;
    inventory: TestInventory = {
        projectUri: '', sourceRootUri: vscode.workspace.workspaceFolders[0].uri.toString() + '/',
        version: versionA, treeHash: 'a'.repeat(64), complete: true,
        items: [item('group', 'suite', undefined, 'Group'), item('pass', 'pass', 'group'), item('skip', 'skip', 'group')],
        hasOnly: false, hasSkipped: true, documentVersions: {}
    };
    discoverHandler?: (projectUri: string, token: vscode.CancellationToken) => Promise<TestInventory>;
    runHandler?: (parameters: ElmTestRunParams, token: vscode.CancellationToken) => Promise<ElmTestRunResult>;

    constructor() {
        this.inventory.items[2].selection = 'Skipped';
    }

    async discover(projectUri: string, token: vscode.CancellationToken): Promise<TestInventory> {
        ++this.discoverCount;
        const result = await (this.discoverHandler ? this.discoverHandler(projectUri, token) : { ...this.inventory, projectUri });
        return {
            ...result, treeHash: result.version.slice('file-tree-'.length),
            discoveryId: result.complete ? 'inventory:' + result.version : undefined
        };
    }

    async run(parameters: ElmTestRunParams, token: vscode.CancellationToken): Promise<ElmTestRunResult> {
        this.requests.push(parameters);
        return this.runHandler ? this.runHandler(parameters, token) : this.completed(parameters);
    }

    completed(parameters: ElmTestRunParams, results?: ElmTestResult[]): ElmTestRunResult {
        return {
            projectUri: parameters.projectUri, version: parameters.version, runId: parameters.runId,
            status: 'completed', settings: { seed: parameters.seed ?? 42, fuzzRuns: parameters.fuzzRuns },
            incomplete: false,
            results: results ?? [...selectedLeafIds(this.inventory.items, parameters.include, parameters.exclude)].map(id => ({
                testId: id, state: id === 'skip' ? 'skipped' : 'passed', durationMilliseconds: 12, messages: []
            }))
        };
    }

    async source(parameters: { projectUri: string; version: string; path: string }): Promise<{
        projectUri: string; version: string; path: string; text: string;
    }> {
        this.sourceRequests.push(parameters);
        return { ...parameters, text: 'module Tests exposing (..)\n' };
    }

    onEvent(listener: (event: ElmTestEvent) => void): vscode.Disposable {
        return this.events.event(listener);
    }

    onChanged(listener: (event: { workspaceRevision: number }) => void): vscode.Disposable {
        return this.changes.event(listener);
    }
}

interface StateCall { state: string; item: vscode.TestItem; messages?: vscode.TestMessage[] }

function fixture() {
    const transport = new Transport();
    const output = vscode.window.createOutputChannel('Pine testing regression');
    const calls: StateCall[] = [];
    const controller = vscode.tests.createTestController('pine.fixture.' + randomUUID(), 'Elm testing regression');
    const wrapped = new Proxy(controller, {
        get(target, property) {
            if (property === 'createTestRun') {
                return (...args: Parameters<vscode.TestController['createTestRun']>) => {
                    const run = target.createTestRun(...args);
                    return new Proxy(run, {
                        get(testRun, key) {
                            const value = Reflect.get(testRun, key, testRun);
                            if (typeof key === 'string' && ['enqueued', 'started', 'passed', 'failed', 'errored', 'skipped'].includes(key)) {
                                return (test: vscode.TestItem, ...args: unknown[]) => {
                                    const messages = args[0];
                                    calls.push({ state: key, item: test, messages: Array.isArray(messages) ? messages : messages instanceof vscode.TestMessage ? [messages] : [] });
                                    return Reflect.apply(value, testRun, [test, ...args]);
                                };
                            }
                            return typeof value === 'function' ? value.bind(testRun) : value;
                        }
                    });
                };
            }
            const value = Reflect.get(target, property, target);
            return typeof value === 'function' ? value.bind(target) : value;
        }
    });
    const adapter = new PineTestController(transport, output, wrapped);
    const cancellation = new vscode.CancellationTokenSource();
    return {
        transport, adapter, calls, cancellation,
        dispose: () => {
            cancellation.cancel();
            adapter.dispose();
            cancellation.dispose();
            output.dispose();
            transport.events.dispose();
            transport.changes.dispose();
        }
    };
}

function descendants(collection: vscode.TestItemCollection): vscode.TestItem[] {
    return [...collection].flatMap(([, value]) => [value, ...descendants(value.children)]);
}

async function until(predicate: () => boolean): Promise<void> {
    const deadline = Date.now() + 10000;
    while (!predicate() && Date.now() < deadline) {
        await sleep(20);
    }
    assert.ok(predicate(), 'Timed out waiting for testing adapter');
}

suite('Native Elm testing adapter', () => {
    test('Matches Windows project URIs from VS Code and filesystem discovery', () => {
        assert.strictEqual(
            vscode.Uri.parse('file:///k%3A/Source/Repos/elm-time/implement/elm-in-elm/').toString(),
            vscode.Uri.parse('file:///K:/Source/Repos/elm-time/implement/elm-in-elm/').toString());
    });

    test('Reads versioned source without double-decoding path or project URI characters', async () => {
        const f = fixture();
        try {
            const projectUri = 'file:///elm%20%23%26%3F%25project/';
            const sourcePath = 'src/Helper %20#?.elm';
            const uri = vscode.Uri.parse(
                `pine-test-source:/${versionA}/${sourcePath.split('/').map(encodeURIComponent).join('/')}?project=${encodeURIComponent(projectUri)}`);
            await vscode.workspace.openTextDocument(uri);
            assert.deepStrictEqual(f.transport.sourceRequests, [{ projectUri, version: versionA, path: sourcePath }]);
        } finally {
            f.dispose();
        }
    });

    test('Rejects ambiguous duplicate-label selections after edits instead of following their ordinals', async () => {
        const f = fixture();
        try {
            f.transport.inventory.items = [item('first', 'duplicate'), item('second', 'duplicate')];
            await f.adapter.refresh();
            const old = descendants(f.adapter.controller.items).find(node => node.label === 'duplicate');
            f.transport.inventory = { ...f.transport.inventory, version: versionB };
            await f.adapter.run(new vscode.TestRunRequest([old], [], f.adapter.profile), f.cancellation.token);
            assert.strictEqual(f.transport.requests.length, 0);
            assert.ok(f.calls.some(call => call.state === 'errored' &&
                call.messages.some(message => String(message.message).includes('ambiguous'))));
        } finally {
            f.dispose();
        }
    });

    test('Negotiates the independent protocol version and does not infer support from server versions', () => {
        assert.ok(!supportsTesting(undefined));
        assert.ok(!supportsTesting({ pineTests: { protocolVersion: 2 } }));
        assert.ok(supportsTesting({ pineTests: { protocolVersion: 1 } }));
    });

    test('Builds project/module/group/test hierarchy, source ranges and distinct skipped metadata', async () => {
        const f = fixture();
        try {
            await f.adapter.refresh();
            const nodes = descendants(f.adapter.controller.items);
            const group = nodes.find(node => node.label === 'suite');
            const pass = nodes.find(node => node.label === 'pass');
            const skip = nodes.find(node => node.label === 'skip');
            assert.strictEqual(pass.parent.id, group.id);
            assert.strictEqual(pass.range.start.line, 4);
            assert.strictEqual(pass.range.start.character, 7);
            assert.strictEqual(pass.uri.toString(), location.uri);
            assert.ok(skip.description.includes('skipped'));
            assert.ok(skip.tags.some(tag => tag.id === 'skipped'));
            assert.strictEqual(f.adapter.profile.kind, vscode.TestRunProfileKind.Run);
            assert.ok(f.adapter.profile.supportsContinuousRun);
            await f.adapter.run(new vscode.TestRunRequest([group], [skip], f.adapter.profile), f.cancellation.token);
            assert.deepStrictEqual(f.transport.requests[0].include, ['group']);
            assert.deepStrictEqual(f.transport.requests[0].exclude, ['skip']);
            assert.strictEqual(f.transport.requests[0].version, versionA);
            assert.ok(f.calls.some(call => call.state === 'passed' && call.item === pass));
            assert.ok(!f.calls.some(call => call.item === skip));
        } finally {
            f.dispose();
        }
    });

    test('Keeps late old-run events on old items and ignores events for another version', async () => {
        const f = fixture();
        let complete: (result: ElmTestRunResult) => void;
        f.transport.runHandler = () => new Promise(resolve => { complete = resolve; });
        try {
            await f.adapter.refresh();
            const oldPass = descendants(f.adapter.controller.items).find(node => node.label === 'pass');
            const running = f.adapter.run(new vscode.TestRunRequest([oldPass], [], f.adapter.profile), f.cancellation.token);
            await until(() => f.transport.requests.length === 1);
            f.transport.inventory = { ...f.transport.inventory, version: versionB, items: [item('pass', 'new')] };
            await f.adapter.refresh();
            const current = descendants(f.adapter.controller.items).find(node => node.label === 'new');
            const request = f.transport.requests[0];
            const event = {
                projectUri: request.projectUri, runId: request.runId, kind: 'test' as const, testId: 'pass',
                sequence: 1, state: 'failed', result: {
                    testId: 'pass', state: 'failed' as const, durationMilliseconds: 3,
                    messages: [{ message: 'Old failure', location }]
                }
            };
            f.transport.events.fire({ ...event, version: versionB, sequence: 10 });
            f.transport.events.fire({ ...event, version: versionA });
            f.transport.events.fire({ ...event, version: versionA });
            complete(f.transport.completed(request, []));
            await running;
            assert.notStrictEqual(current.id, oldPass.id);
            const failed = f.calls.filter(call => call.state === 'failed');
            assert.strictEqual(failed.length, 1);
            assert.strictEqual(failed[0].item.id, oldPass.id);
            assert.strictEqual(failed[0].messages[0].location.uri.scheme, 'pine-test-source');
            assert.ok(failed[0].messages[0].location.uri.path.includes(versionA));
            assert.ok(!f.calls.some(call => call.item.id === current.id));
        } finally {
            f.dispose();
        }
    });

    test('Ignores superseded discovery, preserves explicit errors, and removes disappeared tests on complete replacement', async () => {
        const f = fixture();
        let first: (result: TestInventory) => void;
        try {
            f.transport.discoverHandler = (projectUri) => f.transport.discoverCount === 1 ?
                new Promise(resolve => { first = resolve; }) :
                Promise.resolve({ ...f.transport.inventory, projectUri, version: versionB, items: [item('new', 'new')] });
            const stale = f.adapter.refresh();
            await until(() => f.transport.discoverCount === 1);
            await f.adapter.refresh();
            const projectUri = f.transport.requests[0]?.projectUri ?? [...f.adapter.controller.items][0][0].slice('project:'.length);
            first({ ...f.transport.inventory, projectUri });
            await stale;
            assert.ok(descendants(f.adapter.controller.items).some(node => node.label === 'new'));
            assert.ok(!descendants(f.adapter.controller.items).some(node => node.label === 'pass'));

            f.transport.discoverHandler = async uri => ({
                ...f.transport.inventory, projectUri: uri, complete: false, items: undefined,
                error: { code: 'discoveryFailed', message: 'source error' }
            });
            await f.adapter.refresh();
            const root = [...f.adapter.controller.items][0][1];
            assert.ok(String(root.error).includes('source error'));
            await f.adapter.run(new vscode.TestRunRequest([root], [], f.adapter.profile), f.cancellation.token);
            assert.strictEqual(f.transport.requests.length, 0);
            assert.ok(f.calls.some(call => call.state === 'errored'));

            f.transport.discoverHandler = async uri => ({ ...f.transport.inventory, projectUri: uri, version: 'file-tree-' + 'c'.repeat(64), items: [] });
            await f.adapter.refresh();
            assert.strictEqual(root.children.size, 0);
            assert.strictEqual(root.error, undefined);
        } finally {
            f.dispose();
        }
    });

    test('A manual run waits for a superseding refresh instead of executing the previous inventory', async () => {
        const f = fixture();
        let previous: (result: TestInventory) => void;
        let latest: (result: TestInventory) => void;
        let projectUri: string;
        try {
            await f.adapter.refresh();
            const selected = descendants(f.adapter.controller.items).find(node => node.label === 'pass');
            f.transport.discoverHandler = uri => {
                projectUri = uri;
                return new Promise(resolve => {
                    if (f.transport.discoverCount === 2) {
                        previous = resolve;
                    } else {
                        latest = resolve;
                    }
                });
            };
            const run = f.adapter.run(new vscode.TestRunRequest([selected], [], f.adapter.profile), f.cancellation.token);
            await until(() => f.transport.discoverCount === 2);
            const refresh = f.adapter.refresh();
            await until(() => f.transport.discoverCount === 3);
            previous({ ...f.transport.inventory, projectUri });
            await sleep(50);
            assert.strictEqual(f.transport.requests.length, 0);
            latest({ ...f.transport.inventory, projectUri, version: versionB });
            await refresh;
            await run;
            assert.strictEqual(f.transport.requests[0].version, versionB);
        } finally {
            f.dispose();
        }
    });

    for (const replacementVersion of [versionA, versionB]) {
        test(`Coalesces continuous invalidation without leaving ${replacementVersion === versionA ? 'unchanged' : 'changed'} sources cancelled`, async () => {
            const f = fixture();
            let active = 0;
            let maximum = 0;
            try {
                await f.adapter.refresh();
                f.transport.runHandler = async (parameters, token) => {
                    ++active;
                    maximum = Math.max(maximum, active);
                    try {
                        if (f.transport.requests.length === 1) {
                            await new Promise<void>(resolve => {
                                const subscription = token.onCancellationRequested(() => {
                                    subscription.dispose();
                                    resolve();
                                });
                            });
                            return { ...f.transport.completed(parameters, []), status: 'cancelled' };
                        }
                        return f.transport.completed(parameters);
                    } finally {
                        --active;
                    }
                };
                const initial = f.adapter.run(new vscode.TestRunRequest(undefined, [], f.adapter.profile, true), f.cancellation.token);
                await until(() => f.transport.requests.length === 1);
                f.transport.inventory = { ...f.transport.inventory, version: replacementVersion };
                f.transport.changes.fire({ workspaceRevision: 1 });
                f.transport.changes.fire({ workspaceRevision: 2 });
                await initial;
                await f.adapter.refresh();
                await until(() => f.transport.requests.length === 2);
                f.transport.changes.fire({ workspaceRevision: 3 });
                await sleep(600);
                assert.strictEqual(f.transport.requests.length, 2, 'An unchanged snapshot must not re-run continuously');
                assert.strictEqual(f.transport.requests[1].version, replacementVersion);
                assert.strictEqual(maximum, 1);
            } finally {
                f.dispose();
            }
        });
    }

    test('Retains comparison diffs and fuzz metadata, and replays with the original settings', async () => {
        const f = fixture();
        const result: ElmTestResult = {
            testId: 'fuzz', state: 'failed', durationMilliseconds: 10,
            messages: [{ message: 'Expect.equal', expected: '0', actual: '1', location }],
            fuzz: {
                settings: { seed: 42, fuzzRuns: 3 }, effectiveSeedState: 'state', runsRequested: 3,
                runsElapsed: 1, failingIteration: 1, originalInput: '4', shrunkInput: '1',
                originalChoices: [4], shrunkChoices: [1], shrinkingCompleted: true
            },
            reproductionCommand: "pine elm test --seed 42 --fuzz 3 --filter 'tests/Tests.elm:0'"
        };
        try {
            f.transport.inventory.items = [item('fuzz', 'property', undefined, 'Fuzz')];
            f.transport.runHandler = async parameters => f.transport.completed(parameters, [result]);
            await f.adapter.refresh();
            const property = descendants(f.adapter.controller.items).find(node => node.label === 'property');
            await f.adapter.run(new vscode.TestRunRequest([property], [], f.adapter.profile), f.cancellation.token);
            const message = testMessages(result)[0];
            assert.strictEqual(message.expectedOutput, '0');
            assert.strictEqual(message.actualOutput, '1');
            assert.ok(String(message.message).includes('Counterexample: 1'));
            await f.adapter.replay(property);
            assert.strictEqual(f.transport.requests[1].seed, 42);
            assert.strictEqual(f.transport.requests[1].fuzzRuns, 3);
            assert.strictEqual(f.transport.requests[1].version, versionA);
            await f.adapter.replay(property, true);
            assert.strictEqual(await vscode.env.clipboard.readText(), result.reproductionCommand);
        } finally {
            f.dispose();
        }
    });
});
