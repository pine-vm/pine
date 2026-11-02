import { randomUUID } from 'crypto';
import * as vscode from 'vscode';
import { extensionIdentity } from './identity';
import {
    ElmTestEvent, ElmTestItem, ElmTestResult, ElmTestRunParams, FuzzSettings,
    TestInventory, TestLocation, TestingTransport, selectedLeafIds, stableTestKey
} from './testingProtocol';

interface Selection {
    projectUri: string;
    kind: 'project' | 'file' | 'item';
    key: string;
    discoveryId?: string;
    ambiguous?: boolean;
}

interface Project {
    root: vscode.TestItem;
    inventory?: TestInventory;
    valid: boolean;
    byKey: Map<string, vscode.TestItem>;
    byId: Map<string, vscode.TestItem>;
    files: Map<string, vscode.TestItem>;
    ambiguousKeys: Set<string>;
}

interface RunContext {
    run: vscode.TestRun;
    parameters: ElmTestRunParams;
    items: Map<string, vscode.TestItem>;
    sources: Map<string, ElmTestItem>;
    selected: Set<string>;
    finished: Set<string>;
    sequence: number;
}

interface ContinuousSelection {
    include?: Selection[];
    exclude: Selection[];
    request: vscode.TestRunRequest;
    token: vscode.CancellationToken;
    running?: Promise<void>;
    active?: vscode.CancellationTokenSource;
    pending: boolean;
    signature?: string;
}

interface Replay {
    version: string;
    discoveryId: string;
    settings: FuzzSettings;
    command: string;
}

export function testLocation(location: TestLocation): vscode.Location {
    const uri = vscode.Uri.parse(location.uri);
    return new vscode.Location(
        uri.scheme === 'pine-test-source' ? uri.with({ scheme: extensionIdentity.testSourceScheme }) : uri,
        new vscode.Range(
        location.range.start.line, location.range.start.character,
        location.range.end.line, location.range.end.character));
}

export function testMessages(result: ElmTestResult): vscode.TestMessage[] {
    const fuzz = result.fuzz;
    const details = fuzz ? [
        `Seed: ${fuzz.settings.seed}; requested runs: ${fuzz.runsRequested}; elapsed: ${fuzz.runsElapsed ?? 'unknown'}`,
        fuzz.failingIteration === undefined ? '' : `Failing iteration: ${fuzz.failingIteration}`,
        fuzz.originalInput === undefined ? '' : `Original input: ${fuzz.originalInput}`,
        fuzz.shrunkInput === undefined ? '' : `Counterexample: ${fuzz.shrunkInput}`,
        fuzz.failureReason ?? '',
        fuzz.originalInput !== undefined && !fuzz.shrinkingCompleted ? 'Shrinking did not complete.' : '',
        fuzz.evaluationError ?? ''
    ].filter(Boolean).join('\n') : '';

    return result.messages.map(message => {
        const text = message.message + (details ? '\n\n' + details : '');
        const converted = message.expected !== undefined && message.actual !== undefined ?
            vscode.TestMessage.diff(text, message.expected, message.actual) : new vscode.TestMessage(text);
        if (message.location) {
            converted.location = testLocation(message.location);
        }
        return converted;
    });
}

export class PineTestController implements vscode.Disposable {
    public readonly profile: vscode.TestRunProfile;
    private readonly disposables: vscode.Disposable[] = [];
    private readonly projects = new Map<string, Project>();
    private readonly metadata = new WeakMap<vscode.TestItem, Selection>();
    private readonly activeRuns = new Map<string, RunContext>();
    private readonly runCancellations = new Set<vscode.CancellationTokenSource>();
    private readonly continuous = new Set<ContinuousSelection>();
    private readonly replays = new Map<string, Replay>();
    private refreshCancellation?: vscode.CancellationTokenSource;
    private refreshTimer?: ReturnType<typeof setTimeout>;
    private refreshPromise: Promise<void> = Promise.resolve();
    private generation = 0;
    private serverRevision = -1;
    private disposed = false;

    constructor(
        private readonly transport: TestingTransport,
        private readonly output: vscode.OutputChannel,
        public readonly controller = vscode.tests.createTestController(
            extensionIdentity.testControllerId,
            extensionIdentity.commandPrefix === 'pine' ? 'Elm tests' : `${extensionIdentity.displayName}: Elm tests`)
    ) {
        this.profile = controller.createRunProfile(
            'Run Elm tests', vscode.TestRunProfileKind.Run,
            (request, token) => this.run(request, token), true, undefined, true);
        this.profile.configureHandler = () =>
            vscode.commands.executeCommand(
                'workbench.action.openSettings',
                `@ext:${extensionIdentity.extensionId} ${extensionIdentity.testingConfiguration}`);
        controller.refreshHandler = token => this.refresh(token);
        controller.resolveHandler = () => this.refresh();
        this.disposables.push(
            transport.onEvent(event => this.onEvent(event)),
            transport.onChanged(event => {
                if (event.workspaceRevision > this.serverRevision) {
                    this.serverRevision = event.workspaceRevision;
                    this.invalidate();
                }
            }),
            vscode.workspace.onDidChangeTextDocument(event => {
                if (event.contentChanges.length && this.relevant(event.document.uri)) {
                    this.invalidate();
                }
            }),
            vscode.workspace.onDidOpenTextDocument(document => {
                if (this.relevant(document.uri)) {
                    this.invalidate();
                }
            }),
            vscode.workspace.onDidCloseTextDocument(document => {
                if (this.relevant(document.uri)) {
                    this.invalidate();
                }
            }),
            vscode.workspace.onDidChangeWorkspaceFolders(() => this.invalidate()),
            vscode.workspace.onDidChangeConfiguration(event => {
                if (event.affectsConfiguration(extensionIdentity.testingConfiguration)) {
                    this.invalidate();
                }
            }),
            vscode.workspace.registerTextDocumentContentProvider(extensionIdentity.testSourceScheme, {
                provideTextDocumentContent: async (uri, token) => {
                    // VS Code has already decoded these URI components.
                    const projectUri = uri.query.startsWith('project=') ? uri.query.slice('project='.length) : undefined;
                    const [version, ...path] = uri.path.split('/').filter(Boolean);
                    if (!projectUri || !version || !path.length) {
                        throw new Error('Invalid version-qualified Elm test source URI.');
                    }
                    const parameters = { projectUri, version, path: path.join('/') };
                    const result = await transport.source(parameters, token);
                    if (result.projectUri !== projectUri || result.version !== version || result.path !== parameters.path) {
                        throw new Error('Elm test source response does not match the requested version.');
                    }
                    return result.text;
                }
            }));

        for (const pattern of ['**/*.elm', '**/elm.json']) {
            const watcher = vscode.workspace.createFileSystemWatcher(pattern);
            const changed = (uri: vscode.Uri) => {
                if (this.relevant(uri)) {
                    this.invalidate();
                }
            };
            this.disposables.push(watcher, watcher.onDidCreate(changed), watcher.onDidChange(changed), watcher.onDidDelete(changed));
        }
    }

    private relevant(uri: vscode.Uri): boolean {
        return uri.scheme === 'file' && !uri.path.split('/').some(part =>
            part === '.git' || part === 'elm-stuff' || part === 'node_modules') &&
            (uri.path.endsWith('.elm') || uri.path.endsWith('/elm.json'));
    }

    private invalidate(): void {
        if (this.disposed) {
            return;
        }
        ++this.generation;
        this.refreshCancellation?.cancel();
        for (const project of this.projects.values()) {
            project.valid = false;
            project.root.description = 'Refreshing workspace tests';
        }
        for (const scope of this.continuous) {
            scope.pending = false;
            if (scope.active) {
                scope.signature = undefined;
            }
            scope.active?.cancel();
        }
        clearTimeout(this.refreshTimer);
        this.refreshTimer = setTimeout(() => {
            this.refreshTimer = undefined;
            void this.refresh();
        }, 250);
    }

    public refresh(token?: vscode.CancellationToken): Promise<void> {
        if (this.disposed) {
            return Promise.resolve();
        }
        clearTimeout(this.refreshTimer);
        this.refreshTimer = undefined;
        this.refreshCancellation?.cancel();
        const cancellation = new vscode.CancellationTokenSource();
        this.refreshCancellation = cancellation;
        const external = token?.onCancellationRequested(() => cancellation.cancel());
        if (token?.isCancellationRequested) {
            cancellation.cancel();
        }
        const generation = ++this.generation;
        this.refreshPromise = this.discover(generation, cancellation.token).finally(() => {
            external?.dispose();
            cancellation.dispose();
            if (this.refreshCancellation === cancellation) {
                this.refreshCancellation = undefined;
            }
        });
        return this.refreshPromise;
    }

    private async discover(generation: number, token: vscode.CancellationToken): Promise<void> {
        try {
            const manifests = await vscode.workspace.findFiles('**/elm.json', '**/{.git,elm-stuff,node_modules}/**', undefined, token);
            for (const document of vscode.workspace.textDocuments) {
                if (document.uri.scheme === 'file' && document.uri.path.endsWith('/elm.json') &&
                    !manifests.some(uri => uri.toString() === document.uri.toString())) {
                    manifests.push(document.uri);
                }
            }
            if (token.isCancellationRequested || generation !== this.generation) {
                return;
            }
            const uris = [...new Set(manifests.filter(uri => uri.scheme === 'file')
                .map(uri => uri.with({ path: uri.path.slice(0, -'elm.json'.length) }).toString()))];
            for (const [uri, project] of this.projects) {
                if (!uris.includes(uri)) {
                    this.controller.items.delete(project.root.id);
                    this.projects.delete(uri);
                }
            }
            await Promise.all(uris.map(uri => this.discoverProject(uri, generation, token)));
            if (!token.isCancellationRequested && generation === this.generation) {
                for (const scope of this.continuous) {
                    const signature = this.continuousSignature(scope);
                    if (signature !== undefined && signature !== scope.signature) {
                        this.queueContinuous(scope);
                    }
                }
            }
        } catch (error) {
            if (!token.isCancellationRequested && generation === this.generation) {
                this.output.appendLine('Elm test refresh failed: ' + String(error));
                for (const project of this.projects.values()) {
                    project.valid = false;
                    project.root.error = String(error);
                    project.root.busy = false;
                }
            }
        }
    }

    private async discoverProject(uri: string, generation: number, token: vscode.CancellationToken): Promise<void> {
        let project = this.projects.get(uri);
        if (!project) {
            const projectUri = vscode.Uri.parse(uri);
            const label = vscode.workspace.asRelativePath(projectUri, true).replace(/[/\\]$/, '') ||
                projectUri.path.split('/').filter(Boolean).pop() || 'Elm project';
            const root = this.controller.createTestItem('project:' + uri, label, vscode.Uri.joinPath(projectUri, 'elm.json'));
            project = { root, valid: false, byId: new Map(), byKey: new Map(), files: new Map(), ambiguousKeys: new Set() };
            this.metadata.set(root, { projectUri: uri, kind: 'project', key: '' });
            this.projects.set(uri, project);
            this.controller.items.add(root);
        }
        project.root.busy = true;
        try {
            const result = await this.transport.discover(uri, token);
            if (token.isCancellationRequested || generation !== this.generation) {
                return;
            }
            if (vscode.Uri.parse(result.projectUri).toString() !== vscode.Uri.parse(uri).toString() ||
                !/^file-tree-[a-f0-9]{64}$/.test(result.version) ||
                result.version !== 'file-tree-' + result.treeHash) {
                throw new Error('Elm discovery response does not identify the requested workspace project.');
            }
            if (!result.complete || !result.items) {
                throw new Error(result.error?.message ?? 'Elm discovery did not return a complete inventory.');
            }
            if (!result.discoveryId) {
                throw new Error('Elm discovery did not return an inventory identity.');
            }
            for (const document of vscode.workspace.textDocuments) {
                const version = result.documentVersions[document.uri.toString()];
                if (version !== undefined && version !== document.version) {
                    this.invalidate();
                    return;
                }
            }
            if (project.inventory?.version !== result.version || project.inventory?.discoveryId !== result.discoveryId) {
                this.replaceInventory(project, result);
            }
            project.inventory = result;
            project.valid = true;
            project.root.error = undefined;
            project.root.description = [
                result.hasOnly ? 'Test.only: incomplete suite' : '',
                result.hasSkipped ? 'contains skipped tests' : ''
            ].filter(Boolean).join('; ') || undefined;
        } catch (error) {
            if (!token.isCancellationRequested && generation === this.generation) {
                project.valid = false;
                project.root.error = String(error);
                project.root.description = project.inventory ?
                    'Discovery failed; previous inventory is not runnable' : 'Discovery failed; tests are unavailable';
                this.output.appendLine('Elm test discovery failed for ' + uri + ': ' + String(error));
            }
        } finally {
            if (generation === this.generation) {
                project.root.busy = false;
            }
        }
    }

    private replaceInventory(project: Project, inventory: TestInventory): void {
        const byId = new Map<string, vscode.TestItem>();
        const byKey = new Map<string, vscode.TestItem>();
        const files = new Map<string, vscode.TestItem>();
        const occurrences = new Map<string, number>();
        const keyCounts = new Map<string, number>();
        const ambiguousKeys = new Set<string>();
        for (const item of inventory.items) {
            const key = stableTestKey(item);
            keyCounts.set(key, (keyCounts.get(key) ?? 0) + 1);
        }
        for (const item of inventory.items) {
            if (byId.has(item.id)) {
                throw new Error('Duplicate Elm test ID: ' + item.id);
            }
            let file = files.get(item.filePath);
            if (!file) {
                const uri = vscode.Uri.joinPath(vscode.Uri.parse(inventory.sourceRootUri), ...item.filePath.split('/'));
                file = this.controller.createTestItem(
                    JSON.stringify([inventory.projectUri, inventory.version, inventory.discoveryId, 'file', item.filePath]),
                    item.moduleName, uri);
                file.description = item.filePath;
                this.metadata.set(file, { projectUri: this.metadata.get(project.root).projectUri, kind: 'file', key: item.filePath });
                files.set(item.filePath, file);
            }
            const base = stableTestKey(item);
            const occurrence = occurrences.get(base) ?? 0;
            occurrences.set(base, occurrence + 1);
            const key = base + '#' + occurrence;
            const location = testLocation(item.location);
            const created = this.controller.createTestItem(
                JSON.stringify([inventory.projectUri, inventory.version, inventory.discoveryId, key]),
                item.label, location.uri);
            created.range = location.range;
            created.description = [
                item.kind === 'Fuzz' ? `fuzz (${item.fuzzRuns} runs)` : item.kind === 'Todo' ? 'TODO' : '',
                item.selection === 'Skipped' ? 'skipped' : item.selection === 'ExcludedByOnly' ? 'excluded by Test.only' : '',
                item.isOnly ? 'only' : '',
                item.isEmptyGroup ? 'empty group' : ''
            ].filter(Boolean).join('; ') || undefined;
            created.tags = [
                new vscode.TestTag(item.kind.toLowerCase()),
                new vscode.TestTag(item.selection.toLowerCase())
            ];
            const ambiguous = keyCounts.get(base) > 1;
            if (ambiguous) {
                ambiguousKeys.add(key);
            }
            this.metadata.set(created, {
                projectUri: this.metadata.get(project.root).projectUri, kind: 'item', key,
                discoveryId: inventory.discoveryId, ambiguous
            });
            byId.set(item.id, created);
            byKey.set(key, created);
            const parent = item.parentId ? byId.get(item.parentId) : file;
            if (!parent) {
                throw new Error('Missing Elm test parent: ' + item.parentId);
            }
            parent.children.add(created);
        }
        project.root.children.replace([...files.values()]);
        project.byId = byId;
        project.byKey = byKey;
        project.files = files;
        project.ambiguousKeys = ambiguousKeys;
        for (const key of this.replays.keys()) {
            const selection: Selection = JSON.parse(key);
            if (selection.projectUri === this.metadata.get(project.root).projectUri) {
                this.replays.delete(key);
            }
        }
    }

    private selections(items: readonly vscode.TestItem[] | undefined): Selection[] | undefined {
        return items?.map(item => {
            const selection = this.metadata.get(item);
            if (!selection) {
                throw new Error('Elm test selection is no longer available. Refresh the Test Explorer.');
            }
            return selection;
        });
    }

    private resolve(selection: Selection): vscode.TestItem | undefined {
        const project = this.projects.get(selection.projectUri);
        return selection.kind === 'project' ? project?.root :
            selection.kind === 'file' ? project?.files.get(selection.key) : project?.byKey.get(selection.key);
    }

    public async run(request: vscode.TestRunRequest, token: vscode.CancellationToken): Promise<void> {
        if (token.isCancellationRequested) {
            return;
        }
        const include = this.selections(request.include);
        const exclude = this.selections(request.exclude) ?? [];
        if (request.continuous) {
            const scope: ContinuousSelection = { include, exclude, request, token, pending: false };
            this.continuous.add(scope);
            const cancellation = token.onCancellationRequested(() => {
                scope.pending = false;
                scope.active?.cancel();
                this.continuous.delete(scope);
                cancellation.dispose();
            });
            this.disposables.push(cancellation);
            this.queueContinuous(scope);
            await scope.running;
            return;
        }
        await this.execute(include, exclude, request, token);
    }

    private queueContinuous(scope: ContinuousSelection): void {
        if (scope.token.isCancellationRequested || this.disposed) {
            return;
        }
        scope.pending = true;
        if (scope.running) {
            return;
        }
        scope.running = (async () => {
            while (scope.pending && !scope.token.isCancellationRequested && !this.disposed) {
                scope.pending = false;
                await this.execute(scope.include, scope.exclude, scope.request, scope.token, scope);
            }
        })().catch(error => {
            this.output.appendLine('Continuous Elm testing failed: ' + String(error));
        }).finally(() => {
            scope.running = undefined;
            if (scope.pending) {
                this.queueContinuous(scope);
            }
        });
    }

    private continuousSignature(scope: ContinuousSelection): string | undefined {
        const projects = [...this.projects].filter(([uri]) =>
            scope.include === undefined || scope.include.some(item => item.projectUri === uri));
        if (!projects.length || projects.some(([, project]) => !project.valid)) {
            return undefined;
        }
        return JSON.stringify(projects.map(([uri, project]) => {
            const config = vscode.workspace.getConfiguration(extensionIdentity.testingConfiguration, vscode.Uri.parse(uri));
            return [uri, project.inventory.version, project.inventory.discoveryId, config.get('seed'), config.get('fuzzRuns')];
        }));
    }

    private async execute(
        include: Selection[] | undefined,
        exclude: Selection[],
        request: vscode.TestRunRequest,
        token: vscode.CancellationToken,
        scope?: ContinuousSelection,
        replay?: Replay
    ): Promise<void> {
        if (this.refreshCancellation) {
            await this.refreshPromise;
        }
        if (!scope || !this.projects.size || [...this.projects.values()].some(project => !project.valid)) {
            await this.refresh(token);
        }
        while (this.refreshCancellation && !token.isCancellationRequested && !this.disposed) {
            await this.refreshPromise;
        }
        if (token.isCancellationRequested || this.disposed) {
            return;
        }
        const includedItems = include?.map(item => this.resolve(item)).filter((item): item is vscode.TestItem => !!item);
        const excludedItems = exclude.map(item => this.resolve(item)).filter((item): item is vscode.TestItem => !!item);
        const effectiveRequest = new vscode.TestRunRequest(includedItems, excludedItems, request.profile, request.continuous, request.preserveFocus);
        const run = this.controller.createTestRun(effectiveRequest, replay ? 'Elm fuzz replay' : 'Elm tests');
        const projects = [...this.projects].map(([uri, project]) =>
            [uri, { ...project, byId: new Map(project.byId), byKey: new Map(project.byKey) }] as const);
        const cancellation = new vscode.CancellationTokenSource();
        const stop = token.onCancellationRequested(() => cancellation.cancel());
        const runStop = run.token.onCancellationRequested(() => cancellation.cancel());
        this.runCancellations.add(cancellation);
        if (scope) {
            scope.active = cancellation;
            scope.signature = this.continuousSignature(scope);
            scope.pending = false;
        }
        try {
            for (const [uri, project] of projects) {
                if (cancellation.token.isCancellationRequested) {
                    break;
                }
                const projectInclude = include?.filter(item => item.projectUri === uri);
                const projectExclude = exclude.filter(item => item.projectUri === uri);
                if ((projectInclude && !projectInclude.length) || projectExclude.some(item => item.kind === 'project')) {
                    continue;
                }
                if (!project.valid || !project.inventory) {
                    run.errored(project.root, new vscode.TestMessage(String(project.root.error ?? 'Test discovery is unavailable.')));
                    continue;
                }
                const inventory = project.inventory;
                if (replay && (replay.version !== inventory.version || replay.discoveryId !== inventory.discoveryId)) {
                    throw new Error('Source changed since this fuzz result. Run the current test before replaying it.');
                }
                const config = vscode.workspace.getConfiguration(extensionIdentity.testingConfiguration, vscode.Uri.parse(uri));
                const parameters: ElmTestRunParams = {
                    projectUri: inventory.projectUri,
                    version: inventory.version,
                    discoveryId: inventory.discoveryId,
                    runId: randomUUID(),
                    include: projectInclude?.some(item => item.kind === 'project') ? undefined :
                        projectInclude === undefined ? undefined : this.protocolIds(project, projectInclude),
                    exclude: this.protocolIds(project, projectExclude),
                    seed: replay?.settings.seed ?? config.get<number | null>('seed') ?? undefined,
                    fuzzRuns: replay?.settings.fuzzRuns ?? config.get<number>('fuzzRuns', 100)
                };
                const context: RunContext = {
                    run, parameters, items: new Map(project.byId),
                    sources: new Map(inventory.items.map(item => [item.id, item])),
                    selected: selectedLeafIds(inventory.items, parameters.include, parameters.exclude),
                    finished: new Set(), sequence: 0
                };
                this.activeRuns.set(parameters.runId, context);
                run.appendOutput(`Project: ${uri}\r\nVersion: ${parameters.version}\r\n`);
                try {
                    const result = await this.transport.run(parameters, cancellation.token);
                    if (result.projectUri !== parameters.projectUri || result.version !== parameters.version || result.runId !== parameters.runId) {
                        throw new Error('Elm test run response does not match the requested immutable version.');
                    }
                    run.appendOutput(`Seed: ${result.settings.seed}; fuzz runs: ${result.settings.fuzzRuns}\r\n`);
                    for (const test of result.results) {
                        this.applyResult(context, test);
                    }
                    if (result.incomplete) {
                        run.appendOutput('Incomplete suite (only, skip, TODO, cancellation, or error).\r\n');
                    }
                    if (result.error) {
                        run.appendOutput(result.error.message.replace(/\r?\n/g, '\r\n') + '\r\n');
                    }
                    this.finishRemaining(context, result.status === 'cancelled', result.error?.message);
                } catch (error) {
                    this.output.appendLine('Elm test run failed: ' + String(error));
                    run.appendOutput(String(error).replace(/\r?\n/g, '\r\n') + '\r\n');
                    this.finishRemaining(context, cancellation.token.isCancellationRequested, String(error));
                } finally {
                    this.activeRuns.delete(parameters.runId);
                }
            }
        } catch (error) {
            run.appendOutput(String(error).replace(/\r?\n/g, '\r\n') + '\r\n');
            this.output.appendLine('Elm testing failed: ' + String(error));
            const excluded = new Set(excludedItems);
            const fail = (item: vscode.TestItem) => {
                if (excluded.has(item)) {
                    return;
                }
                if (!item.children.size) {
                    run.errored(item, new vscode.TestMessage(String(error)));
                } else {
                    item.children.forEach(fail);
                }
            };
            if (includedItems) {
                includedItems.forEach(fail);
            } else {
                this.controller.items.forEach(fail);
            }
        } finally {
            if (scope?.active === cancellation) {
                scope.active = undefined;
            }
            stop.dispose();
            runStop.dispose();
            this.runCancellations.delete(cancellation);
            cancellation.dispose();
            run.end();
        }
    }

    private protocolIds(project: Project, selections: Selection[]): string[] {
        const items = project.inventory.items;
        return [...new Set(selections.flatMap(selection => {
            if (selection.kind === 'file') {
                return items.filter(item => item.filePath === selection.key).map(item => item.id);
            }
            if (selection.discoveryId !== project.inventory.discoveryId &&
                (selection.ambiguous || project.ambiguousKeys.has(selection.key))) {
                throw new Error('Duplicate Elm test labels make this selection ambiguous after source changes. Select the tests again in Test Explorer.');
            }
            const selected = project.byKey.get(selection.key);
            return items.filter(item => project.byId.get(item.id) === selected).map(item => item.id);
        }))];
    }

    private onEvent(event: ElmTestEvent): void {
        const context = this.activeRuns.get(event.runId);
        if (!context || context.parameters.projectUri !== event.projectUri ||
            context.parameters.version !== event.version || event.sequence <= context.sequence) {
            return;
        }
        context.sequence = event.sequence;
        if (event.kind === 'phase') {
            context.run.appendOutput(`Elm tests: ${event.state}\r\n`);
        } else if (event.kind === 'test' && event.testId && context.selected.has(event.testId)) {
            const item = context.items.get(event.testId);
            if (!item || context.finished.has(event.testId)) {
                return;
            }
            if (event.result) {
                this.applyResult(context, event.result);
            } else if (event.state === 'queued') {
                context.run.enqueued(item);
            } else if (event.state === 'started') {
                context.run.started(item);
            }
        }
    }

    private applyResult(context: RunContext, result: ElmTestResult): void {
        const item = context.items.get(result.testId);
        if (!item || !context.selected.has(result.testId) || context.finished.has(result.testId)) {
            return;
        }
        const source = context.sources.get(result.testId);
        const currentInventory = this.projects.get(this.metadata.get(item).projectUri)?.inventory;
        const historical = currentInventory?.version !== context.parameters.version;
        const historicalUri = source &&
            `${extensionIdentity.testSourceScheme}:/${context.parameters.version}/${source.sourcePath.split('/').map(encodeURIComponent).join('/')}?project=${encodeURIComponent(context.parameters.projectUri)}`;
        const messages = testMessages(historical && source ? {
            ...result,
            messages: result.messages.map(message =>
                message.location?.uri === source.location.uri ?
                    { ...message, location: { ...message.location, uri: historicalUri } } : message)
        } : result);
        switch (result.state) {
            case 'passed': context.run.passed(item, result.durationMilliseconds); break;
            case 'failed': context.run.failed(item, messages, result.durationMilliseconds); break;
            case 'errored': context.run.errored(item, messages, result.durationMilliseconds); break;
            case 'skipped': context.run.skipped(item); break;
            default: throw new Error('Unknown Elm test result state: ' + result.state);
        }
        context.finished.add(result.testId);
        if (result.fuzz && result.reproductionCommand) {
            const selection = this.metadata.get(item);
            const inventory = this.projects.get(selection.projectUri)?.inventory;
            if (inventory?.version === context.parameters.version && inventory?.discoveryId === context.parameters.discoveryId) {
                this.replays.set(JSON.stringify(selection), {
                    version: context.parameters.version, discoveryId: context.parameters.discoveryId,
                    settings: result.fuzz.settings, command: result.reproductionCommand
                });
            }
            context.run.appendOutput(result.reproductionCommand + '\r\n', undefined, item);
        }
        for (const message of messages) {
            context.run.appendOutput(String(message.message).replace(/\r?\n/g, '\r\n') + '\r\n', message.location, item);
        }
        if (result.fuzz?.distributionReport) {
            context.run.appendOutput(result.fuzz.distributionReport.replace(/\r?\n/g, '\r\n') + '\r\n', undefined, item);
        }
    }

    private finishRemaining(context: RunContext, cancelled: boolean, message?: string): void {
        for (const id of context.selected) {
            const item = context.items.get(id);
            if (!item || context.finished.has(id)) {
                continue;
            }
            if (cancelled) {
                context.run.skipped(item);
            } else {
                context.run.errored(item, new vscode.TestMessage(message ?? 'The server did not return a result for this test.'));
            }
            context.finished.add(id);
        }
    }

    public async replay(item?: vscode.TestItem, copy = false): Promise<void> {
        let selection = item ? this.metadata.get(item) : undefined;
        if (!selection) {
            const choices = [...this.replays].flatMap(([key, replay]) => {
                const value: Selection = JSON.parse(key);
                const current = this.resolve(value);
                const inventory = this.projects.get(value.projectUri)?.inventory;
                return current && inventory?.version === replay.version && inventory?.discoveryId === replay.discoveryId ?
                    [{ label: current.label, description: value.projectUri, selection: value }] : [];
            });
            selection = (await vscode.window.showQuickPick(choices, { placeHolder: 'Choose a previously run fuzz test' }))?.selection;
        }
        const replay = selection && this.replays.get(JSON.stringify(selection));
        if (!selection || !replay) {
            await vscode.window.showWarningMessage('Run an Elm fuzz test first to obtain its reproduction settings.');
            return;
        }
        if (copy) {
            await vscode.env.clipboard.writeText(replay.command);
            return;
        }
        const cancellation = new vscode.CancellationTokenSource();
        try {
            const selected = this.resolve(selection);
            if (!selected) {
                throw new Error('This fuzz test no longer exists in the current workspace.');
            }
            await this.execute([selection], [], new vscode.TestRunRequest([selected], [], this.profile), cancellation.token, undefined, replay);
        } finally {
            cancellation.dispose();
        }
    }

    public dispose(): void {
        this.disposed = true;
        clearTimeout(this.refreshTimer);
        this.refreshCancellation?.cancel();
        for (const cancellation of this.runCancellations) {
            cancellation.cancel();
        }
        for (const disposable of this.disposables) {
            disposable.dispose();
        }
        this.continuous.clear();
        this.controller.dispose();
    }
}
