import { CancellationToken, Disposable } from 'vscode';

export interface TestLocation {
    uri: string;
    range: {
        start: { line: number; character: number };
        end: { line: number; character: number };
    };
}

export interface ElmTestItem {
    id: string;
    parentId?: string;
    label: string;
    filePath: string;
    moduleName: string;
    declarationName: string;
    descriptionPath: string[];
    kind: 'Group' | 'Unit' | 'Fuzz' | 'Todo';
    selection: 'Included' | 'Skipped' | 'ExcludedByOnly';
    isOnly: boolean;
    isEmptyGroup: boolean;
    fuzzRuns?: number;
    sourcePath: string;
    location: TestLocation;
}

export interface TestInventory {
    projectUri: string;
    version: string;
    sourceRootUri: string;
    treeHash: string;
    complete: boolean;
    items?: ElmTestItem[];
    hasOnly: boolean;
    hasSkipped: boolean;
    documentVersions: Record<string, number>;
    resolutionFingerprint?: string;
    error?: { code: string; message: string };
    discoveryId?: string;
}

export interface FuzzSettings {
    seed: number;
    fuzzRuns: number;
}

export interface FuzzResult {
    settings: FuzzSettings;
    effectiveSeedState: string;
    runsRequested: number;
    runsElapsed?: number;
    failingIteration?: number;
    originalInput?: string;
    shrunkInput?: string;
    originalChoices: number[];
    shrunkChoices: number[];
    shrinkingCompleted: boolean;
    evaluationError?: string;
    failureReason?: string;
    distributionReport?: string;
}

export interface ElmTestResult {
    testId: string;
    state: 'passed' | 'failed' | 'errored' | 'skipped';
    durationMilliseconds: number;
    messages: { message: string; expected?: string; actual?: string; location?: TestLocation }[];
    fuzz?: FuzzResult;
    reproductionCommand?: string;
}

export interface ElmTestRunParams {
    projectUri: string;
    version: string;
    runId: string;
    include?: string[];
    exclude?: string[];
    seed?: number;
    fuzzRuns: number;
    discoveryId?: string;
}

export interface ElmTestRunResult {
    projectUri: string;
    version: string;
    runId: string;
    status: 'completed' | 'cancelled' | 'errored';
    settings: FuzzSettings;
    incomplete: boolean;
    results: ElmTestResult[];
    error?: { code: string; message: string };
    resolutionFingerprint?: string;
    discoveryId?: string;
}

export interface ElmTestEvent {
    projectUri: string;
    version: string;
    runId: string;
    sequence: number;
    kind: 'phase' | 'test' | 'end';
    testId?: string;
    state?: string;
    result?: ElmTestResult;
    message?: string;
}

export interface TestingTransport {
    discover(projectUri: string, token: CancellationToken): Promise<TestInventory>;
    run(parameters: ElmTestRunParams, token: CancellationToken): Promise<ElmTestRunResult>;
    source(parameters: { projectUri: string; version: string; path: string }, token: CancellationToken): Promise<{
        projectUri: string; version: string; path: string; text: string;
    }>;
    onEvent(listener: (event: ElmTestEvent) => void): Disposable;
    onChanged(listener: (event: { workspaceRevision: number }) => void): Disposable;
}

export function supportsTesting(experimental: unknown): boolean {
    if (typeof experimental !== 'object' || experimental === null || !('pineTests' in experimental)) {
        return false;
    }
    const testing = experimental.pineTests;
    return typeof testing === 'object' && testing !== null &&
        'protocolVersion' in testing && testing.protocolVersion === 1;
}

export function stableTestKey(item: ElmTestItem): string {
    return JSON.stringify([
        item.filePath, item.moduleName, item.declarationName, item.kind, item.descriptionPath, item.label
    ]);
}

export function selectedLeafIds(items: ElmTestItem[], include?: string[], exclude: string[] = []): Set<string> {
    const byId = new Map(items.map(item => [item.id, item]));
    const includes = include === undefined ? undefined : new Set(include);
    const excludes = new Set(exclude);
    const hasAncestor = (item: ElmTestItem, ids: Set<string>): boolean => {
        while (item) {
            if (ids.has(item.id)) {
                return true;
            }
            item = item.parentId ? byId.get(item.parentId) : undefined;
        }
        return false;
    };
    return new Set(items.filter(item =>
        (item.kind !== 'Group' || item.isEmptyGroup) &&
        (includes === undefined || hasAncestor(item, includes)) &&
        !hasAncestor(item, excludes)).map(item => item.id));
}
