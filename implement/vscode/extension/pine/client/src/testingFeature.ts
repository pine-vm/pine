import { Disposable, OutputChannel } from 'vscode';
import { ClientCapabilities, FeatureState, LanguageClient, ServerCapabilities, StaticFeature } from 'vscode-languageclient/node';
import { PineTestController } from './testing';
import { supportsTesting } from './testingProtocol';

export class PineTestingFeature implements StaticFeature, Disposable {
    public testing?: PineTestController;

    constructor(private readonly client: LanguageClient, private readonly output: OutputChannel) { }

    fillClientCapabilities(capabilities: ClientCapabilities): void {
        capabilities.experimental = {
            ...capabilities.experimental,
            pineTests: { protocolVersion: 1 }
        };
    }

    initialize(capabilities: ServerCapabilities): void {
        if (!supportsTesting(capabilities.experimental)) {
            this.output.appendLine('This language server does not advertise Pine testing protocol version 1.');
            return;
        }
        this.testing = new PineTestController({
            discover: (projectUri, token) => this.client.sendRequest('pine/tests/discover', { projectUri }, token),
            run: (parameters, token) => this.client.sendRequest('pine/tests/run', parameters, token),
            source: (parameters, token) => this.client.sendRequest('pine/tests/source', parameters, token),
            onEvent: listener => this.client.onNotification('pine/tests/event', listener),
            onChanged: listener => this.client.onNotification('pine/tests/changed', listener)
        }, this.output);
    }

    getState(): FeatureState {
        return { kind: 'static' };
    }

    clear(): void {
        this.testing?.dispose();
        this.testing = undefined;
    }

    dispose(): void {
        this.clear();
    }
}
