import { createRequire } from 'module';
import type { CodeLens } from 'vscode';

interface ExtensionManifest {
    publisher: string;
    name: string;
    displayName: string;
}

export function identityFromManifest(manifest: ExtensionManifest) {
    const namespace = manifest.name.replace(/-([a-z])/g, (_, letter: string) => letter.toUpperCase());
    return {
        extensionId: `${manifest.publisher}.${manifest.name}`,
        displayName: manifest.displayName,
        commandPrefix: namespace,
        languageServerConfiguration: `${namespace}LanguageServer`,
        testingConfiguration: `${namespace}.testing`,
        testControllerId: `${namespace}.elm`,
        testSourceScheme: `${manifest.name}-test-source`
    };
}

export const extensionIdentity = identityFromManifest(createRequire(__filename)('../../package.json'));

export function localizeCodeLens(lens: CodeLens, identity = extensionIdentity): CodeLens {
    // The server's wire protocol uses the release command ID for both builds.
    if (lens.command?.command === 'pine.client.peekReferences') {
        lens.command = { ...lens.command, command: `${identity.commandPrefix}.client.peekReferences` };
    }
    return lens;
}
