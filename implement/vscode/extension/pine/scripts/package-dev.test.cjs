const assert = require('node:assert/strict');
const { test } = require('node:test');
const { developmentManifest } = require('./package-dev.cjs');
const { extensionIdentity, identityFromManifest, localizeCodeLens } = require('../client/out/identity');
const release = require('../package.json');

for (const manifest of [release, developmentManifest(release)]) {
    test(`${manifest.displayName} settings place Testing last in the Settings editor`, () => {
        const identity = identityFromManifest(manifest);
        const settings = Object.entries(manifest.contributes.configuration.properties);
        for (const [key, setting] of settings) {
            assert.ok(Number.isInteger(setting.order), `${key} must have an explicit display order`);
        }
        settings.sort(([leftKey, left], [rightKey, right]) =>
            left.order - right.order || leftKey.localeCompare(rightKey));
        assert.deepEqual(settings.map(([key]) => key), [
            `${identity.languageServerConfiguration}.pathToPineExecutableFile`,
            `${identity.languageServerConfiguration}.trace.server`,
            `${identity.testingConfiguration}.fuzzRuns`,
            `${identity.testingConfiguration}.seed`
        ]);
    });
}

test('release, development and lockfile versions stay aligned', () => {
    const lockfile = require('../package-lock.json');
    assert.equal(lockfile.version, release.version);
    assert.equal(lockfile.packages[''].version, release.version);
    assert.equal(developmentManifest(release).version, release.version);
});

test('release runtime identity remains unchanged', () => {
    assert.deepEqual(extensionIdentity, {
        extensionId: 'Pine.pine',
        displayName: 'Pine',
        commandPrefix: 'pine',
        languageServerConfiguration: 'pineLanguageServer',
        testingConfiguration: 'pine.testing',
        testControllerId: 'pine.elm',
        testSourceScheme: 'pine-test-source'
    });
});

test('development manifest isolates settings, commands, menus and runtime identity', () => {
    const original = structuredClone(release);
    const dev = developmentManifest(release);
    const identity = identityFromManifest(dev);
    assert.deepEqual(identity, {
        extensionId: 'Pine.pine-dev',
        displayName: 'Pine Dev',
        commandPrefix: 'pineDev',
        languageServerConfiguration: 'pineDevLanguageServer',
        testingConfiguration: 'pineDev.testing',
        testControllerId: 'pineDev.elm',
        testSourceScheme: 'pine-dev-test-source'
    });
    assert.deepEqual(Object.keys(dev.contributes.configuration.properties).sort(), [
        'pineDev.testing.fuzzRuns',
        'pineDev.testing.seed',
        'pineDevLanguageServer.pathToPineExecutableFile',
        'pineDevLanguageServer.trace.server'
    ]);
    assert.equal(dev.contributes.configuration.title, identity.displayName);
    for (const contribution of dev.contributes.commands) {
        assert.ok(contribution.command.startsWith(`${identity.commandPrefix}.`));
        assert.ok(contribution.title.startsWith(`${identity.displayName}:`));
    }
    for (const menu of Object.values(dev.contributes.menus)) {
        for (const contribution of menu) {
            assert.ok(dev.contributes.commands.some(command => command.command === contribution.command));
            assert.equal(contribution.when, `testControllerId == ${identity.testControllerId}`);
        }
    }
    assert.deepEqual(dev.contributes.languages, release.contributes.languages);
    assert.deepEqual(dev.contributes.grammars, release.contributes.grammars);
    assert.equal(dev.scripts, undefined);
    assert.deepEqual(release, original);
});

test('server reference commands route to their own extension without changing other commands', () => {
    const dev = identityFromManifest(developmentManifest(release));
    const lens = () => ({ command: { command: 'pine.client.peekReferences', title: '1 reference', arguments: ['uri'] } });
    assert.equal(localizeCodeLens(lens()).command.command, 'pine.client.peekReferences');
    const mapped = localizeCodeLens(lens(), dev);
    assert.equal(mapped.command.command, 'pineDev.client.peekReferences');
    assert.deepEqual(mapped.command.arguments, ['uri']);
    const other = { command: { command: 'editor.action.showReferences', title: 'References' } };
    assert.equal(localizeCodeLens(other, dev), other);
    const unresolved = {};
    assert.equal(localizeCodeLens(unresolved, dev), unresolved);
});
