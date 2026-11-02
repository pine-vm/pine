const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const { createVSIX } = require('@vscode/vsce');
const { identityFromManifest } = require('../client/out/identity');

function developmentManifest(release) {
    const manifest = structuredClone(release);
    manifest.name = 'pine-dev';
    manifest.displayName = 'Pine Dev';
    const identity = identityFromManifest(manifest);
    const command = value => value.replace(/^pine\./, `${identity.commandPrefix}.`);

    for (const contribution of manifest.contributes.commands) {
        contribution.command = command(contribution.command);
        contribution.title = contribution.title.replace(/^Pine:/, `${identity.displayName}:`);
    }
    for (const menu of Object.values(manifest.contributes.menus)) {
        for (const contribution of menu) {
            contribution.command = command(contribution.command);
            contribution.when = contribution.when.replace('pine.elm', identity.testControllerId);
        }
    }
    manifest.contributes.configuration.title = identity.displayName;
    manifest.contributes.configuration.properties = Object.fromEntries(
        Object.entries(manifest.contributes.configuration.properties).map(([key, value]) => [
            key.replace(/^pine\./, `${identity.commandPrefix}.`)
                .replace(/^pineLanguageServer\./, `${identity.languageServerConfiguration}.`),
            value
        ]));
    delete manifest.scripts;
    delete manifest.devDependencies;
    return manifest;
}

async function main() {
    const root = path.resolve(__dirname, '..');
    const manifest = developmentManifest(require('../package.json'));
    const staging = fs.mkdtempSync(path.join(os.tmpdir(), 'pine-dev-vsix-'));
    const output = path.join(root, 'out', `${manifest.name}-${manifest.version}.vsix`);
    try {
        for (const entry of ['README.md', 'LICENSE.txt', '.vscodeignore', 'src', 'client/out', 'client/node_modules']) {
            fs.cpSync(path.join(root, entry), path.join(staging, entry), {
                recursive: true,
                filter: source => !source.endsWith('.map') && source !== path.join(root, 'client', 'out', 'test')
            });
        }
        fs.writeFileSync(path.join(staging, 'package.json'), JSON.stringify(manifest, null, '\t') + '\n');
        fs.mkdirSync(path.dirname(output), { recursive: true });
        await createVSIX({ cwd: staging, packagePath: output, dependencies: false });
        console.log(`Development VSIX written to: ${output}`);
    } finally {
        fs.rmSync(staging, { recursive: true, force: true });
    }
}

module.exports = { developmentManifest };

if (require.main === module) {
    main().catch(error => {
        console.error(error);
        process.exitCode = 1;
    });
}
