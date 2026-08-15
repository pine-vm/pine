using ElmTime;
using Pine.Core;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Elm019;

public static class ElmAppCompilationToHtml
{
    public static async Task<ReadOnlyMemory<byte>> CompileHtmlDocumentAsync(
        FileTree sourceFiles,
        IReadOnlyList<string> entryPointFilePath,
        string? entryPointDeclarationName = null,
        CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(sourceFiles);
        ArgumentNullException.ThrowIfNull(entryPointFilePath);

        if (sourceFiles is not FileTree.DirectoryNode)
            throw new ArgumentException("The Elm app source files must have a directory root.", nameof(sourceFiles));

        ValidatePath(entryPointFilePath, nameof(entryPointFilePath));

        var loweringResult =
            ElmAppCompilation.AsCompletelyLoweredElmApp(
                FileTreeExtensions.ToFlatDictionaryWithPathComparer(sourceFiles),
                workingDirectoryRelative: [],
                ElmAppInterfaceConfig.Default with
                {
                    CompilationRootFilePath = entryPointFilePath
                });

        if (loweringResult.IsErrOrNull() is { } loweringErr)
        {
            throw new InvalidOperationException(
                "Failed lowering Elm code with " + loweringErr.Count + " error(s):\n" +
                ElmAppCompilation.CompileCompilationErrorsDisplayText(loweringErr));
        }

        if (loweringResult.IsOkOrNull() is not { } loweringOk)
            throw new InvalidOperationException("Unexpected lowering result type: " + loweringResult);

        sourceFiles =
            FileTree.FromSetOfFilesWithStringPath(loweringOk.Result.CompiledFiles);

        var entryPointFile =
            sourceFiles.GetNodeAtPath(entryPointFilePath) as FileTree.FileNode
            ??
            throw new ArgumentException(
                "The entry point path does not identify a file in the source tree.",
                nameof(entryPointFilePath));

        var entryPointModuleName =
            ParseModuleName(
                Encoding.UTF8.GetString(entryPointFile.Bytes.Span),
                nameof(entryPointFilePath));

        var compilationEntryPointFilePath = entryPointFilePath;
        (IReadOnlyList<string> path, string content)? virtualModule = null;

        if (entryPointDeclarationName is not null)
        {
            ValidateDeclarationName(entryPointDeclarationName);

            var sourceDirectoryPath =
                InferSourceDirectoryPath(entryPointFilePath, entryPointModuleName);

            var virtualModuleName =
                "PineWebBrowserEntryPoint" + Guid.NewGuid().ToString("N");

            var virtualModulePath =
                sourceDirectoryPath
                .Append(virtualModuleName + ".elm")
                .ToArray();

            virtualModule =
                (
                    virtualModulePath,
                    $"""
                    module {virtualModuleName} exposing (main)

                    import {string.Join('.', entryPointModuleName)}


                    main =
                        {string.Join('.', entryPointModuleName)}.{entryPointDeclarationName}
                    """);

            compilationEntryPointFilePath = virtualModulePath;
            entryPointModuleName = [virtualModuleName];
        }

        return
            await CompileHtmlDocumentAsync(
                sourceFiles,
                compilationEntryPointFilePath,
                entryPointModuleName,
                virtualModule,
                cancellationToken);
    }

    private static async Task<ReadOnlyMemory<byte>> CompileHtmlDocumentAsync(
        FileTree sourceFiles,
        IReadOnlyList<string> compilationEntryPointFilePath,
        IReadOnlyList<string> entryPointModuleName,
        (IReadOnlyList<string> path, string content)? virtualModule,
        CancellationToken cancellationToken)
    {
        var temporaryDirectory =
            Path.Combine(
                Path.GetTempPath(),
                "pine-elm-app-browser-" + Guid.NewGuid().ToString("N"));

        Directory.CreateDirectory(temporaryDirectory);

        try
        {
            foreach (var (path, content) in sourceFiles.EnumerateFilesTransitive())
            {
                ValidatePath(path, nameof(sourceFiles));

                var filePath = Path.Combine([temporaryDirectory, .. path]);
                Directory.CreateDirectory(Path.GetDirectoryName(filePath)!);

                await File.WriteAllBytesAsync(filePath, content.ToArray(), cancellationToken)
                .ConfigureAwait(false);
            }

            if (virtualModule is { } module)
            {
                var virtualModuleFilePath =
                    Path.Combine([temporaryDirectory, .. module.path]);

                Directory.CreateDirectory(Path.GetDirectoryName(virtualModuleFilePath)!);

                await File.WriteAllTextAsync(
                    virtualModuleFilePath,
                    module.content,
                    new UTF8Encoding(encoderShouldEmitUTF8Identifier: false),
                    cancellationToken)
                .ConfigureAwait(false);
            }

            var outputFilePath = Path.Combine(temporaryDirectory, "elm-app.js");

            var processOutput =
                await ElmMakeRunner.ElmMakeAsync(
                    temporaryDirectory,
                    Path.Combine([.. compilationEntryPointFilePath]),
                    outputFilePath,
                    cancellationToken)
                .ConfigureAwait(false);

            if (processOutput.ExitCode is not 0)
            {
                throw new ElmAppCompilationException(
                    processOutput.ExitCode,
                    processOutput.StandardOutput,
                    processOutput.StandardError);
            }

            var compiledJavascript =
                await File.ReadAllBytesAsync(outputFilePath, cancellationToken)
                .ConfigureAwait(false);

            return BuildHtmlDocument(compiledJavascript, string.Join('.', entryPointModuleName));
        }
        finally
        {
            try
            {
                Directory.Delete(temporaryDirectory, recursive: true);
            }
            catch (Exception)
            {
            }
        }
    }

    private static ReadOnlyMemory<byte> BuildHtmlDocument(
        byte[] compiledJavascript,
        string entryPointModuleName)
    {
        var compiledJavascriptBase64 = Convert.ToBase64String(compiledJavascript);

        return
            Encoding.UTF8.GetBytes(
                $$"""
                <!doctype html>
                <html>
                <head>
                  <meta charset="utf-8">
                  <meta name="viewport" content="width=device-width, initial-scale=1">
                  <style>html, body { margin: 0; }</style>
                </head>
                <body>
                  <div id="elm-app"></div>
                  <script>
                    const elmBytes = Uint8Array.from(
                      atob("{{compiledJavascriptBase64}}"),
                      character => character.charCodeAt(0));
                    (0, eval)(new TextDecoder().decode(elmBytes));
                    const elmEntryPoint =
                      "{{entryPointModuleName}}"
                        .split(".")
                        .reduce((scope, name) => scope[name], Elm);
                    elmEntryPoint.init({
                      node: document.getElementById("elm-app")
                    });
                    document.documentElement.dataset.elmAppReady = "true";
                  </script>
                </body>
                </html>
                """);
    }

    private static IReadOnlyList<string> ParseModuleName(string moduleText, string parameterName)
    {
        var parseResult = ElmModule.ParseModuleName(moduleText);

        if (parseResult.IsErrOrNull() is { } parseError)
        {
            throw new ArgumentException(
                "Failed to parse the Elm entry point module name: " + parseError,
                parameterName);
        }

        return
            parseResult.IsOkOrNull()
            ??
            throw new InvalidOperationException("Unexpected Elm module name parse result.");
    }

    private static IReadOnlyList<string> InferSourceDirectoryPath(
        IReadOnlyList<string> entryPointFilePath,
        IReadOnlyList<string> moduleName)
    {
        if (entryPointFilePath.Count < moduleName.Count)
        {
            throw new ArgumentException(
                "The entry point path does not match its Elm module name.",
                nameof(entryPointFilePath));
        }

        var modulePathStart = entryPointFilePath.Count - moduleName.Count;

        for (var index = 0; index < moduleName.Count; index++)
        {
            var expectedPathSegment =
                moduleName[index] + (index == moduleName.Count - 1 ? ".elm" : string.Empty);

            if (!string.Equals(
                entryPointFilePath[modulePathStart + index],
                expectedPathSegment,
                StringComparison.Ordinal))
            {
                throw new ArgumentException(
                    "The entry point path does not match its Elm module name.",
                    nameof(entryPointFilePath));
            }
        }

        return entryPointFilePath.Take(modulePathStart).ToArray();
    }

    private static void ValidateDeclarationName(string declarationName)
    {
        if (declarationName.Length is 0 ||
            !char.IsLower(declarationName[0]) ||
            declarationName.Skip(1).Any(character => !char.IsLetterOrDigit(character) && character is not '_'))
        {
            throw new ArgumentException(
                "The entry point declaration name must be an Elm lower-case identifier.",
                nameof(declarationName));
        }
    }

    private static void ValidatePath(IReadOnlyList<string> path, string parameterName)
    {
        if (path.Count is 0)
            throw new ArgumentException("The file path must not be empty.", parameterName);

        foreach (var segment in path)
        {
            if (string.IsNullOrWhiteSpace(segment) ||
                segment is "." or ".." ||
                segment.IndexOfAny(['/', '\\', '\0']) >= 0 ||
                segment.IndexOfAny(Path.GetInvalidFileNameChars()) >= 0)
            {
                throw new ArgumentException(
                    "File paths must contain only valid, relative path segments.",
                    parameterName);
            }
        }
    }
}

public sealed class ElmAppCompilationException(
    int exitCode,
    string standardOutput,
    string standardError)
    : Exception("elm make failed with exit code " + exitCode + ":" + Environment.NewLine + standardError)
{
    public int ExitCode { get; } = exitCode;

    public string StandardOutput { get; } = standardOutput;

    public string StandardError { get; } = standardError;
}
