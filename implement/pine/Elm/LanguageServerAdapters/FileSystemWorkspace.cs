using Pine.Core;
using Pine.Core.Elm.LanguageServer;
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Pine.Elm.LanguageServerAdapters;

/// <summary>
/// Adapts the local file system to the language-server workspace boundary, mapping
/// <c>file://</c> document URIs to local paths.
/// </summary>
public sealed class FileSystemWorkspace : ILanguageServerWorkspace
{
    private static readonly Encoding s_strictUtf8 =
        new UTF8Encoding(encoderShouldEmitUTF8Identifier: false, throwOnInvalidBytes: true);

    /// <inheritdoc/>
    public Result<WorkspaceAccessError, IReadOnlyList<WorkspaceFile>> EnumerateFiles(
        string rootDocumentUri,
        Func<string, bool>? fileNameFilter = null)
    {
        var rootPathResult = LocalPathFromDocumentUri(rootDocumentUri);

        if (rootPathResult.IsErrOrNull() is { } rootError)
            return rootError;

        var rootPath =
            rootPathResult.IsOkOrNull()
            ??
            throw new InvalidOperationException("Unexpected document URI conversion result.");

        if (!Directory.Exists(rootPath))
        {
            return Result<WorkspaceAccessError, IReadOnlyList<WorkspaceFile>>.ok([]);
        }

        var files = new List<WorkspaceFile>();

        try
        {
            foreach (var filePath in
                Directory.EnumerateFiles(rootPath, "*", SearchOption.AllDirectories))
            {
                if (fileNameFilter is not null && !fileNameFilter(Path.GetFileName(filePath)))
                {
                    continue;
                }

                if (!Uri.TryCreate(filePath, UriKind.Absolute, out var uri))
                {
                    return
                        new WorkspaceAccessError(
                            WorkspaceAccessErrorKind.InvalidUri,
                            "Failed to create URI for file path: " + filePath);
                }

                var textResult = ReadAllText(filePath);

                if (textResult.IsErrOrNull() is { } textError)
                    return textError;

                files.Add(
                    new WorkspaceFile(
                        uri.AbsoluteUri,
                        textResult.IsOkOrNull()
                        ??
                        throw new InvalidOperationException("Unexpected text reading result.")));
            }
        }
        catch (Exception exception)
        {
            return
                new WorkspaceAccessError(
                    WorkspaceAccessErrorKind.BackendFailure,
                    "Failed to enumerate files below " + rootPath + ": " + exception.Message);
        }

        return files;
    }

    /// <inheritdoc/>
    public Result<WorkspaceAccessError, WorkspaceFile?> ReadFile(string documentUri)
    {
        var filePathResult = LocalPathFromDocumentUri(documentUri);

        if (filePathResult.IsErrOrNull() is { } pathError)
            return pathError;

        var filePath =
            filePathResult.IsOkOrNull()
            ??
            throw new InvalidOperationException("Unexpected document URI conversion result.");

        if (!File.Exists(filePath))
        {
            return Result<WorkspaceAccessError, WorkspaceFile?>.ok(null);
        }

        var textResult = ReadAllText(filePath);

        if (textResult.IsErrOrNull() is { } textError)
            return textError;

        return
            new WorkspaceFile(
                documentUri,
                textResult.IsOkOrNull()
                ??
                throw new InvalidOperationException("Unexpected text reading result."));
    }

    /// <inheritdoc/>
    public Result<WorkspaceAccessError, ElmProjectLocation?> FindNearestElmProject(string documentUri)
    {
        var filePathResult = LocalPathFromDocumentUri(documentUri);

        if (filePathResult.IsErrOrNull() is { } pathError)
            return pathError;

        var filePath =
            filePathResult.IsOkOrNull()
            ??
            throw new InvalidOperationException("Unexpected document URI conversion result.");

        try
        {
            var directoryName = Path.GetDirectoryName(filePath);

            while (directoryName is not null)
            {
                var elmJsonFilePath = Path.Combine(directoryName, "elm.json");

                if (File.Exists(elmJsonFilePath))
                {
                    if (!Uri.TryCreate(elmJsonFilePath, UriKind.Absolute, out var elmJsonUri))
                    {
                        return
                            new WorkspaceAccessError(
                                WorkspaceAccessErrorKind.InvalidUri,
                                "Failed to create URI for file path: " + elmJsonFilePath);
                    }

                    return new ElmProjectLocation(elmJsonUri.AbsoluteUri);
                }

                directoryName = Path.GetDirectoryName(directoryName);
            }

            return Result<WorkspaceAccessError, ElmProjectLocation?>.ok(null);
        }
        catch (Exception exception)
        {
            return
                new WorkspaceAccessError(
                    WorkspaceAccessErrorKind.BackendFailure,
                    "Failed to locate the nearest Elm project for " + filePath + ": " + exception.Message);
        }
    }

    private static Result<WorkspaceAccessError, string> ReadAllText(string filePath)
    {
        try
        {
            return File.ReadAllText(filePath, s_strictUtf8);
        }
        catch (DecoderFallbackException exception)
        {
            return
                new WorkspaceAccessError(
                    WorkspaceAccessErrorKind.InvalidText,
                    "File is not valid UTF-8 at '" + filePath + "': " + exception.Message);
        }
        catch (Exception exception)
        {
            return
                new WorkspaceAccessError(
                    WorkspaceAccessErrorKind.BackendFailure,
                    "Failed to read file '" + filePath + "': " + exception.Message);
        }
    }

    private static Result<WorkspaceAccessError, string> LocalPathFromDocumentUri(string documentUri)
    {
        if (!Uri.TryCreate(documentUri, UriKind.Absolute, out var uri))
        {
            return
                new WorkspaceAccessError(
                    WorkspaceAccessErrorKind.InvalidUri,
                    "The document URI is not absolute: " + documentUri);
        }

        if (!uri.IsFile)
        {
            return
                new WorkspaceAccessError(
                    WorkspaceAccessErrorKind.InvalidUri,
                    "Only file URIs are supported: " + documentUri);
        }

        return uri.LocalPath;
    }
}
