using Pine.Core.Elm.Elm019;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.LanguageServer.LanguageServiceInterface;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.Elm.LanguageServer;

/// <summary>
/// Loads Elm package versions from directories following the layout used by the Elm 0.19 package
/// cache: <c>&lt;search root&gt;/&lt;author&gt;/&lt;package&gt;/&lt;version&gt;</c>.
/// </summary>
/// <param name="workspace">Provides read access to the search roots.</param>
/// <param name="searchRootUris">
/// Directory URIs searched in order. The first root containing the package version wins.
/// </param>
/// <param name="logDelegate">Optional delegate receiving progress reports.</param>
public class ElmPackageSourceFromWorkspace(
    ILanguageServerWorkspace workspace,
    IReadOnlyList<string> searchRootUris,
    Action<string>? logDelegate = null)
    : IElmPackageSource
{
    /// <inheritdoc/>
    public Result<PackageLoadError, ElmPackageContent?> LoadPackage(
        ElmPackageVersion019Identifer packageVersionId)
    {
        var packageNameComponents = packageVersionId.PackageName.Split('/');

        if (packageNameComponents.Length is not 2 ||
            packageNameComponents.Any(string.IsNullOrEmpty))
        {
            return
                new PackageLoadError(
                    PackageLoadErrorKind.InvalidPackageIdentifier,
                    "Invalid package name: " + packageVersionId.PackageName);
        }

        if (string.IsNullOrEmpty(packageVersionId.VersionTag) ||
            packageVersionId.VersionTag.Contains('/'))
        {
            return
                new PackageLoadError(
                    PackageLoadErrorKind.InvalidPackageIdentifier,
                    "Invalid package version tag: " + packageVersionId.VersionTag);
        }

        foreach (var searchRootUri in searchRootUris)
        {
            var packageRootUri =
                CombineUri(
                    searchRootUri,
                    [.. packageNameComponents, packageVersionId.VersionTag]);

            var enumerateResult =
                workspace.EnumerateFiles(packageRootUri, IsRelevantFileName);

            if (enumerateResult.IsErrOrNull() is { } enumerateError)
            {
                logDelegate?.Invoke(
                    "Failed to enumerate files in " + packageRootUri + ": " +
                    enumerateError.Kind + ": " + enumerateError.Message);

                continue;
            }

            if (enumerateResult.IsOkOrNull() is not { } files)
            {
                throw new InvalidOperationException(
                    "Unexpected enumeration result type: " + enumerateResult.GetType());
            }

            if (files.Count is 0)
            {
                continue;
            }

            var contentResult = BuildPackageContent(packageVersionId, packageRootUri, files);

            if (contentResult.IsErrOrNull() is { } contentError)
            {
                return contentError;
            }

            return
                Result<PackageLoadError, ElmPackageContent?>.ok(
                    contentResult.IsOkOrNull()
                    ??
                    throw new InvalidOperationException(
                        "Unexpected package content result type: " + contentResult.GetType()));
        }

        return Result<PackageLoadError, ElmPackageContent?>.ok(null);
    }

    private Result<PackageLoadError, ElmPackageContent> BuildPackageContent(
        ElmPackageVersion019Identifer packageVersionId,
        string packageRootUri,
        IReadOnlyList<WorkspaceFile> files)
    {
        var exposedModuleNames = new HashSet<string>();

        foreach (var file in files)
        {
            if (LastPathComponent(file.DocumentUri) is not "elm.json")
            {
                continue;
            }

            try
            {
                var elmJsonParsed =
                    System.Text.Json.JsonSerializer.Deserialize<ElmJsonStructure>(file.Text);

                if (elmJsonParsed is null)
                {
                    logDelegate?.Invoke("Failed parsing elm.json file: " + file.DocumentUri);
                    continue;
                }

                if (elmJsonParsed.ExposedModules is { } exposedModules)
                {
                    foreach (var exposedModuleName in exposedModules)
                    {
                        exposedModuleNames.Add(exposedModuleName);
                    }
                }
            }
            catch (Exception e)
            {
                return
                    new PackageLoadError(
                        PackageLoadErrorKind.InvalidPackageContent,
                        "Failed parsing " + file.DocumentUri + ": " + e.Message);
            }
        }

        var modules =
            new List<KeyValuePair<IReadOnlyList<string>, string>>();

        var seenPaths =
            new HashSet<IReadOnlyList<string>>(
                comparer: EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        foreach (var file in files)
        {
            if (LastPathComponent(file.DocumentUri) is not { } fileName ||
                !fileName.EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            {
                continue;
            }

            var parseModuleNameResult = ElmModule.ParseModuleName(file.Text);

            if (parseModuleNameResult.IsErrOrNull() is { } parseErr)
            {
                logDelegate?.Invoke(
                    "Failed parsing module name in " + file.DocumentUri + ": " + parseErr);

                continue;
            }

            if (parseModuleNameResult.IsOkOrNull() is not { } moduleName)
            {
                throw new InvalidOperationException(
                    "Unexpected module name result type: " + parseModuleNameResult.GetType());
            }

            var moduleNameFlat = string.Join('.', moduleName);

            if (0 < exposedModuleNames.Count && !exposedModuleNames.Contains(moduleNameFlat))
            {
                continue;
            }

            if (RelativePathComponents(packageRootUri, file.DocumentUri) is not { } relativePath)
            {
                logDelegate?.Invoke(
                    "Ignoring file outside package root " + packageRootUri + ": " + file.DocumentUri);

                continue;
            }

            if (!seenPaths.Add(relativePath))
            {
                continue;
            }

            modules.Add(
                new KeyValuePair<IReadOnlyList<string>, string>(relativePath, file.Text));
        }

        logDelegate?.Invoke(
            "Package " + packageVersionId.PackageName + " " + packageVersionId.VersionTag +
            ": Found " + modules.Count + " Elm modules in " + packageRootUri);

        return new ElmPackageContent(packageRootUri, modules);
    }

    /// <summary>
    /// File names read to load a package version.
    /// </summary>
    public static bool IsRelevantFileName(string fileName) =>
        fileName.EndsWith(".elm", StringComparison.OrdinalIgnoreCase) ||
        string.Equals(fileName, "elm.json", StringComparison.OrdinalIgnoreCase);

    /// <summary>
    /// Appends path components to a directory URI, returning a directory URI ending with a slash.
    /// </summary>
    public static string CombineUri(string directoryUri, IReadOnlyList<string> pathComponents)
    {
        var builder = new System.Text.StringBuilder(directoryUri);

        if (!directoryUri.EndsWith('/'))
        {
            builder.Append('/');
        }

        foreach (var pathComponent in pathComponents)
        {
            builder.Append(Uri.EscapeDataString(pathComponent));
            builder.Append('/');
        }

        return builder.ToString();
    }

    private static string? LastPathComponent(string documentUri)
    {
        var withoutQuery = documentUri.Split('?', '#')[0];

        var lastSlashIndex = withoutQuery.LastIndexOf('/');

        var lastComponent =
            lastSlashIndex < 0
            ?
            withoutQuery
            :
            withoutQuery[(lastSlashIndex + 1)..];

        if (lastComponent.Length is 0)
        {
            return null;
        }

        return Uri.UnescapeDataString(lastComponent);
    }

    private static IReadOnlyList<string>? RelativePathComponents(
        string rootUri,
        string documentUri)
    {
        var rootUriWithSlash =
            rootUri.EndsWith('/')
            ?
            rootUri
            :
            rootUri + "/";

        if (!documentUri.StartsWith(rootUriWithSlash, StringComparison.Ordinal))
        {
            return null;
        }

        var relative = documentUri[rootUriWithSlash.Length..];

        if (relative.Length is 0)
        {
            return null;
        }

        return
            [.. relative.Split('/').Select(Uri.UnescapeDataString)];
    }
}
