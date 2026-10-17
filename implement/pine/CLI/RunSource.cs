using Pine.Core;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;

namespace Pine.CLI;

internal sealed record RunSource(FileTree Tree, string[] FilePath, string? Declaration, int? Line, string? DirectoryPath)
{
    internal sealed class Loader(
        string cacheDirectory,
        TextWriter diagnostics,
        Func<string, string, string>? resolveBranch = null,
        Func<string, string, CancellationToken, GitRemoteRepository>? openRemote = null,
        Func<DateTimeOffset>? clock = null)
    {
        private readonly GitBranchResolutionCache _branches =
            resolveBranch is null && clock is null
            ?
            GitBranchResolutionCache.Active ?? new(cacheDirectory)
            :
            new(cacheDirectory, resolveBranch, clock);

        private readonly Dictionary<(string Repository, string Commit), GitRemoteRepository> _repositories = [];

        private GitRemoteRepository Repository(LoadFromGitHubOrGitLab.ParsedUrl pinned, CancellationToken token)
        {
            token.ThrowIfCancellationRequested();
            var commit = pinned.inRepository!.@ref;

            if (_repositories.TryGetValue((pinned.repository, commit), out var existing))
                return existing;

            diagnostics.WriteLine("Loading Git source " + pinned.repository + " at " + commit + ".");

            var repository =
                openRemote is not null
                ?
                openRemote(pinned.repository + ".git", commit, token)
                :
                GitRemoteRepository.Open(
                    pinned.repository + ".git",
                    commit,
                    depth: 1,
                    token,
                    blobCacheDirectory: Path.Combine(cacheDirectory, "git", "blobs"));

            token.ThrowIfCancellationRequested();
            _repositories.Add((pinned.repository, commit), repository);
            return repository;
        }

        private string Pin(string source, CancellationToken token)
        {
            if (!Uri.TryCreate(source, UriKind.Absolute, out var uri) ||
                uri.Scheme is not "https" ||
                uri.Host is not ("github.com" or "gitlab.com") ||
                !uri.IsDefaultPort || uri.UserInfo.Length > 0 || uri.Query.Length > 0 || uri.Fragment.Length > 0)
            {
                throw new ArgumentException("Use an HTTPS GitHub or GitLab file URL without credentials, query, or fragment.");
            }

            var parsed = LoadFromGitHubOrGitLab.ParseUrl(source);

            if (parsed?.inRepository is not { } location)
            {
                throw new ArgumentException(
                    "Select a file or directory URL containing an explicit branch or full commit ID; repository-only URLs are not supported.");
            }

            if (new Uri(parsed.repository).AbsolutePath.Split('/', StringSplitOptions.RemoveEmptyEntries)
                .Select(Uri.UnescapeDataString)
                .Any(part => part is "." or ".." || part.IndexOfAny(['/', '\\', ':']) >= 0))
            {
                throw new ArgumentException("Invalid repository path.");
            }

            var reference = Uri.UnescapeDataString(location.@ref);

            if (reference.Contains('/') || reference.Contains('\\'))
                throw new ArgumentException("Branch names containing slashes are not supported.");

            var commit = _branches.Pin(parsed.repository, reference, token, diagnostics);

            return
                LoadFromGitHubOrGitLab.BackToUrl(
                    parsed with { inRepository = location with { @ref = commit } });
        }

        public ReadOnlyMemory<byte> ReadInput(string source, CancellationToken token)
        {
            token.ThrowIfCancellationRequested();

            if (!IsRemote(source))
                return File.ReadAllBytes(source);

            var pinned = LoadFromGitHubOrGitLab.ParseUrl(Pin(source, token))!;
            var path = DecodePath(pinned);
            var repository = Repository(pinned, token);

            if (repository.Find(path) is not { IsDirectory: false })
                throw new IOException("The input URL must select a file.");

            return ((FileTree.FileNode)repository.ReadPath(path, token)).Bytes;
        }

        public RunSource Read(string source, string? inputDirectory, CancellationToken token)
        {
            var selector =
                Regex.Match(
                    source,
                    @"^(?<file>.*\.elm)(?:[/\\](?<decl>[a-z][A-Za-z0-9_]*))?(?:#L(?<line>[1-9][0-9]*))?$");

            if (!selector.Success)
            {
                if (IsRemote(source))
                    _ = Pin(source, token);

                throw new ArgumentException(
                    "Select an Elm module (.elm), a declaration (.elm/name), or a declaration line (.elm#Lnumber).");
            }

            var fileName = selector.Groups["file"].Value;
            var declaration = selector.Groups["decl"].Success ? selector.Groups["decl"].Value : null;

            int? line =
                selector.Groups["line"].Success
                ?
                int.Parse(selector.Groups["line"].Value, CultureInfo.InvariantCulture)
                :
                null;

            if (IsRemote(fileName))
            {
                if (inputDirectory is not null)
                    throw new ArgumentException("--input-directory cannot be used with a remote entry point.");

                var pinned = LoadFromGitHubOrGitLab.ParseUrl(Pin(fileName, token))!;
                var path = DecodePath(pinned);
                return ReadRemote(Repository(pinned, token), path, declaration, line, token);
            }

            var absoluteFile =
                Path.GetFullPath(
                    fileName,
                    inputDirectory is null ? Environment.CurrentDirectory : Path.GetFullPath(inputDirectory));

            if (!File.Exists(absoluteFile))
                throw new FileNotFoundException("Elm module not found: " + absoluteFile);

            var directory =
                inputDirectory is not null
                ?
                Path.GetFullPath(inputDirectory)
                :
                FindLocalRoot(absoluteFile);

            var relative = Path.GetRelativePath(directory, absoluteFile).Split(Path.DirectorySeparatorChar);

            if (relative[0] is "..")
                throw new ArgumentException("The entry point must be inside --input-directory.");

            var files = ReadDirectory(directory, token);

            if (inputDirectory is null && !File.Exists(Path.Combine(directory, "elm.json")))
            {
                while (MissingImports(files, relative) is { Length: > 0 } missing &&
                    Directory.GetParent(directory) is { } parent &&
                    !Directory.Exists(Path.Combine(directory, ".git")) && !File.Exists(Path.Combine(directory, ".git")))
                {
                    var inRepository =
                        Ancestors(parent).Any(
                            item =>
                            Directory.Exists(Path.Combine(item.FullName, ".git")) ||
                            File.Exists(Path.Combine(item.FullName, ".git")));

                    if (!inRepository &&
                        !missing.Any(name => File.Exists(Path.Combine(parent.FullName, Path.Combine(name.Split('.')) + ".elm"))))
                    {
                        break;
                    }

                    directory = parent.FullName;
                    relative = Path.GetRelativePath(directory, absoluteFile).Split(Path.DirectorySeparatorChar);
                    files = ReadDirectory(directory, token);
                }
            }

            return new(files, relative, declaration, line, directory);
        }

        private static FileTree ReadDirectory(string directory, CancellationToken token)
        {
            var files = new List<(IReadOnlyList<string>, ReadOnlyMemory<byte>)>();

            void Read(string current)
            {
                token.ThrowIfCancellationRequested();

                foreach (var file in Directory.EnumerateFiles(current))
                {
                    if (Path.GetExtension(file) is not ".elm" && Path.GetFileName(file) is not "elm.json")
                        continue;

                    files.Add(
                        (Path.GetRelativePath(directory, file).Split(Path.DirectorySeparatorChar), File.ReadAllBytes(file)));
                }

                foreach (var child in Directory.EnumerateDirectories(current))
                {
                    if (Path.GetFileName(child) is not (".git" or "elm-stuff" or "bin" or "obj" or "node_modules"))
                        Read(child);
                }
            }

            Read(directory);
            return FileTree.FromSetOfFilesWithStringPath(files);
        }

        private static string FindLocalRoot(string file)
        {
            var parent = new DirectoryInfo(Path.GetDirectoryName(file)!);

            for (var directory = parent; directory is not null; directory = directory.Parent)
            {
                var manifest = Path.Combine(directory.FullName, "elm.json");

                if (File.Exists(manifest))
                {
                    var prefix =
                        Path.GetRelativePath(Path.GetPathRoot(file)!, directory.FullName).Split(
                            Path.DirectorySeparatorChar);

                    var tree =
                        FileTree.FromSetOfFilesWithStringPath(
                            new[] { ((IReadOnlyList<string>)[.. prefix, "elm.json"], (ReadOnlyMemory<byte>)File.ReadAllBytes(manifest)) });

                    var entry =
                        (IReadOnlyList<string>)[
                        .. prefix,
                        .. Path.GetRelativePath(directory.FullName, file).Split(Path.DirectorySeparatorChar)
                        ];

                    if (ElmAppDependencyResolution.FindElmJsonForEntryPoint(tree, entry) is not null)
                        return directory.FullName;
                }

                if (Directory.Exists(Path.Combine(directory.FullName, ".git")) ||
                    File.Exists(Path.Combine(directory.FullName, ".git")))
                {
                    break;
                }
            }

            return parent.FullName;
        }

        private static string[] DecodePath(LoadFromGitHubOrGitLab.ParsedUrl pinned)
        {
            var path =
                pinned.inRepository!.path.Split('/', StringSplitOptions.RemoveEmptyEntries)
                .Select(Uri.UnescapeDataString).ToArray();

            if (path.Any(part => part is "." or ".." || part.IndexOfAny(['/', '\\']) >= 0))
                throw new ArgumentException("Invalid remote file path.");

            return path;
        }

        private static RunSource ReadRemote(
            GitRemoteRepository repository, string[] path, string? declaration, int? line, CancellationToken token)
        {
            var probes =
                repository.ReadFiles(
                    new[] { (IReadOnlyList<string>)path }.Concat(
                        Enumerable.Range(0, path.Length).Select(
                            length => (IReadOnlyList<string>)[
                            .. path.Take(length),
                            "elm.json"
                            ])),
                    token);

            if (!probes.ContainsKey(path))
                throw new FileNotFoundException("Elm module not found in the repository.");

            var files = repository.ListFiles([], token);

            var metadata =
                FileTree.FromSetOfFilesWithStringPath(
                    files.Select(
                        file =>
                        (file.Path, probes.TryGetValue(file.Path, out var bytes) ? bytes : ReadOnlyMemory<byte>.Empty)));

            if (ElmAppDependencyResolution.FindElmJsonForEntryPoint(metadata, path) is { } manifest)
            {
                var selected =
                    ElmResolvedBuildPreparation.SelectProjectSources(
                        metadata,
                        manifest.filePath,
                        manifest.elmJsonParsed,
                        includeTests: false);

                var contents =
                    repository.ReadFiles(selected.EnumerateFilesTransitive().Select(file => file.path), token);

                var root = manifest.filePath.Take(manifest.filePath.Count - 1).ToArray();

                foreach (var file in contents.Keys)
                {
                    while (!file.Take(root.Length).SequenceEqual(root))
                    {
                        root = root[..^1];
                    }
                }

                return FromRemoteFiles(contents, path, root, declaration, line);
            }

            for (var length = path.Length - 1; length >= 0; --length)
            {
                token.ThrowIfCancellationRequested();
                var root = path[..length];

                var contents =
                    repository.ReadFiles(
                        files.Where(
                            file =>
                            file.Path.Take(length).SequenceEqual(root) &&
                            !file.Path.Any(part => part is ".git" or "elm-stuff" or "bin" or "obj" or "node_modules") &&
                            (file.Path[^1] is "elm.json" ||
                            file.Path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase)))
                        .Select(file => file.Path),
                        token);

                var source = FromRemoteFiles(contents, path, root, declaration, line);

                if (length is 0 || MissingImports(source.Tree, source.FilePath).Length is 0)
                    return source;
            }

            throw new InvalidOperationException("No source directory for the selected Elm module.");
        }

        private static RunSource FromRemoteFiles(
            IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> files,
            string[] path,
            string[] root,
            string? declaration,
            int? line) =>
            new(
                FileTree.FromSetOfFilesWithStringPath(
                    files.Select(
                        file =>
                        ((IReadOnlyList<string>)file.Key.Skip(root.Length).ToArray(), file.Value))),
                path[root.Length..],
                declaration,
                line,
                null);

        private static IEnumerable<DirectoryInfo> Ancestors(DirectoryInfo directory)
        {
            for (var current = directory; current is not null; current = current.Parent)
            {
                yield return current;
            }
        }

        private static readonly Lazy<HashSet<string>> s_bundledModules =
            new(
                () =>
                new HashSet<string>(
                    Core.Elm.ElmInElm.BundledFiles.ElmKernelModulesDefault.Value.EnumerateFilesTransitive()
                    .Select(
                        file => ElmModule.ParseModuleName(Encoding.UTF8.GetString(file.fileContent.Span)).IsOkOrNull())
                    .Where(name => name is not null).Select(name => string.Join(".", name!)),
                    StringComparer.Ordinal)
                {
                    "Basics",
                    "Debug",
                    "Pine_builtin",
                    "Pine_kernel"
                });

        private static string[] MissingImports(FileTree tree, IReadOnlyList<string> entryPath)
        {
            var modules = new Dictionary<string, ReadOnlyMemory<byte>>(StringComparer.Ordinal);

            foreach (var file in tree.EnumerateFilesTransitive().Where(file => file.path[^1].EndsWith(".elm", StringComparison.Ordinal)))
            {
                if (ElmModule.ParseModuleName(Encoding.UTF8.GetString(file.fileContent.Span)).IsOkOrNull() is { } name)
                    modules.TryAdd(string.Join(".", name), file.fileContent);
            }

            var missing = new HashSet<string>(StringComparer.Ordinal);
            var visited = new HashSet<string>(StringComparer.Ordinal);

            var entry =
                (FileTree.FileNode)(tree.GetNodeAtPath(entryPath) ?? throw new IOException("Elm module not found."));

            void Visit(ReadOnlyMemory<byte> bytes)
            {
                var syntax = ElmSyntaxParser.ParseModuleText(Encoding.UTF8.GetString(bytes.Span)).IsOkOrNull();

                if (syntax is null)
                    return; // Compilation reports syntax errors in demanded declarations.

                foreach (var import in syntax.Imports)
                {
                    var name = string.Join(".", import.Value.ModuleName.Value);

                    if (!visited.Add(name))
                        continue;

                    if (modules.TryGetValue(name, out var imported))
                        Visit(imported);

                    else if (!s_bundledModules.Value.Contains(name))
                        missing.Add(name);
                }
            }

            Visit(entry.Bytes);
            return [.. missing];
        }

    }

    internal static bool IsRemote(string source) =>
        source.Contains("://", StringComparison.Ordinal);
}
