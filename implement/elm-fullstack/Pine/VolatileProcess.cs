using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Security.Cryptography;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Scripting;
using Microsoft.CodeAnalysis.Scripting;

namespace Pine;

public class VolatileProcess
{
    readonly object @lock = new();

    ScriptState<object>? scriptState;

    readonly MetadataResolver metadataResolver;

    readonly Func<byte[], byte[]?> getFileFromHashSHA256;

    public VolatileProcess(
        Func<byte[], byte[]?> getFileFromHashSHA256,
        string csharpScriptCode)
    {
        this.getFileFromHashSHA256 = getFileFromHashSHA256;

        metadataResolver = new MetadataResolver(csharpScriptCode: csharpScriptCode, getFileFromHashSHA256);

        var runSetupScriptResult = RunScript(csharpScriptCode);

        if (runSetupScriptResult.Exception != null)
            throw new Exception("Failed to setup the volatile process: " + runSetupScriptResult.Exception.ToString());
    }

    /// <summary>
    /// Based on example from https://github.com/dotnet/roslyn/wiki/Scripting-API-Samples#-continue-script-execution-from-a-previous-state
    /// </summary>
    private RunResult RunScript(string script)
    {
        lock (@lock)
        {
            try
            {
                var scriptOptions =
                    ScriptOptions.Default
                    .WithMetadataResolver(metadataResolver);

                scriptState =
                    scriptState == null
                    ?
                    CSharpScript.RunAsync(
                        script,
                        options: scriptOptions).Result
                    :
                    scriptState.ContinueWithAsync(
                        script,
                        options: scriptOptions).Result;
            }
            catch (Exception e)
            {
                string? metadataNotFoundErrorsDetails = null;

                if (e is CompilationErrorException compilationErrorException)
                {
                    var metadataNotFoundErrors =
                        compilationErrorException.Diagnostics
                        .Where(d => d.Severity == DiagnosticSeverity.Error && d.Id == "CS0006")
                        .ToImmutableList();

                    if (metadataNotFoundErrors.Any())
                    {
                        var metadataNotFoundErrorsReports =
                            metadataNotFoundErrors
                            .Select(error =>
                            {
                                var referencesCandidates =
                                    error.GetMessage().Split('\'').Where(c => !c.Contains(' ')).ToImmutableList();

                                var referencesCandidatesDetails =
                                    referencesCandidates
                                    .Select(referenceCandidate =>
                                    {
                                        var referenceResolution =
                                            metadataResolver.ResolutionsFromAssemblyReference(referenceCandidate)?.ToImmutableList() ??
                                            ImmutableList<(string url, IEnumerable<Composition.TreeWithStringPath>? loadedTrees)>.Empty;

                                        return string.Join("\n",
                                            "Found " + referenceResolution.Count + " hint URLS for reference " + referenceCandidate + ":",
                                            DescribeAssemblyResolutionForErrorMessage(referenceResolution));

                                    }).ToImmutableList();

                                return string.Join("\n",
                                    "Details on error regarding reference resolution:",
                                    error.GetMessage(),
                                    "Found " + referencesCandidates.Count + " candidate(s) for references: " + string.Join(", ", referencesCandidates),
                                    string.Join("\n", referencesCandidatesDetails));
                            });

                        metadataNotFoundErrorsDetails = string.Join("\n", metadataNotFoundErrorsReports);
                    }
                }

                var detailsComposition =
                    string.Join("\n",
                    new[] { (title: nameof(metadataNotFoundErrorsDetails), detailsString: metadataNotFoundErrorsDetails) }
                    .Where(namedDetail => namedDetail.detailsString != null)
                    .Select(namedDetail => namedDetail.title + ":\n" + namedDetail.detailsString));

                return new RunResult
                {
                    Exception = new Exception(detailsComposition, e)
                };
            }

            return new RunResult
            {
                Exception = scriptState.Exception,
                ReturnValue = scriptState.ReturnValue,
            };
        }
    }

    static string DescribeAssemblyResolutionForErrorMessage(
        IEnumerable<(string url, IEnumerable<Composition.TreeWithStringPath>? loadedTrees)> resolution)
    {
        return string.Join("\n",
            resolution.Select(urlAndLoadedTrees =>
                {
                    var loadedTrees =
                        urlAndLoadedTrees.loadedTrees?.ToImmutableList() ?? ImmutableList<Composition.TreeWithStringPath>.Empty;

                    return
                        string.Join("\n",
                        "Found " + loadedTrees.Count + " trees under URL " + urlAndLoadedTrees.url + ":",
                        string.Join("\n", loadedTrees.Select(DescribeTreeContentsForErrorMessage)));
                }));
    }

    static string DescribeTreeContentsForErrorMessage(Composition.TreeWithStringPath tree)
    {
        return string.Join("\n",
            tree.EnumerateBlobsTransitive().Select(blobAtPath =>
            "Found " +
            CommonConversion.StringBase16FromByteArray(SHA256.HashData(blobAtPath.blobContent.Span)) +
                " at " + string.Join("/", blobAtPath.path)));
    }

    public RunResult ProcessRequest(string request)
    {
        return RunScript(RequestExpression(request));
    }

    static string RequestExpression(string request)
    {
        return "InterfaceToHost_Request(\"" + request.Replace(@"\", @"\\").Replace("\"", "\\\"") + "\")";
    }

    class MetadataResolver : MetadataReferenceResolver
    {
        readonly object @lock = new();

        readonly SyntaxTree csharpScriptCodeSyntaxTree;

        readonly Func<byte[], byte[]?> getFileFromHashSHA256;

        ImmutableList<AssemblyMetadata> resolvedAssemblies = ImmutableList<AssemblyMetadata>.Empty;

        static readonly ConcurrentBag<(AssemblyMetadata metadata, byte[] assembly)> globalResolvedAssemblies = new();

        static readonly ConcurrentDictionary<string, Assembly> appdomainResolvedAssemblies = new();

        readonly ConcurrentDictionary<string, IEnumerable<string>> hintUrlsFromAssemblyReference = new();

        readonly ConcurrentDictionary<string, IEnumerable<Composition.TreeWithStringPath>> loadedTreesFromUrl = new();

        static readonly ConcurrentDictionary<ResolveReferenceRequest, ImmutableArray<PortableExecutableReference>> resolveReferenceCache =
            new(new ResolveReferenceRequestEqualityComparer());

        public IEnumerable<(string url, IEnumerable<Composition.TreeWithStringPath>? loadedTrees)>? ResolutionsFromAssemblyReference(string assemblyReference)
        {
            hintUrlsFromAssemblyReference.TryGetValue(assemblyReference, out var hintUrls);

            if (hintUrls == null)
                yield break;

            foreach (var hintUrl in hintUrls)
            {
                loadedTreesFromUrl.TryGetValue(hintUrl, out var loadedTrees);

                yield return (hintUrl, loadedTrees);
            }
        }

        public MetadataResolver(
            string csharpScriptCode,
            // TODO: Make getFileFromHashSHA256 optional after migration phase.
            Func<byte[], byte[]?> getFileFromHashSHA256)
        {
            csharpScriptCodeSyntaxTree = CSharpScript.Create(csharpScriptCode).GetCompilation().SyntaxTrees.First();

            this.getFileFromHashSHA256 = getFileFromHashSHA256;
        }

        static MetadataResolver()
        {
            //  TODO: Explore: Can we avoid this by adding '#r' directives to the later script submissions?
            AppDomain.CurrentDomain.AssemblyResolve += CurrentDomain_AssemblyResolve;
        }

        static private Assembly? CurrentDomain_AssemblyResolve(object? sender, ResolveEventArgs args)
        {
            /*
             * Value observed for args.Name:
             * "BotEngine.Interface, Version=1606.3019.0.0, Culture=neutral, PublicKeyToken=null"
             * */
            var assemblySimpleName = Regex.Match(args.Name, "([^,]+)").Value;

            appdomainResolvedAssemblies.TryGetValue(assemblySimpleName, out var alreadyResolved);

            if (alreadyResolved != null)
                return alreadyResolved;

            foreach (var resolvedAssembly in globalResolvedAssemblies)
            {
                foreach (var module in resolvedAssembly.metadata.GetModules())
                {
                    /*
                     * Value observed for module.Name:
                     * "BotEngine.Interface.dll"
                     * */

                    if (new[] { assemblySimpleName + ".dll", assemblySimpleName + ".exe" }
                        .Any(expectedModuleName => string.Equals(expectedModuleName, module.Name, StringComparison.InvariantCultureIgnoreCase)))
                    {
                        var assembly = Assembly.Load(resolvedAssembly.assembly);

                        appdomainResolvedAssemblies[assemblySimpleName] = assembly;

                        return assembly;
                    }
                }
            }

            return null;
        }

        public override bool Equals(object? other) => this == other;

        public override int GetHashCode() => 0;


        record struct ResolveReferenceRequest
        {
            public string reference;
            public string? baseFilePath;
            public MetadataReferenceProperties properties;
        }

        class ResolveReferenceRequestEqualityComparer : IEqualityComparer<ResolveReferenceRequest>
        {
            public bool Equals(ResolveReferenceRequest x, ResolveReferenceRequest y) =>
                x.reference == y.reference &&
                x.baseFilePath == y.baseFilePath &&
                x.properties.Equals(y.properties);

            public int GetHashCode(ResolveReferenceRequest obj) => 0;
        }

        public override ImmutableArray<PortableExecutableReference> ResolveReference(
            string reference,
            string? baseFilePath,
            MetadataReferenceProperties properties)
        {
            //  Implement cache to avoid more memory usage (https://github.com/dotnet/roslyn/issues/33304)

            var request = new ResolveReferenceRequest
            {
                reference = reference,
                baseFilePath = baseFilePath,
                properties = properties
            };

            if (resolveReferenceCache.TryGetValue(request, out var resolvedReferences))
                return resolvedReferences;

            resolvedReferences = ResolveReferenceWithoutAssemblyCache(request);

            resolveReferenceCache[request] = resolvedReferences;

            return resolvedReferences;
        }

        ImmutableArray<PortableExecutableReference> ResolveReferenceWithoutAssemblyCache(ResolveReferenceRequest request)
        {
            lock (@lock)
            {
                //  System.Console.WriteLine("ResolveReference: " + reference);

                var sha256Match = Regex.Match(request.reference, "sha256:([\\d\\w]+)", RegexOptions.IgnoreCase);

                if (sha256Match.Success)
                {
                    var hash = CommonConversion.ByteArrayFromStringBase16(sha256Match.Groups[1].Value);

                    var hintUrls =
                        hintUrlsFromAssemblyReference.GetOrAdd(
                            request.reference,
                            ParseHintUrlsFromAssemblyReference);

                    var assemblyFromCacheOrLink =
                        BlobLibrary.GetBlobWithSHA256Cached(
                        hash,
                        getIfNotCached: () =>
                        {
                            if (hintUrls == null)
                                return null;

                            return GetBlobFromHashAndHintUrls(hash, hintUrls)?.ToArray();
                        });

                    var assembly = assemblyFromCacheOrLink?.ToArray() ?? getFileFromHashSHA256?.Invoke(hash);

                    if (assembly == null)
                        return new ImmutableArray<PortableExecutableReference>();

                    if (!Composition.GetHash(Composition.Component.Blob(assembly)).Span.SequenceEqual(hash) &&
                        !CommonConversion.HashSHA256(assembly).Span.SequenceEqual(hash))
                        return new ImmutableArray<PortableExecutableReference>();

                    var assemblyMetadata = AssemblyMetadata.CreateFromImage(assembly);

                    resolvedAssemblies = resolvedAssemblies.Add(assemblyMetadata);

                    globalResolvedAssemblies.Add((assemblyMetadata, assembly));

                    return ImmutableArray.Create(MetadataReference.CreateFromImage(assembly));
                }
            }

            return
                ScriptMetadataResolver.Default.ResolveReference(
                    request.reference,
                    request.baseFilePath,
                    request.properties);
        }

        IEnumerable<string> ParseHintUrlsFromAssemblyReference(string assemblyReference)
        {
            var allNodesAndTokens =
                csharpScriptCodeSyntaxTree.GetRoot()
                .DescendantNodesAndTokens(descendIntoChildren: _ => true, descendIntoTrivia: true)
                .ToImmutableList();

            var allTrivia =
                csharpScriptCodeSyntaxTree.GetRoot()
                .DescendantTrivia(descendIntoChildren: _ => true, descendIntoTrivia: true)
                .ToImmutableList();

            var allCommentsByLocation =
                allTrivia.Where(IsComment).OrderBy(trivia => trivia.SpanStart).ToImmutableList();

            var tokensContainingReference =
                allNodesAndTokens
                .Where(nodeOrToken => nodeOrToken.IsToken && nodeOrToken.ToString().Contains(assemblyReference))
                .ToImmutableList();

            var commentsBeforeReference =
                tokensContainingReference
                .SelectMany(tokenContainingRef =>
                    allCommentsByLocation
                    .Where(c => c.SpanStart < tokenContainingRef.SpanStart)
                    .TakeLast(1)).ToImmutableList();

            var linksFromComments =
                commentsBeforeReference
                .SelectMany(comment => EnumerateUrlsFromText(comment.ToFullString()))
                .ToImmutableList();

            return linksFromComments.Select(url => url.ToString());
        }

        IReadOnlyList<byte>? GetBlobFromHashAndHintUrls(byte[] hash, IEnumerable<string> hintUrls)
        {
            foreach (var hintUrl in hintUrls)
            {
                var hintUrlTrees =
                    loadedTreesFromUrl.GetOrAdd(
                        hintUrl,
                        hintUrl =>
                        {
                            try
                            {
                                return BlobLibrary.DownloadFromUrlAndExtractTrees(hintUrl).ToImmutableList();
                            }
                            catch { }

                            return ImmutableList<Composition.TreeWithStringPath>.Empty;
                        });

                if (hintUrlTrees == null)
                    continue;

                foreach (var tree in hintUrlTrees)
                {
                    var matchingBlob =
                        tree.EnumerateBlobsTransitive()
                        .Select(blobWithPath => blobWithPath.blobContent)
                        .Where(BlobLibrary.BlobHasSHA256(hash))
                        .Cast<ReadOnlyMemory<byte>?>()
                        .FirstOrDefault();

                    if (matchingBlob != null)
                        return matchingBlob.Value.ToArray();
                }
            }

            return null;
        }
    }

    static bool IsComment(SyntaxTrivia trivia) =>
        trivia.IsKind(Microsoft.CodeAnalysis.CSharp.SyntaxKind.SingleLineCommentTrivia) ||
        trivia.IsKind(Microsoft.CodeAnalysis.CSharp.SyntaxKind.MultiLineCommentTrivia);

    static public IEnumerable<Uri> EnumerateUrlsFromText(string text)
    {
        foreach (var nonSpace in Regex.Split(text, "\\s+"))
        {
            if (Uri.TryCreate(nonSpace, UriKind.Absolute, out var uri))
            {
                yield return uri;
            }
        }
    }

    public record RunResult(object? ReturnValue = null, Exception? Exception = null);
}
