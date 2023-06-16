using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Scripting;
using Microsoft.CodeAnalysis.Scripting;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Security.Cryptography;
using System.Text.RegularExpressions;

namespace Pine;

public class VolatileProcess
{
    private readonly object @lock = new();

    private readonly object? scriptGlobals;

    private ScriptState<object>? scriptState;

    private readonly MetadataResolver metadataResolver;

    private readonly Func<byte[], byte[]?> getFileFromHashSHA256;

    public VolatileProcess(
        Func<byte[], byte[]?> getFileFromHashSHA256,
        string csharpScriptCode,
        object? scriptGlobals)
    {
        this.getFileFromHashSHA256 = getFileFromHashSHA256;
        this.scriptGlobals = scriptGlobals;

        metadataResolver = new MetadataResolver(csharpScriptCode: csharpScriptCode, getFileFromHashSHA256);

        var runSetupScriptResult = RunScript(csharpScriptCode);

        if (runSetupScriptResult.Exception != null)
            throw new Exception("Failed to setup the volatile process: " + runSetupScriptResult.Exception);
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
                        options: scriptOptions,
                        globals: scriptGlobals,
                        globalsType: scriptGlobals?.GetType()).Result
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
                                    error.GetMessage()
                                    /*
                                     * 2023-04-16 samples:
                                     * 未能找到元数据文件“sha256:9c6dac74d50062c63d9df5e6d5c23a4da6ae64cf1898138cb0b5982de2869ce7”
                                     * 
                                     * Metadata file 'sha256:11111111ac74d50062c63d9df5e6d5c23a4da6ae64cf1898138cb0b5982de286' could not be found
                                     * */
                                    .Split('\'', '“', '”')
                                    .Where(c => !c.Contains(' ') && 0 < c.Trim().Length)
                                    .ToImmutableList();

                                var referencesCandidatesDetails =
                                    referencesCandidates
                                    .Select(referenceCandidate =>
                                    {
                                        var referenceResolutions =
                                            metadataResolver.ResolutionsFromAssemblyReference(referenceCandidate)
                                            .ToImmutableList();

                                        return
                                        string.Join("\n",
                                            "Found " + referenceResolutions.Count + " resolution report(s) for reference " + referenceCandidate + ":\n",
                                            string.Join("\n", referenceResolutions.Select(DescribeAssemblyResolutionForErrorMessage)).Trim('\n'))
                                        .Trim('\n');

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

    private static string DescribeAssemblyResolutionForErrorMessage(
        MetadataResolver.AssemblyReferenceResolutionReport resolution)
    {
        static string composeHashResolutionReport(Result<string, ImmutableArray<PortableExecutableReference>> hashResolutionResult) =>
            hashResolutionResult
            .Unpack(
                fromErr:
                err => "Failed to load: " + err,
                fromOk:
                ok => "Successfully loaded " + ok.Length + " assembly");

        return
            resolution.HashSHA256Base16 switch
            {
                { } hashBase16 =>
                resolution.HashResolution is null ?
                "Missing resolution report for hash: " + hashBase16
                :
                "resolution report for hash " + hashBase16 + ":\n" + composeHashResolutionReport(resolution.HashResolution),

                _ => "No hash reference"
            };
    }

    public static string DescribeBlobOrTreeContentsForErrorMessage(TreeNodeWithStringPath tree) =>
        tree switch
        {
            TreeNodeWithStringPath.BlobNode => "is a blob",

            _ => "is a tree:\n" + DescribeTreeContentsForErrorMessage(tree)
        };

    public static string DescribeTreeContentsForErrorMessage(TreeNodeWithStringPath tree) =>
        string.Join("\n",
            tree.EnumerateBlobsTransitive().Select(blobAtPath =>
            "Found " +
            CommonConversion.StringBase16(SHA256.HashData(blobAtPath.blobContent.Span)) +
            " at " + string.Join("/", blobAtPath.path)));

    public RunResult ProcessRequest(string request)
    {
        return RunScript(RequestExpression(request));
    }

    private static string RequestExpression(string request)
    {
        return "InterfaceToHost_Request(\"" + request.Replace(@"\", @"\\").Replace("\"", "\\\"") + "\")";
    }

    private class MetadataResolver : MetadataReferenceResolver
    {
        private readonly object @lock = new();

        private readonly SyntaxTree csharpScriptCodeSyntaxTree;

        private readonly Func<byte[], byte[]?> getFileFromHashSHA256;

        private ImmutableList<AssemblyMetadata> resolvedAssemblies = ImmutableList<AssemblyMetadata>.Empty;

        private static readonly ConcurrentBag<(AssemblyMetadata metadata, byte[] assembly)> globalResolvedAssemblies = new();

        private static readonly ConcurrentDictionary<string, Assembly> appdomainResolvedAssemblies = new();

        private readonly ConcurrentDictionary<string, IEnumerable<string>> hintUrlsFromAssemblyReference = new();

        private readonly ConcurrentDictionary<string, IEnumerable<TreeNodeWithStringPath>> loadedTreesFromUrl = new();

        private static readonly ConcurrentDictionary<ResolveReferenceRequest, AssemblyReferenceResolutionReport> resolveReferenceCache =
            new(new ResolveReferenceRequestEqualityComparer());

        public record AssemblyReferenceResolutionReport(
            string? HashSHA256Base16,
            Result<string, ImmutableArray<PortableExecutableReference>>? HashResolution)
        {
            public ImmutableArray<PortableExecutableReference> References =>
                HashResolution
                ?.Extract(_ => ImmutableArray<PortableExecutableReference>.Empty) ??
                ImmutableArray<PortableExecutableReference>.Empty;
        }

        /*
        public record AssemblyReferenceHashResolutionReport(
            Result<string, ImmutableArray<PortableExecutableReference>> Result);
        */

        public IEnumerable<AssemblyReferenceResolutionReport> ResolutionsFromAssemblyReference(string assemblyReference)
        {
            foreach (var requestAndReport in resolveReferenceCache)
            {
                if (requestAndReport.Key.reference != assemblyReference)
                    continue;

                yield return requestAndReport.Value;
            }
        }

        public MetadataResolver(
            string csharpScriptCode,
            // TODO: Make getFileFromHashSHA256 optional after migration phase.
            Func<byte[], byte[]?> getFileFromHashSHA256)
        {
            csharpScriptCodeSyntaxTree = CSharpSyntaxTree.ParseText(csharpScriptCode);

            this.getFileFromHashSHA256 = getFileFromHashSHA256;
        }

        static MetadataResolver()
        {
            //  TODO: Explore: Can we avoid this by adding '#r' directives to the later script submissions?
            AppDomain.CurrentDomain.AssemblyResolve += CurrentDomain_AssemblyResolve;
        }

        private static Assembly? CurrentDomain_AssemblyResolve(object? sender, ResolveEventArgs args)
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


        private record struct ResolveReferenceRequest
        {
            public string reference;
            public string? baseFilePath;
            public MetadataReferenceProperties properties;
        }

        private class ResolveReferenceRequestEqualityComparer : IEqualityComparer<ResolveReferenceRequest>
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

            var resolutionReport =
                resolveReferenceCache
                .AddOrUpdate(
                    request,
                    addValueFactory: ResolveReferenceWithoutCache,
                    updateValueFactory: (_, previousReport) => previousReport);

            var fromDefaultResolver =
                ScriptMetadataResolver.Default.ResolveReference(
                    request.reference,
                    request.baseFilePath,
                    request.properties);

            return resolutionReport.References.AddRange(fromDefaultResolver);
        }

        private AssemblyReferenceResolutionReport ResolveReferenceWithoutCache(ResolveReferenceRequest request)
        {
            lock (@lock)
            {
                //  System.Console.WriteLine("ResolveReference: " + reference);

                var sha256Match = Regex.Match(request.reference, "sha256:([\\d\\w]+)", RegexOptions.IgnoreCase);

                if (!sha256Match.Success)
                    return new AssemblyReferenceResolutionReport(
                        HashSHA256Base16: null,
                        HashResolution: null);

                var hintUrls =
                    hintUrlsFromAssemblyReference.GetOrAdd(
                        request.reference,
                        ParseHintUrlsFromAssemblyReference);

                var hashBase16 = sha256Match.Groups[1].Value;

                return
                    new AssemblyReferenceResolutionReport(
                        HashSHA256Base16: hashBase16,
                        ResolveHashReferenceWithoutCache(hashBase16, hintUrls));
            }
        }

        private Result<string, ImmutableArray<PortableExecutableReference>> ResolveHashReferenceWithoutCache(
            string hashBase16,
            IEnumerable<string> hintUrls)
        {
            var hash = CommonConversion.ByteArrayFromStringBase16(hashBase16);

            IReadOnlyDictionary<string, string>? errorFromHintUrl = null;

            var assemblyFromCacheOrLink =
                BlobLibrary.GetBlobWithSHA256Cached(
                hash,
                getIfNotCached: () =>
                {
                    if (hintUrls is null)
                        return null;

                    return
                    GetBlobFromHashAndHintUrlsCached(hash, hintUrls)
                    .Unpack(
                        fromErr: err =>
                        {
                            errorFromHintUrl = err;

                            return null;
                        },
                        fromOk: ok => ok);
                });

            Result<string, ImmutableArray<PortableExecutableReference>> returnError(string? error)
            {
                var errorFromDictionary =
                    errorFromHintUrl is null ? null
                    :
                    "Failed loading from " + errorFromHintUrl.Count + " hint URL(s):\n" +
                    string.Join("\n", errorFromHintUrl.Select(hintUrlAndError => hintUrlAndError.Key + ": " + hintUrlAndError.Value));

                return
                    Result<string, ImmutableArray<PortableExecutableReference>>.err(
                        string.Join("\n", new[] { error, errorFromDictionary }.WhereNotNull()));
            }

            var assembly = assemblyFromCacheOrLink?.ToArray() ?? getFileFromHashSHA256?.Invoke(hash);

            if (assembly is null)
                return returnError("Did not find assembly image");

            if (!PineValueHashTree.ComputeHash(PineValue.Blob(assembly)).Span.SequenceEqual(hash) &&
                !CommonConversion.HashSHA256(assembly).Span.SequenceEqual(hash))
                return returnError("Selected assembly image hash does not match " + hashBase16);

            var assemblyMetadata = AssemblyMetadata.CreateFromImage(assembly);

            resolvedAssemblies = resolvedAssemblies.Add(assemblyMetadata);

            globalResolvedAssemblies.Add((assemblyMetadata, assembly));

            return
                Result<string, ImmutableArray<PortableExecutableReference>>.ok(
                    ImmutableArray.Create(MetadataReference.CreateFromImage(assembly)));
        }

        public IEnumerable<string> ParseHintUrlsFromAssemblyReference(string assemblyReference) =>
            ParseHintUrlsFromAssemblyReference(csharpScriptCodeSyntaxTree, assemblyReference);

        public static IEnumerable<string> ParseHintUrlsFromAssemblyReference(
            SyntaxTree syntaxTree,
            string assemblyReference)
        {
            var allNodesAndTokens =
                syntaxTree.GetRoot()
                .DescendantNodesAndTokens(descendIntoChildren: _ => true, descendIntoTrivia: true)
                .ToImmutableList();

            var allTrivia =
                syntaxTree.GetRoot()
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

        private Result<IReadOnlyDictionary<string, string>, ReadOnlyMemory<byte>> GetBlobFromHashAndHintUrlsCached(
            byte[] hash, IEnumerable<string> hintUrls)
        {
            Result<string, ReadOnlyMemory<byte>> AttemptForUrl(string url)
            {
                if (!loadedTreesFromUrl.TryGetValue(url, out var treesFromUrl))
                {
                    try
                    {
                        treesFromUrl =
                        loadedTreesFromUrl.GetOrAdd(
                            url,
                            url => BlobLibrary.DownloadFromUrlAndExtractTrees(url).ToImmutableList());
                    }
                    catch (Exception e)
                    {
                        Result<string, ReadOnlyMemory<byte>>.err("Loading from URL failed with runtime exception: " + e);
                    }
                }

                if (treesFromUrl is null || !treesFromUrl.Any())
                    return Result<string, ReadOnlyMemory<byte>>.err("Found no trees at that URL");

                var searchTreesResult =
                    treesFromUrl
                    .Aggregate(
                        seed:
                        Result<ImmutableList<TreeNodeWithStringPath>, ReadOnlyMemory<byte>>.err(ImmutableList<TreeNodeWithStringPath>.Empty),
                        func:
                        (aggregate, tree) =>
                        {
                            return
                            aggregate
                            .Unpack(
                                fromErr:
                                err =>
                                {
                                    var matchingBlob =
                                        tree.EnumerateBlobsTransitive()
                                        .Select(blobWithPath => blobWithPath.blobContent)
                                        .Where(BlobLibrary.BlobHasSHA256(hash))
                                        .Cast<ReadOnlyMemory<byte>?>()
                                        .FirstOrDefault();

                                    if (matchingBlob != null)
                                    {
                                        return Result<ImmutableList<TreeNodeWithStringPath>, ReadOnlyMemory<byte>>.ok(matchingBlob.Value);
                                    }

                                    return
                                    Result<ImmutableList<TreeNodeWithStringPath>, ReadOnlyMemory<byte>>.err(err.Add(tree));
                                },
                                fromOk:
                                ok => Result<ImmutableList<TreeNodeWithStringPath>, ReadOnlyMemory<byte>>.ok(ok));
                        });

                return
                    searchTreesResult
                    .MapError(
                        searchedTrees =>
                        "Searched " + searchedTrees.Count + " tree nodes but none of those contained a matching blob:\n" +
                        string.Join("\n",
                        searchedTrees.Select((tree, treeIndex) => "Node " + treeIndex + " " +
                        CommonConversion.StringBase16(PineValueHashTree.ComputeHashNotSorted(tree)) + " " +
                        DescribeBlobOrTreeContentsForErrorMessage(tree))).Trim('\n'));
            }

            return
                hintUrls
                .Aggregate(
                    seed: Result<ImmutableDictionary<string, string>, ReadOnlyMemory<byte>>.err(ImmutableDictionary<string, string>.Empty),
                    func: (aggregate, hintUrl) =>
                    {
                        return
                        aggregate.Unpack(
                            fromErr:
                            errorFromHintUrl =>
                            AttemptForUrl(hintUrl).MapError(err => errorFromHintUrl.SetItem(hintUrl, err)),

                            fromOk:
                            ok => Result<ImmutableDictionary<string, string>, ReadOnlyMemory<byte>>.ok(ok));
                    })
                .MapError(dict => (IReadOnlyDictionary<string, string>)dict);
        }
    }

    public static bool IsComment(SyntaxTrivia trivia) =>
        trivia.IsKind(Microsoft.CodeAnalysis.CSharp.SyntaxKind.SingleLineCommentTrivia) ||
        trivia.IsKind(Microsoft.CodeAnalysis.CSharp.SyntaxKind.MultiLineCommentTrivia);

    public static IEnumerable<Uri> EnumerateUrlsFromText(string text)
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
