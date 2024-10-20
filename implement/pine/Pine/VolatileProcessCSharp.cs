using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Scripting;
using Microsoft.CodeAnalysis.Scripting;
using Pine.Core;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Text.RegularExpressions;

namespace Pine;

public class VolatileProcessCSharp : VolatileProcess
{
    private readonly object @lock = new();

    private readonly object? scriptGlobals;

    private ScriptState<object>? scriptState;

    private readonly MetadataResolver metadataResolver;

    private readonly Func<byte[], byte[]?> getFileFromHashSHA256;

    public VolatileProcessCSharp(
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

                    if (!metadataNotFoundErrors.IsEmpty)
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
                                            MetadataResolver.ResolutionsFromAssemblyReference(referenceCandidate)
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

    public RunResult ProcessRequest(string request)
    {
        return RunScript(RequestExpression(request));
    }

    private static string RequestExpression(string request)
    {
        return "InterfaceToHost_Request(\"" + request.Replace(@"\", @"\\").Replace("\"", "\\\"") + "\")";
    }

    private class MetadataResolver(
        string csharpScriptCode,
        // TODO: Make getFileFromHashSHA256 optional after migration phase.
        Func<byte[], byte[]?> getFileFromHashSHA256)
        : MetadataReferenceResolver
    {
        private readonly object @lock = new();

        private readonly SyntaxTree csharpScriptCodeSyntaxTree = CSharpSyntaxTree.ParseText(csharpScriptCode);

        private ImmutableList<AssemblyMetadata> resolvedAssemblies = [];

        private static readonly ConcurrentBag<(AssemblyMetadata metadata, byte[] assembly)> globalResolvedAssemblies = [];

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
                ?.Extract(_ => []) ??
                [];
        }

        public static IEnumerable<AssemblyReferenceResolutionReport> ResolutionsFromAssemblyReference(string assemblyReference)
        {
            foreach (var requestAndReport in resolveReferenceCache)
            {
                if (requestAndReport.Key.reference != assemblyReference)
                    continue;

                yield return requestAndReport.Value;
            }
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
            return
                LoadBlob(loadedTreesFromUrl, getFileFromHashSHA256, hashBase16, hintUrls)
                .Map(a => a.ToArray())
                .Map(assembly =>
                {
                    var assemblyMetadata = AssemblyMetadata.CreateFromImage(assembly);

                    resolvedAssemblies = resolvedAssemblies.Add(assemblyMetadata);

                    globalResolvedAssemblies.Add((assemblyMetadata, assembly));

                    return
                        ImmutableArray.Create(MetadataReference.CreateFromImage(assembly));
                })
                .MapError(err => "Failed to load assembly image: " + err);
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
            byte[] hash, IEnumerable<string> hintUrls) =>
            DependenciesLoader.GetBlobFromHashAndHintUrlsCached(
                loadedTreesFromUrl,
                hash,
                hintUrls);
    }

    public static bool IsComment(SyntaxTrivia trivia) =>
        trivia.IsKind(SyntaxKind.SingleLineCommentTrivia) ||
        trivia.IsKind(SyntaxKind.MultiLineCommentTrivia);

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
