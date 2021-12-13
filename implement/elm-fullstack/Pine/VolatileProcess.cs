using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Scripting;
using Microsoft.CodeAnalysis.Scripting;

namespace Pine;

public class VolatileProcess
{
    readonly object @lock = new();

    ScriptState<object>? scriptState;

    readonly MetadataReferenceResolver metadataResolver;

    readonly Func<byte[], byte[]?> getFileFromHashSHA256;

    public VolatileProcess(
        Func<byte[], byte[]?> getFileFromHashSHA256,
        string csharpScript)
    {
        this.getFileFromHashSHA256 = getFileFromHashSHA256;

        metadataResolver = new MetadataResolver(getFileFromHashSHA256);

        var runSetupScriptResult = RunScript(csharpScript);

        if (runSetupScriptResult.Exception != null)
            throw new Exception("Failed to setup the volatile process:" + runSetupScriptResult.Exception.ToString());
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
                return new RunResult
                {
                    Exception = e,
                };
            }

            return new RunResult
            {
                Exception = scriptState.Exception,
                ReturnValue = scriptState.ReturnValue,
            };
        }
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

        readonly Func<byte[], byte[]?> getFileFromHashSHA256;

        ImmutableList<AssemblyMetadata> resolvedAssemblies = ImmutableList<AssemblyMetadata>.Empty;

        static readonly ConcurrentBag<(AssemblyMetadata metadata, byte[] assembly)> globalResolvedAssemblies = new();

        static readonly ConcurrentDictionary<string, Assembly> appdomainResolvedAssemblies = new();

        static readonly ConcurrentDictionary<ResolveReferenceRequest, ImmutableArray<PortableExecutableReference>> resolveReferenceCache =
            new(new ResolveReferenceRequestEqualityComparer());

        public MetadataResolver(Func<byte[], byte[]?> getFileFromHashSHA256)
        {
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

            resolvedReferences = ResolveReferenceWithoutCache(request);

            resolveReferenceCache[request] = resolvedReferences;

            return resolvedReferences;
        }

        ImmutableArray<PortableExecutableReference> ResolveReferenceWithoutCache(ResolveReferenceRequest request)
        {
            lock (@lock)
            {
                //  System.Console.WriteLine("ResolveReference: " + reference);

                var sha256Match = Regex.Match(request.reference, "sha256:([\\d\\w]+)", RegexOptions.IgnoreCase);

                if (sha256Match.Success)
                {
                    var hash = CommonConversion.ByteArrayFromStringBase16(sha256Match.Groups[1].Value);

                    var assembly = getFileFromHashSHA256?.Invoke(hash);

                    if (assembly == null)
                        return new ImmutableArray<PortableExecutableReference>();

                    if (!Composition.GetHash(Composition.Component.Blob(assembly)).SequenceEqual(hash) &&
                        !CommonConversion.HashSHA256(assembly).SequenceEqual(hash))
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
    }

    public record RunResult(object? ReturnValue = null, Exception? Exception = null);
}
