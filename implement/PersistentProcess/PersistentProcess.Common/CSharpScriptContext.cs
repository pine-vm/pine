using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Scripting;
using Microsoft.CodeAnalysis.Scripting;
using System;
using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Text.RegularExpressions;

namespace Kalmit
{
    public class CSharpScriptContext
    {
        readonly object @lock = new object();

        ScriptState<object> scriptState;

        Func<byte[], byte[]> getFileFromHashSHA256;

        public CSharpScriptContext(Func<byte[], byte[]> getFileFromHashSHA256)
        {
            this.getFileFromHashSHA256 = getFileFromHashSHA256;
        }

        /// <summary>
        /// Based on example from https://github.com/dotnet/roslyn/wiki/Scripting-API-Samples#-continue-script-execution-from-a-previous-state
        /// </summary>
        public RunResult RunScript(string script)
        {
            lock (@lock)
            {
                try
                {
                    var scriptOptions =
                        ScriptOptions.Default
                        .WithMetadataResolver(new MetadataResolver(getFileFromHashSHA256));

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

        class MetadataResolver : MetadataReferenceResolver
        {
            readonly object @lock = new object();

            readonly Func<byte[], byte[]> getFileFromHashSHA256;

            ImmutableList<AssemblyMetadata> resolvedAssemblies = ImmutableList<AssemblyMetadata>.Empty;

            static readonly ConcurrentBag<(AssemblyMetadata metadata, byte[] assembly)> globalResolvedAssemblies =
                new ConcurrentBag<(AssemblyMetadata, byte[])>();

            static readonly ConcurrentDictionary<string, Assembly> appdomainResolvedAssemblies = new ConcurrentDictionary<string, Assembly>();

            public MetadataResolver(Func<byte[], byte[]> getFileFromHashSHA256)
            {
                this.getFileFromHashSHA256 = getFileFromHashSHA256;
            }

            static MetadataResolver()
            {
                //  TODO: Explore: Can we avoid this by adding '#r' directives to the later script submissions?
                AppDomain.CurrentDomain.AssemblyResolve += CurrentDomain_AssemblyResolve;
            }

            static private Assembly CurrentDomain_AssemblyResolve(object sender, ResolveEventArgs args)
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

            public override bool Equals(object other) => false;

            public override int GetHashCode() => 0;

            public override ImmutableArray<PortableExecutableReference> ResolveReference(string reference, string baseFilePath, MetadataReferenceProperties properties)
            {
                lock (@lock)
                {
                    //  System.Console.WriteLine("ResolveReference: " + reference);

                    var sha256Match = Regex.Match(reference, "sha256:([\\d\\w]+)", RegexOptions.IgnoreCase);

                    if (sha256Match.Success)
                    {
                        var hash = Kalmit.CommonConversion.ByteArrayFromStringBase16(sha256Match.Groups[1].Value);

                        var assembly = getFileFromHashSHA256?.Invoke(hash);

                        if (assembly == null)
                            return new ImmutableArray<PortableExecutableReference>();

                        if (!Kalmit.CommonConversion.HashSHA256(assembly).SequenceEqual(hash))
                            return new ImmutableArray<PortableExecutableReference>();

                        var assemblyMetadata = AssemblyMetadata.CreateFromImage(assembly);

                        resolvedAssemblies = resolvedAssemblies.Add(assemblyMetadata);

                        globalResolvedAssemblies.Add((assemblyMetadata, assembly));

                        return ImmutableArray.Create(MetadataReference.CreateFromImage(assembly));
                    }
                }

                return
                    ScriptMetadataResolver.Default.ResolveReference(
                            reference,
                            baseFilePath,
                            properties);
            }
        }

        public class RunResult
        {
            public object ReturnValue;

            public Exception Exception;
        }
    }
}