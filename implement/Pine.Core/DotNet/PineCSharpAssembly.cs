using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Emit;
using Microsoft.CodeAnalysis.Text;
using Pine.Core.Functional;
using Pine.Core.Internal;
using Pine.Core.Runtime;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.Loader;
using System.Text;
using System.Threading;

namespace Pine.Core.DotNet;

/// <summary>
/// A collectible generated assembly. Dispose only after its delegates are no longer in use.
/// </summary>
public sealed class PineCSharpAssembly : IDisposable
{
    private readonly AssemblyLoadContext _context;

    private readonly Func<PineValueInProcess, CancellationToken, PineValueInProcess?> _evaluate;

    private readonly PineCSharpCompilation _compilation;

    /// <summary>The generated assembly, including public spread worker methods.</summary>
    public Assembly Assembly { get; }

    private PineCSharpAssembly(
        AssemblyLoadContext context,
        Assembly assembly,
        Func<PineValueInProcess, CancellationToken, PineValueInProcess?> evaluate,
        PineCSharpCompilation compilation)
    {
        _context = context;
        Assembly = assembly;
        _evaluate = evaluate;
        _compilation = compilation;
    }

    /// <summary>
    /// Compiles both source modules and loads them without writing an assembly to disk.
    /// Roslyn diagnostics are returned on failure; execution never silently falls back.
    /// </summary>
    public static Result<IReadOnlyList<Diagnostic>, PineCSharpAssembly> Load(PineCSharpCompilation compilation)
    {
        var platformPaths =
            (AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") as string ??
            throw new InvalidOperationException("Missing trusted platform assemblies."))
            .Split(Path.PathSeparator);

        var references =
            platformPaths.Append(typeof(PineValueInProcess).Assembly.Location).Distinct()
            .Select(path => MetadataReference.CreateFromFile(path));

        var options =
            new CSharpCompilationOptions(
                OutputKind.DynamicallyLinkedLibrary,
                optimizationLevel: OptimizationLevel.Release,
                nullableContextOptions: NullableContextOptions.Enable);

        var syntaxOptions = new CSharpParseOptions(LanguageVersion.Latest);

        var source =
            CSharpCompilation.Create(
                "PineGenerated_" + Guid.NewGuid().ToString("N"),
                [
                    CSharpSyntaxTree.ParseText(
                        SourceText.From(compilation.MethodsSource, Encoding.UTF8),
                        syntaxOptions,
                        "Methods.g.cs"),
                    CSharpSyntaxTree.ParseText(
                        SourceText.From(compilation.LiteralsSource, Encoding.UTF8),
                        syntaxOptions,
                        "Literals.g.cs")
                ],
                references,
                options);

        using var stream = new MemoryStream();
        using var symbols = new MemoryStream();

        var emitted =
            source.Emit(
                stream,
                symbols,
                options: new EmitOptions(debugInformationFormat: DebugInformationFormat.PortablePdb),
                embeddedTexts: source.SyntaxTrees.Select(tree => EmbeddedText.FromSource(tree.FilePath, tree.GetText())));

        if (!emitted.Success)
            return Result<IReadOnlyList<Diagnostic>, PineCSharpAssembly>.err(emitted.Diagnostics);

        stream.Position = 0;
        symbols.Position = 0;
        var context = new AssemblyLoadContext(source.AssemblyName, isCollectible: true);

        try
        {
            var assembly = context.LoadFromStream(stream, symbols);

            var method =
                assembly.GetType(compilation.Options.MethodsTypeName, throwOnError: true)!
                .GetMethod(compilation.DispatchMethodName, BindingFlags.Public | BindingFlags.Static)!;

            Func<PineValueInProcess, CancellationToken, PineValueInProcess?> evaluate;

            if (compilation.Options.EnableCancellation)
                evaluate = method.CreateDelegate<Func<PineValueInProcess, CancellationToken, PineValueInProcess?>>();

            else
            {
                var withoutToken = method.CreateDelegate<Func<PineValueInProcess, PineValueInProcess?>>();
                evaluate = (environment, _) => withoutToken(environment);
            }

            return Result<IReadOnlyList<Diagnostic>, PineCSharpAssembly>.ok(new(context, assembly, evaluate, compilation));
        }
        catch
        {
            context.Unload();
            throw;
        }
    }

    /// <summary>Invokes the named, guarded canonical dispatcher with the caller's cancellation token.</summary>
    public PineValueInProcess? Evaluate(PineValueInProcess environment, CancellationToken cancellationToken = default) =>
        _evaluate(environment, cancellationToken);

    /// <summary>
    /// Binds a public spread worker once to an exact delegate signature, avoiding canonical
    /// environment/result transport. Keep this assembly alive while the delegate is in use.
    /// </summary>
    public TDelegate CreateWorkerDelegate<TDelegate>(PineCSharpMethod worker) where TDelegate : Delegate
    {
        if (!_compilation.Methods.Contains(worker))
            throw new ArgumentException("The worker does not belong to this compilation.", nameof(worker));

        var method =
            Assembly.GetType(_compilation.Options.MethodsTypeName, throwOnError: true)!
            .GetMethod(worker.Name, BindingFlags.Public | BindingFlags.Static)
            ?? throw new MissingMethodException(_compilation.Options.MethodsTypeName, worker.Name);

        return method.CreateDelegate<TDelegate>();
    }

    /// <summary>
    /// Adapts the generated entry to PineVM's optional precompiled-leaf registry. The closure is
    /// created once, not per invocation. Constraint mismatch returns null to the external selector.
    /// </summary>
    public PrecompiledLeaf CreateLeaf(CancellationToken cancellationToken = default) =>
        environment => _evaluate(environment, cancellationToken);

    /// <inheritdoc/>
    public void Dispose() => _context.Unload();
}
