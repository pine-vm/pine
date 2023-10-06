using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Reflection;
using static Pine.CompilePineToDotNet.CompileToCSharp;

namespace Pine.CompilePineToDotNet;

public record CompileToAssemblyResult(
    byte[] Assembly,
    Func<Result<string, IReadOnlyDictionary<PineValue, Func<PineVM.PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>>>
    BuildCompiledExpressionsDictionary);


public class CompileToAssembly
{
    public static Result<string, CompileToAssemblyResult> Compile(
        SyntaxContainerConfig syntaxContainerConfig,
        CompileCSharpClassResult compileCSharpClassResult)
    {
        var syntaxText = GenerateCSharpFile(compileCSharpClassResult, additionalMembers: null).FileText;

        var syntaxTree = CSharpSyntaxTree.ParseText(syntaxText);

        var compilation = CSharpCompilation.Create("assembly-name")
            .WithOptions(new CSharpCompilationOptions(
                OutputKind.DynamicallyLinkedLibrary,
                optimizationLevel: OptimizationLevel.Release)
            .WithAssemblyIdentityComparer(DesktopAssemblyIdentityComparer.Default))
            .WithReferences(MetadataReferences.Value)
            .AddSyntaxTrees(syntaxTree);

        using var codeStream = new MemoryStream();

        var compilationResult = compilation.Emit(codeStream);

        var compilationErrors =
            compilationResult.Diagnostics
            .Where(d => d.Severity == DiagnosticSeverity.Error)
            .ToImmutableList();

        if (!compilationResult.Success && 0 < compilationErrors.Count)
        {
            return Result<string, CompileToAssemblyResult>.err(
                "Compilation failed with " + compilationErrors.Count + " errors:\n" +
                string.Join("\n", compilationErrors.Select(d => d.ToString())));
        }

        var assembly = codeStream.ToArray();

        Result<string, IReadOnlyDictionary<PineValue, Func<PineVM.PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>> buildDictionary()
        {
            var loadedAssembly = Assembly.Load(assembly);

            var compiledType = loadedAssembly.GetType(syntaxContainerConfig.containerTypeName);

            if (compiledType is null)
                return Result<string, IReadOnlyDictionary<PineValue, Func<PineVM.PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>>.err(
                    "Did not find type " + syntaxContainerConfig.containerTypeName + " in assembly " + loadedAssembly.FullName);

            var dictionaryMember =
                compiledType.GetMethod(syntaxContainerConfig.dictionaryMemberName, BindingFlags.Public | BindingFlags.Static);

            if (dictionaryMember is null)
                return Result<string, IReadOnlyDictionary<PineValue, Func<PineVM.PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>>.err(
                    "Did not find method " + syntaxContainerConfig.dictionaryMemberName + " in type " + compiledType.FullName);

            return
                Result<string, IReadOnlyDictionary<PineValue, Func<PineVM.PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>>.ok(
                    (IReadOnlyDictionary<PineValue, Func<PineVM.PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>)
                    dictionaryMember.Invoke(null, null));
        }

        return Result<string, CompileToAssemblyResult>.ok(
            new CompileToAssemblyResult(
                Assembly: assembly,
                BuildCompiledExpressionsDictionary: buildDictionary));
    }


    private static readonly Lazy<IImmutableList<MetadataReference>> MetadataReferences =
        new(() => ListMetadataReferences().ToImmutableList());

    private static IEnumerable<MetadataReference> ListMetadataReferences()
    {
        var types = new[]
        {
            typeof(object),
            typeof(Func<>),
            typeof(BigInteger),
            typeof(IImmutableList<>),
            typeof(PineVM.PineVM)
        };

        var typesAssembliesLocations =
            types
            .Select(t => t.Assembly)
            .Concat(AppDomain.CurrentDomain.GetAssemblies())
            .Select(a => a.Location)
            .Where(loc => 0 < loc?.Length)
            .Distinct()
            .ToImmutableList();

        foreach (var assemblyLocation in typesAssembliesLocations)
        {
            yield return MetadataReference.CreateFromFile(assemblyLocation);
        }
    }
}
