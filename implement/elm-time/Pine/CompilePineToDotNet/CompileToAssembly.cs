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

using CompileDictionaryResult =
    IReadOnlyDictionary<PineValue, Func<PineVM.PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>;

public record CompileToAssemblyResult(
    byte[] Assembly,
    Func<Result<string, CompileDictionaryResult>>
    BuildCompiledExpressionsDictionary);


public class CompileToAssembly
{
    public static Result<string, CompileToAssemblyResult> Compile(
        CompileCSharpClassResult compileCSharpClassResult)
    {
        var csharpFile = GenerateCSharpFile(compileCSharpClassResult, additionalMembers: null);

        return Compile(csharpFile);
    }

    public static Result<string, CompileToAssemblyResult> Compile(
        GenerateCSharpFileResult csharpFile)
    {
        var syntaxText = csharpFile.FileText;

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

        Result<string, CompileDictionaryResult> buildDictionary()
        {
            var loadedAssembly = Assembly.Load(assembly);

            var compiledType = loadedAssembly.GetType(csharpFile.SyntaxContainerConfig.containerTypeName);

            if (compiledType is null)
                return Result<string, CompileDictionaryResult>.err(
                    "Did not find type " + csharpFile.SyntaxContainerConfig.containerTypeName + " in assembly " + loadedAssembly.FullName);

            var dictionaryMember =
                compiledType.GetMethod(csharpFile.SyntaxContainerConfig.dictionaryMemberName, BindingFlags.Public | BindingFlags.Static);

            if (dictionaryMember is null)
                return Result<string, CompileDictionaryResult>.err(
                    "Did not find method " + csharpFile.SyntaxContainerConfig.dictionaryMemberName + " in type " + compiledType.FullName);

            return
                Result<string, CompileDictionaryResult>.ok(
                    (CompileDictionaryResult)
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
