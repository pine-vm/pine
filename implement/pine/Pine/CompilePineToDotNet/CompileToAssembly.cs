using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Pine.Core;
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
    IReadOnlyDictionary<PineValue, Func<EvalExprDelegate, PineValue, Result<string, PineValue>>>;

public record CompileToAssemblyResult(
    GenerateCSharpFileResult GenerateCSharpFileResult,
    byte[] Assembly,
    Func<Result<string, CompileDictionaryResult>> BuildCompiledExpressionsDictionary);


public class CompileToAssembly
{
    public static Result<string, CompileToAssemblyResult> Compile(
        CompileCSharpClassResult compileCSharpClassResult) =>
        Compile(
            compileCSharpClassResult,
            OptimizationLevel.Release);

    public static Result<string, CompileToAssemblyResult> Compile(
        CompileCSharpClassResult compileCSharpClassResult,
        OptimizationLevel optimizationLevel)
    {
        var csharpFile = GenerateCSharpFile(compileCSharpClassResult, additionalMembers: null);

        /*
         * 2023-12-01:
         * We frequently observed the compilation to .NET CIL assembly taking more than ten minutes, with C# file lengths between 1 and 3 MB and resulting assembly sizes between 0.5 and 1 MB.
         * (TODO: Search for more efficient representations in terms of C# text. This will also help reduce expenses for formatting, which now takes more than a second)
         * 
         * Using the OptimizationLevel.Debug did not seem to improve the compilation time a lot.
         * */

        return Compile(csharpFile, optimizationLevel);
    }

    public static Result<string, CompileToAssemblyResult> Compile(
        GenerateCSharpFileResult csharpFile,
        OptimizationLevel optimizationLevel)
    {
        var syntaxText = csharpFile.FileText;

        var syntaxTree = CSharpSyntaxTree.ParseText(syntaxText);

        var compilation = CSharpCompilation.Create("assembly-name")
            .WithOptions(new CSharpCompilationOptions(
                OutputKind.DynamicallyLinkedLibrary,
                optimizationLevel: optimizationLevel)
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

            var compiledType =
                loadedAssembly.GetType(csharpFile.SyntaxContainerConfig.ContainerTypeName);

            if (compiledType is null)
                return
                    "Did not find type " + csharpFile.SyntaxContainerConfig.ContainerTypeName +
                    " in assembly " + loadedAssembly.FullName;

            var dictionaryMember =
                compiledType.GetMethod(
                    csharpFile.SyntaxContainerConfig.DictionaryMemberName,
                    BindingFlags.Public | BindingFlags.Static);

            if (dictionaryMember is null)
                return
                    "Did not find method " + csharpFile.SyntaxContainerConfig.DictionaryMemberName +
                    " in type " + compiledType.FullName;

            var originalDictionary =
                (IReadOnlyDictionary<PineValue, Func<EvalExprDelegate, PineValue, PineValue>>)
                dictionaryMember.Invoke(null, null)!;

            var dictionaryWithEvalReturnTypeResult =
                originalDictionary
                .ToImmutableDictionary(
                    keySelector: kv => kv.Key,
                    elementSelector: kv => new Func<EvalExprDelegate, PineValue, Result<string, PineValue>>(
                        (evalDelegate, envValue) => kv.Value(evalDelegate, envValue)));

            return dictionaryWithEvalReturnTypeResult;
        }

        return Result<string, CompileToAssemblyResult>.ok(
            new CompileToAssemblyResult(
                csharpFile,
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
            typeof(Core.Interpreter.IntermediateVM.PineVM)
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
