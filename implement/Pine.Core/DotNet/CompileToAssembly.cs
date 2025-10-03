using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Reflection;

namespace Pine.Core.DotNet;

using CompiledDictionary =
    IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>>;

public record CompileToAssemblyResult(
    byte[] Assembly,
    Func<CompiledDictionary> BuildCompiledExpressionsDictionary);


public class CompileToAssembly
{
    private static readonly CSharpParseOptions s_parseOptions =
        new(languageVersion: LanguageVersion.CSharp13);

    public static Result<string, CompileToAssemblyResult> Compile(
        StaticProgramCSharp staticProgram,
        IReadOnlyList<string> namespacePrefix,
        OptimizationLevel optimizationLevel)
    {
        var csharpFiles =
            staticProgram.BuildCSharpProjectFiles(namespacePrefix);

        return Compile(csharpFiles, optimizationLevel);
    }

    public static Result<string, CompileToAssemblyResult> Compile(
        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> csharpFiles,
        OptimizationLevel optimizationLevel)
    {
        SyntaxTree SyntaxTreeFromFile(
            KeyValuePair<IReadOnlyList<string>, ReadOnlyMemory<byte>> file) =>
            CSharpSyntaxTree.ParseText(
                Microsoft.CodeAnalysis.Text.SourceText.From(System.Text.Encoding.UTF8.GetString(file.Value.Span)),
                options: s_parseOptions)
            .WithFilePath(string.Join('/', file.Key));

        IReadOnlyList<SyntaxTree> syntaxTrees =
            [.. csharpFiles.Select(SyntaxTreeFromFile)
            ];

        var compilation =
            CSharpCompilation.Create("assembly-name")
            .WithOptions(
                new CSharpCompilationOptions(
                    OutputKind.DynamicallyLinkedLibrary,
                    optimizationLevel: optimizationLevel)
            .WithAssemblyIdentityComparer(DesktopAssemblyIdentityComparer.Default))
            .WithReferences(s_metadataReferences.Value)
            .AddSyntaxTrees(syntaxTrees);

        using var codeStream = new MemoryStream();

        var compilationResult = compilation.Emit(codeStream);

        var compilationErrors =
            compilationResult.Diagnostics
            .Where(d => d.Severity is DiagnosticSeverity.Error)
            .ToImmutableList();

        if (!compilationResult.Success)
        {
            return
                "Compilation failed with " + compilationErrors.Count + " errors:\n" +
                string.Join("\n", compilationErrors.Select(d => d.ToString()));
        }

        var assembly = codeStream.ToArray();

        var searchResult =
            SearchDictionaryBuilderInAssembly(assembly);

        {
            if (searchResult.IsErrOrNull() is { } err)
            {
                return
                    "Failed to find dictionary builder in compiled assembly: " + err;
            }
        }

        if (searchResult.IsOkOrNull() is not { } buildDictionary)
        {
            throw new NotImplementedException(
                "Unexpected return type: " + searchResult.GetType().FullName);
        }

        return
            new CompileToAssemblyResult(
                Assembly: assembly,
                BuildCompiledExpressionsDictionary: buildDictionary);
    }

    public static Result<string, Func<CompiledDictionary>> SearchDictionaryBuilderInAssembly(
        byte[] assemblyBytes)
    {
        var loadedAssembly = Assembly.Load(assemblyBytes);

        var candidateContainerTypes =
            loadedAssembly.GetTypes()
            .Where(t => t.IsClass && t.IsPublic && t.Name.Contains("Dispatch"))
            .ToImmutableList();

        var allMatches =
            candidateContainerTypes
            .Select(containerType => (containerType, builderMethod: SearchTypeForDispatcherDictionary(containerType)))
            .Where(m => m.builderMethod is not null)
            .ToImmutableList();

        if (allMatches.Count is 0)
        {
            return
                "Did not find any suitable dictionary builder method in assembly " +
                loadedAssembly.FullName +
                ". Searched types: " +
                string.Join(", ", candidateContainerTypes.Select(t => t.FullName));
        }

        if (allMatches.Count is not 1)
        {
            return
                "Ambiguous dictionary builder methods in assembly " +
                loadedAssembly.FullName + ": " +
                string.Join(", ", allMatches.Select(m => m.containerType.FullName + "." + m.builderMethod!.Name));
        }

        var (containerType, builderMethod) = allMatches[0]!;

        CompiledDictionary BuildDictionary()
        {
            return
                (CompiledDictionary?)
                builderMethod!.Invoke(null, [])!
                ?? throw new InvalidOperationException(
                    "The dictionary builder method " + containerType.FullName + "." + builderMethod.Name +
                    " returned null.");
        }

        return Result<string, Func<CompiledDictionary>>.ok(BuildDictionary);
    }

    private static MethodInfo? SearchTypeForDispatcherDictionary(Type containerType)
    {
        var targetDictionaryType =
            typeof(CompiledDictionary);

        bool DictionaryPredicate(MethodInfo method)
        {
            if (!method.IsStatic)
                return false;

            if (!method.IsPublic)
                return false;

            if (!method.ReturnType.IsAssignableTo(targetDictionaryType))
                return false;

            return true;
        }

        var matchingDictionaryMembers =
            containerType
            .GetMethods(BindingFlags.Public | BindingFlags.Static)
            .Where(DictionaryPredicate)
            .ToArray();

        if (matchingDictionaryMembers.Length is 0)
        {
            return null;
        }

        if (matchingDictionaryMembers.Length is not 1)
        {
            throw new InvalidOperationException(
                "Ambiguous matching methods in type " + containerType.FullName +
                ": " + string.Join(", ", matchingDictionaryMembers.Select(m => m.Name)));
        }

        return matchingDictionaryMembers[0];
    }

    private static readonly Lazy<IImmutableList<MetadataReference>> s_metadataReferences =
        new(() => ListMetadataReferences().ToImmutableList());

    private static IEnumerable<MetadataReference> ListMetadataReferences()
    {
        var types = new[]
        {
            typeof(object),
            typeof(Func<>),
            typeof(BigInteger),
            typeof(IImmutableList<>),
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
