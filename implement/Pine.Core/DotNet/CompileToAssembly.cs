using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
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
    Func<Result<string, CompiledDictionary>> BuildCompiledExpressionsDictionary);


public class CompileToAssembly
{
    private static readonly CSharpParseOptions s_parseOptions =
        new(languageVersion: LanguageVersion.CSharp13);

    public static Result<string, CompileToAssemblyResult> Compile(
        StaticProgramCSharp staticProgram,
        IReadOnlyList<string> namespacePrefix,
        OptimizationLevel optimizationLevel)
    {
        IReadOnlyList<ClassDeclarationSyntax> classDeclarations =
            [
            staticProgram.CommonValueClass,
            ..staticProgram.ModulesClasses.Values.Select(mc => mc.ClassDeclarationSyntax),
            staticProgram.GlobalAnonymousClass,
            staticProgram.DispatcherClass,
            ];

        IReadOnlyList<SyntaxTree> syntaxTrees =
            [
            ..classDeclarations
            .Select(cd =>
            BuildCompilationUnitSyntaxTree(
                cd,
                staticProgram.DeclarationSyntaxContext,
                namespacePrefix)),
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
            return Result<string, CompileToAssemblyResult>.err(
                "Compilation failed with " + compilationErrors.Count + " errors:\n" +
                string.Join("\n", compilationErrors.Select(d => d.ToString())));
        }

        var assembly = codeStream.ToArray();

        IReadOnlyList<string>
            dictionaryFullNameSegments =
            [
                ..namespacePrefix,
                ..staticProgram.DeclarationSyntaxContext.CurrentNamespace is { } commonNamespace
                ?
                (IReadOnlyList<string>)[commonNamespace]
                :
                [],
                staticProgram.DispatcherClass.Identifier.Text
            ];

        Result<string, CompiledDictionary> BuildDictionary()
        {
            return
                BuildDictionaryFromAssembly(
                    assembly,
                    dictionaryFullNameSegments);
        }

        return
            new CompileToAssemblyResult(
                Assembly: assembly,
                BuildCompiledExpressionsDictionary: BuildDictionary);
    }


    public static Result<string, CompiledDictionary> BuildDictionaryFromAssembly(
        byte[] assemblyBytes,
        IReadOnlyList<string> dictionaryFullName)
    {
        return
            BuildDictionaryFromAssembly(
                assemblyBytes,
                string.Join('.', dictionaryFullName));
    }

    public static Result<string, CompiledDictionary> BuildDictionaryFromAssembly(
        byte[] assemblyBytes,
        string dictionaryFullName)
    {
        var loadedAssembly = Assembly.Load(assemblyBytes);

        var compiledType =
            loadedAssembly.GetType(dictionaryFullName);

        if (compiledType is null)
        {
            return
                "Did not find type " + dictionaryFullName +
                " in assembly " + loadedAssembly.FullName;
        }

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
            compiledType
            .GetMethods(BindingFlags.Public | BindingFlags.Static)
            .Where(DictionaryPredicate)
            .ToArray();

        if (matchingDictionaryMembers.Length is 0)
        {
            return
                "Did not find matching method in type " + compiledType.FullName;
        }

        if (matchingDictionaryMembers.Length is not 1)
        {
            return
                "Ambiguous matching methods in type " + compiledType.FullName +
                ": " + string.Join(", ", matchingDictionaryMembers.Select(m => m.Name));
        }

        var dictionaryMember = matchingDictionaryMembers[0];

        var originalDictionary =
            (CompiledDictionary)
            dictionaryMember.Invoke(null, null)!;

        return Result<string, CompiledDictionary>.ok(originalDictionary);
    }

    public static SyntaxTree BuildCompilationUnitSyntaxTree(
        ClassDeclarationSyntax classDeclaration,
        DeclarationSyntaxContext declarationSyntaxContext,
        IReadOnlyList<string> namespacePrefix,
        string? fileIdentifier = null)
    {
        IReadOnlyList<string> aggregateNamepace =
            [.. namespacePrefix
            ,..declarationSyntaxContext.CurrentNamespace is { } currentNs ? currentNs.Split('.'):[]
            ];

        MemberDeclarationSyntax
            compilationUnitMember =
            aggregateNamepace.Count is 0
            ?
            classDeclaration
            :
            SyntaxFactory.FileScopedNamespaceDeclaration(
                SyntaxFactory.ParseName(string.Join('.', aggregateNamepace)))
            .WithMembers([classDeclaration]);

        var compilationUnitSyntax =
            SyntaxFactory.CompilationUnit()
            .WithUsings(
                [.. declarationSyntaxContext.UsingDirectives
                .OrderBy(ud => ud.ToFullString())
                ])
            .WithMembers([compilationUnitMember]);

        var formattedNode =
            FormatCSharpSyntaxRewriter.FormatSyntaxTree(
                compilationUnitSyntax.NormalizeWhitespace(eol: "\n"));

        // Use supplied identifier or derive a stable default like "Ns.SubNs.ClassName.g.cs"
        var defaultFileIdentifier =
            (aggregateNamepace.Count is 0
            ?
            classDeclaration.Identifier.Text
            :
            string.Join('.', aggregateNamepace.Append(classDeclaration.Identifier.Text)))
            + ".g.cs";

        var cuSyntaxTree =
            CSharpSyntaxTree.Create(
                formattedNode,
                options: s_parseOptions,
                path: fileIdentifier ?? defaultFileIdentifier);

        return cuSyntaxTree;
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
