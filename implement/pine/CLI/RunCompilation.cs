using Pine.Core;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using Pine.Core.IO;
using Pine.Core.Json;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Text.RegularExpressions;
using System.Threading;

using AST = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using Syntax = Pine.Core.Elm.ElmSyntax.SyntaxModel;
using Type = Pine.Core.Elm.ElmCompilerInDotnet.TypeInference.InferredType;

namespace Pine.CLI;

internal sealed record RunCompilation(
    PineValue Environment,
    string ModuleName,
    string Declaration,
    RunCompilation.TypeInformation Types,
    string DeclarationSignature)
{
    internal sealed record TypeInformation(Type Type, Dictionary<string, TypeInference.ChoiceTypeDefinition> Choices);

    private static readonly JsonSerializerOptions s_json =
        new() { IncludeFields = true, Converters = { new JsonConverterForChoiceType<Type>() } };

    public static RunCompilation Compile(
        RunSource source,
        string cacheDirectory,
        TextWriter diagnostics,
        CancellationToken token,
        Func<IReadOnlyList<RunDeclarationSelection.Candidate>, string>? selectDeclaration = null)
    {
        var file =
            source.Tree.GetNodeAtPath(source.FilePath) as FileTree.FileNode
            ?? throw new IOException("Selected Elm module is not a file.");

        var syntax =
            ElmSyntaxParser.ParseModuleText(Encoding.UTF8.GetString(file.Bytes.Span))
            .Extract(error => throw new ArgumentException("Cannot parse selected module: " + error));

        var functions =
            syntax.Declarations
            .Where(node => node.Value is Syntax.Declaration.FunctionDeclaration)
            .Select(node => (node.Range, Function: ((Syntax.Declaration.FunctionDeclaration)node.Value).Function))
            .ToArray();

        var declaration = source.Declaration;

        if (source.Line is { } line)
        {
            declaration =
                functions.Where(
                    item =>
                    line >= (item.Function.Documentation?.Range.Start.Row ?? item.Range.Start.Row) &&
                    (line < item.Range.End.Row || line == item.Range.End.Row && item.Range.End.Column > 1))
                .Select(item => item.Function.Declaration.Value.Name.Value).SingleOrDefault()
                ?? throw new ArgumentException(
                    "The selected line is not inside a function declaration, signature, or documentation comment.");
        }

        if (declaration is null)
        {
            var candidates =
                functions.Select(
                    item =>
                    {
                        var name = item.Function.Declaration.Value.Name.Value;

                        return
                            new RunDeclarationSelection.Candidate(
                                name,
                                item.Function.Signature is { } signature
                                ?
                                name + " : " +
                                Regex.Replace(
                                    Rendering.RenderTypeAnnotation(signature.Value.TypeAnnotation),
                                    @"\s+",
                                    " ")
                                :
                                null);
                    })
                .OrderBy(item => item.Name, StringComparer.Ordinal)
                .ToArray();

            if (candidates.Length is 0)
                throw new ArgumentException("No function declarations in the selected module.");

            declaration =
                selectDeclaration?.Invoke(candidates)
                ?? throw new ArgumentException(
                    "Select a declaration explicitly using .elm/name or .elm#Lnumber. Function declarations: " +
                    string.Join(", ", candidates.Select(item => item.Name)));
        }

        if (declaration is null || !functions.Any(item => item.Function.Declaration.Value.Name.Value == declaration))
        {
            throw new ArgumentException(
                "Declaration '" + declaration + "' not found." +
                " Function declarations: " +
                string.Join(", ", functions.Select(item => item.Function.Declaration.Value.Name.Value)));
        }

        token.ThrowIfCancellationRequested();
        var moduleName = string.Join(".", Syntax.Module.GetModuleName(syntax.ModuleDefinition.Value).Value);
        var tree = source.Tree;
        var manifest = ElmAppDependencyResolution.FindElmJsonForEntryPoint(tree, source.FilePath);
        ElmResolvedBuild? build = null;

        if (manifest is { } selected)
        {
            build =
                ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                    tree,
                    selected.filePath,
                    [source.FilePath],
                    ElmPackageSubstitutions.DefaultBuild.Value,
                    cancellationToken: token,
                    projectDirectory: selected.filePath.Count is 1 ? source.DirectoryPath : null).GetAwaiter().GetResult();

            tree = build.Sources;
            // Parent-source loading can prefix the original source path.
            moduleName =
                build.CompilerModuleOrigins.Single(
                    entry =>
                    entry.Value is ElmModuleOrigin.Project project && project.ModuleName == moduleName).Key;
        }

        string ChoiceTypeName(Type.ChoiceType choice)
        {
            var canonicalName = string.Join(".", choice.ModuleName);

            var sourceName =
                build is not null && build.CompilerModuleOrigins.TryGetValue(canonicalName, out var origin)
                ?
                origin.ModuleName
                :
                canonicalName;

            if (canonicalName == moduleName || sourceName.Length is 0 ||
                (sourceName, choice.TypeName) is ("Maybe", "Maybe") or ("Result", "Result") or ("Basics", "Order"))
            {
                return choice.TypeName;
            }

            var import =
                syntax.Imports.Select(node => node.Value)
                .FirstOrDefault(item => string.Join(".", item.ModuleName.Value) == sourceName);

            if (import?.ExposingList?.ExposingList.Value is Syntax.Exposing.All ||
                import?.ExposingList?.ExposingList.Value is Syntax.Exposing.Explicit exposed &&
                exposed.Nodes.Any(
                    node =>
                    node.Value is Syntax.TopLevelExpose.TypeOrAliasExpose type && type.Name == choice.TypeName ||
                    node.Value is Syntax.TopLevelExpose.TypeExpose openType &&
                    openType.ExposedType.Name == choice.TypeName))
            {
                return choice.TypeName;
            }

            return
                (import?.ModuleAlias is { } alias ? string.Join(".", alias.Alias.Value) : sourceName) +
                "." + choice.TypeName;
        }

        RunCompilation Compilation(PineValue environment, TypeInformation types) =>
            new(
                environment,
                moduleName,
                declaration,
                types,
                declaration + " : " + TypeInference.RenderInferredType(types.Type, ChoiceTypeName));

        var root = DeclQualifiedName.Create(moduleName.Split('.'), declaration);
        var treeHash = Convert.ToHexStringLower(PineValueHashTree.ComputeHashSorted(tree).Span);

        var key =
            Convert.ToHexStringLower(
                System.Security.Cryptography.SHA256.HashData(
                    Encoding.UTF8.GetBytes(
                        typeof(ElmCompiler).Assembly.ManifestModule.ModuleVersionId + ":" +
                        typeof(RunCompilation).Assembly.ManifestModule.ModuleVersionId + ":" +
                        "run-v1:" +
                        treeHash +
                        ":" +
                        root.FullName +
                        ":" +
                        (build?.Resolution.Fingerprint ?? "bundled"))));

        var cachePath = Path.Combine(cacheDirectory, "run", key);

        if (File.Exists(cachePath))
        {
            try
            {
                var values = StringNamedPineValueBinaryEncoding.Decode(File.ReadAllBytes(cachePath)).decls;

                var types =
                    JsonSerializer.Deserialize<TypeInformation>(
                        StringEncoding.StringFromValue(values["types"]).Extract(
                            error => throw new InvalidDataException(error)),
                        s_json)
                    ?? throw new InvalidDataException("Missing cached type information.");

                token.ThrowIfCancellationRequested();
                diagnostics.WriteLine("Using cached run compilation.");
                return Compilation(values["environment"], types);
            }
            catch (Exception exception) when (exception is IOException or ArgumentException or JsonException or KeyNotFoundException)
            {
                diagnostics.WriteLine("Cannot read run compilation cache; recompiling: " + exception.Message);
            }
        }

        var (environment, pipeline) =
            (build is null
            ?
            ElmCompiler.CompileInteractiveEnvironment(tree, [root])
            :
            ElmCompiler.CompileResolvedEnvironment(build, [root]))
            .Extract(error => throw new ElmCompilationException(error));

        token.ThrowIfCancellationRequested();
        var information = InferTypes(pipeline.Canonicalized, root);
        using var buffer = new MemoryStream();

        StringNamedPineValueBinaryEncoding.Encode(
            buffer,
            new Dictionary<string, PineValue>
            {
                ["environment"] = environment,
                ["types"] = StringEncoding.ValueFromString(JsonSerializer.Serialize(information, s_json))
            });

        Directory.CreateDirectory(Path.GetDirectoryName(cachePath)!);
        AtomicFile.WriteAllBytes(cachePath, buffer.ToArray(), token);
        return Compilation(environment, information);
    }

    private static TypeInformation InferTypes(IReadOnlyList<Syntax.File> modules, DeclQualifiedName root)
    {
        var abstractModules = modules.Select(AST.ConvertFromConcrete.FromFile).ToArray();
        var declarations = ElmCompiler.FlattenModulesToDeclarationDictionary(abstractModules);

        var aliases =
            abstractModules.SelectMany(
                module =>
                TypeInference.BuildTypeAliasDefinitions(
                    module,
                    string.Join(".", AST.Module.GetModuleName(module.ModuleDefinition))))
            .ToImmutableDictionary();

        var signatures =
            abstractModules.SelectMany(
                module =>
                TypeInference.BuildFunctionSignaturesMap(
                    module,
                    string.Join(".", AST.Module.GetModuleName(module.ModuleDefinition)),
                    aliases))
            .ToImmutableDictionary();

        var function = ((AST.Declaration.FunctionDeclaration)declarations[root]).Function;
        Type type;

        if (function.Signature is { } signature)
            type = TypeInference.TypeAnnotationToInferredType(signature.TypeAnnotation);

        else
        {
            var inferred =
                TypeInference.InferFunctionDeclarationType(
                    function.Declaration.Expression,
                    function.Declaration.Arguments,
                    string.Join(".", root.Namespaces),
                    signatures);

            type = inferred.returnType;

            foreach (var argument in function.Declaration.Arguments.Reverse())
            {
                var parameter =
                    argument is AST.Pattern.VarPattern variable &&
                    inferred.parameterTypes.TryGetValue(variable.Name, out var known)
                    ?
                    known
                    :
                    new Type.UnknownType();

                type = new Type.FunctionType(parameter, type);
            }
        }

        var choices =
            TypeInference.BuildChoiceTypeDefinitions(declarations)
            .ToDictionary(
                entry => entry.Key.FullName,
                entry =>
                entry.Value with
                {
                    Constructors =
                    entry.Value.Constructors.Select(
                        constructor =>
                        constructor with
                        {
                            ArgumentTypes =
                            constructor.ArgumentTypes
                            .Select(
                                argument => TypeInference.ExpandTypeAliases(argument, aliases, entry.Key.Namespaces)).ToArray()
                        }).ToArray()
                });

        return new(TypeInference.ExpandTypeAliases(type, aliases, root.Namespaces), choices);
    }
}
