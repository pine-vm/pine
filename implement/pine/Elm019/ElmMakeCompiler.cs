using Pine.Core.Caching;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.Files;
using Pine.Core.Functional;
using Pine.Core.Internal;
using Pine.Core.IO;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Elm019;

public enum ElmMakeExecutionPath
{
    PineVM, SyntaxInterpreter
}

/// <summary>
/// Runs the Elm implementation. This path does not invoke a native compiler, read an expectation,
/// or use reference registry/artifacts. The reference compiler is confined to maintenance.
/// </summary>
public static class ElmMakeCompiler
{
    private static readonly Lazy<ElmSyntaxInterpreter.Prepared> s_program = new(PrepareProgram);

    private static readonly Lazy<FunctionRecord> s_vmFunction = new(PrepareVmFunction);

    private static readonly ElmMakeCompilationCache s_compilationCache = new();

    private static readonly Core.Runtime.IntermediateVM.PineVM.EvaluationConfig s_compilationBudget =
        Core.Runtime.IntermediateVM.PineVM.EvaluationConfig.Default with { InvocationCountLimit = 100_000_000, LoopIterationCountLimit = 100_000_000 };

    private static readonly MemoryCache<ElmSyntaxInterpreter.ApplicationCacheInput, PineValue, int>
        s_parseCache = new(0);

    private static readonly ICache<ElmSyntaxInterpreter.ApplicationCacheInput, PineValue> s_persistentParseCache =
        new LayeredCache<ElmSyntaxInterpreter.ApplicationCacheInput, PineValue>(
            s_parseCache,
            new FileStoreCache<ElmSyntaxInterpreter.ApplicationCacheInput, PineValue>(
                new FileStoreFromSystemIOFile(System.IO.Path.Combine(Filesystem.CacheDirectory, "elm-make-ecmascript")),
                ["parse-module"],
                input => input.ComputeHash(),
                ElmSyntaxInterpreter.ApplicationResultCodec));

    public static async Task<Result<ElmMakeError, string>> CompileWithPackagesAsync(
        FileTree appFiles,
        IReadOnlyList<string> entryPoint,
        ElmMakeOutputType outputType,
        ElmMakeFlavor flavor,
        CancellationToken cancellationToken = default,
        ElmMakeCompilationCache? compilationCache = null)
    {
        IReadOnlyList<string>? manifestPath = null;

        for (var count = entryPoint.Count - 1; count >= 0; --count)
        {
            string[] candidate = [.. entryPoint.Take(count), "elm.json"];

            if (appFiles.GetNodeAtPath(candidate) is not null)
            {
                manifestPath = candidate;
                break;
            }
        }

        if (manifestPath is null)
        {
            return
                new ElmMakeError.Diagnostics(
                    new(new(new ElmMakeLocation.Project(), new(ElmMakeProblemCode.NoManifest, [])), []));
        }

        var packages = new Dictionary<ElmPackageIdentity, FileTree>();

        var provider =
            new ElmDelegatePackageProvider(
                (name, version) =>
                {
                    var identity = new ElmPackageIdentity(name, ElmPackageVersion.Parse(version));
                    var files = ElmPackageSource.LoadElmPackageAsync(name, version).GetAwaiter().GetResult();
                    packages[identity] = FileTree.FromSetOfFilesWithStringPath(files);
                    return files;
                });

        var resolution =
            await ElmDependencyResolver.ResolveAsync(
                appFiles,
                manifestPath,
                new() { IncludeTests = true },
                provider,
                cancellationToken);

        if (!resolution.Succeeded)
        {
            if (resolution.Failures.All(
                failure => failure.Kind is
                ElmResolutionFailureKind.InvalidManifest or ElmResolutionFailureKind.CompilerIncompatible or
                ElmResolutionFailureKind.ConstraintConflict or ElmResolutionFailureKind.UndeclaredDependency or
                ElmResolutionFailureKind.DependencyCycle))
            {
                var location = new ElmMakeLocation.File(new ElmMakeFile.Application([.. manifestPath]));

                var problems =
                    resolution.Failures.Select(
                        failure =>
                        new ElmMakeProblem(location, new(ElmMakeProblemCode.Manifest, [], failure.Message))).ToArray();

                return new ElmMakeError.Diagnostics(new(problems[0], problems.Skip(1)));
            }

            return
                new ElmMakeError.Internal(
                    new(
                        ElmMakeInternalProblemKind.PackageResolution,
                        string.Join("\n", resolution.Failures.Select(failure => failure.Message))));
        }

        return
            Compile(
                appFiles,
                manifestPath,
                entryPoint,
                outputType,
                flavor,
                packages,
                cancellationToken,
                compilationCache: compilationCache);
    }

    public static Result<ElmMakeError, string> Compile(
        FileTree appFiles,
        IReadOnlyList<string> manifestPath,
        IReadOnlyList<string> entryPoint,
        ElmMakeOutputType outputType,
        ElmMakeFlavor flavor,
        IReadOnlyDictionary<ElmPackageIdentity, FileTree> packages,
        CancellationToken cancellationToken = default,
        ElmMakeExecutionPath executionPath = ElmMakeExecutionPath.PineVM,
        ElmMakeCompilationCache? compilationCache = null)
    {
        cancellationToken.ThrowIfCancellationRequested();

        var request =
            ElmValue.RecordInstance(
                [
                    ("files", EncodeFiles(appFiles)),
                    ("manifestPath", EncodePath(manifestPath)),
                    ("entryPoint", EncodePath(entryPoint)),
                    ("outputType",
                    ElmValue.TagInstance(
                        outputType switch
                        {
                            ElmMakeOutputType.Html => "Html",
                            ElmMakeOutputType.JavaScript => "JavaScript",

                            _ =>
                            throw new ArgumentOutOfRangeException(nameof(outputType)),
                        },
                        [])),
                    ("flavor",
                    ElmValue.TagInstance(
                        flavor switch
                        {
                            ElmMakeFlavor.Development => "Development",
                            ElmMakeFlavor.Debug => "Debug",
                            ElmMakeFlavor.Optimize => "Optimize",

                            _ =>
                            throw new ArgumentOutOfRangeException(nameof(flavor)),
                        },
                        [])),
                    ("packages",
                    ElmValue.ListInstance(
                        [
                            .. packages.OrderBy(item => item.Key.Name, StringComparer.Ordinal).Select(
                                item => ElmValue.RecordInstance(
                                    [
                                        ("name", ElmValue.StringInstance(item.Key.Name)),
                                        ("version", ElmValue.StringInstance(item.Key.Version.ToString())),
                                        ("files", EncodeFiles(item.Value, packageOnly: true)),
                                    ]))
                        ])),
                ]);

        var argument = ElmValueEncoding.ElmValueAsPineValue(request);

        if (executionPath is ElmMakeExecutionPath.PineVM)
        {
            var vm = (compilationCache ?? s_compilationCache).CreateVM(s_compilationBudget);

            var vmResult =
                ElmInteractiveEnvironment.ApplyFunction(vm, s_vmFunction.Value, [argument], cancellationToken);

            if (vmResult.IsErrOrNull() is { } vmError)
            {
                cancellationToken.ThrowIfCancellationRequested();
                return new ElmMakeError.Internal(new(ElmMakeInternalProblemKind.PineVM, vmError));
            }

            return DecodeResult(vmResult.Extract(error => throw new InvalidOperationException(error)));
        }

        if (executionPath is not ElmMakeExecutionPath.SyntaxInterpreter)
            throw new ArgumentOutOfRangeException(nameof(executionPath));

        var result =
            ElmSyntaxInterpreter.Interpret(
                DeclQualifiedName.Create(["ElmMake"], "make"),
                [PineValueInProcess.Create(argument)],
                s_program.Value,
                ElmSyntaxInterpreter.EvaluationConfig.Default with
                {
                    CancellationToken = cancellationToken,
                    ApplicationCache =
                    new(
                        s_persistentParseCache,
                        name => name.FullName == "ElmMake.Canonicalize.parseSourceModule"),
                });

        if (result.IsErrOrNull() is { } interpretationError)
        {
            cancellationToken.ThrowIfCancellationRequested();

            return
                new ElmMakeError.Internal(
                    new(
                        ElmMakeInternalProblemKind.Interpreter,
                        interpretationError.Message + "\n" +
                        string.Join(
                            "\n",
                            interpretationError.CallStack.Take(12).Select(frame => frame.FunctionName.FullName))));
        }

        return DecodeResult(result.Extract(error => throw new InvalidOperationException(error.Message)).Evaluate());
    }

    internal static Result<ElmMakeError, string> DecodeResult(PineValue result)
    {
        var value =
            ElmValueEncoding.PineValueAsElmValue(
                result,
                null,
                null)
            .Extract(error => throw new InvalidOperationException("Invalid Elm make result: " + error));

        if (value is ElmValue.ElmTag { TagName: "Ok", Arguments: [ElmValue.ElmString text] })
            return text.Value;

        if (value is ElmValue.ElmTag
            {
                TagName: "Err",
                Arguments: [ElmValue.ElmTag { TagName: "CompilerErrors", Arguments: [var first, ElmValue.ElmList diagnostics] }]
            })
        {
            return new ElmMakeError.Diagnostics(new(DecodeProblem(first), diagnostics.Items.Select(DecodeProblem)));
        }

        if (value is ElmValue.ElmTag { TagName: "Err", Arguments: [ElmValue.ElmTag { Arguments: [ElmValue.ElmRecord error] } tag] })
        {
            var fields = error.Fields.ToDictionary(field => field.FieldName, field => field.Value);

            return tag.TagName switch
            {
                "UnsupportedFeature" => new ElmMakeError.Unsupported(DecodeUnsupported(fields)),

                "InternalFailure" =>
                new ElmMakeError.Internal(
                    new(
                        Enum.Parse<ElmMakeInternalProblemKind>(Tag(fields["kind"]).TagName),
                        Text(fields["context"]))),

                _ =>
                throw new InvalidOperationException("Unexpected Elm make error: " + tag),
            };
        }

        throw new InvalidOperationException("Unexpected Elm make result: " + value);
    }

    private static ElmMakeProblem DecodeProblem(ElmValue value)
    {
        if (value is not ElmValue.ElmRecord record)
            throw new InvalidOperationException("Invalid Elm compiler problem: " + value);

        var fields = record.Fields.ToDictionary(field => field.FieldName, field => field.Value);
        return new(DecodeLocation(fields["location"]), DecodeProblemKind(fields["kind"]));
    }

    private static ElmValue.ElmTag Tag(ElmValue value)
    {
        if (value is ElmValue.ElmTag tag)
            return tag;

        throw new InvalidOperationException("Expected a diagnostic variant, found " + value);
    }

    private static string Text(ElmValue value) =>
        value is ElmValue.ElmString text
        ?
        text.Value
        :
        throw new InvalidOperationException("Expected diagnostic text.");

    private static int Integer(ElmValue value) =>
        value is ElmValue.ElmInteger integer
        ?
        checked((int)integer.Value)
        :
        throw new InvalidOperationException("Expected diagnostic integer.");

    private static ImmutableArray<string> Strings(ElmValue value) =>
        value is ElmValue.ElmList list
        ?
        [.. list.Items.Select(Text)]
        :
        throw new InvalidOperationException("Expected diagnostic names/path.");

    private static IReadOnlyDictionary<string, ElmValue> Fields(ElmValue value) =>
        value is ElmValue.ElmRecord record
        ?
        record.Fields.ToDictionary(field => field.FieldName, field => field.Value)
        :
        throw new InvalidOperationException("Expected diagnostic record.");

    private static ElmMakeFile DecodeFile(ElmValue value)
    {
        var tag = Tag(value);

        if (tag is { TagName: "ApplicationFile", Arguments: [var path] })
            return new ElmMakeFile.Application(Strings(path));

        if (tag is { TagName: "PackageFile", Arguments: [var package] })
        {
            var fields = Fields(package);

            return
                new ElmMakeFile.Package(
                    new(Text(fields["name"]), ElmPackageVersion.Parse(Text(fields["version"]))),
                    Strings(fields["path"]));
        }

        throw new InvalidOperationException("Invalid compiler source identity: " + value);
    }

    private static ElmMakeLocation DecodeLocation(ElmValue value)
    {
        var tag = Tag(value);

        return tag.TagName switch
        {
            "Project" when tag.Arguments.Count is 0 => new ElmMakeLocation.Project(),
            "File" when tag.Arguments is [var file] => new ElmMakeLocation.File(DecodeFile(file)),

            "Source" when tag.Arguments is [var file, var region] =>
            new ElmMakeLocation.Source(DecodeFile(file), DecodeRegion(region)),

            _ =>
            throw new InvalidOperationException("Invalid compiler diagnostic location: " + value),
        };
    }

    private static ElmMakeProblemKind DecodeProblemKind(ElmValue value)
    {
        var kind = Tag(value);

        if (kind is { TagName: "NamingProblem", Arguments: [var naming] })
            kind = Tag(naming);

        var args = kind.Arguments;

        return kind.TagName switch
        {
            "SyntaxProblem" when args is [var details] => new(ElmMakeProblemCode.Syntax, [], Text(details)),
            "ManifestProblem" when args is [var details] => new(ElmMakeProblemCode.Manifest, [], Text(details)),
            "NoManifest" when args.Count is 0 => new(ElmMakeProblemCode.NoManifest, []),
            "EntryPointNotFound" when args is [var path] => new(ElmMakeProblemCode.EntryPointNotFound, Strings(path)),
            "NoMain" when args.Count is 0 => new(ElmMakeProblemCode.NoMain, []),
            "CyclicTypeAlias" when args is [var name] => new(ElmMakeProblemCode.CyclicTypeAlias, [Text(name)]),
            "DebugRemnants" when args is [var names] => new(ElmMakeProblemCode.DebugRemnants, Strings(names)),

            "UnknownValue" or "UnknownType" when args is [var qualifier, var name] =>
            new(Enum.Parse<ElmMakeProblemCode>(kind.TagName), [.. Strings(qualifier), Text(name)]),

            "MissingModule" or "AmbiguousModule" or "AmbiguousName" or "DuplicateModule" or "DuplicateDeclaration" or
            "ShadowedBinding" or "NotAConstructor" when args is [var name] =>
            new(Enum.Parse<ElmMakeProblemCode>(kind.TagName), [Text(name)]),

            "NotExposed" or "InvalidExposure" when args is [var module, var name] =>
            new(Enum.Parse<ElmMakeProblemCode>(kind.TagName), [Text(module), Text(name)]),

            "DuplicateBinding" when args is [var names] => new(ElmMakeProblemCode.DuplicateBinding, Strings(names)),

            "WrongConstructorArity" when args is [var name, var expected, var actual] =>
            new(
                ElmMakeProblemCode.WrongConstructorArity,
                [
                Text(name)
                ],
                ExpectedArity: Integer(expected),
                ActualArity: Integer(actual)),

            "RestrictedKernelImport" when args.Count is 0 => new(ElmMakeProblemCode.RestrictedKernelImport, []),

            _ =>
            throw new InvalidOperationException("Invalid compiler diagnostic kind: " + kind),
        };
    }

    private static ElmMakeUnsupportedProblem DecodeUnsupported(IReadOnlyDictionary<string, ElmValue> fields)
    {
        var tag = Tag(fields["feature"]);
        var capability = Enum.Parse<ElmMakeCapability>(tag.TagName);

        return tag.TagName switch
        {
            "CustomOperator" or "RecursiveMainType" or "PartialMainType" or "UnsupportedMainType" when tag.Arguments is [var name] =>
            new(DecodeLocation(fields["location"]), capability, Name: Text(name)),

            "ConstructorFieldCount" or "TupleArity" when tag.Arguments is [var arity] =>
            new(DecodeLocation(fields["location"]), capability, Arity: Integer(arity)),

            _ when tag.Arguments.Count is 0 =>
            new(DecodeLocation(fields["location"]), capability),

            _ =>
            throw new InvalidOperationException("Invalid unsupported capability: " + tag),
        };
    }

    private static ElmMakeRegion DecodeRegion(ElmValue value)
    {
        if (Tag(value) is not { TagName: "Region", Arguments: [var start, var end] })
            throw new InvalidOperationException("Invalid compiler diagnostic region.");

        ElmMakePosition Position(ElmValue position)
        {
            var coordinates = Fields(position);
            return new(Integer(coordinates["line"]), Integer(coordinates["column"]));
        }

        return new(Position(start), Position(end));
    }

    private static ElmValue EncodeFiles(FileTree tree, bool packageOnly = false) =>
        ElmValue.ListInstance(
            [
            .. tree.EnumerateFilesTransitive()
            .Where(file => !packageOnly || file.path.Count is 1 && file.path[0] is "elm.json" || file.path[0] is "src")
            .Where(
                file =>
                file.path[^1] is "elm.json" || file.path[^1].EndsWith(".elm", StringComparison.Ordinal) ||
                file.path[^1].EndsWith(".js", StringComparison.Ordinal))
            .Where(file => !file.path.Contains("elm-stuff"))
            .Select(
                file => ElmValue.RecordInstance(
                    [
                        ("path", EncodePath(file.path)),
                        ("content", ElmValue.StringInstance(new UTF8Encoding(false, true).GetString(file.fileContent.Span))),
                    ]))
            ]);

    private static ElmValue EncodePath(IEnumerable<string> path) =>
        ElmValue.ListInstance([.. path.Select(ElmValue.StringInstance)]);

    private static FunctionRecord PrepareVmFunction()
    {
        var container = BundledFiles.CompilerSourceContainerFilesDefault.Value;

        var sources =
            FileTree.FromSetOfFilesWithStringPath(
                container.EnumerateFilesTransitive().Where(
                    file => file.path[^1].EndsWith(".elm", StringComparison.Ordinal) &&
                        ((file.path[0] == "elm-make-ecmascript" && !file.path.Contains("tests")) ||
                        file.path[0] == "pine-elm-syntax")));

        var (environment, _) =
            ElmCompiler.CompileInteractiveEnvironment(
                sources,
                [DeclQualifiedName.Create(["ElmMake"], "make")])
            .Extract(
                error =>
                throw new InvalidOperationException("Failed compiling Elm ECMAScript compiler to Pine: " + error));

        var parsed =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(environment)
            .Extract(error => throw new InvalidOperationException(error));

        var function =
            parsed.Modules.Single(module => module.moduleName == "ElmMake").moduleContent.FunctionDeclarations["make"];

        return
            FunctionRecord.ParseFunctionRecordTagged(function, new PineVMParseCache())
            .Extract(error => throw new InvalidOperationException(error));
    }

    private static ElmSyntaxInterpreter.Prepared PrepareProgram()
    {
        var container = BundledFiles.CompilerSourceContainerFilesDefault.Value;

        var root =
            container.GetNodeAtPath(["elm-make-ecmascript", "src", "ElmMake.elm"]) as FileTree.FileNode
            ?? throw new InvalidOperationException("Missing bundled elm-make-ecmascript/src/ElmMake.elm.");

        var available =
            BundledFiles.ElmKernelModulesDefault.Value.EnumerateFilesTransitive()
            .Concat(
                container.EnumerateFilesTransitive().Where(
                    file => !file.path.Contains("tests") && !file.path.Contains("elm-kernel-modules")))
            .Where(file => file.path[^1].EndsWith(".elm", StringComparison.Ordinal))
            .Select(file => Encoding.UTF8.GetString(file.fileContent.Span))
            .ToImmutableArray();

        var modules =
            ElmModule.ModulesTextOrderedForCompilationByDependencies(
                [Encoding.UTF8.GetString(root.Bytes.Span)],
                available);

        return
            ElmSyntaxInterpreter.PrepareModules(modules)
            .Extract(
                error => throw new InvalidOperationException("Failed preparing Elm ECMAScript compiler: " + error));
    }
}
