using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Files;
using Pine.Core.Functional;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class ElmModuleTypeInterfaceTests
{
    [Theory]
    [InlineData("Cmd String", "Cmd.none")]
    [InlineData("Cmd Int", "Cmd.map String.length Cmd.none")]
    [InlineData("Sub String", "Sub.batch [ Sub.none ]")]
    [InlineData("Sub Int", "Sub.map String.length Sub.none")]
    [InlineData("Task.Task x Int", "Task.succeed 42")]
    [InlineData("Task.Task String a", "Task.fail \"error\"")]
    [InlineData("Task.Task x Int", "Task.map String.length (Task.succeed \"hello\")")]
    [InlineData("Task.Task x Int", "Task.map2 (+) (Task.succeed 1) (Task.succeed 2)")]
    [InlineData("Task.Task x Int", "Task.map5 (\\a b c d e -> a + b + c + d + e) (Task.succeed 1) (Task.succeed 2) (Task.succeed 3) (Task.succeed 4) (Task.succeed 5)")]
    [InlineData("Task.Task x (List Int)", "Task.sequence [ Task.succeed 1, Task.succeed 2 ]")]
    [InlineData("Task.Task x ()", "Process.sleep 100")]
    [InlineData("Task.Task x Process.Id", "Process.spawn (Task.succeed ())")]
    [InlineData("Cmd Int", "Task.perform identity (Task.succeed 1)")]
    [InlineData("Sub Time.Posix", "Time.every 200 identity")]
    [InlineData("Task.Task x Time.Posix", "Time.now")]
    [InlineData("Task.Task x Time.Zone", "Time.here")]
    [InlineData("Task.Task x Time.ZoneName", "Time.getZoneName")]
    [InlineData("Time.Zone", "Time.customZone 60 []")]
    [InlineData("Int", "Time.toDay Time.utc (Time.millisToPosix 0)")]
    [InlineData("Time.Month", "Time.toMonth Time.utc (Time.millisToPosix 0)")]
    [InlineData("Time.ZoneName", "Time.Name \"Europe/Paris\"")]
    [InlineData("Program () Int String", "Platform.worker { init = \\_ -> ( 0, Cmd.none ), update = \\_ state -> ( state, Cmd.none ), subscriptions = \\_ -> Sub.none }")]
    public async Task Type_only_interfaces_supply_scoped_diagnostics_and_query_types(string type, string expression)
    {
        var build =
            await Prepare(
                $$"""
                module Main exposing (..)
                import Task
                import Process
                import Platform
                import Time

                value : {{type}}
                value = {{expression}}
                """);

        build.ImportDiagnostics.Should().BeEmpty();
        ElmCompiler.AnalyzeApplication(build).Should().BeEmpty();

        var query =
            ElmCompiler.QueryDeclaration(build, Name("value"))
            .Extract(error => throw new Exception(error));

        query.Diagnostics.Should().BeEmpty();
        query.Type.Should().NotBeNull();
    }

    [Fact]
    public async Task Type_only_interfaces_infer_unannotated_functions()
    {
        var build =
            await Prepare(
                """
                module Main exposing (..)
                import Time

                subscriptions =
                    Sub.batch [ Time.every 200 identity, Sub.none ]

                command = Cmd.none
                """);

        ElmCompiler.AnalyzeApplication(build).Should().BeEmpty();
        var query = ElmCompiler.QueryDeclaration(build, Name("command")).Extract(error => throw new Exception(error));
        query.Diagnostics.Should().BeEmpty();
        TypeInference.RenderInferredType(query.Type!).Should().Be("Platform.Cmd.Cmd msg");

        var subscription =
            ElmCompiler.QueryDeclaration(build, Name("subscriptions"))
            .Extract(error => throw new Exception(error));

        subscription.Diagnostics.Should().BeEmpty();
        subscription.Type.Should().NotBeNull();
    }

    [Theory]
    [InlineData("Sub String", "Cmd.none")]
    [InlineData("Cmd String", "Sub.none")]
    [InlineData("Int", "Time.now")]
    [InlineData("Task.Task x String", "Task.succeed 42")]
    public async Task Type_only_interfaces_preserve_real_type_errors(string type, string expression)
    {
        var build =
            await Prepare(
                $$"""
                module Main exposing (..)
                import Task
                import Time

                value : {{type}}
                value = {{expression}}
                """);

        ElmCompiler.AnalyzeApplication(build).Should().Contain(diagnostic => diagnostic.Code == "elm-type");
    }

    [Theory]
    [InlineData("import Platform.Sub exposing (Sub(..))", "value = 1", "elm-import")]
    [InlineData("import Task exposing (missing)", "value = 1", "elm-import")]
    [InlineData("import Time", "value = Time.missing", "elm-name")]
    [InlineData("import Task", "value = Task.Task", "elm-name")]
    [InlineData("import Process", "value : Process.Missing\nvalue = 1", "elm-name")]
    public async Task Type_only_interfaces_do_not_invent_symbols_or_opaque_constructors(
        string import, string declaration, string code)
    {
        var build = await Prepare("module Main exposing (..)\n" + import + "\n" + declaration);
        ElmCompiler.AnalyzeApplication(build).Should().Contain(diagnostic => diagnostic.Code == code);
    }

    [Fact]
    public async Task Type_only_interfaces_accept_exposed_constructors_and_qualified_aliases()
    {
        var build =
            await Prepare(
                """
                module Main exposing (..)
                import Time as Clock exposing (ZoneName(..))
                import Platform.Sub as Subscription exposing (Sub)

                value : Clock.ZoneName
                value = Offset 60

                subscriptions : Sub Int
                subscriptions = Subscription.none
                """);

        ElmCompiler.AnalyzeApplication(build).Should().BeEmpty();
    }

    [Fact]
    public async Task Type_only_interfaces_never_supply_executable_stubs_including_on_cache_hits()
    {
        var build = await Prepare("module Main exposing (..)\nvalue = Cmd.none\nunrelated = 42");
        var cache = new Canonicalization.DeclarationCache();

        ElmCompiler.AnalyzeApplication(build, canonicalizationCache: cache).Should().BeEmpty();

        for (var attempt = 0; attempt < 2; attempt++)
        {
            ElmCompiler.CompileResolvedEnvironment(build, [Name("value")], canonicalizationCache: cache)
                .IsErrOrNull().Should().Contain("Platform.Cmd.none")
                .And.Contain("type-only interface")
                .And.Contain("no executable implementation");
        }

        ElmCompiler.CompileResolvedEnvironment(build, [Name("unrelated")], canonicalizationCache: cache)
            .IsErrOrNull().Should().BeNull();
    }

    [Fact]
    public async Task Type_only_annotations_do_not_prevent_compiling_pure_functions()
    {
        var build =
            await Prepare(
                """
                module Main exposing (..)
                import Task

                value : Task.Task x a -> Task.Task x a
                value task = task
                """);

        ElmCompiler.CompileResolvedEnvironment(build, [Name("value")]).IsErrOrNull().Should().BeNull();
    }

    [Fact]
    public async Task Type_only_interfaces_are_available_to_strict_builds_for_implicit_annotations()
    {
        var source = "module Main exposing (..)\nvalue : Cmd msg -> Cmd msg\nvalue command = command";
        var prepared = await Prepare(source);

        var strict =
            await ElmResolvedBuildPreparation.PrepareAsync(
                Tree(
                    ("elm.json",
                    Encoding.UTF8.GetString(
                        ((FileTree.FileNode)prepared.ProjectSources.GetNodeAtPath(["elm.json"])!).Bytes.Span)),
                    ("src/Main.elm", source)),
                ["elm.json"],
                [["src", "Main.elm"]],
                ElmPackageSubstitutions.DefaultBuild.Value);

        ElmCompiler.AnalyzeApplication(strict).Should().BeEmpty();
        ElmCompiler.CompileResolvedEnvironment(strict, [Name("value")]).IsErrOrNull().Should().BeNull();
    }

    [Fact]
    public void Type_only_interfaces_preserve_executable_pure_effect_module_helpers()
    {
        var tree =
            Tree(
                ("src/Main.elm", "module Main exposing (..)\nimport Effects\nvalue = Effects.pure 41"),
                ("elm-packages/author/effects/src/Effects.elm",
                """
                effect module Effects where { subscription = MySub } exposing (pure)
                type MySub msg = MySub msg
                pure : Int -> Int
                pure value = value + 1
                """));

        var cache = new Canonicalization.DeclarationCache();
        ElmCompiler.AnalyzeApplication(tree, canonicalizationCache: cache).Should().BeEmpty();

        ElmCompiler.CompileInteractiveEnvironment(tree, [Name("value")], canonicalizationCache: cache)
            .IsErrOrNull().Should().BeNull();
    }

    [Fact]
    public void Type_only_interfaces_read_effect_module_signatures_without_claiming_kernel_implementations()
    {
        var tree =
            Tree(
                ("src/Main.elm",
                """
                module Main exposing (..)
                import Browser.Events
                import Time

                type Event = TimeArrived Time.Posix

                subscriptions : Bool -> Sub Event
                subscriptions animating =
                    if animating then
                        Browser.Events.onAnimationFrame TimeArrived
                    else
                        Time.every 200 TimeArrived
                """),
                ("elm-packages/elm/browser/src/Browser/Events.elm",
                """
                effect module Browser.Events where { subscription = MySub } exposing (onAnimationFrame)
                import Elm.Kernel.Browser
                import Time

                onAnimationFrame : (Time.Posix -> msg) -> Sub msg
                onAnimationFrame = Elm.Kernel.Browser.onAnimationFrame
                """));

        var cache = new Canonicalization.DeclarationCache();

        ElmCompiler.AnalyzeApplication(tree, canonicalizationCache: cache).Should().BeEmpty();

        var query =
            ElmCompiler.QueryDeclaration(tree, Name("subscriptions"), canonicalizationCache: cache)
            .Extract(error => throw new Exception(error));

        query.Diagnostics.Should().BeEmpty();
        query.Type.Should().NotBeNull();

        ElmCompiler.CompileInteractiveEnvironment(tree, [Name("subscriptions")], canonicalizationCache: cache)
            .IsErrOrNull().Should().Contain("Elm.Kernel.Browser").And.NotContain("effect_module_");
    }

    [Fact]
    public void Type_only_interfaces_do_not_hide_invalid_effect_module_signatures()
    {
        var tree =
            Tree(
                ("src/Main.elm", "module Main exposing (..)\nimport Effects\nvalue = Effects.value"),
                ("elm-packages/author/effects/src/Effects.elm",
                """
                effect module Effects where { subscription = MySub } exposing (value)
                import Missing

                value : Missing.Type
                value = missingImplementation
                """));

        ElmCompiler.AnalyzeApplication(tree).Should().Contain(diagnostic => diagnostic.Code == "elm-dependency");
    }

    [Fact]
    public void Type_only_interfaces_read_annotated_kernel_backed_module_APIs()
    {
        var tree =
            Tree(
                ("src/Main.elm",
                "module Main exposing (..)\nimport Browser\nvalue : Cmd Int\nvalue = Browser.command 1"),
                ("elm-packages/elm/browser/src/Browser.elm",
                """
                module Browser exposing (command)
                import Elm.Kernel.Browser
                command : Int -> Cmd Int
                command = Elm.Kernel.Browser.command
                """));

        ElmCompiler.AnalyzeApplication(tree).Should().BeEmpty();

        ElmCompiler.CompileInteractiveEnvironment(tree, [Name("value")])
            .IsErrOrNull().Should().Contain("Elm.Kernel.Browser.command");
    }

    [Fact]
    public void Type_only_interfaces_invalidate_cached_references_when_real_sources_are_supplied()
    {
        var tree = Tree(("src/Main.elm", "module Main exposing (..)\nimport Task\nvalue = Task.succeed 42"));
        var cache = new Canonicalization.DeclarationCache();

        var parses =
            new Dictionary<string, Result<Core.Elm.Syntax.Concrete.ParseError, Core.Elm.Syntax.Concrete.File>>();

        ElmCompiler.AnalyzeApplication(tree, sourceParseCache: parses, canonicalizationCache: cache).Should().BeEmpty();

        ElmCompiler.CompileInteractiveEnvironment(
            tree,
            [Name("value")],
            sourceParseCache: parses,
            canonicalizationCache: cache).IsErrOrNull().Should().Contain("type-only interface");

        var implemented =
            FileTree.MergeFiles(
                tree,
                Tree(
                    ("src/Task.elm", "module Task exposing (succeed)\nsucceed value = value")));

        ElmCompiler.CompileInteractiveEnvironment(
            implemented,
            [Name("value")],
            sourceParseCache: parses,
            canonicalizationCache: cache).IsErrOrNull().Should().BeNull();
    }

    [Fact]
    public void Type_only_interfaces_are_shared_with_unresolved_compiler_and_inference()
    {
        var tree =
            Tree(
                ("src/Main.elm",
                "module Main exposing (..)\nimport Task\nvalue = Task.succeed 42"));

        ElmCompiler.AnalyzeApplication(tree).Should().BeEmpty();

        ElmCompiler.CompileInteractiveEnvironment(tree, [Name("value")]).IsErrOrNull()
            .Should().Contain("Task.succeed").And.Contain("type-only interface");

        var environment =
            new TypeInference.FunctionInferenceEnvironment(
                ImmutableDictionary<string, TypeInference.InferredType>.Empty);

        var function =
            new Core.Elm.Syntax.Abstract.Expression.Application(
                Core.Elm.Syntax.Abstract.Expression.Identifier.Create(["Process"], "sleep"),
                [new Core.Elm.Syntax.Abstract.Expression.FloatLiteral(100, 1)]);

        var inferred = TypeInference.InferFunctionDeclarationType(function, [], "Main", environment);
        TypeInference.RenderInferredType(inferred.returnType).Should().Be("Task.Task x ()");
    }

    [Fact]
    public async Task Type_only_interfaces_respect_package_visibility_and_custom_replacements()
    {
        var build = await Prepare("module Main exposing (..)\nimport Time\nvalue = Time.now", timeDirect: false);
        build.ImportDiagnostics.Should().Contain(diagnostic => diagnostic.ImportedModuleName == "Time");
        ElmCompiler.AnalyzeApplication(build).Should().Contain(diagnostic => diagnostic.Code == "elm-name");

        var configuration = ElmPackageSubstitutions.DefaultBuild.Value;

        var custom =
            configuration.Substitutions.Single(package => package.PackageName == "elm/core") with
            {
                ImplementationId = "custom-core"
            };

        var customBuild =
            await Prepare(
                "module Main exposing (..)\nimport Task\nvalue = Task.succeed 42",
                configuration:
                configuration with
                {
                    Substitutions =
                    [
                    .. configuration.Substitutions.Where(package => package.PackageName != "elm/core"),
                    custom
                    ]
                });

        customBuild.CompilerModuleTypeInterfaces.Should().NotContainKey("Task");
        customBuild.ImportDiagnostics.Should().Contain(diagnostic => diagnostic.ImportedModuleName == "Task");
    }

    private static DeclQualifiedName Name(string name) => DeclQualifiedName.Create(["Main"], name);

    private static Task<ElmResolvedBuild> Prepare(
        string source,
        bool timeDirect = true,
        ElmDependencyResolutionConfiguration? configuration = null) =>
        ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
            Tree(
                ("elm.json",
                $$"""
                {
                    "type": "application",
                    "source-directories": ["src"],
                    "elm-version": "0.19.1",
                    "dependencies": {
                        "direct": { "elm/core": "1.0.5"{{(timeDirect ? ", \"elm/time\": \"1.0.0\"" : "")}} },
                        "indirect": { {{(timeDirect ? "" : "\"elm/time\": \"1.0.0\"")}} }
                    },
                    "test-dependencies": { "direct": {}, "indirect": {} }
                }
                """),
                ("src/Main.elm", source)),
            ["elm.json"],
            [],
            configuration ?? ElmPackageSubstitutions.DefaultBuild.Value);

    private static FileTree Tree(params (string path, string content)[] files) =>
        FileTree.FromSetOfFilesWithStringPath(
            files.Select(
                file =>
                ((IReadOnlyList<string>)file.path.Split('/'), (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(file.content))));
}
