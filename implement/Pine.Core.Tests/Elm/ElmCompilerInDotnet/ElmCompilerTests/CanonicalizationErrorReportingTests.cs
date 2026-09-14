using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class CanonicalizationErrorReportingTests
{
    [Fact]
    public void Effect_module_command_errors_are_reported_separately_with_import_provenance()
    {
        var rootModule =
            """
            module Root exposing (main)

            import Task

            main =
                Task.perform 1
            """;

        var taskModule =
            """
            effect module Task where { command = MyCmd } exposing (perform, attempt)

            type MyCmd msg = MyCmd

            perform value =
                command MyCmd

            attempt value =
                command MyCmd
            """;

        var appCodeTree =
            TestCase.FileTreeFromElmModulesWithoutPackages([rootModule, taskModule]);

        IReadOnlyList<IReadOnlyList<string>> rootFilePaths =
            [["src", "Root.elm"]];

        var result =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths);

        var error = result.IsErrOrNull();

        error.Should().NotBeNull();
        error.Should().Contain("Elm canonicalization failed with 2 errors.");
        error.Should().Contain("Cannot resolve reference 'command' at src/Task.elm:6:5-6:12");
        error.Should().Contain("Cannot resolve reference 'command' at src/Task.elm:9:5-9:12");
        error.Should().Contain("2. Task (src/Task.elm) — explicit import at 3:1");
    }

    [Fact]
    public void Unresolved_reference_reports_source_location_and_dependency_chain()
    {
        var rootModule =
            """
            module Root exposing (main)

            import Intermediate

            main =
                Intermediate.value
            """;

        var intermediateModule =
            """
            module Intermediate exposing (value)

            import Broken as Dependency exposing (value)

            value =
                Dependency.value
            """;

        var brokenModule =
            """
            module Broken exposing (value)

            value =
                command
            """;

        var appCodeTree =
            TestCase.FileTreeFromElmModulesWithoutPackages(
                [rootModule, intermediateModule, brokenModule]);

        IReadOnlyList<IReadOnlyList<string>> rootFilePaths =
            [["src", "Root.elm"]];

        var result =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths);

        var error = result.IsErrOrNull();

        error.Should().NotBeNull();
        error.Should().Contain("Cannot resolve reference 'command' at src/Broken.elm:4:5-4:12");
        error.Should().Contain("Dependency chain from a compilation root:");
        error.Should().Contain("1. Root (src/Root.elm)");
        error.Should().Contain("2. Intermediate (src/Intermediate.elm) — explicit import at 3:1");

        error.Should().Contain(
            "3. Broken (src/Broken.elm) — explicit import at 3:1 as Dependency exposing (value)");

        error.Should().Contain(
            "searched for 'command' while canonicalizing 'Broken' because that module is reachable from root 'Root'");
    }
}
