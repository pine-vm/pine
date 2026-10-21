using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class DeclarationDependencyFilteringTests
{
    [Fact]
    public void Lowering_input_excludes_declarations_not_reachable_from_root_declarations()
    {
        var appCodeTree =
            TestCase.DefaultAppWithoutPackages(
                [
                    """
                    module Main exposing (..)

                    import Supporting

                    main =
                        Supporting.used
                    """,
                    """
                    module Supporting exposing (..)

                    used =
                        42

                    unused =
                        13
                    """
                ])
            .AsFileTree();

        var result =
            ElmCompiler.LowerToCompilerModel(
                appCodeTree,
                rootDeclarations: [DeclQualifiedName.Create(["Main"], "main")])
            .Extract(error => throw new System.Exception(error));

        result.CanonicalDeclarations.Keys.Should().Contain(DeclQualifiedName.Create(["Main"], "main"));
        result.CanonicalDeclarations.Keys.Should().Contain(DeclQualifiedName.Create(["Supporting"], "used"));
        result.CanonicalDeclarations.Keys.Should().NotContain(DeclQualifiedName.Create(["Supporting"], "unused"));
    }
}
