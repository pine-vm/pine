using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using System.Collections.Immutable;
using Xunit;

using ElmSyntaxAbstract = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class DeclarationDependencyFilteringTests
{
    [Fact]
    public void Lowering_input_excludes_declarations_not_reachable_from_root_modules()
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
            ElmCompiler.LowerToElmSyntaxForCompilation<
                ImmutableDictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration>>(
                appCodeTree,
                rootFilePaths: [["src", "Main.elm"]],
                lower: (declarations, _) => declarations,
                extractFilteredDeclarations: declarations => declarations)
            .Extract(error => throw new System.Exception(error));

        result.Lowered.Keys.Should().Contain(DeclQualifiedName.Create(["Main"], "main"));
        result.Lowered.Keys.Should().Contain(DeclQualifiedName.Create(["Supporting"], "used"));
        result.Lowered.Keys.Should().NotContain(DeclQualifiedName.Create(["Supporting"], "unused"));
    }
}
