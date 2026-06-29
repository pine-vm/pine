using AwesomeAssertions;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ApplicationTests;

/// <summary>
/// Regression test for a prebuild failure where compiling a declaration that
/// referenced <c>Basics.floor</c> failed with
/// <c>Function 'Basics.floor' not found in dependency layout</c>.
/// <para>
/// The <c>Basics</c> module is implemented natively in the .NET assembly (see
/// <see cref="Core.Elm.ElmCompilerInDotnet.CoreLibraryModule.CoreBasics"/>), so
/// its Elm source is not compiled and every referenced <c>Basics</c> function
/// must have a native implementation. This test mirrors the
/// <c>ElmCompiler.searchRatioForPositiveFloat</c> declaration that first
/// exercised <c>floor</c> and asserts that it compiles successfully.
/// </para>
/// </summary>
public class FloorDependencyLayoutRegressionTests
{
    private const string TestModuleText =
        """"
        module Test exposing (..)


        searchRatioForPositiveFloat : Int -> Float -> ( Int, Int )
        searchRatioForPositiveFloat denom float =
            let
                prod =
                    toFloat denom * float
            in
            if toFloat (floor prod) == prod then
                ( floor prod, denom )

            else
                searchRatioForPositiveFloat (denom + 1) float
        """";

    [Fact]
    public void Declaration_using_Basics_floor_compiles()
    {
        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [TestModuleText],
                disableInlining: false).parsedEnv;

        var functionDeclarations =
            parsedEnv.Modules
            .First(m => m.moduleName is "Test")
            .moduleContent.FunctionDeclarations;

        functionDeclarations.Keys.Should().Contain("searchRatioForPositiveFloat");
    }
}
