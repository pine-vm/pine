using AwesomeAssertions;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ApplicationTests;

/// <summary>
/// Regression test for compiling declarations that transitively depend on
/// <c>Basics.ceiling</c>, such as <c>BigInt.maxDigitBits</c>.
/// </summary>
public class CeilingDependencyLayoutRegressionTests
{
    private const string TestModuleText =
        """"
        module Test exposing (..)


        roundUp : Float -> Int
        roundUp value =
            ceiling value
        """";

    [Fact]
    public void Declaration_using_Basics_ceiling_compiles()
    {
        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [TestModuleText],
                disableInlining: false).parsedEnv;

        var functionDeclarations =
            parsedEnv.Modules
            .First(m => m.moduleName is "Test")
            .moduleContent.FunctionDeclarations;

        functionDeclarations.Keys.Should().Contain("roundUp");
    }
}
