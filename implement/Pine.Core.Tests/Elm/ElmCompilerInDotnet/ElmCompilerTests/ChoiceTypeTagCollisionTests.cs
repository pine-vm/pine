using AwesomeAssertions;
using Pine.Core.Elm;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

/// <summary>
/// Tests that the compiler correctly handles multiple modules defining
/// choice type constructors with the same unqualified name but different
/// argument counts. This verifies no cross-module tag collision occurs
/// in the <c>ChoiceTagArgumentTypes</c> dictionary.
/// </summary>
public class ChoiceTypeTagCollisionTests
{
    [Fact]
    public void Same_tag_name_different_arg_counts_across_modules()
    {
        var moduleA =
            """
            module ModuleA exposing (..)


            type StepA a
                = Good a Int
                | Bad String


            matchGood : StepA Int -> Int
            matchGood step =
                case step of
                    Good value _ ->
                        value

                    Bad _ ->
                        -1


            makeGood : Int -> StepA Int
            makeGood n =
                Good n 0
            """;

        var moduleB =
            """
            module ModuleB exposing (..)


            type StepB a
                = Good Bool a Int
                | Bad Bool String


            matchGood : StepB Int -> Int
            matchGood step =
                case step of
                    Good _ value _ ->
                        value

                    Bad _ _ ->
                        -2


            makeGood : Int -> StepB Int
            makeGood n =
                Good True n 0
            """;

        var moduleTest =
            """
            module TestModule exposing (..)

            import ModuleA
            import ModuleB


            testModuleA : Int -> Int
            testModuleA n =
                ModuleA.matchGood (ModuleA.makeGood n)


            testModuleB : Int -> Int
            testModuleB n =
                ModuleB.matchGood (ModuleB.makeGood n)
            """;

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [moduleA, moduleB, moduleTest],
                disableInlining: true);

        var vm = ElmCompilerTestHelper.PineVMForProfiling(_ => { });

        // Test Module A (Good with 2 args) - construct and match
        {
            var funcValue =
                parsedEnv.Modules
                .First(m => m.moduleName is "TestModule")
                .moduleContent.FunctionDeclarations["testModuleA"];

            var (result, _) =
                CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                    funcValue,
                    ElmValue.Integer(42),
                    vm);

            result.Should().Be(ElmValue.Integer(42));
        }

        // Test Module B (Good with 3 args) - construct and match
        {
            var funcValue =
                parsedEnv.Modules
                .First(m => m.moduleName is "TestModule")
                .moduleContent.FunctionDeclarations["testModuleB"];

            var (result, _) =
                CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
                    funcValue,
                    ElmValue.Integer(99),
                    vm);

            result.Should().Be(ElmValue.Integer(99));
        }
    }
}
