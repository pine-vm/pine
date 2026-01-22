using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class RemoveUnusedTests
{
    [Fact]
    public void Remove_unused_declarations_over_chain_of_let_blocks_but_utilize_contained_type_constraints()
    {
        /*
         * The type annotation on `beta` constrains the argument to 'Int' and the compiler must pick
         * up this typing information, but still remove the unused instantiations of 'beta'.
         * */

        var elmModuleText =
            """"
            module Test exposing (..)


            alfa x =
                let
                    xa =
                        beta (x + 7)

                    xb =
                        beta (x + 13)
                in
                let
                    xc =
                        beta (xa + xb)
                in
                x * 21


            beta : Int -> Int
            beta x =
                case x of
                    0 ->
                        41

                    13 ->
                        43

                    _ ->
                        47

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Pine_builtin.int_mul
                    [ param_1_0
                    , 21
                    ]
            
            """"
            .Trim());
    }
}
