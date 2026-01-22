using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class ApplyChoiceTypeTagTests
{
    [Fact]
    public void Simple_function_applying_tag()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            type ChoiceType
                = NoArgs
                | SingleArgInt Int
                | TwoArgsStringAndInt String Int
            

            alfa : ChoiceType
            alfa =
                NoArgs


            beta : Int -> ChoiceType
            beta x =
                SingleArgInt x


            gamma : Int -> String -> ChoiceType
            gamma x s =
                TwoArgsStringAndInt s x

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.Namespaces.SequenceEqual(["Test"]),
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.alfa =
                NoArgs


            Test.beta param_1_0 =
                [ SingleArgInt
                , [ param_1_0
                  ]
                ]


            Test.gamma param_1_0 param_1_1 =
                [ TwoArgsStringAndInt
                , [ param_1_1
                  , param_1_0
                  ]
                ]
            
            """"
            .Trim());
    }
}
