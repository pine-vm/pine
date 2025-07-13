using ElmTime.ElmSyntax;
using AwesomeAssertions;
using Pine.Core;
using System.Collections.Generic;
using System.Collections.Immutable;
using Xunit;

namespace Pine.IntegrationTests;

public class ElmSyntaxTests
{
    [Fact]
    public void Parse_Elm_module_name()
    {
        var testCases = new[]
        {
            new
            {
                moduleText = @"module TestModule exposing (..)",

                expectedModuleName =
                (IReadOnlyList<string>)["TestModule"]
            },

            new
            {
                moduleText =
                """
                port module LanguageServiceWorker exposing (..)
                """,
                expectedModuleName =
                (IReadOnlyList<string>)["LanguageServiceWorker"]
            },

            new
            {
                moduleText =
                """
                module Elm.JsArray
                    exposing
                        ( JsArray
                        , empty
                        , singleton
                        , length
                        , initialize
                        , initializeFromList
                        , unsafeGet
                        , unsafeSet
                        , push
                        , foldl
                        , foldr
                        , map
                        , indexedMap
                        , slice
                        , appendN
                        )
                """,

                expectedModuleName =
                (IReadOnlyList<string>)["Elm", "JsArray"]
            },

            new
            {
                moduleText =
                """
                {-| Multi-line comment
                -}

                module Test exposing ( .. )
                """,

                expectedModuleName =
                (IReadOnlyList<string>)["Test"]
            },

            new
            {
                moduleText =
                """
                effect module Task where { command = MyCmd } exposing
                  ( Task
                  , succeed, fail
                  , map, map2, map3, map4, map5
                  , sequence
                  , andThen
                  , onError, mapError
                  , perform, attempt
                  )
                """,

                expectedModuleName =
                (IReadOnlyList<string>)["Task"]
            },
        };

        foreach (var testCase in testCases)
        {
            var parseModuleNameResult = ElmModule.ParseModuleName(testCase.moduleText);

            if (parseModuleNameResult.IsErrOrNull() is { } err)
            {
                // Using FluentAssertion's approach to fail a test
                "Failed test".Should().BeNull("Failed to parse module name: " + err + "\nmodule text:\n" + testCase.moduleText);
            }

            if (parseModuleNameResult.IsOkOrNull() is not { } parsedName)
            {
                // Using FluentAssertion's approach to fail a test
                "Failed test".Should().BeNull("Unknown result type: " + parseModuleNameResult);
                return;
            }

            parsedName.Should().BeEquivalentTo(testCase.expectedModuleName, "Module name");
        }
    }

    [Fact]
    public void Parse_Elm_module_name_ignores_string_literal_content()
    {
        var moduleText =
            """"
            -- module TestModule exposing (..)

            test =
                [ ""
                , """
            module Bytes.Decode exposing (..)

            """ ]

                
            """";

        var parseModuleNameResult =
            ElmModule.ParseModuleName(moduleText);

        parseModuleNameResult.IsErr().Should().BeTrue("Expected error");
    }

    [Fact]
    public void Parse_Elm_module_text_imports()
    {
        var testCases = new[]
        {
            new
            {
                moduleText = @"module TestModule exposing (..)",
                expectedImports = System.Array.Empty<IReadOnlyList<string>>()
            },

            new
            {
                moduleText = """
                module TestModule exposing (..)

                import Basics exposing (Bool, Int)
                import Dict
                import List exposing ((::))
                import Maybe exposing (Maybe(..))
                """,

                expectedImports = new IReadOnlyList<string>[]
                {
                    ["Basics"],
                    ["Dict"],
                    ["List"],
                    ["Maybe"]
                }
            },

            new
            {
                moduleText = """"
                module TestModule exposing (..)

                import Dict

                d = """
                import List exposing ((::))
                import Maybe exposing (Maybe(..))
                """

                """",

                expectedImports = new IReadOnlyList<string>[]
                {
                    ["Dict"],
                }
            },

            new
            {
                moduleText = """"
                module TestModule exposing (..)

                import Dict

                {-
                    import List exposing ((::))
                    import Maybe exposing (Maybe(..))
                -}

                """",

                expectedImports = new IReadOnlyList<string>[]
                {
                    ["Dict"],
                }
            },
        };

        foreach (var testCase in testCases)
        {
            var parsedImports =
                ElmModule.ParseModuleImportedModulesNames(testCase.moduleText)
                .ToImmutableHashSet(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

            parsedImports.Should().BeEquivalentTo(
                testCase.expectedImports,
                config => config.Using(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>()));
        }
    }

    [Fact]
    public void Enumerate_module_lines()
    {
        var testCases = new[]
            {
                new
                {
                    moduleText =
                    "",

                    expectedLines =
                    (IReadOnlyList<string>)[""]
                },

                new
                {
                    moduleText =
                    "focaccia\narancino",

                    expectedLines =
                    (IReadOnlyList<string>)["focaccia","arancino"]
                },

                new
                {
                    moduleText =
                    "focaccia\narancino\n",

                    expectedLines =
                    (IReadOnlyList<string>)["focaccia","arancino",""]
                },

                new
                {
                    moduleText =
                    "focaccia\rarancino",

                    expectedLines =
                    (IReadOnlyList<string>)["focaccia","arancino"]
                },

                new
                {
                    moduleText =
                    "focaccia\r\narancino",

                    expectedLines =
                    (IReadOnlyList<string>)["focaccia","arancino"]
                },

                new
                {
                    moduleText =
                    "focaccia\n\rarancino",

                    expectedLines =
                    (IReadOnlyList<string>)["focaccia","arancino"]
                },

                new
                {
                    moduleText =
                    "focaccia\n\narancino",

                    expectedLines =
                    (IReadOnlyList<string>)["focaccia","","arancino"]
                },

                new
                {
                    moduleText =
                    "focaccia\n\r\narancino",

                    expectedLines =
                    (IReadOnlyList<string>)["focaccia","","arancino"]
                },

                new
                {
                    moduleText =
                    "focaccia\r\n\rarancino",

                    expectedLines =
                    (IReadOnlyList<string>)["focaccia","","arancino"]
                },
            };

        foreach (var testCase in testCases)
        {
            IReadOnlyList<string> lines =
                [.. ElmModule.ModuleLines(moduleText: testCase.moduleText)];

            try
            {
                lines.Count.Should().Be(testCase.expectedLines.Count, "Number of lines");

                lines.Should().BeEquivalentTo(testCase.expectedLines, "Lines contents");
            }
            catch (System.Exception ex)
            {
                throw new System.Exception(
                    "Failed for test case:\n" + testCase.moduleText,
                    ex);
            }
        }
    }
}
