using AwesomeAssertions;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Elm;
using Pine.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.IntegrationTests.Elm.ElmAppCompilation;

public class ElmAppCompilationComponentsTests
{
    private static readonly string s_elmAppCompilerElmJsonString =
        """"
        {
            "type": "application",
            "source-directories": [
                "src",
                "elm-syntax/src",
                "elm-syntax-encode-json/src"
            ],
            "elm-version": "0.19.1",
            "dependencies": {
                "direct": {
                    "cmditch/elm-bigint": "2.0.1",
                    "elm/bytes": "1.0.8",
                    "elm/core": "1.0.5",
                    "elm/json": "1.1.3",
                    "elm/parser": "1.1.0",
                    "elm-community/result-extra": "2.4.0"
                },
                "indirect": {
                    "elm/regex": "1.0.0",
                    "elm-community/list-extra": "8.7.0",
                    "elm-community/maybe-extra": "5.3.0",
                    "rtfeldman/elm-hex": "1.0.0"
                }
            },
            "test-dependencies": {
                "direct": {
                    "elm-explorations/test": "2.2.0"
                },
                "indirect": {
                    "elm/html": "1.0.0",
                    "elm/random": "1.0.0",
                    "elm/time": "1.0.0",
                    "elm/virtual-dom": "1.0.3"
                }
            }
        }
            
        """";

    [Fact(Skip = "TODO: Reimplement after switch to new Elm compiler")]
    public void DecodeElmJsonString_ForCompilerApp()
    {
        var elmJsonStringArgument =
            ElmValueEncoding.StringAsPineValue(s_elmAppCompilerElmJsonString);

        IReadOnlyList<PineValue> arguments =
            [
            elmJsonStringArgument
            ];

        var applyResult =
            ApplyFunctionInCompilerEnv(
                moduleName: "CompileElmApp",
                functionDeclarationName: "decodeElmJsonString",
                arguments: arguments);

        var parsedElmValue =
            PineValueAsElmValue(applyResult);

        var asString =
            ElmValue.RenderAsElmExpression(parsedElmValue);

        asString.expressionString.Should().Be("""Ok { sourceDirectories = [ "src", "elm-syntax/src", "elm-syntax-encode-json/src" ] }""");
    }

    [Fact(Skip = "TODO: Reimplement after switch to new Elm compiler")]
    public void ParseElmJsonSourceDirectoryPath_src()
    {
        IReadOnlyList<PineValue> arguments =
            [
            ElmValueEncoding.StringAsPineValue("src")
            ];

        var applyResult =
            ApplyFunctionInCompilerEnv(
                moduleName: "CompileElmApp",
                functionDeclarationName: "parseElmJsonSourceDirectoryPath",
                arguments: arguments);

        var parsedElmValue =
            PineValueAsElmValue(applyResult);

        var asString =
            ElmValue.RenderAsElmExpression(parsedElmValue);

        asString.expressionString.Should().Be("""{ parentLevel = 0, subdirectories = [ "src" ] }""");
    }

    [Fact(Skip = "TODO: Reimplement after switch to new Elm compiler")]
    public void FindSourceDirectories_forAppCompiler()
    {
        /*
        type alias AppFiles =
            List ( List String, Bytes.Bytes )

        type alias SourceDirectories =
            { mainSourceDirectoryPath : List String
            , elmJsonDirectoryPath : List String
            , secondarySourceDirectories : List (List String)
            }

        findSourceDirectories :
            { a | compilationRootFilePath : List String, sourceFiles : AppFiles }
            -> Result String SourceDirectories
        findSourceDirectories arguments =
         * */

        var sourceFiles =
            new[]
            {
                new
                {
                    filePath = (IReadOnlyList<string>)["src", "Main.elm"],
                    contentString = "module Main exposing (..)",
                },
                new
                {
                    filePath = (IReadOnlyList<string>)["elm.json"],
                    contentString = s_elmAppCompilerElmJsonString
                },
            };

        static PineValue SourceFileItemValue(IReadOnlyList<string> filePath, string contentString)
        {
            var filePathElmValue =
                PineValue.List(
                    [.. filePath.Select(ElmValueEncoding.StringAsPineValue)]);

            var contentBytesElmValue =
                ElmValueEncoding.AsElmBytesBytes(
                    System.Text.Encoding.UTF8.GetBytes(contentString));

            return
                PineValue.List(
                    [
                        filePathElmValue,
                        contentBytesElmValue
                    ]);
        }

        IReadOnlyList<PineValue> arguments =
            [
            ElmValueEncoding.ElmRecordAsPineValue(
                [
                ("compilationRootFilePath",
                PineValue.List(
                    [
                    ElmValueEncoding.StringAsPineValue("src"),
                    ElmValueEncoding.StringAsPineValue("Main.elm"),
                    ])),

                ("sourceFiles",
                PineValue.List(
                    [
                    .. sourceFiles.Select(
                        sf => SourceFileItemValue(sf.filePath, sf.contentString))
                    ]))
                ])
            ];

        var applyResult =
            ApplyFunctionInCompilerEnv(
                moduleName: "CompileElmApp",
                functionDeclarationName: "findSourceDirectories",
                arguments: arguments);

        var parsedElmValue =
            PineValueAsElmValue(applyResult);

        var asString =
            ElmValue.RenderAsElmExpression(parsedElmValue);

        asString.expressionString.Should().Be(
            """Ok { elmJsonDirectoryPath = [], mainSourceDirectoryPath = [ "src" ], secondarySourceDirectories = [ [ "elm-syntax", "src" ], [ "elm-syntax-encode-json", "src" ] ] }""");
    }

    private static ElmValue PineValueAsElmValue(PineValue pineValue) =>
        s_elmCompilerCache.PineValueDecodedAsElmValue(pineValue)
        .Extract(err => throw new Exception($"Failed to decode PineValue as ElmValue: {err}"));

    private static readonly ElmCompilerInElm s_elmCompilerInElm = LoadBundledElmCompilerInElm();

    private static readonly Dictionary<Core.Interpreter.IntermediateVM.EvalCacheEntryKey, PineValue> s_vmEvalCache = [];

    private static readonly PineVMParseCache s_parseCache = new();

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_pineVM = InitVM();

    private static readonly ElmCompilerCache s_elmCompilerCache = new();

    private static Core.Interpreter.IntermediateVM.PineVM InitVM()
    {
        return SetupVM.Create(s_vmEvalCache, parseCache: s_parseCache);
    }

    private static ElmCompilerInElm LoadBundledElmCompilerInElm()
    {
        var elmCompilerFromBundle =
            BundledElmEnvironments.BundledElmCompilerCompiledEnvValue()
            ??
            throw new Exception("Failed to load Elm compiler from bundle.");

        var elmCompiler =
            ElmCompilerInElm.ElmCompilerFromEnvValue(elmCompilerFromBundle)
            .Extract(err => throw new Exception(err));

        return elmCompiler;
    }

    private static PineValue ApplyFunctionInCompilerEnv(
        string moduleName,
        string functionDeclarationName,
        IReadOnlyList<PineValue> arguments)
    {
        var parseFunctionResult =
            ElmInteractiveEnvironment.ParseFunctionFromElmModule(
                interactiveEnvironment: s_elmCompilerInElm.CompilerEnvironment,
                moduleName: moduleName,
                declarationName: functionDeclarationName,
                s_parseCache)
            .Extract(err => throw new Exception(err));

        var applyResult =
            ElmInteractiveEnvironment.ApplyFunction(
                pineVM: s_pineVM,
                functionRecord: parseFunctionResult.functionRecord,
                arguments: arguments)
            .Extract(err => throw new Exception(err));

        return applyResult;
    }
}
