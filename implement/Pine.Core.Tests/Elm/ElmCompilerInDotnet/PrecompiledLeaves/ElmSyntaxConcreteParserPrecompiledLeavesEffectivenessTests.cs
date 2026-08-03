using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.PrecompiledLeaves;

public class ElmSyntaxConcreteParserPrecompiledLeavesEffectivenessTests
{
    private const string TestModuleText =
        """"
        module ElmSyntaxConcreteParserPrecompiledLeavesTestModule exposing (..)

        import ElmSyntax.Concrete.Parser.FromString as FromString
        import ElmSyntax.Concrete.Parser.TokensFromString as TokensFromString


        exercise _ =
            let
                tokens =
                    case TokensFromString.parseExpression "{- first -}{- second -}identifier" of
                        Ok parsed ->
                            parsed

                        Err _ ->
                            []
            in
            { whitespace = TokensFromString.skipInlineWhitespace "                                x" 0
            , identifier = TokensFromString.skipToIdentifierEnd "identifier0123456789_rest!" 0
            , decimal = TokensFromString.skipToAsciiDecimalDigitEnd "01234567890123456789x" 0
            , hexadecimal = TokensFromString.skipToAsciiHexDigitEnd "0123456789abcdefABCDEFx" 0
            , unicode = TokensFromString.scanUnicodeEscapeDigits "0123456789abcdefABCDEFx" 0
            , literal =
                TokensFromString.findLiteralRunEnd
                    TokensFromString.DoubleQuoteTermination
                    "a fairly long literal run ending here\""
                    0
            , operator = TokensFromString.skipOperatorChars "+-/*=.$<>:&|^?%#!" 0 19
            , withoutTrivia = FromString.dropTrivia tokens
            , lexemes = FromString.tokenLexemes tokens
            , parsedHex = FromString.hexStringToInt "123456789abcdef"
            , parsedHexLeadingZero = FromString.hexStringToInt "0F"
            , parsedHexLeadingZeroBeforeInvalid = FromString.hexStringToInt "0g"
            }
        """"
        ;

    private static readonly Lazy<PineValue> s_exerciseFunction =
        new(BuildExerciseFunction);

    [Fact]
    public void Requested_leaves_short_circuit_parser_helpers()
    {
        var enteredLeaves = new HashSet<PineValue>();

        var vmWithoutLeaves =
            CreateVM(
                ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty,
                null);

        var vmWithLeaves =
            CreateVM(
                IntermediateVM.SetupVM.DefaultPrecompiledLeaves,
                (leaf, _) => enteredLeaves.Add(leaf));

        var withoutLeaves = Apply(vmWithoutLeaves);
        var withLeaves = Apply(vmWithLeaves);

        ElmValue.RenderAsElmExpression(withLeaves.value).expressionString
            .Should().Be(ElmValue.RenderAsElmExpression(withoutLeaves.value).expressionString);

        withLeaves.counters.InstructionCount.Should().BeLessThan(withoutLeaves.counters.InstructionCount);
        withLeaves.counters.InvocationCount.Should().BeLessThan(withoutLeaves.counters.InvocationCount);

        enteredLeaves.Should().Contain(
            [
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipInlineWhitespaceLeafKey,
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipToIdentifierEndLeafKey,
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipToAsciiDecimalDigitEndLeafKey,
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipToAsciiHexDigitEndLeafKey,
            ElmSyntaxConcreteParserPrecompiledLeaves.ScanUnicodeEscapeDigitsLeafKey,
            ElmSyntaxConcreteParserPrecompiledLeaves.FindLiteralRunEndLeafKey,
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipOperatorCharsLeafKey,
            ElmSyntaxConcreteParserPrecompiledLeaves.DropTriviaLeafKey,
            ElmSyntaxConcreteParserPrecompiledLeaves.TokenLexemesLeafKey,
            ElmSyntaxConcreteParserPrecompiledLeaves.HexStringToIntLeafKey,
            ]);
    }

    private static PineValue BuildExerciseFunction()
    {
        var mergedTree = BundledFiles.ElmKernelModulesDefault.Value;
        var compilerSourceTree = BundledFiles.CompilerSourceContainerFilesDefault.Value;

        var parserSourceTree =
            compilerSourceTree.GetNodeAtPath(["pine-elm-syntax", "src"])
            ?? throw new Exception("Did not find pine-elm-syntax/src");

        foreach (var (path, file) in parserSourceTree.EnumerateFilesTransitive())
        {
            mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
        }

        mergedTree =
            mergedTree.SetNodeAtPathSorted(
                ["ElmSyntaxConcreteParserPrecompiledLeavesTestModule.elm"],
                FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                mergedTree,
                rootFilePaths: [["ElmSyntaxConcreteParserPrecompiledLeavesTestModule.elm"]])
            .Map(result => result.compiledEnvValue)
            .Extract(error => throw new Exception("Failed compiling: " + error));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(error => throw new Exception("Failed parsing: " + error));

        return
            parsedEnv.Modules
            .First(module => module.moduleName is "ElmSyntaxConcreteParserPrecompiledLeavesTestModule")
            .moduleContent.FunctionDeclarations["exercise"];
    }

    private static Core.Interpreter.IntermediateVM.PineVM CreateVM(
        IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> precompiledLeaves,
        Action<PineValue, PineValue>? reportEnterPrecompiledLeaf) =>
        Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
            evalCache: null,
            evaluationConfigDefault: null,
            reportFunctionApplication: _ => { },
            compilationEnvClasses: null,
            disableReductionInCompilation: true,
            selectPrecompiled: null,
            skipInlineForExpression: _ => false,
            enableTailRecursionOptimization: false,
            parseCache: null,
            precompiledLeaves: precompiledLeaves,
            reportEnterPrecompiledLeaf: reportEnterPrecompiledLeaf,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null);

    private static (ElmValue value, PerformanceCounters counters) Apply(
        Core.Interpreter.IntermediateVM.PineVM vm) =>
        CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
            s_exerciseFunction.Value,
            ElmValue.Integer(0),
            vm);
}
