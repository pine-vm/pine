using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using Pine.Core.Internal;
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
        import ElmSyntax.Concrete.Parser.StringParsing as StringParsing
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
            { whitespace = StringParsing.skipInlineWhitespace "                                x" 0
            , trivia =
                case StringParsing.skipWhitespaceAt "  \u{000D}\n  value" 0 1 1 of
                    ( offset, _, _ ) ->
                        offset
            , identifier = StringParsing.skipToIdentifierEnd "identifier0123456789_rest!" 0
            , decimal = StringParsing.skipToAsciiDecimalDigitEnd "01234567890123456789x" 0
            , hexadecimal = StringParsing.skipToAsciiHexDigitEnd "0123456789abcdefABCDEFx" 0
            , number = StringParsing.numberEndDecimal "0123456789.0123e+4x" 0
            , isFloat = StringParsing.isFloatLiteralAt "0123456789.0123e+4" 0
            , hex = StringParsing.hexStringToInt "123456789abcdef"
            , unicode = StringParsing.scanUnicodeEscapeDigits "0123456789abcdefABCDEFx" 0
            , literal =
                StringParsing.findLiteralRunEnd
                    StringParsing.DoubleQuoteTermination
                    "a fairly long literal run ending here\""
                    0
            , operator = StringParsing.skipOperatorChars "+-/*=.$<>:&|^?%#!" 0 19
            , tokenCount = List.length tokens
            }


        exerciseParseFile source =
            case FromString.parseFile source of
                Ok _ ->
                    True

                Err _ ->
                    False
        """"
        ;

    private static readonly Lazy<PineValue> s_exerciseFunction =
        new(() => BuildFunction("exercise"));

    private static readonly Lazy<PineValue> s_exerciseParseFileFunction =
        new(() => BuildFunction("exerciseParseFile"));

    private const string ParsedModuleText =
        """"
        module LeafExercise exposing (..)

        import Basics exposing (..)

        {- A comment before the declarations. -}
        number = 0x1F + 42.5e10

        escaped = "lit \u{1F600} run"

        hexPattern =
            case 0x1F of
                0x1F ->
                    True

                _ ->
                    False
        """";

    [Fact]
    public void Requested_leaves_short_circuit_parser_helpers()
    {
        var enteredLeaves = new HashSet<PineValue>();

        var vmWithoutLeaves =
            CreateVM(
                ImmutableDictionary<PineValue, PrecompiledLeaf>.Empty,
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
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipInlineWhitespaceLeafKey,
            because: "the skipInlineWhitespace leaf must short-circuit its Elm implementation");

        enteredLeaves.Should().Contain(
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipWhitespaceAtLeafKey,
            because: "the location-aware whitespace scanner must short-circuit its Elm implementation");

        enteredLeaves.Should().Contain(
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipToIdentifierEndLeafKey,
            because: "the skipToIdentifierEnd leaf must short-circuit its Elm implementation");

        enteredLeaves.Should().Contain(
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipToAsciiDecimalDigitEndLeafKey,
            because: "the skipToAsciiDecimalDigitEnd leaf must short-circuit its Elm implementation");

        enteredLeaves.Should().Contain(
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipToAsciiHexDigitEndLeafKey,
            because: "the skipToAsciiHexDigitEnd leaf must short-circuit its Elm implementation");

        enteredLeaves.Should().Contain(
            ElmSyntaxConcreteParserPrecompiledLeaves.NumberEndDecimalLeafKey,
            because: "the decimal number scanner must short-circuit its Elm implementation");

        enteredLeaves.Should().Contain(
            ElmSyntaxConcreteParserPrecompiledLeaves.IsFloatLiteralAtLeafKey,
            because: "the number-kind scanner must short-circuit its Elm implementation");

        enteredLeaves.Should().Contain(
            ElmSyntaxConcreteParserPrecompiledLeaves.Convert0OrMoreHexadecimalValueLeafKey,
            because: "the hexadecimal accumulator must short-circuit its Elm implementation");

        enteredLeaves.Should().Contain(
            ElmSyntaxConcreteParserPrecompiledLeaves.ScanUnicodeEscapeDigitsLeafKey,
            because: "the scanUnicodeEscapeDigits leaf must short-circuit its Elm implementation");

        enteredLeaves.Should().Contain(
            ElmSyntaxConcreteParserPrecompiledLeaves.FindLiteralRunEndLeafKey,
            because: "the findLiteralRunEnd leaf must short-circuit its Elm implementation");

        enteredLeaves.Should().Contain(
            ElmSyntaxConcreteParserPrecompiledLeaves.SkipOperatorCharsLeafKey,
            because: "the skipOperatorChars leaf must short-circuit its Elm implementation");
    }

    [Fact]
    public void Shared_leaves_are_reached_from_parse_file()
    {
        var enteredLeaves = new HashSet<PineValue>();

        var vmWithoutLeaves =
            CreateVM(
                ImmutableDictionary<PineValue, PrecompiledLeaf>.Empty,
                null);

        var vmWithLeaves =
            CreateVM(
                IntermediateVM.SetupVM.DefaultPrecompiledLeaves,
                (leaf, _) => enteredLeaves.Add(leaf));

        var withoutLeaves = Apply(vmWithoutLeaves, s_exerciseParseFileFunction.Value, ElmValue.StringInstance(ParsedModuleText));
        var withLeaves = Apply(vmWithLeaves, s_exerciseParseFileFunction.Value, ElmValue.StringInstance(ParsedModuleText));

        withoutLeaves.value.Should().Be(ElmValue.TrueValue);
        withLeaves.value.Should().Be(ElmValue.TrueValue);

        withLeaves.counters.InstructionCount.Should().BeLessThan(withoutLeaves.counters.InstructionCount);

        // numberEndDecimal normally short-circuits its nested exponent-digit scanner.
        var leavesWithoutNumberEndDecimal =
            IntermediateVM.SetupVM.DefaultPrecompiledLeaves
            .Where(entry => !entry.Key.Equals(ElmSyntaxConcreteParserPrecompiledLeaves.NumberEndDecimalLeafKey))
            .ToImmutableDictionary();

        var vmWithNestedScanner =
            CreateVM(
                leavesWithoutNumberEndDecimal,
                (leaf, _) => enteredLeaves.Add(leaf));

        Apply(vmWithNestedScanner, s_exerciseParseFileFunction.Value, ElmValue.StringInstance(ParsedModuleText))
            .value.Should().Be(ElmValue.TrueValue);

        var reachableLeaves = new (string name, PineValue key)[]
        {
            ("skipWhitespaceAt", ElmSyntaxConcreteParserPrecompiledLeaves.SkipWhitespaceAtLeafKey),
            ("skipToIdentifierEnd", ElmSyntaxConcreteParserPrecompiledLeaves.SkipToIdentifierEndLeafKey),
            ("skipToAsciiDecimalDigitEnd", ElmSyntaxConcreteParserPrecompiledLeaves.SkipToAsciiDecimalDigitEndLeafKey),
            ("skipToAsciiHexDigitEnd", ElmSyntaxConcreteParserPrecompiledLeaves.SkipToAsciiHexDigitEndLeafKey),
            ("numberEndDecimal", ElmSyntaxConcreteParserPrecompiledLeaves.NumberEndDecimalLeafKey),
            ("isFloatLiteralAt", ElmSyntaxConcreteParserPrecompiledLeaves.IsFloatLiteralAtLeafKey),
            ("convert0OrMoreHexadecimalValue", ElmSyntaxConcreteParserPrecompiledLeaves.Convert0OrMoreHexadecimalValueLeafKey),
            ("scanUnicodeEscapeDigits", ElmSyntaxConcreteParserPrecompiledLeaves.ScanUnicodeEscapeDigitsLeafKey),
            ("findLiteralRunEnd", ElmSyntaxConcreteParserPrecompiledLeaves.FindLiteralRunEndLeafKey),
            ("skipOperatorChars", ElmSyntaxConcreteParserPrecompiledLeaves.SkipOperatorCharsLeafKey),
        };

        foreach (var (name, key) in reachableLeaves)
        {
            enteredLeaves.Should().Contain(
                key,
                because: $"{name} must be reached through FromString.parseFile");
        }
    }

    [Fact]
    public void String_scanner_leaves_do_not_allocate_proportional_to_source_length()
    {
        var enteredLeafEnvironments = new Dictionary<PineValue, PineValueInProcess>();

        var vm =
            CreateVM(
                IntermediateVM.SetupVM.DefaultPrecompiledLeaves,
                (leaf, environment) => enteredLeafEnvironments.TryAdd(leaf, environment));

        _ = Apply(vm);

        var scanners =
            new (PineValue leafKey, PrecompiledLeaf scanner)[]
            {
                (ElmSyntaxConcreteParserPrecompiledLeaves.SkipInlineWhitespaceLeafKey,
                ElmSyntaxConcreteParserPrecompiledLeaves.SkipInlineWhitespaceLeafDelegate),
                (ElmSyntaxConcreteParserPrecompiledLeaves.SkipToIdentifierEndLeafKey,
                ElmSyntaxConcreteParserPrecompiledLeaves.SkipToIdentifierEndLeafDelegate),
            };

        foreach (var (leafKey, scanner) in scanners)
        {
            var environment =
                (PineValue.ListValue)enteredLeafEnvironments[leafKey].Evaluate();

            var shortEnvironment = EnvironmentWithSource(environment, "!");
            var longEnvironment = EnvironmentWithSource(environment, "!" + new string('a', 100_000));
            var shortEnvironmentInProcess = PineValueInProcess.Create(shortEnvironment);
            var longEnvironmentInProcess = PineValueInProcess.Create(longEnvironment);

            scanner(shortEnvironmentInProcess)?.Evaluate()
                .Should().Be(scanner(longEnvironmentInProcess)?.Evaluate());

            const int invocationCount = 100;

            var shortAllocatedBefore = GC.GetAllocatedBytesForCurrentThread();

            for (var invocation = 0; invocation < invocationCount; ++invocation)
            {
                _ = scanner(shortEnvironmentInProcess);
            }

            var shortAllocatedBytes =
                GC.GetAllocatedBytesForCurrentThread() - shortAllocatedBefore;

            var longAllocatedBefore = GC.GetAllocatedBytesForCurrentThread();

            for (var invocation = 0; invocation < invocationCount; ++invocation)
            {
                _ = scanner(longEnvironmentInProcess);
            }

            var longAllocatedBytes =
                GC.GetAllocatedBytesForCurrentThread() - longAllocatedBefore;

            longAllocatedBytes.Should().BeLessThanOrEqualTo(shortAllocatedBytes + 1_024);
        }
    }

    private static PineValue EnvironmentWithSource(
        PineValue.ListValue environment,
        string source)
    {
        var items = environment.Items.ToArray();

        items[1] =
            ElmValueEncoding.ElmValueAsPineValue(
                ElmValue.StringInstance(source));

        items[2] =
            ElmValueEncoding.ElmValueAsPineValue(
                ElmValue.Integer(0));

        return PineValue.List(items);
    }

    private static PineValue BuildFunction(string declarationName)
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
            .moduleContent.FunctionDeclarations[declarationName];
    }

    private static Core.Interpreter.IntermediateVM.PineVM CreateVM(
        IReadOnlyDictionary<PineValue, PrecompiledLeaf> precompiledLeaves,
        Action<PineValue, PineValueInProcess>? reportEnterPrecompiledLeaf) =>
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
        Apply(vm, s_exerciseFunction.Value);

    private static (ElmValue value, PerformanceCounters counters) Apply(
        Core.Interpreter.IntermediateVM.PineVM vm,
        PineValue function,
        ElmValue? argument = null) =>
        CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
            function,
            argument ?? ElmValue.Integer(0),
            vm);
}
