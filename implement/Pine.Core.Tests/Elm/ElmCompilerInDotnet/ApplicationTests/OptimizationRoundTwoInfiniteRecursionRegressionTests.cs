using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Xunit;

using Stil4mFromFull = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel;
using Stil4mToFull = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.ToFullSyntaxModel;
using SyntaxModel = Pine.Core.Elm.ElmSyntax.SyntaxModel;
using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ApplicationTests;

/// <summary>
/// Regression tests for a defect in the second iteration of the optimization
/// pipeline. The defect surfaces when the pipeline is run with
/// <c>maxOptimizationRounds &gt;= 2</c> over Elm code that uses string-literal
/// parsing from the elm-syntax library.
///
/// <para>
/// <b>Symptom on the compiled-to-PineVM path.</b> Evaluating
/// <c>Elm.Parser.Expression.expression</c> on an input that contains a string
/// literal (e.g. <c>"\"hello world\""</c>) terminates with:
/// <code>
/// Failed eval: Detected infinite recursion.
/// Cycle length: 1 invocation (stack-growing cycle).
/// Cycle expressions: [0] 3e207948
/// </code>
/// The same input parses successfully when <c>maxOptimizationRounds = 1</c>,
/// so the defect is introduced by the second optimization iteration's transform
/// of one of the post-round-0 declarations.
/// </para>
///
/// <para>
/// <b>Why we also reproduce via the syntax interpreter.</b> The PineVM error
/// only carries an opaque expression hash (<c>3e207948</c>), which makes it
/// hard to map back to the offending Elm declaration. The syntax interpreter
/// (<see cref="ElmSyntaxInterpreter"/>) walks the post-pipeline declaration
/// dictionary directly with surface-language pattern-match discipline, so the
/// same underlying defect surfaces as a structured
/// <see cref="ElmInterpretationError"/> with an Elm-level call stack that
/// names the offending function and prints its argument values. Concretely,
/// the interpreter pinpoints
/// <c>Elm.Parser.Tokens.loopUntilHelp__specialized__2</c> as the function
/// whose recursive call passes a closure where the parser-state record
/// <c>PState</c> is expected — surfacing as
/// <c>Cannot bind named pattern 'PState' to value of type
/// Pine.Core.Elm.ElmValue+ElmFunction</c>. The PineVM cannot detect this
/// (Pine has no runtime type checking) and only reports the resulting
/// stack-growing cycle.
/// </para>
///
/// <para>
/// <b>Bisection.</b> The three <c>..._after_iteration1_*</c> tests interpret
/// the per-sub-stage snapshots in <c>OptimizationIterations[Round=1]</c>.
/// They establish that the defect is introduced by the
/// <em>specialization sub-stage of the second iteration</em>: that snapshot
/// already fails, and the subsequent higher-order-inlining and
/// size-based-inlining sub-stages neither introduce nor mitigate the defect.
/// See
/// <c>explore/internal-analysis/2026-04-25-max-optimization-rounds-2-string-literal-infinite-recursion.md</c>
/// for a longer write-up.
/// </para>
/// </summary>
public class OptimizationRoundTwoInfiniteRecursionRegressionTests
{
    /// <summary>
    /// Minimal test module that exercises string literal parsing via
    /// <c>ParserFast.loopUntil</c>, which internally uses <c>loopUntilHelp</c>.
    /// </summary>
    private const string TestModuleText =
        """"
        module TestModule exposing (..)

        import Elm.Parser.Expression
        import Elm.Syntax.Expression exposing (Expression(..))
        import Elm.Syntax.Node exposing (Node(..))
        import ParserFast
        import ParserWithComments exposing (WithComments(..))


        parseExpression : String -> Result String Expression
        parseExpression exprText =
            case ParserFast.run Elm.Parser.Expression.expression exprText of
                Err deadEnds ->
                    Err ("parse-failed:" ++ String.fromInt (List.length deadEnds))

                Ok (WithComments _ nodeExpr) ->
                    Ok (Elm.Syntax.Node.value nodeExpr)


        parseStringLiteral : String -> String
        parseStringLiteral input =
            case parseExpression input of
                Ok (Literal s) ->
                    s

                _ ->
                    ""
        """";

    private static readonly Core.Interpreter.IntermediateVM.PineVM s_vm =
        ElmCompilerTestHelper.PineVMForProfiling(_ => { });

    /// <summary>
    /// Builds the compiled environment with the specified <paramref name="maxOptimizationRounds"/>.
    /// Uses the standard bundled elm-syntax sources plus the test module.
    /// </summary>
    private static ElmInteractiveEnvironment.ParsedInteractiveEnvironment BuildEnv(
        int maxOptimizationRounds)
    {
        var bundledTree = BundledFiles.CompilerSourceContainerFilesDefault.Value;

        var kernelModulesTree =
            BundledFiles.ElmKernelModulesDefault.Value;

        var elmSyntaxSrcTree =
            bundledTree.GetNodeAtPath(["elm-syntax", "src"])
            ?? throw new Exception("Did not find elm-syntax/src");

        var mergedTree = kernelModulesTree;

        foreach (var (path, file) in elmSyntaxSrcTree.EnumerateFilesTransitive())
            mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));

        var treeWithTest =
            mergedTree.SetNodeAtPathSorted(
                ["TestModule.elm"],
                FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

        var rootFilePaths =
            treeWithTest.EnumerateFilesTransitive()
            .Where(b => b.path[^1].Equals("TestModule.elm", StringComparison.OrdinalIgnoreCase))
            .Select(b => (IReadOnlyList<string>)b.path)
            .ToList();

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                treeWithTest,
                rootFilePaths: rootFilePaths,
                disableInlining: false,
                maxOptimizationRounds: maxOptimizationRounds)
            .Map(r => r.compiledEnvValue)
            .Extract(err => throw new Exception("Failed compiling: " + err));

        return
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new Exception("Failed parsing: " + err));
    }

    /// <summary>
    /// <b>This test is expected to FAIL on the current branch</b> because the
    /// bug is present. It demonstrates the infinite recursion when
    /// <c>maxOptimizationRounds=2</c> is used.
    /// <para>
    /// When the bug is fixed, this test will pass.
    /// </para>
    /// </summary>
    [Fact]
    public void String_literal_parsing_with_maxOptimizationRounds_2_does_not_infinitely_recurse()
    {
        var env = BuildEnv(maxOptimizationRounds: 2);

        var testFunc =
            env.Modules
            .First(m => m.moduleName is "TestModule")
            .moduleContent.FunctionDeclarations["parseStringLiteral"];

        var parseCache = new PineVMParseCache();

        var funcRecord =
            FunctionRecord.ParseFunctionRecordTagged(testFunc, parseCache)
            .Extract(err => throw new Exception("Failed to parse function record: " + err));

        var evalArgs =
            ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(
                funcRecord,
                [ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("\"hello world\""))])
            .Extract(err => throw new Exception(err.ToString()));

        // This call throws with "Detected infinite recursion. Cycle length: 1"
        // when the bug is present.
        var result =
            s_vm.EvaluateExpressionOnCustomStack(
                evalArgs.expression,
                evalArgs.environment,
                config: ElmCompilerTestHelper.DefaultTestEvaluationConfig)
            .Extract(err => throw new Exception("Failed eval: " + err))
            .ReturnValue.Evaluate();

        // When the bug is fixed, we expect the result to be the parsed string content
        var resultElm =
            ElmValueEncoding.PineValueAsElmValue(result, null, null)
            .Extract(err => throw new Exception("Failed to decode: " + err));

        // The parseStringLiteral function returns just the string content
        // But parseExpression internally returns Result String Expression
        // Looking at the Elm code, parseStringLiteral extracts the string from the Literal case
        // Actually, the result is Ok with a string if parsing succeeded
        resultElm.Should().Be(ElmValue.StringInstance("hello world"));
    }

    /// <summary>
    /// Control test: verifies that string literal parsing works correctly with
    /// <c>maxOptimizationRounds=1</c>. This confirms that the bug is specifically
    /// introduced by Round 1 of optimization (i.e., the second iteration).
    /// </summary>
    [Fact]
    public void String_literal_parsing_with_maxOptimizationRounds_1_succeeds()
    {
        var env = BuildEnv(maxOptimizationRounds: 1);

        var testFunc =
            env.Modules
            .First(m => m.moduleName is "TestModule")
            .moduleContent.FunctionDeclarations["parseStringLiteral"];

        var parseCache = new PineVMParseCache();

        var funcRecord =
            FunctionRecord.ParseFunctionRecordTagged(testFunc, parseCache)
            .Extract(err => throw new Exception("Failed to parse function record: " + err));

        var evalArgs =
            ElmInteractiveEnvironment.ApplyFunctionArgumentsForEvalExpr(
                funcRecord,
                [ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance("\"hello world\""))])
            .Extract(err => throw new Exception(err.ToString()));

        var result =
            s_vm.EvaluateExpressionOnCustomStack(
                evalArgs.expression,
                evalArgs.environment,
                config: ElmCompilerTestHelper.DefaultTestEvaluationConfig)
            .Extract(err => throw new Exception("Failed eval: " + err))
            .ReturnValue.Evaluate();

        var resultElm =
            ElmValueEncoding.PineValueAsElmValue(result, null, null)
            .Extract(err => throw new Exception("Failed to decode: " + err));

        // parseStringLiteral returns the parsed string directly (not wrapped in Ok)
        resultElm.Should().Be(ElmValue.StringInstance("hello world"));
    }

    /// <summary>
    /// Reproduces the same defect via <see cref="ElmSyntaxInterpreter"/> (no
    /// compile-to-PineVM step): compiles the test corpus with
    /// <c>maxOptimizationRounds=2</c>, takes the post-pipeline declaration
    /// dictionary, and invokes <c>parseStringLiteral "\"hello world\""</c>
    /// directly through the syntax interpreter.
    ///
    /// <para>
    /// On the current branch this test is expected to FAIL with an
    /// <see cref="ElmInterpretationError"/> whose message says
    /// <c>"Infinite recursion detected: the call stack contains a repeated
    /// (function, arguments) pair."</c>, followed by an Elm-level call stack
    /// that names the exact post-pipeline function whose body forms the cycle
    /// (and prints the argument values it cycles on). Compared with the PineVM
    /// path's opaque <c>3e207948</c> expression hash, this lets a reader pin
    /// the offending declaration without any extra instrumentation.
    /// </para>
    ///
    /// <para>
    /// The <see cref="ElmInterpretationError.ToString"/> rendering is
    /// included in the assertion failure exception via
    /// <see cref="Result{ErrT, OkT}.Extract(Func{ErrT, OkT})"/>'s
    /// <c>err =&gt; throw new Exception(err.ToString())</c> wrapper, so a
    /// failed run prints the full call stack to the test log. When the
    /// underlying defect is fixed, this test will pass.
    /// </para>
    /// </summary>
    [Fact]
    public void String_literal_parsing_via_syntax_interpreter_after_round_two_does_not_infinitely_recurse()
    {
        var (tree, pipelineStageResults) = CompileTestCorpus(maxOptimizationRounds: 2);

        AssertParseStringLiteralSucceedsAgainst(
            tree,
            pipelineStageResults.Inlined!);
    }

    /// <summary>
    /// Control test for
    /// <see cref="String_literal_parsing_via_syntax_interpreter_after_round_two_does_not_infinitely_recurse"/>:
    /// runs the same input through the syntax interpreter against the
    /// post-pipeline declaration dictionary produced with
    /// <c>maxOptimizationRounds=1</c>. A pass here confirms that the
    /// declaration set produced after a single optimization round is
    /// semantically correct, so the defect is introduced strictly by the
    /// transforms applied during the second iteration.
    /// </summary>
    [Fact]
    public void String_literal_parsing_via_syntax_interpreter_after_round_one_succeeds()
    {
        var (tree, pipelineStageResults) = CompileTestCorpus(maxOptimizationRounds: 1);

        AssertParseStringLiteralSucceedsAgainst(
            tree,
            pipelineStageResults.Inlined!);
    }

    /// <summary>
    /// Bisection test #1 for the round-1 (i.e. second-iteration) defect:
    /// interprets the post-pipeline declaration dictionary captured AFTER
    /// the second iteration's specialization phase, but BEFORE its
    /// higher-order-inlining and size-based-inlining phases. A pass here
    /// would rule out specialization as the introducer of the bad rewrite;
    /// a failure would localise the defect to that sub-stage.
    /// </summary>
    [Fact]
    public void String_literal_parsing_via_syntax_interpreter_after_iteration1_specialization()
    {
        var (tree, pipelineStageResults) = CompileTestCorpus(maxOptimizationRounds: 2);

        var iterations =
            pipelineStageResults.OptimizationIterations
            ?? throw new Exception("Pipeline did not record per-iteration results");

        var iter1 =
            iterations.FirstOrDefault(i => i.Round is 1)
            ?? throw new Exception("Pipeline did not run a second iteration (round 1)");

        AssertParseStringLiteralSucceedsAgainst(tree, iter1.AfterSpecialization);
    }

    /// <summary>
    /// Bisection test #2: as
    /// <see cref="String_literal_parsing_via_syntax_interpreter_after_iteration1_specialization"/>,
    /// but takes the snapshot AFTER higher-order inlining (still within the
    /// second iteration). Combined with the specialization snapshot above
    /// this brackets the offending rewrite to a single sub-stage.
    /// </summary>
    [Fact]
    public void String_literal_parsing_via_syntax_interpreter_after_iteration1_higher_order_inlining()
    {
        var (tree, pipelineStageResults) = CompileTestCorpus(maxOptimizationRounds: 2);

        var iterations =
            pipelineStageResults.OptimizationIterations
            ?? throw new Exception("Pipeline did not record per-iteration results");

        var iter1 =
            iterations.FirstOrDefault(i => i.Round is 1)
            ?? throw new Exception("Pipeline did not run a second iteration (round 1)");

        AssertParseStringLiteralSucceedsAgainst(tree, iter1.AfterHigherOrderInlining);
    }

    /// <summary>
    /// Bisection test #3: snapshot taken AFTER size-based inlining of the
    /// second iteration (i.e. the end of round 1). If the previous two
    /// bisection tests pass and this one fails, the defect is in the
    /// size-based inlining sub-stage of the second iteration.
    /// </summary>
    [Fact]
    public void String_literal_parsing_via_syntax_interpreter_after_iteration1_size_based_inlining()
    {
        var (tree, pipelineStageResults) = CompileTestCorpus(maxOptimizationRounds: 2);

        var iterations =
            pipelineStageResults.OptimizationIterations
            ?? throw new Exception("Pipeline did not record per-iteration results");

        var iter1 =
            iterations.FirstOrDefault(i => i.Round is 1)
            ?? throw new Exception("Pipeline did not run a second iteration (round 1)");

        AssertParseStringLiteralSucceedsAgainst(tree, iter1.AfterSizeBasedInlining);
    }

    /// <summary>
    /// Shared assertion for the syntax-interpreter-based reproductions:
    /// overlays the natively-implemented (and pipeline-stripped) <c>Basics</c>
    /// declarations onto <paramref name="stageDict"/>, converts the
    /// stage-format declarations to the syntax-model form expected by
    /// <see cref="ElmSyntaxInterpreter"/>, and asserts that
    /// <c>TestModule.parseStringLiteral "\"hello world\""</c> evaluates to
    /// the Elm string <c>"hello world"</c>. On failure the
    /// <see cref="ElmInterpretationError.ToString"/> rendering — which
    /// includes the named Elm call stack and rendered argument values — is
    /// raised through <see cref="Result{ErrT, OkT}.Extract(Func{ErrT, OkT})"/>
    /// so test-log readers see the offending function and arguments
    /// directly.
    /// </summary>
    private static void AssertParseStringLiteralSucceedsAgainst(
        FileTree tree,
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> stageDict)
    {
        var declarations = BuildBaseDeclarationsFromNativelyImplementedModules(tree);

        foreach (var (key, stil4mDecl) in stageDict)
        {
            declarations[key] = Stil4mToFull.Convert(stil4mDecl);
        }

        var functionName =
            new DeclQualifiedName(["TestModule"], "parseStringLiteral");

        var result =
            ElmSyntaxInterpreter.Interpret(
                functionName,
                [ElmValue.StringInstance("\"hello world\"")],
                declarations);

        var value =
            result.Extract(err => throw new Exception(err.ToString()));

        value.Should().Be(ElmValue.StringInstance("hello world"));
    }

    /// <summary>
    /// Compiles the bundled elm-syntax + kernel sources together with the
    /// regression test module via
    /// <see cref="ElmCompiler.CompileInteractiveEnvironment"/> with the
    /// requested <paramref name="maxOptimizationRounds"/>, returning the
    /// bundled <see cref="FileTree"/> (so the syntax-interpreter overlay can
    /// re-canonicalize natively-implemented modules) and the
    /// <see cref="CompilationPipelineStageResults"/>.
    /// </summary>
    private static (FileTree tree, CompilationPipelineStageResults results)
        CompileTestCorpus(int maxOptimizationRounds)
    {
        var bundledTree = BundledFiles.CompilerSourceContainerFilesDefault.Value;

        var kernelModulesTree =
            BundledFiles.ElmKernelModulesDefault.Value;

        var elmSyntaxSrcTree =
            bundledTree.GetNodeAtPath(["elm-syntax", "src"])
            ?? throw new Exception("Did not find elm-syntax/src");

        var mergedTree = kernelModulesTree;

        foreach (var (path, file) in elmSyntaxSrcTree.EnumerateFilesTransitive())
            mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));

        var treeWithTest =
            mergedTree.SetNodeAtPathSorted(
                ["TestModule.elm"],
                FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

        var rootFilePaths =
            treeWithTest.EnumerateFilesTransitive()
            .Where(b => b.path[^1].Equals("TestModule.elm", StringComparison.OrdinalIgnoreCase))
            .Select(b => (IReadOnlyList<string>)b.path)
            .ToList();

        var (_, pipelineStageResults) =
            ElmCompiler.CompileInteractiveEnvironment(
                treeWithTest,
                rootFilePaths: rootFilePaths,
                disableInlining: false,
                maxOptimizationRounds: maxOptimizationRounds)
            .Extract(err => throw new Exception("Failed compiling: " + err));

        return (treeWithTest, pipelineStageResults);
    }

    /// <summary>
    /// Parses + canonicalizes every Elm source file under
    /// <c>elm-kernel-modules</c> in <paramref name="tree"/> that is natively
    /// implemented by the compiler (currently just <c>Basics</c>). Those
    /// modules are stripped from every stage of
    /// <see cref="CompilationPipelineStageResults"/>, but the syntax
    /// interpreter still needs their declarations to resolve references such
    /// as <c>Basics.identity</c> that the optimized declaration dictionary
    /// emits. Mirrors the helper in
    /// <c>ElmParserExpressionStageInterpretationTests</c>.
    /// </summary>
    private static Dictionary<DeclQualifiedName, SyntaxModel.Declaration>
        BuildBaseDeclarationsFromNativelyImplementedModules(FileTree tree)
    {
        var kernelNode = tree.GetNodeAtPath(["elm-kernel-modules"]) ?? tree;

        var parsedFiles = new List<SyntaxTypes.File>();

        foreach (var (_, fileContent) in kernelNode.EnumerateFilesTransitive())
        {
            var moduleText = Encoding.UTF8.GetString(fileContent.Span);

            var headerResult = ElmSyntaxParser.ParseModuleHeader(moduleText);

            if (headerResult.IsOkOrNull() is not { } header)
                continue;

            var moduleNameFlat = string.Join(".", header.ModuleName);

            // Currently only "Basics" is natively implemented and stripped.
            if (moduleNameFlat is not "Basics")
                continue;

            var parseResult = ElmSyntaxParser.ParseModuleText(moduleText);

            if (parseResult.IsOkOrNull() is { } parsedFile)
                parsedFiles.Add(Stil4mFromFull.Convert(parsedFile));
        }

        if (parsedFiles.Count is 0)
            return [];

        var canonicalizeResult = Canonicalization.CanonicalizeAllowingErrors(parsedFiles);

        if (canonicalizeResult.IsOkOrNull() is not { } canonicalized)
            return [];

        var declarations = new Dictionary<DeclQualifiedName, SyntaxModel.Declaration>();

        foreach (var (moduleNameKey, (canonicalizedFile, _, _)) in canonicalized)
        {
            var fullModuleFile = Stil4mToFull.Convert(canonicalizedFile);

            var moduleNameParts = moduleNameKey.ToList();

            foreach (var declNode in fullModuleFile.Declarations)
            {
                if (declNode.Value is SyntaxModel.Declaration.InfixDeclaration infixDecl)
                {
                    declarations[new DeclQualifiedName(moduleNameParts, infixDecl.Infix.Operator.Value)] =
                        declNode.Value;

                    continue;
                }

                var declName = DeclarationSimpleName(declNode.Value);

                if (declName is null)
                    continue;

                declarations[new DeclQualifiedName(moduleNameParts, declName)] = declNode.Value;
            }
        }

        return declarations;
    }

    private static string? DeclarationSimpleName(SyntaxModel.Declaration declaration) =>
        declaration switch
        {
            SyntaxModel.Declaration.FunctionDeclaration functionDeclaration =>
            functionDeclaration.Function.Declaration.Value.Name.Value,

            SyntaxModel.Declaration.AliasDeclaration aliasDeclaration =>
            aliasDeclaration.TypeAlias.Name.Value,

            SyntaxModel.Declaration.ChoiceTypeDeclaration choiceTypeDeclaration =>
            choiceTypeDeclaration.TypeDeclaration.Name.Value,

            _ =>
            null,
        };
}
