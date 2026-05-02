using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

/// <summary>
/// Tests for the <see cref="ReportExpressionCompiled"/> callback of
/// <see cref="Core.Interpreter.IntermediateVM.PineVM"/>.
/// <para />
/// The callback is the per-entry signal of a "dispatch table snapshot": the VM
/// compiles each expression to IR (stack-frame instructions) the first time it
/// is evaluated, then caches the result. Recording every notification yields
/// the set of expressions currently cached for execution.
/// </summary>
public class ExpressionCompiledCallbackTests
{
    [Fact]
    public void Callback_fires_once_for_root_expression_compiled_to_IR()
    {
        var compiled = new List<ExpressionCompiled>();

        var vm =
            Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: null,
                compilationEnvClasses: null,
                disableReductionInCompilation: true,
                selectPrecompiled: null,
                skipInlineForExpression: _ => false,
                enableTailRecursionOptimization: false,
                parseCache: null,
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null,
                reportExpressionCompiled:
                (in expressionCompiled) =>
                compiled.Add(expressionCompiled));

        var expression =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.int_add),
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(3)),
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(4))
                    ]));

        var result = vm.EvaluateExpression(expression, PineValue.EmptyBlob);

        result.IsOkOrNull().Should().Be(IntegerEncoding.EncodeSignedInteger(7));

        // The root expression has been compiled to IR exactly once.
        compiled.Should().HaveCount(1);

        var only = compiled[0];

        only.Expression.Should().Be(expression);
        only.Compilation.Should().NotBeNull();
        only.Compilation.Generic.Should().NotBeNull();
        only.Compilation.Generic.Instructions.Should().NotBeEmpty();

        // The hash is a 64-character lower-case hex string (32-byte hash).
        only.ExpressionHashBase16.Should().HaveLength(64);
        only.ExpressionHashBase16.Should().MatchRegex("^[0-9a-f]+$");
    }

    [Fact]
    public void Callback_fires_once_per_unique_expression_compiled()
    {
        // Evaluate two distinct root expressions on the same VM. Each unique
        // expression should be compiled to IR exactly once and reported once.
        var compiled = new List<ExpressionCompiled>();

        var vm =
            Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: null,
                compilationEnvClasses: null,
                disableReductionInCompilation: true,
                selectPrecompiled: null,
                skipInlineForExpression: _ => false,
                enableTailRecursionOptimization: false,
                parseCache: null,
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null,
                reportExpressionCompiled:
                (in expressionCompiled) =>
                compiled.Add(expressionCompiled));

        var exprA =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.int_add),
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(2))
                    ]));

        var exprB =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.int_mul),
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(3)),
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(4))
                    ]));

        vm.EvaluateExpression(exprA, PineValue.EmptyBlob)
            .IsOkOrNull().Should().Be(IntegerEncoding.EncodeSignedInteger(3));

        vm.EvaluateExpression(exprB, PineValue.EmptyBlob)
            .IsOkOrNull().Should().Be(IntegerEncoding.EncodeSignedInteger(12));

        // Re-evaluate exprA: it must NOT be reported again because its
        // compilation is already cached.
        vm.EvaluateExpression(exprA, PineValue.EmptyBlob)
            .IsOkOrNull().Should().Be(IntegerEncoding.EncodeSignedInteger(3));

        compiled.Select(item => item.Expression).Should().Equal([exprA, exprB]);

        // Different expressions must yield different hashes.
        compiled[0].ExpressionHashBase16.Should().NotBe(compiled[1].ExpressionHashBase16);
    }

    [Fact]
    public void Callback_fires_for_nested_expression_compiled_on_first_evaluation()
    {
        // The root expression is a ParseAndEval that, on evaluation, parses an
        // inner expression and dispatches to it. The VM must compile both the
        // root expression and the inner expression to IR — the latter on first
        // dispatch — and report each compilation exactly once.
        var compiled = new List<ExpressionCompiled>();

        var vm =
            Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: null,
                compilationEnvClasses: null,
                disableReductionInCompilation: true,
                selectPrecompiled: null,
                skipInlineForExpression: _ => false,
                enableTailRecursionOptimization: false,
                parseCache: null,
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null,
                reportExpressionCompiled:
                (in expressionCompiled) =>
                compiled.Add(expressionCompiled));

        var nestedEnvironment = IntegerEncoding.EncodeSignedInteger(7);

        var rootExpression =
            new Expression.ParseAndEval(
                encoded:
                Expression.LiteralInstance(
                    ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance)),
                environment:
                Expression.LiteralInstance(nestedEnvironment));

        var result = vm.EvaluateExpression(rootExpression, PineValue.EmptyBlob);

        result.IsOkOrNull().Should().Be(nestedEnvironment);

        var compiledExpressions = compiled.Select(item => item.Expression).ToList();

        compiledExpressions.Should().Contain(
            rootExpression,
            "the root expression is compiled to IR on first evaluation");

        compiledExpressions.Should().Contain(
            Expression.EnvironmentInstance,
            "the inner expression dispatched via ParseAndEval is compiled to IR on its first invocation");

        // Each unique expression compiled exactly once: no duplicates.
        compiledExpressions.Distinct().Should().HaveCount(compiledExpressions.Count);
    }

    [Fact]
    public void Callback_is_not_invoked_when_no_callback_is_provided()
    {
        // Sanity check: a VM with no reportExpressionCompiled callback must
        // still evaluate correctly without throwing.
        var vm =
            Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: null,
                compilationEnvClasses: null,
                disableReductionInCompilation: true,
                selectPrecompiled: null,
                skipInlineForExpression: _ => false,
                enableTailRecursionOptimization: false,
                parseCache: null,
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null);

        var expression =
            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(11));

        vm.EvaluateExpression(expression, PineValue.EmptyBlob)
            .IsOkOrNull().Should().Be(IntegerEncoding.EncodeSignedInteger(11));
    }
}
