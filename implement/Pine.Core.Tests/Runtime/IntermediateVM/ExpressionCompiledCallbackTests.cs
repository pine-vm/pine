using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using Xunit;

using Retained = Pine.Core.Runtime.InternalExpression.Expression;
using Conversion = Pine.Core.Runtime.InternalExpression.ExpressionConversion;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

/// <summary>
/// Tests for the <see cref="ReportExpressionCompiled"/> callback of
/// <see cref="IntermediateVM.PineVM"/>.
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
            Core.Runtime.IntermediateVM.PineVM.CreateCustom(
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
            Expression.BuiltinInst(
                nameof(BuiltinFunction.int_add),
                Expression.ListInst(
                    [
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(3)),
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(4))
                    ]));

        var result = vm.EvaluateExpression(expression, PineValue.EmptyBlob);

        result.IsOkOrNull().Should().Be(IntegerEncoding.EncodeSignedInteger(7));

        // The root expression has been compiled to IR exactly once.
        compiled.Should().HaveCount(1);

        var only = compiled[0];

        only.Identity.IsCanonicalExpressionCreated.Should().BeFalse();
        only.Identity.IsEncodedValueCreated.Should().BeFalse();
        only.Identity.IsHashCreated.Should().BeFalse();
        only.Expression.Should().Be(expression);
        only.Identity.IsEncodedValueCreated.Should().BeFalse();
        only.Identity.IsHashCreated.Should().BeFalse();
        only.Compilation.Should().NotBeNull();
        only.Compilation.Generic.Should().NotBeNull();
        only.Compilation.Generic.Instructions.Should().NotBeEmpty();

        // The hash is a 64-character lower-case hex string (32-byte hash).
        only.ExpressionHashBase16.Should().HaveLength(64);
        only.ExpressionHashBase16.Should().MatchRegex("^[0-9a-f]+$");

        only.ExpressionHashBase16.Should().Be(
            Convert.ToHexStringLower(
                PineValueHashFlat.ComputeHashForValue(ExpressionEncoding.EncodeExpressionAsValue(expression))
                .hashBytes.Span));
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void Execution_and_counter_only_observers_defer_canonical_views_in_custom_caches(
        bool observe, bool overrideCompilation)
    {
        var compilations = new Dictionary<Retained, ExpressionCompilationCacheEntry>();
        var encodings = new PineVMExpressionEncodingCache();
        var identities = new List<ExpressionIdentity>();
        var applicationReports = new List<EvaluationReport>();
        var child = Expression.BuiltinInst("length", Expression.EnvironmentInstance);
        var childValue = ExpressionEncoding.EncodeExpressionAsValue(child);
        var root = new Expression.Eval(Expression.LitralInst(childValue), Expression.EnvironmentInstance);
        var retainedRoot = Conversion.FromCanonicalCached(root);

        var overrides =
            new Dictionary<Retained, ExpressionCompilation>
            {
                [retainedRoot] =
                new(
                    new StackFrameInstructions(
                        StaticFunctionInterface.Generic,
                        [
                        StackInstruction.Local_Get(0),
                        StackInstruction.Eval_Const(childValue),
                        StackInstruction.Return
                        ]),
                    [])
            };

        Core.Runtime.IntermediateVM.PineVM CreateVm() =>
            Core.Runtime.IntermediateVM.PineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: observe ? report => applicationReports.Add(report) : null,
                compilationEnvClasses: null,
                disableReductionInCompilation: true,
                selectPrecompiled:
                (identity, _, _) =>
                {
                    identities.Add(identity);
                    return null;
                },
                skipInlineForExpression:
                identity =>
                {
                    identities.Add(identity);
                    return true;
                },
                enableTailRecursionOptimization: false,
                parseCache: null,
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null,
                expressionCompilationOverrides: overrideCompilation ? overrides : null,
                disableDirectContinueForSimpleEval: true,
                disableDirectEvalForSimpleTemplate: true,
                reportExpressionCompiled: observe ? (in item) => identities.Add(item.Identity) : null,
                reportExecutedStackInstruction: observe ? (in item) => identities.Add(item.Identity) : null,
                reportEvaluationEvent:
                observe
                ?
                (in item) =>
                {
                    identities.Add(item.Identity);
                    _ = item.LoadCounters();
                    identities.AddRange(item.LoadStackTrace().Select(frame => frame.Identity));
                }
        :
                null,
                tryGetExpressionCompilation:
                (Retained expression, out ExpressionCompilationCacheEntry entry) =>
                compilations.TryGetValue(expression, out entry),
                getOrAddExpressionCompilation:
                (expression, build) =>
                {
                    if (!compilations.TryGetValue(expression, out var entry))
                        compilations.Add(expression, entry = build());

                    return entry;
                },
                expressionEncodingCache: encodings);

        for (var index = 0; index < 2; ++index)
        {
            var report =
                CreateVm().EvaluateExpressionOnCustomStack(
                    root,
                    PineValue.List([PineValue.EmptyBlob, PineValue.EmptyList]),
                    Core.Runtime.IntermediateVM.PineVM.EvaluationConfig.Unbounded,
                    observe ? (in item) => identities.Add(item.Identity) : null)
                .Extract(error => throw new Exception(error.ToString()));

            report.ReturnValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(2));
            identities.Add(report.Identity);
            identities.AddRange(report.ExpressionStackTrace);
        }

        compilations.Should().HaveCount(2);
        applicationReports.Should().HaveCount(observe ? 2 : 0);
        identities.AddRange(compilations.Values.Select(entry => entry.Identity));
        identities.AddRange(applicationReports.Select(report => report.Identity));
        identities.AddRange(applicationReports.SelectMany(report => report.ExpressionStackTrace));

        foreach (var identity in identities)
        {
            identity.IsCanonicalExpressionCreated.Should().BeFalse();
            identity.IsEncodedValueCreated.Should().BeFalse();
            identity.IsHashCreated.Should().BeFalse();
        }

        encodings.GetIdentity(retainedRoot).CanonicalExpression.Should().Be(root);
        encodings.GetIdentity(retainedRoot).IsEncodedValueCreated.Should().BeFalse();
        encodings.GetIdentity(retainedRoot).IsHashCreated.Should().BeFalse();
    }

    [Fact]
    public void Structured_error_snapshots_do_not_materialize_canonical_views()
    {
        using var cancellation = new CancellationTokenSource();
        var snapshots = new List<EvaluationStackTraceFrame>();

        var vm =
            Core.Runtime.IntermediateVM.PineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: null,
                compilationEnvClasses: null,
                disableReductionInCompilation: true,
                selectPrecompiled: null,
                skipInlineForExpression: null,
                enableTailRecursionOptimization: false,
                parseCache: null,
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null,
                reportEvaluationEvent:
                (in item) =>
                {
                    snapshots.AddRange(item.LoadStackTrace());
                    cancellation.Cancel();
                });

        var expression = new Expression.Eval(Expression.EnvironmentInstance, Expression.EnvironmentInstance);

        var error =
            vm.EvaluateExpressionOnCustomStack(
                expression, PineValue.EmptyList,
                Core.Runtime.IntermediateVM.PineVM.EvaluationConfig.Unbounded,
                cancellation.Token)
            .IsErrOrNull()!;

        error.Reason.Should().BeOfType<EvaluationErrorReason.CancellationRequested>();
        snapshots.AddRange(error.StackTrace);
        snapshots.Should().NotBeEmpty();

        foreach (var frame in snapshots)
        {
            frame.Identity.IsCanonicalExpressionCreated.Should().BeFalse();
            frame.Identity.IsEncodedValueCreated.Should().BeFalse();
            frame.Identity.IsHashCreated.Should().BeFalse();
        }

        error.StackTrace[0].Expression.Should().Be(expression);
    }

    [Fact]
    public void Invocation_reports_preserve_distinct_encodings_sharing_one_compilation()
    {
        var reports = new List<EvaluationReport>();
        var compilations = new ConcurrentExpressionCompilationCache();

        var child =
            Expression.ConditionalInst(
                Expression.EnvironmentInstance,
                Expression.LitralInst(PineValue.EmptyBlob),
                Expression.LitralInst(PineValue.EmptyList));

        var canonicalValue = ExpressionEncoding.EncodeExpressionAsValue(child);

        var aliasValue =
            PineValue.List(
                [
                    StringEncoding.ValueFromString("Condition"),
                    ExpressionEncoding.EncodeExpressionAsValue(child.Condition),
                    ExpressionEncoding.EncodeExpressionAsValue(child.FalseBranch),
                    ExpressionEncoding.EncodeExpressionAsValue(child.TrueBranch)
                ]);

        var root =
            new Expression.Eval(
                Expression.EnvironmentInstance,
                Expression.LitralInst(PineKernelValues.TrueValue));

        var vm =
            Core.Runtime.IntermediateVM.PineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: reports.Add,
                compilationEnvClasses: null,
                disableReductionInCompilation: true,
                selectPrecompiled: null,
                skipInlineForExpression: null,
                enableTailRecursionOptimization: false,
                parseCache: null,
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null,
                disableDirectContinueForSimpleEval: true,
                disableDirectEvalForSimpleTemplate: true,
                tryGetExpressionCompilation: compilations.TryGet,
                getOrAddExpressionCompilation: compilations.GetOrAdd);

        foreach (var encoded in new[] { canonicalValue, aliasValue })
            vm.EvaluateExpression(root, encoded).IsOkOrNull().Should().Be(PineValue.EmptyList);

        compilations.Count.Should().Be(2);
        reports.Should().HaveCount(2);
        reports[0].Identity.InternalExpression.Should().Be(reports[1].Identity.InternalExpression);

        foreach (var report in reports)
        {
            report.Identity.IsCanonicalExpressionCreated.Should().BeFalse();
            report.Identity.IsEncodedValueCreated.Should().BeFalse();
            report.Identity.IsHashCreated.Should().BeFalse();
        }

        reports[0].ExpressionValue.Should().BeSameAs(canonicalValue);
        reports[1].ExpressionValue.Should().BeSameAs(aliasValue);
        reports[0].Identity.HashBase16.Should().NotBe(reports[1].Identity.HashBase16);
    }

    [Fact]
    public void Callback_fires_once_per_unique_expression_compiled()
    {
        // Evaluate two distinct root expressions on the same VM. Each unique
        // expression should be compiled to IR exactly once and reported once.
        var compiled = new List<ExpressionCompiled>();

        var vm =
            Core.Runtime.IntermediateVM.PineVM.CreateCustom(
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
            Expression.BuiltinInst(
                nameof(BuiltinFunction.int_add),
                Expression.ListInst(
                    [
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(1)),
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(2))
                    ]));

        var exprB =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.int_mul),
                Expression.ListInst(
                    [
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(3)),
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(4))
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
            Core.Runtime.IntermediateVM.PineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: null,
                compilationEnvClasses: null,
                disableReductionInCompilation: true,
                disableDirectEvalForSimpleTemplate: true,
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
            new Expression.Eval(
                encoded:
                Expression.LitralInst(
                    ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance)),
                environment:
                Expression.LitralInst(nestedEnvironment));

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
            Core.Runtime.IntermediateVM.PineVM.CreateCustom(
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
            Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(11));

        vm.EvaluateExpression(expression, PineValue.EmptyBlob)
            .IsOkOrNull().Should().Be(IntegerEncoding.EncodeSignedInteger(11));
    }

    [Fact]
    public void Shared_cache_reuses_compilation_across_VM_instances()
    {
        var cache = new ConcurrentExpressionCompilationCache();
        var compilationCount = 0;

        Core.Runtime.IntermediateVM.PineVM CreateVm() =>
            Core.Runtime.IntermediateVM.PineVM.CreateCustom(
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
                (in _) =>
                compilationCount++,
                tryGetExpressionCompilation: cache.TryGet,
                getOrAddExpressionCompilation: cache.GetOrAdd);

        var expression =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.int_add),
                Expression.ListInst(
                    [
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(5)),
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(8))
                    ]));

        CreateVm().EvaluateExpression(expression, PineValue.EmptyBlob)
            .IsOkOrNull().Should().Be(IntegerEncoding.EncodeSignedInteger(13));

        CreateVm().EvaluateExpression(expression, PineValue.EmptyBlob)
            .IsOkOrNull().Should().Be(IntegerEncoding.EncodeSignedInteger(13));

        compilationCount.Should().Be(1);
    }

    [Fact]
    public void Shared_auxiliary_caches_survive_VM_replacement()
    {
        var sharedCaches = new PineVMSharedCaches();

        Core.Runtime.IntermediateVM.PineVM CreateVm() =>
            Core.Runtime.IntermediateVM.PineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: null,
                compilationEnvClasses: null,
                disableReductionInCompilation: false,
                selectPrecompiled: null,
                skipInlineForExpression: _ => false,
                enableTailRecursionOptimization: false,
                parseCache: sharedCaches.ParsedExpressions,
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null,
                expressionEncodingCache: sharedCaches.EncodedExpressions,
                reducedExpressionCache: sharedCaches.ReducedExpressions);

        var expression =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.int_add),
                Expression.ListInst(
                    [
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(21)),
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(34))
                    ]));

        CreateVm().EvaluateExpression(expression, PineValue.EmptyBlob)
            .IsOkOrNull().Should().Be(IntegerEncoding.EncodeSignedInteger(55));

        var encodedExpressionCount = sharedCaches.EncodedExpressions.Count;
        var reducedExpressionCount = sharedCaches.ReducedExpressions.Count;

        encodedExpressionCount.Should().BeGreaterThan(0);
        reducedExpressionCount.Should().BeGreaterThan(0);

        CreateVm().EvaluateExpression(expression, PineValue.EmptyBlob)
            .IsOkOrNull().Should().Be(IntegerEncoding.EncodeSignedInteger(55));

        sharedCaches.EncodedExpressions.Count.Should().Be(encodedExpressionCount);
        sharedCaches.ReducedExpressions.Count.Should().Be(reducedExpressionCount);
    }
}
