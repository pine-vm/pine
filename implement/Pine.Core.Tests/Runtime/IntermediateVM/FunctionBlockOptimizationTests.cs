using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using Pine.Core.Runtime.IntermediateVM;
using Pine.Core.Runtime.InternalExpression;
using System;
using System.IO;
using System.Linq;
using Xunit;

using Canonical = Pine.Core.PineLanguage.Expression;
using Intrinsic = Pine.Core.Runtime.InternalExpression.BuiltinFunction;
using Layout = Pine.Core.Runtime.InternalExpression.FunctionBlockLowering.EnvironmentLayout;
using Retained = Pine.Core.Runtime.InternalExpression.Expression;
using VM = Pine.Core.Runtime.IntermediateVM.PineVM;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class FunctionBlockOptimizationTests(ITestOutputHelper output)
{
    [Fact]
    public void Recursive_choice_marker_pruning_snapshot_and_work_counters()
    {
        var initial = Program(call => new Retained.HeadSkip(2, call));
        var before = ExpressionConversion.ToCanonical(initial, Layout.Packed);
        var cache = new PineVMParseCache();
        var lifted = FunctionBlockAdapters.TryLift(ExpressionConversion.FromCanonical(before), cache);
        lifted.Should().NotBeNull();
        var optimized = FunctionBlockOptimization.Optimize(lifted!);

        var proof = optimized.Proofs.Should().ContainSingle().Subject;
        proof.Functions.Should().Equal(0, 1);
        proof.Producers.Should().Be(3);
        proof.Projections.Should().Be(2);

        optimized.Block.Should().Be(Program(call => new Retained.HeadSkip(1, call), pruned: true));

        var after = ExpressionConversion.ToCanonical(optimized.Block, Layout.Packed);
        var beforeVM = MakeVM();
        var afterVM = MakeVM();

        foreach (var (depth, value) in new[] { (0, 42), (7, 100), (8, 101) })
        {
            var environment = PineValue.List([Int(depth), Int(value)]);
            Interpret(before, environment).Should().Be(Int(value));
            Interpret(after, environment).Should().Be(Int(value));

            var unoptimized = Run(beforeVM, before, environment);
            var pruned = Run(afterVM, after, environment);

            output.WriteLine(
                $"Depth {depth}: list slots {unoptimized.BuildListItemCount} -> {pruned.BuildListItemCount}; " +
                $"instructions {unoptimized.InstructionCount} -> {pruned.InstructionCount}");

            unoptimized.ReturnValue.Evaluate().Should().Be(Int(value));
            pruned.ReturnValue.Evaluate().Should().Be(Int(value));
            pruned.BuildListItemCount.Should().BeLessThan(unoptimized.BuildListItemCount, $"at depth {depth}");
            pruned.InstructionCount.Should().BeLessThanOrEqualTo(unoptimized.InstructionCount, $"at depth {depth}");
        }
    }

    [Fact]
    public void Recursive_return_prefix_is_not_pruned_when_observed_or_budget_is_exhausted()
    {
        foreach (var block in new[]
        {
            Program(call => call),
            Program(call => new Retained.HeadSkip(0, call)),
            Program(call => Retained.BuiltinInst(Intrinsic.Length, call)),
            Program(call => new Retained.List([new Retained.HeadSkip(2, call), call]))
        })
        {
            var result = FunctionBlockOptimization.Optimize(block);
            result.Proofs.Should().BeEmpty();
            result.Block.Should().BeSameAs(block);
        }

        var candidate = Program(call => new Retained.HeadSkip(2, call));
        var budget = FunctionBlockOptimization.Optimize(candidate, new(MaximumAnalysisSteps: 5));
        budget.BudgetExhausted.Should().BeTrue();
        budget.Block.Should().BeSameAs(candidate);

        FunctionBlockOptimization.Optimize(candidate, publicFunctions: [1]).Proofs.Should().BeEmpty();
    }

    [Fact]
    public void Backend_preparation_applies_the_pass_before_stack_compilation()
    {
        var initial = Program(call => new Retained.HeadSkip(2, call));
        var canonical = ExpressionConversion.ToCanonical(initial, Layout.Packed);
        var environment = PineValue.List([Int(8), Int(42)]);

        var actual = Run(MakeVM(reduce: true), canonical, environment);
        var unoptimized = Run(MakeVM(), canonical, environment);

        actual.ReturnValue.Evaluate().Should().Be(Int(42));
        (unoptimized.BuildListItemCount - actual.BuildListItemCount).Should().Be(5);
        actual.InstructionCount.Should().BeLessThanOrEqualTo(unoptimized.InstructionCount);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Literal_branch_results_are_forwarded_before_constructing_the_cfg(bool compareWithFalse)
    {
        var env = Retained.Environment.Instance;

        var result =
            new Retained.Conditional(
                env,
                new Retained.Literal(PineKernelValues.FalseValue),
                new Retained.Literal(PineKernelValues.TrueValue));

        Retained predicate =
            compareWithFalse
            ?
            new Retained.BuiltinBinary(Intrinsic.Equal, new Retained.Literal(PineKernelValues.FalseValue), result)
            :
            result;

        var expression = ExpressionConversion.ToCanonical(new Retained.Conditional(predicate, Number(3), Number(7)));

        var rewritten =
            FunctionBlockOptimization.ForwardBranchPredicates(ExpressionConversion.FromCanonical(expression));

        rewritten.ConditionCount.Should().Be(1);
        var baseline = MakeVM();
        var optimized = MakeVM(reduce: true);

        foreach (var input in new[] { PineKernelValues.TrueValue, PineKernelValues.FalseValue, PineValue.EmptyList })
        {
            var before = Run(baseline, expression, input);
            var after = Run(optimized, expression, input);

            after.ReturnValue.Evaluate().Should().Be(Interpret(expression, input));
            after.Counters.Should().Be(before.Counters);
        }
    }

    [Fact]
    public void Scoped_switch_targets_are_selected_at_the_predicate_producer()
    {
        var env = Retained.Environment.Instance;
        var predicate = new Retained.Call(0, env);
        var selector = new Retained.Conditional(predicate, Number(5), Number(7));
        var fallback = new Retained.Call(1, Number(10));
        var firstMatch = new Retained.Call(1, Number(20));
        var shadowed = new Retained.Call(1, Number(30));

        var dispatch =
            Switch(
                selector,
                fallback,
                (Int(7), firstMatch),
                (Int(7), shadowed),
                (Int(9), shadowed));

        var block = new Retained.LetFunctions([new(0, env), new(1, env)], dispatch);

        var optimized = FunctionBlockOptimization.Optimize(block).Block;

        optimized.Entry.Should().Be(new Retained.Conditional(predicate, fallback, firstMatch));
        var before = ExpressionConversion.ToCanonical(block, Layout.Packed);
        var after = ExpressionConversion.ToCanonical(optimized, Layout.Packed);
        var vm = MakeVM();

        foreach (var (input, expected) in new[]
        {
            (PineKernelValues.TrueValue, Int(20)),
            (PineKernelValues.FalseValue, Int(10)),
            (PineValue.EmptyList, Int(10))
        })
        {
            Interpret(before, input).Should().Be(expected);
            Interpret(after, input).Should().Be(expected);
            var original = Run(vm, before, input);
            var rewritten = Run(vm, after, input);
            rewritten.ReturnValue.Evaluate().Should().Be(expected);

            output.WriteLine(
                $"Switch with exact true: {input == PineKernelValues.TrueValue}; " +
                $"{original.InstructionCount} -> {rewritten.InstructionCount} instructions");

            rewritten.InstructionCount.Should().BeLessThan(original.InstructionCount);
        }
    }

    [Fact]
    public void Scoped_switch_prunes_literal_dispatch_but_not_a_required_predicate()
    {
        var env = Retained.Environment.Instance;
        var call = new Retained.Call(0, env);
        var block = new Retained.LetFunctions([new(0, env)], Switch(Number(7), call, (Int(7), Number(20))));

        FunctionBlockOptimization.Optimize(block).Block.Entry.Should().Be(Number(20));

        var failing = new Retained.Eval(new Retained.Literal(PineValue.EmptyList), env);

        var selector = new Retained.Conditional(failing, Number(4), Number(5));
        var withPredicate = new Retained.LetFunctions([new(0, env)], Switch(selector, call, (Int(7), Number(20))));

        var optimized = FunctionBlockOptimization.Optimize(withPredicate).Block;
        optimized.Should().BeSameAs(withPredicate);

        FluentActions.Invoking(
            () => Interpret(ExpressionConversion.ToCanonical(optimized, Layout.Packed), PineValue.EmptyList))
            .Should().Throw<ParseExpressionException>();
    }

    private static Retained Switch(
        Retained selector,
        Retained fallback,
        params (PineValue Key, Retained Result)[] arms) =>
        new Retained.Switch(
            selector,
            [
            .. arms.Select(arm => new Retained.Switch.Arm(arm.Key, true, arm.Result))
            ],
            fallback);

    [Fact]
    public void Closed_blocks_reject_wrong_scope_and_allow_repeat_ids_in_independent_blocks()
    {
        var body = Retained.Environment.Instance;
        var unknown = new Retained.Label(Int(5), new Retained.List([new Retained.Call(1, body)]));

        FluentActions.Invoking(() => new Retained.LetFunctions([new(0, body)], unknown))
            .Should().Throw<ArgumentException>();

        FluentActions.Invoking(() => new Retained.LetFunctions([new(0, body), new(0, body)], body))
            .Should().Throw<ArgumentException>();

        var nested = new Retained.LetFunctions([new(0, body)], new Retained.Call(0, Number(17)));
        var outer = new Retained.LetFunctions([new(0, Number(23))], nested);
        Interpret(ExpressionConversion.ToCanonical(outer, Layout.Packed), PineValue.EmptyList).Should().Be(Int(17));
    }

    [Theory]
    [InlineData(Layout.Packed, 2)]
    [InlineData(Layout.FlatArguments, 3)]
    public void Scoped_conversion_uses_the_requested_layout_and_keeps_existing_binary_encoding(
        Layout layout,
        int physicalSlots)
    {
        var env = Retained.Environment.Instance;
        var block = new Retained.LetFunctions([new(7, new Retained.HeadSkip(1, env))], new Retained.Call(7, env));
        var program = new Retained.List([block]);

        var lowered = FunctionBlockLowering.Lower(program, layout);
        var canonical = ExpressionConversion.ToCanonical(program, layout);
        var entry = ((Canonical.List)canonical).Items[0].Should().BeOfType<Canonical.Eval>().Which;
        var frame = Interpret(entry.Environment, PineValue.List([Int(3), Int(17)]));
        frame.Should().BeOfType<PineValue.ListValue>().Which.Items.Length.Should().Be(physicalSlots);
        Interpret(canonical, PineValue.List([Int(3), Int(17)])).Should().Be(PineValue.List([Int(17)]));

        ExpressionValueEncoding.EncodeExpressionAsValue(lowered).Should().Be(
            ExpressionEncoding.EncodeExpressionAsValue(canonical));

        using var stream = new MemoryStream();
        ExpressionBinary.Encode(stream, lowered);
        stream.Length.Should().BePositive();
    }

    [Fact]
    public void Encoders_require_lowering_and_do_not_write_a_partial_binary_root()
    {
        var call = new Retained.Call(0, Number(17));
        var block = new Retained.LetFunctions([new(0, Retained.Environment.Instance)], call);

        using var stream = new MemoryStream();

        foreach (var expression in new Retained[] { call, block, Switch(Number(1), Number(2), (Int(1), block)) })
        {
            FluentActions.Invoking(() => ExpressionBinary.Encode(stream, expression))
                .Should().Throw<InvalidOperationException>().WithMessage("*environment layout*");

            stream.Length.Should().Be(0);

            FluentActions.Invoking(() => ExpressionValueEncoding.EncodeExpressionAsValue(expression))
                .Should().Throw<InvalidOperationException>().WithMessage("*environment layout*");

            FluentActions.Invoking(() => ExpressionConversion.ToCanonical(expression))
                .Should().Throw<InvalidOperationException>().WithMessage("*environment layout*");
        }

        FluentActions.Invoking(() => ExpressionConversion.ToCanonical(call, Layout.Packed))
            .Should().Throw<ArgumentException>().WithMessage("*outside its block*");
    }

    [Fact]
    public void Frontend_can_export_pruned_private_recursive_bodies_with_flat_elm_arguments()
    {
        // Function 2 is the exported function. The two choice-returning bodies are private.
        var recursive = Program(call => call);
        var env = Retained.Environment.Instance;
        var forwarded = new Retained.List([new Retained.HeadSkip(0, env), new Retained.HeadSkip(1, env)]);

        var block =
            new Retained.LetFunctions(
                [.. recursive.Functions, new(2, new Retained.HeadSkip(2, new Retained.Call(0, forwarded)))],
                env);

        var flatBodies = FunctionBlockLowering.ExportBodies(block, Layout.FlatArguments);
        var imported = FunctionBlockAdapters.TryImportBodies(flatBodies, Layout.FlatArguments);
        imported.Should().NotBeNull();
        var optimized = FunctionBlockOptimization.Optimize(imported!, publicFunctions: [2]);
        optimized.Proofs.Should().ContainSingle().Subject.Functions.Should().Equal(0, 1);

        var optimizedBodies = FunctionBlockLowering.ExportBodies(optimized.Block, Layout.FlatArguments);

        var table =
            PineValue.List([.. optimizedBodies.Select(body => ExpressionEncoding.EncodeExpressionAsValue(body))]);

        var entry =
            new Canonical.Eval(
                Canonical.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(optimizedBodies[2])),
                Canonical.ListInst(
                    [
                    Canonical.LitralInst(table),
                    Canonical.LitralInst(Int(5)),
                    Canonical.LitralInst(Int(42))
                    ]));

        Interpret(entry, PineValue.EmptyList).Should().Be(Int(42));
        FunctionBlockAdapters.TryImportBodies([Canonical.EnvironmentInstance], Layout.FlatArguments).Should().BeNull();
    }

    private static Retained.LetFunctions Program(Func<Retained, Retained> observe, bool pruned = false)
    {
        var env = Retained.Environment.Instance;
        var depth = new Retained.HeadSkip(0, env);
        var payload = new Retained.HeadSkip(1, env);

        Retained Choice(Retained value)
        {
            var name = new Retained.Literal(StringEncoding.ValueFromString("Just"));

            return
                pruned
                ?
                new Retained.List([name, value])
                :
                new Retained.List([new Retained.Literal(StringEncoding.ValueFromString("<Choice>")), name, value]);
        }

        var current = Choice(payload);
        var zero = new Retained.BuiltinBinary(Intrinsic.Equal, Number(0), depth);
        var nextDepth = new Retained.BuiltinBinary(Intrinsic.IntAdd, Number(-1), depth);
        var next = new Retained.List([nextDepth, payload]);

        return
            new(
                [
                    new(0, new Retained.Conditional(zero, new Retained.Call(1, next), current)),
                    new(
                        1,
                        new Retained.Conditional(
                            zero,
                            Choice(new Retained.HeadSkip(pruned ? 1 : 2, new Retained.Call(0, next))),
                            current))
                ],
                observe(new Retained.Call(0, env)));
    }

    private static PineValue Int(int number) => IntegerEncoding.EncodeSignedInteger(number);

    private static Retained.Literal Number(int number) => new(Int(number));

    private static PineValue Interpret(Canonical expression, PineValue environment) =>
        DirectInterpreter.WithoutEvalCaching(new PineVMParseCache()).EvaluateExpression(expression, environment)
        .Extract(error => throw new InvalidOperationException(error));

    private static EvaluationReport Run(VM vm, Canonical expression, PineValue environment) =>
        vm.EvaluateExpressionOnCustomStack(expression, environment, new VM.EvaluationConfig(null, null, null))
        .Extract(error => throw new InvalidOperationException(error.ToString()));

    private static VM MakeVM(bool reduce = false) =>
        VM.CreateCustom(
            expressionTemplatePlanCache: new(),
            evalCache: null,
            evaluationConfigDefault: null,
            reportFunctionApplication: null,
            compilationEnvClasses: null,
            disableReductionInCompilation: !reduce,
            selectPrecompiled: null,
            skipInlineForExpression: _ => true,
            enableTailRecursionOptimization: false,
            parseCache: null,
            precompiledLeaves: null,
            reportEnterPrecompiledLeaf: null,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null,
            disableDirectInvocation: true);
}
