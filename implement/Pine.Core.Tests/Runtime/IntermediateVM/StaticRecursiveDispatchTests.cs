using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen.Pine;
using Pine.Core.Elm;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using Pine.Core.Runtime.IntermediateVM;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using Xunit;

using VM = Pine.Core.Runtime.IntermediateVM.PineVM;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class StaticRecursiveDispatchTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Non_tail_recursion_has_fixed_targets_and_no_environment_lists(bool mutual, bool reduce)
    {
        var first = RecursiveBody(mutual ? 1 : 0, 2, 1);
        var second = RecursiveBody(0, 5, 2);

        var group =
            PineValue.List(
                mutual
                ?
                [ExpressionEncoding.EncodeExpressionAsValue(first), ExpressionEncoding.EncodeExpressionAsValue(second)]
                :
                [ExpressionEncoding.EncodeExpressionAsValue(first)]);

        var entry =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(first)),
                Expression.ListInst(
                    [
                    Expression.LitralInst(group),
                    Expression.ListInst([Path(0), Path(1)]),
                    Path(2),
                    ]));

        var trace = new List<ExecutedStackInstruction>();
        var vm = CreateVM(trace, reduce);
        var environment = PineValue.List([NumberValue(4), NumberValue(10), NumberValue(7)]);

        var report = Evaluate(vm, entry, environment);
        report.ReturnValue.Evaluate().Should().Be(NumberValue(mutual ? 37 : 29));

        report.ReturnValue.Evaluate().Should().Be(
            DirectInterpreter.WithoutEvalCaching(new PineVMParseCache())
            .EvaluateExpression(entry, environment).Extract(error => throw new Exception(error)));

        report.BuildListCount.Should().Be(0);
        report.InvocationCount.Should().BeGreaterThanOrEqualTo(5);

        trace.Should().NotContain(
            item => item.Instruction.Kind == StackInstructionKind.Eval_Binary ||
                item.Instruction.Kind == StackInstructionKind.Eval_Multi);

        var target =
            trace.First(item => item.Instruction.OptimizedInvocation is not null)
            .Instruction.OptimizedInvocation!;

        var firstFrame = target.LinkedStackFrameInstructions!;
        firstFrame.Parameters.Should().Be(StaticFunctionInterface.FromPathsSorted([[1, 0], [1, 1], [2]]));

        var next =
            firstFrame.Instructions.Single(item => item.OptimizedInvocation is not null)
            .LinkedStackFrameInstructions!;

        var back =
            mutual
            ?
            next.Instructions.Single(item => item.OptimizedInvocation is not null).LinkedStackFrameInstructions
            :
            next;

        back.Should().BeSameAs(firstFrame);
        var hashBefore = firstFrame.GetHashCode();

        var changedEnvironment = PineValue.List([NumberValue(6), NumberValue(20), NumberValue(100)]);
        var warmed = Evaluate(vm, entry, changedEnvironment);
        warmed.ReturnValue.Evaluate().Should().Be(NumberValue(mutual ? 150 : 138));
        firstFrame.GetHashCode().Should().Be(hashBefore);

        var otherEntry =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.int_add),
                Expression.ListInst([entry, Number(1)]));

        var traceStart = trace.Count;

        Evaluate(vm, otherEntry, environment).ReturnValue.Evaluate()
            .Should().Be(NumberValue(mutual ? 38 : 30));

        trace.Skip(traceStart).First(item => item.Instruction.OptimizedInvocation is not null)
            .Instruction.LinkedStackFrameInstructions.Should().BeSameAs(firstFrame);

        using var profile = new PineVMWorkProfile(referenceCapacity: 0);

        using (profile.Enter())
            Evaluate(vm, entry, changedEnvironment).ReturnValue.Evaluate().Should().Be(warmed.ReturnValue.Evaluate());

        profile[PineVMWorkProfile.Metric.InProcessChildArrays].Should().Be(0);
        profile[PineVMWorkProfile.Metric.ListValueConstructorAllocations].Should().Be(0);

        var persisted =
            StackInstructionTraceRenderer.RenderStackFrameInstructions(
                firstFrame,
                StackInstructionTraceRenderer.RenderingOptions.Persistence);

        var restored =
            StackInstructionTextFormat.ParseStackFrameInstructions(
                persisted.Text,
                hash => persisted.ObservedValues[hash]);

        restored.Parameters.Should().Be(firstFrame.Parameters);
        restored.GetHashCode().Should().NotBe(0);

        restored.Instructions.Single(item => item.OptimizedInvocation is not null)
            .OptimizedInvocation!.EnvironmentConstraint.Should().Be(target.EnvironmentConstraint);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Constrained_interfaces_and_invocation_cache_keep_eliminated_inputs_distinct(bool staticCall)
    {
        var body =
            Expression.ConditionalInst(
                Path(0),
                falseBranch: Path(3),
                trueBranch:
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.int_add),
                    Expression.ListInst([new Expression.Eval(Path(4), Path(1)), Path(2)])));

        var constraints =
            new[] { 7, 100 }.Select(
                offset => PineValueClass.Create(
                    [
                    new([0], PineKernelValues.TrueValue),
                    new([2], NumberValue(offset)),
                    ])).ToArray();

        var cache = new InvocationCache();
        var identity = ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance);

        var dynamicEntry =
            new Expression.Eval(
                Path(0),
                Path(1));

        var trace = new List<ExecutedStackInstruction>();

        var vm =
            CreateVM(
                trace,
                reduce: true,
                new Dictionary<Expression, IReadOnlyList<PineValueClass>> { [body] = constraints },
                cache);

        foreach (var offset in new[] { 7, 100, 7 })
        {
            var environment =
                PineValue.List(
                    [PineKernelValues.TrueValue, NumberValue(10), NumberValue(offset), NumberValue(-1), identity]);
            // Keep a compiled caller rather than taking the root-Eval shortcut.
            Expression entry =
                staticCall
                ?
                Add(
                    new Expression.Eval(
                        Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(body)),
                        Expression.ListInst(
                            [
                            Expression.LitralInst(PineKernelValues.TrueValue),
                            Path(0),
                            Number(offset),
                            Path(1),
                            Path(2)
                            ])),
                    0)
                :
                dynamicEntry;

            var report =
                Evaluate(
                    vm,
                    entry,
                    staticCall
                    ?
                    PineValue.List([NumberValue(10), NumberValue(-1), identity])
                    :
                    PineValue.List([ExpressionEncoding.EncodeExpressionAsValue(body), environment]));

            report.ReturnValue.Evaluate().Should().Be(NumberValue(10 + offset));

            var cachedInputs =
                cache.Keys.Where(
                    key => key.ExprValue.Equals(ExpressionEncoding.EncodeExpressionAsValue(body)) &&
                        key.StackFrameInput.EnvironmentConstraint!.TryGetValue([2])!.Equals(NumberValue(offset)))
                .ToArray();

            cachedInputs.Length.Should().Be(1, "{0}", StackInstructionTraceRenderer.RenderInstructionTrace(trace));
            var input = cachedInputs[0].StackFrameInput;
            input.Parameters.Should().Be(StaticFunctionInterface.FromPathsSorted([[1], [4]]));
            input.ToValueClass().TryGetValue([2]).Should().Be(NumberValue(offset));

            if (staticCall)
            {
                var targets =
                    trace.Where(
                        item =>
                        item.Instruction.OptimizedInvocation?.EnvironmentConstraint?.TryGetValue([2])
                    ?.Equals(NumberValue(offset)) is true)
                    .Select(item => item.Instruction.OptimizedInvocation!)
                    .Distinct().ToArray();

                targets.Length.Should().Be(1, "{0}", StackInstructionTraceRenderer.RenderInstructionTrace(trace));
                targets[0].LinkedStackFrameInstructions!.Parameters.Should().Be(input.Parameters);
            }
        }

        cache.Keys.Where(key => key.ExprValue.Equals(ExpressionEncoding.EncodeExpressionAsValue(body)))
            .Select(key => key.StackFrameInput).Distinct().Should().HaveCount(2);
    }

    [Fact]
    public void Code_shaped_data_does_not_create_function_specializations()
    {
        var callee = Expression.BuiltinInst(nameof(BuiltinFunction.length), Path(0));
        var data = ExpressionEncoding.EncodeExpressionAsValue(Number(42));

        var call =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(callee)),
                Expression.ListInst([Expression.LitralInst(data)]));

        var trace = new List<ExecutedStackInstruction>();

        Evaluate(CreateVM(trace, reduce: false), call, PineValue.EmptyList)
            .ReturnValue.Evaluate().Should().Be(BuiltinFunction.length(data));

        trace.Single(item => item.Instruction.OptimizedInvocation is not null)
            .Instruction.OptimizedInvocation!.EnvironmentConstraint.Should().BeNull();
    }

    [Fact]
    public void Automatic_code_constraints_do_not_freeze_a_whole_environment_used_as_data()
    {
        var callee =
            Expression.ListInst([Expression.EnvironmentInstance, new Expression.Eval(Path(0), Path(1))]);

        var environment =
            PineValue.List([ExpressionEncoding.EncodeExpressionAsValue(Number(42)), NumberValue(17)]);

        var call =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(callee)),
                Expression.LitralInst(environment));

        var trace = new List<ExecutedStackInstruction>();

        Evaluate(CreateVM(trace, reduce: false), call, PineValue.EmptyList)
            .ReturnValue.Evaluate().Should().Be(PineValue.List([environment, NumberValue(42)]));

        trace.Single(item => item.Instruction.OptimizedInvocation is not null)
            .Instruction.OptimizedInvocation!.EnvironmentConstraint.Should().BeNull();
    }

    [Fact]
    public void Function_factory_arguments_are_not_treated_as_callee_code()
    {
        var factory =
            Expression.ConditionalInst(
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.equal),
                    Expression.ListInst(
                        [
                        Expression.BuiltinInst(nameof(BuiltinFunction.length), Expression.EnvironmentInstance),
                        Number(0)
                        ])),
                falseBranch: Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(Number(42))),
                trueBranch: Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(Number(17))));

        var callee =
            new Expression.Eval(
                new Expression.Eval(
                    Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(factory)),
                    Path(1)),
                Path(2));

        var data = ExpressionEncoding.EncodeExpressionAsValue(Number(99));

        var call =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(callee)),
                Expression.ListInst([Number(0), Expression.LitralInst(data), Number(0)]));

        var frames = new List<EnteredStackFrame>();

        var report =
            CreateVM([], reduce: false).EvaluateExpressionOnCustomStack(
                call,
                PineValue.EmptyList,
                VM.EvaluationConfig.Unbounded,
                reportEnteredStackFrame: (in frame) => frames.Add(frame))
            .Extract(error => throw new Exception(error.ToString()));

        report.ReturnValue.Evaluate().Should().Be(NumberValue(42));
        frames.Should().NotBeEmpty();

        frames.Select(frame => frame.Instructions.TrackEnvConstraint?.TryGetValue([1]))
            .Should().NotContain(data);
    }

    [Fact]
    public void Template_code_arguments_do_not_create_a_specialized_wrapper_for_each_code_value()
    {
        var terminal = new Expression.Eval(Path(1), Path(2));
        var frames = new List<EnteredStackFrame>();
        var vm = CreateVM([], reduce: false);

        foreach (var number in new[] { 17, 42, 99 })
        {
            var code = ExpressionEncoding.EncodeExpressionAsValue(Number(number));

            var template =
                new Expression.Eval(
                    Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(terminal)),
                    Expression.ListInst([Number(0), Expression.LitralInst(code), Expression.EnvironmentInstance]));

            var call =
                new Expression.Eval(
                    Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(template)),
                    Path(0));

            var report =
                vm.EvaluateExpressionOnCustomStack(
                    call,
                    PineValue.List([NumberValue(10)]),
                    VM.EvaluationConfig.Unbounded,
                    reportEnteredStackFrame: (in frame) => frames.Add(frame))
                .Extract(error => throw new Exception(error.ToString()));

            report.ReturnValue.Evaluate().Should().Be(NumberValue(number));

            frames.Select(frame => frame.Instructions.TrackEnvConstraint?.TryGetValue([1]))
                .Should().NotContain(code);
        }

        frames.Should().NotBeEmpty();
    }

    [Fact]
    public void Retained_recursive_code_does_not_keep_its_vm_alive_through_an_uncalled_target()
    {
        var (owner, frame) = CompileUncalledRecursiveTarget();
        GC.Collect();
        GC.WaitForPendingFinalizers();
        GC.Collect();

        owner.IsAlive.Should().BeFalse();
        GC.KeepAlive(frame);
    }

    [Fact]
    public void Recursive_edges_do_not_reuse_constraints_that_the_next_arguments_change()
    {
        var body =
            Expression.ConditionalInst(
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.equal),
                    Expression.ListInst([Path(1), Number(0)])),
                falseBranch:
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.int_add),
                    Expression.ListInst(
                        [
                        new Expression.Eval(
                            Path(0),
                            Expression.ListInst(
                                [
                                Path(0),
                                Expression.BuiltinInst(
                                    nameof(BuiltinFunction.int_add),
                                    Expression.ListInst([Path(1), Number(-1)])),
                                Expression.BuiltinInst(
                                    nameof(BuiltinFunction.int_add),
                                    Expression.ListInst([Path(2), Number(1)])),
                                ])),
                        Path(2),
                        ])),
                trueBranch: Path(2));

        var encoded = ExpressionEncoding.EncodeExpressionAsValue(body);

        var entry =
            new Expression.Eval(
                Expression.LitralInst(encoded),
                Expression.ListInst([Expression.LitralInst(encoded), Path(0), Number(7)]));

        var trace = new List<ExecutedStackInstruction>();

        var vm =
            CreateVM(
                trace,
                reduce: true,
                new Dictionary<Expression, IReadOnlyList<PineValueClass>>
                {
                    [body] = [PineValueClass.Create([new([2], NumberValue(7))])]
                });

        Evaluate(vm, entry, PineValue.List([NumberValue(3)]))
            .ReturnValue.Evaluate().Should().Be(NumberValue(34));

        var first =
            trace.First(item => item.Instruction.OptimizedInvocation is not null)
            .Instruction.OptimizedInvocation!;

        first.EnvironmentConstraint!.TryGetValue([2]).Should().Be(NumberValue(7));

        var recursive =
            first.LinkedStackFrameInstructions!.Instructions.Single(item => item.OptimizedInvocation is not null)
            .OptimizedInvocation!;

        recursive.EnvironmentConstraint!.TryGetValue([2]).Should().BeNull();
        recursive.InvocationInterface.Should().Be(StaticFunctionInterface.FromPathsSorted([[1], [2]]));
    }

    [Fact]
    public void Static_argument_projection_preserves_unused_failing_evaluations()
    {
        var callee = Path(0);
        var failure = new Expression.Eval(Expression.LitralInst(PineValue.EmptyList), Expression.EnvironmentInstance);

        var call =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(callee)),
                Expression.ListInst([Number(42), failure]));

        CreateVM([], reduce: false).EvaluateExpression(call, PineValue.EmptyList)
            .IsErrOrNull().Should().NotBeNull();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Static_argument_projection_evaluates_a_shared_producer_only_once(bool usesEval)
    {
        Expression producer =
            usesEval
            ?
            new Expression.Eval(
                Path(0),
                Expression.ListInst([Number(3), Number(5)]))
            :
            Expression.BuiltinInst(nameof(BuiltinFunction.reverse), Path(0));

        var callee =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.int_add),
                Expression.ListInst([Path(0, 0), Path(0, 1)]));

        var call =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(callee)),
                Expression.ListInst([producer]));

        var trace = new List<ExecutedStackInstruction>();

        var report =
            Evaluate(
                CreateVM(trace, reduce: false),
                call,
                PineValue.List(
                    [
                    usesEval
                    ?
                    ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance)
                    :
                    PineValue.List([NumberValue(3), NumberValue(5)])
                    ]));

        report.ReturnValue.Evaluate().Should().Be(NumberValue(8));
        trace.Count(item => item.Instruction.Kind == StackInstructionKind.Eval_Binary).Should().Be(usesEval ? 1 : 0);
        trace.Count(item => item.Instruction.Kind == StackInstructionKind.Reverse).Should().BeLessThanOrEqualTo(1);
        report.BuildListCount.Should().Be(usesEval ? 1 : 0, "only the producer's program data requires a list");
    }

    [Theory]
    [InlineData("repeat", 4, 22)]
    [InlineData("forward", 4, 30)]
    [InlineData("backward", 3, 27)]
    public void Elm_non_tail_recursive_functions_dispatch_without_environment_allocations(
        string name,
        int depth,
        int expected)
    {
        var function =
            s_elmEnvironment.Value.Modules.Single(module => module.moduleName is "RecursiveDispatch")
            .moduleContent.FunctionDeclarations[name];

        var trace = new List<ExecutedStackInstruction>();
        var vm = CreateVM(trace, reduce: true);

        var (value, counters) =
            CoreLibraryTestHelper.ApplyAndProfileBinary(
                function,
                ElmValue.Integer(depth),
                ElmValue.Integer(10),
                vm);

        value.Should().Be(ElmValue.Integer(expected));
        counters.BuildListCount.Should().Be(0, StackInstructionTraceRenderer.RenderInstructionTrace(trace));

        trace.Should().NotContain(
            item => item.Instruction.Kind == StackInstructionKind.Eval_Binary ||
                item.Instruction.Kind == StackInstructionKind.Eval_Multi);

        trace.Count(item => item.Instruction.OptimizedInvocation?.EnvironmentConstraint is not null)
            .Should().BeGreaterThanOrEqualTo(depth);
    }

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_elmEnvironment =
        new(
            () => ElmCompilerTestHelper.CompileElmModules(
                [
                """
                module RecursiveDispatch exposing (backward, forward, repeat)

                repeat : Int -> Int -> Int
                repeat remaining value =
                    if remaining <= 0 then
                        value
                    else
                        repeat (remaining - 1) (value + 2) + 1

                forward : Int -> Int -> Int
                forward remaining value =
                    if remaining <= 0 then
                        value
                    else
                        backward (remaining - 1) (value + 2) + 1

                backward : Int -> Int -> Int
                backward remaining value =
                    if remaining <= 0 then
                        value
                    else
                        forward (remaining - 1) (value + 5) + 2
                """
                ],
                disableInlining: false).parsedEnv);

    private static Expression RecursiveBody(int nextIndex, int increment, int afterCall)
    {
        var recursive =
            new Expression.Eval(
                Path(0, nextIndex),
                Expression.ListInst(
                    [
                    Path(0),
                    Expression.ListInst([Add(Path(1, 0), -1), Add(Path(1, 1), increment)]),
                    Path(2),
                    ]));

        return
            Expression.ConditionalInst(
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.int_is_sorted_asc),
                    Expression.ListInst([Path(1, 0), Number(0)])),
                falseBranch: Add(recursive, afterCall),
                trueBranch:
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.int_add),
                    Expression.ListInst([Path(1, 1), Path(2)])));
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static (WeakReference owner, StackFrameInstructions frame) CompileUncalledRecursiveTarget()
    {
        var first = RecursiveBody(1, 2, 1);

        var group =
            PineValue.List(
                [
                ExpressionEncoding.EncodeExpressionAsValue(first),
                ExpressionEncoding.EncodeExpressionAsValue(RecursiveBody(0, 5, 2)),
                ]);

        var entry =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(first)),
                Expression.ListInst(
                    [Expression.LitralInst(group), Expression.ListInst([Path(0), Path(1)]), Path(2)]));

        var trace = new List<ExecutedStackInstruction>();
        var vm = CreateVM(trace, reduce: false);

        Evaluate(vm, entry, PineValue.List([NumberValue(0), NumberValue(10), NumberValue(7)]))
            .ReturnValue.Evaluate().Should().Be(NumberValue(17));

        return
            (new(vm),
            trace.First(item => item.Instruction.OptimizedInvocation is not null)
            .Instruction.LinkedStackFrameInstructions!);
    }

    private static Expression Path(params int[] path) =>
        ExpressionBuilder.BuildExpressionForPathInExpression(path, Expression.EnvironmentInstance);

    private static PineValue NumberValue(int number) => IntegerEncoding.EncodeSignedInteger(number);

    private static Expression Number(int number) => Expression.LitralInst(NumberValue(number));

    private static Expression Add(Expression expression, int number) =>
        Expression.BuiltinInst(nameof(BuiltinFunction.int_add), Expression.ListInst([expression, Number(number)]));

    private static EvaluationReport Evaluate(VM vm, Expression expression, PineValue environment) =>
        vm.EvaluateExpressionOnCustomStack(
            expression,
            environment,
            new VM.EvaluationConfig(null, null, null))
        .Extract(error => throw new Exception(error.ToString()));

    private static VM CreateVM(
        List<ExecutedStackInstruction> trace,
        bool reduce,
        IReadOnlyDictionary<Expression, IReadOnlyList<PineValueClass>>? constraints = null,
        InvocationCache? cache = null) =>
        VM.CreateCustom(
            evalCache: cache,
            evaluationConfigDefault: null,
            reportFunctionApplication: null,
            compilationEnvClasses:
            constraints?.ToDictionary(
                    item => Core.Runtime.InternalExpression.ExpressionConversion.FromCanonicalCached(item.Key),
                    item => item.Value),
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
            reportExecutedStackInstruction: (in item) => trace.Add(item),
            invocationCacheConfiguration: new(0, 0, 0, 0));
}
