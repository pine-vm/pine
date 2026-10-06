using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class PineControlFlowGraphInliningTests
{
    private static Expression Path(params int[] path) =>
        ExpressionBuilder.BuildExpressionForPathInExpression(path, Expression.EnvironmentInstance);

    private static Expression Number(int number) =>
        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(number));

    private static Expression Invoke(Expression target, Expression environment) =>
        new Expression.Eval(
            Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(target)),
            environment);

    private static ExpressionCompilation Compile(
        Expression expression,
        Func<Expression, PineValueClass?, bool>? skip = null) =>
        ExpressionCompilation.CompileExpression(
            expression,
            specializations: [],
            parseCache: new(),
            disableReduction: true,
            enableTailRecursionOptimization: false,
            skipInlining: skip ?? ((_, _) => false));

    private static PineControlFlowFragment Ops(params StackInstruction[] instructions) =>
        PineControlFlowFragment.FromOperations(instructions);

    [Fact]
    public void Inlining_shifts_all_indices_in_multi_local_gets()
    {
        var invocation =
            StackInstruction.Invoke_StackFrame_Const(
                Expression.EnvironmentInstance,
                StaticFunctionInterface.Generic);

        var caller =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get([0, 2]),
                    StackInstruction.PopMultiple(2),
                    StackInstruction.Local_Get(0),
                    invocation));

        var callee =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Set(1),
                    StackInstruction.Pop,
                    StackInstruction.Local_Get([0, 1]),
                    StackInstruction.Build_List(2)));

        var inlined =
            caller.InlineInvocation(caller.Entry, callee, callerParameterCount: 3, calleeParameterCount: 1);

        inlined.LowerToStackInstructions()
            .Should().Contain(StackInstruction.Local_Get([3, 4]));
    }

    [Fact]
    public void Splicing_twice_preserves_live_caller_stack_and_two_independent_local_loops()
    {
        var loopBody =
            Ops(StackInstruction.Local_Get(0), StackInstruction.Local_Set(1))
            .Append(
                new PineControlFlowNode.Conditional(
                    PineKernelValues.TrueValue,
                    Ops(StackInstruction.Local_Get(1)),
                    PineControlFlowFragment.Empty.Append(new PineControlFlowNode.JumpToEntry())));

        var callee = PineControlFlowGraph.FromFragment(loopBody);

        var invocation =
            StackInstruction.Invoke_StackFrame_Const(
                Expression.EnvironmentInstance,
                StaticFunctionInterface.Generic);

        var caller =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Local_Get(0),
                    StackInstruction.Local_Get(1),
                    invocation,
                    StackInstruction.Local_Get(2),
                    invocation,
                    StackInstruction.Build_List(3)));

        var first =
            caller.InlineInvocation(new PineBlockId(0), callee, callerParameterCount: 3, calleeParameterCount: 1);

        var secondCall = first.Blocks.Single(block => block.Terminator is PineControlFlowTerminator.Invoke);
        var result = first.InlineInvocation(secondCall.Id, callee, callerParameterCount: 3, calleeParameterCount: 1);

        result.Blocks.Should().NotContain(block => block.Terminator is PineControlFlowTerminator.Invoke);

        result.Blocks.Count(
            block => block.Terminator is PineControlFlowTerminator.Jump jump &&
                jump.Target.Value < block.Id.Value).Should().Be(2);

        var instructions = result.LowerToStackInstructions();

        instructions.Count(
            instruction => instruction.Kind is StackInstructionKind.Jump_Const &&
                instruction.JumpOffset < 0).Should().Be(2);

        instructions.Count(
            instruction => instruction.Kind is StackInstructionKind.Local_Set &&
                instruction.PopCount is > 0).Should().Be(2);

        new StackFrameInstructions(StaticFunctionInterface.Generic, instructions).MaxStackUsage.Should().BeGreaterThan(
            1);
    }

    [Fact]
    public void Nested_known_callees_inline_transitively_and_preserve_results()
    {
        var leaf =
            Expression.ConditionalInst(
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.equal),
                    Expression.ListInst([Expression.EnvironmentInstance, Number(3)])),
                falseBranch: Number(4),
                trueBranch: Number(5));

        var middle =
            Expression.ListInst([Expression.EnvironmentInstance, Invoke(leaf, Expression.EnvironmentInstance)]);

        var root = Invoke(middle, Expression.EnvironmentInstance);
        var instructions = Compile(root).Generic.Instructions;

        instructions.Count(instruction => PineControlFlowGraph.IsInvocation(instruction.Kind))
            .Should().Be(0, string.Join(", ", instructions.Select(instruction => instruction.Kind)));

        var vm = CreateVM();

        vm.EvaluateExpression(root, IntegerEncoding.EncodeSignedInteger(3))
            .IsOkOrNull().Should().Be(
            PineValue.List(
                [IntegerEncoding.EncodeSignedInteger(3), IntegerEncoding.EncodeSignedInteger(5)]));

        vm.EvaluateExpression(root, IntegerEncoding.EncodeSignedInteger(2))
            .IsOkOrNull().Should().Be(
            PineValue.List(
                [IntegerEncoding.EncodeSignedInteger(2), IntegerEncoding.EncodeSignedInteger(4)]));
    }

    [Fact]
    public void Inlining_respects_skip_predicate_and_keeps_dynamic_invocations()
    {
        var leaf =
            Expression.ListInst([Expression.EnvironmentInstance, Number(1)]);

        var call = Invoke(leaf, Expression.EnvironmentInstance);

        Compile(call, (expression, _) => expression.Equals(leaf)).Generic.Instructions
            .Should().Contain(instruction => PineControlFlowGraph.IsInvocation(instruction.Kind));

        var dynamicCall = new Expression.Eval(Path(0), Path(1));

        Compile(dynamicCall).Generic.Instructions
            .Should().Contain(instruction => PineControlFlowGraph.IsInvocation(instruction.Kind));
    }

    [Fact]
    public void Compilation_override_of_callee_is_not_bypassed_by_inlining()
    {
        var leaf = Expression.ListInst([Expression.EnvironmentInstance, Number(1)]);

        var overrideCompilation =
            new ExpressionCompilation(
                new StackFrameInstructions(
                    StaticFunctionInterface.FromExpression(leaf),
                    [StackInstruction.Push_Literal(IntegerEncoding.EncodeSignedInteger(42)), StackInstruction.Return]),
                Specialized: []);

        var vm =
            CreateVM(
                overrides: new Dictionary<Expression, ExpressionCompilation>
                {
                    [leaf] = overrideCompilation
                });

        vm.EvaluateExpression(Invoke(leaf, Number(3)), PineValue.EmptyBlob)
            .IsOkOrNull().Should().Be(IntegerEncoding.EncodeSignedInteger(42));
    }

    [Fact]
    public void Large_callee_is_not_inlined_even_when_expression_has_a_static_target()
    {
        var body = Expression.EnvironmentInstance;

        for (var i = 0; i < 270; i++)
        {
            body =
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.int_add),
                    Expression.ListInst([body, Number(1)]));
        }

        var root = Invoke(body, Expression.EnvironmentInstance);

        Compile(root).Generic.Instructions
            .Should().Contain(instruction => PineControlFlowGraph.IsInvocation(instruction.Kind));
    }

    [Fact]
    public void Large_expression_with_many_shared_case_arms_is_inlined_by_instruction_size()
    {
        var body = Number(0);

        for (var i = 0; i < 140; i++)
        {
            body =
                Expression.ConditionalInst(
                    Expression.BuiltinInst(
                        nameof(BuiltinFunction.equal),
                        Expression.ListInst([Expression.EnvironmentInstance, Number(i + 1)])),
                    falseBranch: body,
                    trueBranch: Number(1));
        }

        body.SubexpressionCount.Should().BeGreaterThan(500);

        var instructions = Compile(Invoke(body, Expression.EnvironmentInstance)).Generic.Instructions;

        instructions.Count(instruction => PineControlFlowGraph.IsInvocation(instruction.Kind)).Should().Be(0);

        instructions.Count(instruction => instruction.Kind is StackInstructionKind.Switch_Jump_If_Equal_Const)
            .Should().Be(1);
    }

    [Fact]
    public void Zero_parameter_callee_can_be_inlined_without_popping_caller_values()
    {
        var callee =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(PineValue.EmptyList),
                    StackInstruction.Push_Literal(PineValue.EmptyBlob),
                    StackInstruction.Build_List(2)));

        var caller =
            PineControlFlowGraph.FromFragment(
                Ops(
                    StackInstruction.Push_Literal(PineValue.EmptyBlob),
                    StackInstruction.Invoke_StackFrame_Const(Number(1), StaticFunctionInterface.ZeroParameters),
                    StackInstruction.Build_List(2)));

        var result = caller.InlineInvocation(caller.Entry, callee, 0, 0);

        result.LowerToStackInstructions().Should().NotContain(
            instruction => PineControlFlowGraph.IsInvocation(instruction.Kind));

        new StackFrameInstructions(StaticFunctionInterface.ZeroParameters, result.LowerToStackInstructions())
            .MaxStackUsage.Should().Be(3);
    }

    [Fact]
    public void Inlined_recursive_callee_uses_local_loop_without_invocations_at_multiple_sites()
    {
        var self = Path(0);
        var counter = Path(1);

        var decrement =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.int_add),
                Expression.ListInst([counter, Number(-1)]));

        var recursive =
            new Expression.Eval(
                self,
                Expression.ListInst([self, decrement]));

        var countdown =
            Expression.ConditionalInst(
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.equal),
                    Expression.ListInst([counter, Number(0)])),
                falseBranch: recursive,
                trueBranch: counter);

        var encoded = ExpressionEncoding.EncodeExpressionAsValue(countdown);

        var calleeGraph =
            ExpressionCompilation.ControlFlowGraphFromExpression(
                countdown,
                rootExprAlternativeForms: [],
                envClass: PineValueClass.Create(
                    [new KeyValuePair<IReadOnlyList<int>, PineValue>([0], encoded)]),
                parametersAsLocals: StaticFunctionInterface.FromExpression(countdown),
                parseCache: new(),
                enableTailRecursionOptimization: true);

        calleeGraph.Blocks.Count(block => block.Terminator is PineControlFlowTerminator.Invoke)
            .Should().Be(0);

        Expression Call(int count) =>
            Invoke(
                countdown,
                Expression.ListInst([Expression.LitralInst(encoded), Number(count)]));

        var root = Expression.ListInst([Call(3), Call(5)]);

        var rootGraph =
            ExpressionCompilation.ControlFlowGraphFromExpression(
                root,
                rootExprAlternativeForms: [],
                envClass: null,
                parametersAsLocals: StaticFunctionInterface.FromExpression(root),
                parseCache: new());

        var firstCall = (PineControlFlowTerminator.Invoke)rootGraph.Blocks[0].Terminator;
        var firstInput = firstCall.Inputs[0];

        rootGraph.Blocks[0].Operations
            .Single(operation => operation.Results.Contains(firstInput))
            .Instruction.Literal!.Evaluate().Should().Be(encoded);

        var instructions = Compile(root).Generic.Instructions;

        instructions.Count(instruction => PineControlFlowGraph.IsInvocation(instruction.Kind))
            .Should().Be(0, string.Join(", ", instructions.Select(instruction => instruction.Kind)));

        instructions.Count(
            instruction => instruction.Kind is StackInstructionKind.Jump_Const &&
                instruction.JumpOffset < 0).Should().Be(2);

        CreateVM().EvaluateExpression(root, PineValue.EmptyList)
            .IsOkOrNull().Should().Be(
            PineValue.List(
                [IntegerEncoding.EncodeSignedInteger(0), IntegerEncoding.EncodeSignedInteger(0)]));
    }

    private static Core.Interpreter.IntermediateVM.PineVM CreateVM(
        IReadOnlyDictionary<Expression, ExpressionCompilation>? overrides = null) =>
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
            expressionCompilationOverrides: overrides);
}
