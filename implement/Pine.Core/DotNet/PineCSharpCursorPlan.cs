using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

using Function = Pine.Core.DotNet.PineCSharpCompiler.Function;

namespace Pine.Core.DotNet;

/// <summary>
/// Proves a cursor-shaped CFG: decisions inspect one invariant source at one numeric cursor,
/// successful edges update state, and misses return without evaluating state arithmetic.
/// </summary>
internal sealed record PineCSharpCursorPlan(
    int SourceParameter, ImmutableArray<int> SourcePath, int OffsetParameter,
    BigInteger Multiplier, PineCSharpCursorPlan.Decision Body,
    PineCSharpCursorPlan.Decision.Return Miss, int MinimumLength)
{
    internal abstract record Value
    {
        internal sealed record Parameter(int Index) : Value;

        internal sealed record Projection(Value Source, int Index) : Value;

        internal sealed record Constant(PineValue Literal) : Value;

        internal sealed record Add(Value Operand, BigInteger Amount) : Value;
    }

    internal abstract record Decision
    {
        internal sealed record Return(ImmutableArray<Value> Values) : Decision;

        internal sealed record Continue(ImmutableArray<Value> Arguments) : Decision;

        internal sealed record Match(
            ImmutableArray<(PineValue Literal, Decision Next)> Cases, Decision Otherwise) : Decision;
    }

    public static PineCSharpCursorPlan? TryCreate(Function function, int matchLimit)
    {
        if (matchLimit == 0 || function.Calls.Count != 0)
            return null;

        var parameterCount = function.Prepared.Parameters.ParamsPaths.Count;
        Value? source = null;
        int? offsetParameter = null;
        BigInteger? multiplier = null;
        var minimumLength = int.MaxValue;
        var continuations = new List<Decision.Continue>();
        var projections = new HashSet<Value.Projection>();
        var remaining = 512;
        var supported = true;
        var cleanMiss = true;

        var parameters =
            Enumerable.Range(0, parameterCount).Select(index => (Value)new Value.Parameter(index)).ToArray();

        Decision? Visit(
            PineBlockId id,
            IReadOnlyList<Value> arguments,
            Dictionary<int, Value> locals,
            HashSet<PineBlockId> path,
            bool arithmetic)
        {
            if (--remaining < 0 || !path.Add(id))
                return null;

            var block = function.Graph.Blocks[id.Value];
            var values = block.Parameters.Zip(arguments).ToDictionary(pair => pair.First, pair => pair.Second);
            var stack = arguments.ToList();

            foreach (var operation in block.Operations)
            {
                var instruction = operation.Instruction;
                var inputs = operation.Inputs.Select(input => values[input]).ToArray();
                Value[] results;

                switch (instruction.Kind)
                {
                    case StackInstructionKind.Local_Get:
                        if (instruction.LocalIndices.Any(index => !locals.ContainsKey(index)))
                            return null;

                        results = instruction.LocalIndices.Select(index => locals[index]).ToArray();
                        break;

                    case StackInstructionKind.Local_Get_Skip_Head_Const:
                        if (instruction.LocalIndices.Any(index => !locals.ContainsKey(index)))
                            return null;

                        results =
                            instruction.LocalIndices.Select(
                                index =>
                                (Value)new Value.Projection(locals[index], instruction.SkipCount!.Value)).ToArray();

                        break;

                    case StackInstructionKind.Skip_Head_Const:
                        results = [new Value.Projection(inputs[0], instruction.SkipCount!.Value)];
                        break;

                    case StackInstructionKind.Local_Set:
                        for (var index = 0; index < instruction.LocalIndices.Length; index++)
                            locals[instruction.LocalIndices[index]] = stack[stack.Count - index - 1];

                        results = [];
                        break;

                    case StackInstructionKind.Local_Set_Literal:
                        locals[instruction.SingleLocalIndex] = new Value.Constant(instruction.Literal!.Evaluate());
                        results = [];
                        break;

                    case StackInstructionKind.Push_Literal:
                        results = [new Value.Constant(instruction.Literal!.Evaluate())];
                        break;

                    case StackInstructionKind.Int_Add_Const:
                        arithmetic = true;
                        results = [new Value.Add(inputs[0], instruction.IntegerLiteral!.Value)];
                        break;

                    case StackInstructionKind.Local_Int_Add_Const:
                        arithmetic = true;

                        foreach (var index in instruction.LocalIndices)
                        {
                            if (!locals.TryGetValue(index, out var value))
                                return null;

                            locals[index] = new Value.Add(value, instruction.IntegerLiteral!.Value);
                        }

                        results = [];
                        break;

                    case StackInstructionKind.Pop:
                        results = [];
                        break;

                    default:
                        return null;
                }

                if (results.Length != operation.Results.Length)
                    return null;

                foreach (var pair in operation.Results.Zip(results))
                {
                    values[pair.First] = pair.Second;

                    if (pair.Second is Value.Projection projection)
                        projections.Add(projection);
                }

                stack.RemoveRange(stack.Count - operation.Inputs.Length, operation.Inputs.Length);
                stack.AddRange(results);
            }

            Decision? Edge(PineBlockId target, IReadOnlyList<Value> args)
            {
                if (target == function.Graph.Entry)
                {
                    if (args.Count != 0 || Enumerable.Range(0, parameterCount).Any(index => !locals.ContainsKey(index)))
                        return null;

                    var continuation =
                        new Decision.Continue([.. Enumerable.Range(0, parameterCount).Select(index => locals[index])]);

                    continuations.Add(continuation);
                    return continuation;
                }

                return Visit(target, args, new(locals), [.. path], arithmetic);
            }

            switch (block.Terminator)
            {
                case PineControlFlowTerminator.Return returned:
                    if (arithmetic)
                        cleanMiss = false;

                    return new Decision.Return([.. stack.TakeLast(returned.ValueCount)]);

                case PineControlFlowTerminator.Jump jump:
                    return Edge(jump.Target, jump.Arguments.Select(argument => values[argument]).ToArray());

                case PineControlFlowTerminator.Switch choice:
                    if (choice.Kind != PineSwitchKind.SliceSkipVarEqual ||
                        stack[^1] is not Value.Parameter cursor ||
                        choice.Cases.Any(item => item.Literal is not PineValue.BlobValue { Bytes.Length: > 0 }) ||
                        source is not null && source != stack[^2] ||
                        offsetParameter is not null && offsetParameter != cursor.Index ||
                        multiplier is not null && multiplier != choice.SkipCountMultiplier)
                    {
                        return null;
                    }

                    source = stack[^2];
                    offsetParameter = cursor.Index;
                    multiplier = choice.SkipCountMultiplier;
                    var cases = ImmutableArray.CreateBuilder<(PineValue, Decision)>();

                    foreach (var item in choice.Cases)
                    {
                        minimumLength = Math.Min(minimumLength, ((PineValue.BlobValue)item.Literal).Bytes.Length);
                        var next = Edge(item.Target, choice.Arguments.Select(argument => values[argument]).ToArray());

                        if (next is null)
                            return null;

                        cases.Add((item.Literal, next));
                    }

                    var fallback =
                        Edge(choice.FallThrough, choice.Arguments.Select(argument => values[argument]).ToArray());

                    return fallback is null ? null : new Decision.Match(cases.ToImmutable(), fallback);

                case PineControlFlowTerminator.ConditionalJump:
                case PineControlFlowTerminator.Invoke:
                case PineControlFlowTerminator.ValueMap:
                case PineControlFlowTerminator.TailInvoke:
                    return null;

                default:
                    throw new NotImplementedException("TryCreate does not handle: " + block.Terminator.GetType().Name);
            }
        }

        var body =
            Visit(
                function.Graph.Entry,
                [],
                parameters.Select((value, index) => (value, index))
                .ToDictionary(item => item.index, item => item.value),
                [],
                false);

        if (body is null || source is null || offsetParameter is null || multiplier is null || !cleanMiss ||
            continuations.Count == 0)
        {
            return null;
        }

        var sourceAncestors = new HashSet<Value>();

        for (var value = source; value is Value.Projection projection; value = projection.Source)
            sourceAncestors.Add(value);

        if (projections.Any(projection => !sourceAncestors.Contains(projection)))
            return null;

        var path = new List<int>();

        while (source is Value.Projection projection)
        {
            path.Add(projection.Index);
            source = projection.Source;
        }

        path.Reverse();

        if (source is not Value.Parameter sourceParameter ||
            continuations.Any(next => next.Arguments[sourceParameter.Index] != parameters[sourceParameter.Index]))
        {
            return null;
        }

        Value? OffsetExpression(Value value) =>
            value switch
            {
                Value.Parameter parameter => parameter.Index == offsetParameter ? value : null,
                Value.Add add => OffsetExpression(add.Operand),
                Value.Constant => null,
                Value.Projection => null,

                _ =>
                throw new NotImplementedException("OffsetExpression does not handle: " + value.GetType().Name)
            };

        if (continuations.Any(next => OffsetExpression(next.Arguments[offsetParameter.Value]) is null))
            return null;

        var miss = body;

        while (miss is Decision.Match match)
            miss = match.Otherwise;

        if (miss is not Decision.Return returnedMiss ||
            returnedMiss.Values.Any(
                value =>
                value is not Value.Parameter and not Value.Constant))
        {
            return null;
        }

        void Check(Decision decision)
        {
            switch (decision)
            {
                case Decision.Match match:
                    foreach (var group in match.Cases.GroupBy(item => ((PineValue.BlobValue)item.Literal).Bytes.Length))
                    {
                        supported &=
                            PineCSharpConstantMatch.CanInline(
                                group.Select(
                                    item =>
                                    new PineCSharpConstantMatch.Case(item.Literal, _ => { })).ToArray(),
                                matchLimit);
                    }

                    foreach (var item in match.Cases)
                        Check(item.Next);

                    Check(match.Otherwise);
                    break;

                case Decision.Return:
                case Decision.Continue:
                    break;

                default:
                    throw new NotImplementedException("Check does not handle: " + decision.GetType().Name);
            }
        }

        Check(body);

        return
            supported
            ?
            new(
                sourceParameter.Index,
                [.. path],
                offsetParameter.Value,
                multiplier.Value,
                body,
                returnedMiss,
                minimumLength)
            :
            null;
    }
}
