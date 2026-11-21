using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Text;

using Function = Pine.Core.DotNet.PineCSharpCompiler.Function;
using CursorValue = Pine.Core.DotNet.PineCSharpCursorPlan.Value;
using CursorDecision = Pine.Core.DotNet.PineCSharpCursorPlan.Decision;

namespace Pine.Core.DotNet;

internal sealed partial class PineCSharpEmitter
{
    private PineCSharpExpressionScope _expressionScope = new();

    private IReadOnlySet<string> _lengthVariables = new HashSet<string>();

    private IReadOnlyDictionary<(int Local, int Skip), string> _projectionCaches =
        new Dictionary<(int, int), string>();

    private void PrepareProjectionCaches(StringBuilder text, IReadOnlyList<Function> group)
    {
        var caches = new Dictionary<(int Local, int Skip), string>();

        if (group.Count == 1 && _tailCalls[group[0]].Count == 0)
        {
            var function = group[0];
            var operations = _plans[function].Blocks.SelectMany(block => block.Operations).ToArray();

            var written =
                operations.Where(
                    operation => operation.Instruction.Kind is StackInstructionKind.Local_Set or
                StackInstructionKind.Local_Set_Literal or StackInstructionKind.Local_Int_Add_Const)
                .SelectMany(operation => operation.Instruction.LocalIndices).ToHashSet();

            foreach (var projections in operations.Where(
                operation =>
                operation.Instruction.Kind == StackInstructionKind.Local_Get_Skip_Head_Const)
                .SelectMany(
                operation => operation.Instruction.LocalIndices.Select(
                    index =>
                    (Local: index, Skip: operation.Instruction.SkipCount!.Value))).GroupBy(item => item))
            {
                if (projections.Count() < 2 || written.Contains(projections.Key.Local) ||
                    projections.Key.Local >= function.Prepared.Parameters.ParamsPaths.Count)
                {
                    continue;
                }

                var name = "projection_" + caches.Count;
                caches.Add(projections.Key, name);
                text.AppendLine("PineValueInProcess? " + name + " = null;");
            }
        }

        _projectionCaches = caches;
    }

    private string LocalProjection(Function function, int index, int skip)
    {
        var projection = Pine(Local(function, index)) + ".GetElementAt(" + skip + ")";

        return
            _projectionCaches.TryGetValue((index, skip), out var cache)
            ?
            "(" + cache + " ??= " + projection + ")"
            :
            projection;
    }

    private string Numeric(string variable) =>
        _lengthVariables.Contains(variable)
        ?
        "new BigInteger(" + variable + ")"
        :
        variable + ".AsInteger()";

    private string LengthEqual(string source, BigInteger length) =>
        length >= 0 && length <= int.MaxValue
        ?
        Pine(source) + ".LengthEqual(" + length.ToString(System.Globalization.CultureInfo.InvariantCulture) + ")"
        :
        "false";

    private static PineControlFlowOperation? LengthCondition(PineBasicBlock block) =>
        block.Operations.LastOrDefault() is { Instruction.Kind: StackInstructionKind.Length_Equal_Const } operation &&
        block.Terminator is PineControlFlowTerminator.ConditionalJump branch &&
        (branch.Literal.Equals(PineKernelValues.TrueValue) || branch.Literal.Equals(PineKernelValues.FalseValue)) &&
        !branch.BranchArguments.Contains(operation.Results[0]) &&
        !branch.FallThroughArguments.Contains(operation.Results[0])
        ?
        operation
        :
        null;

    private string BinaryEquality(string left, string right, bool negated)
    {
        if (ConstantValue(right) is { } rightLiteral)
            return Equal(left, rightLiteral, negated);

        if (ConstantValue(left) is { } leftLiteral)
            return Equal(right, leftLiteral, negated);

        if (_expressionScope.Integer(left) is { } leftInteger &&
            _expressionScope.Integer(right) is { } rightInteger)
        {
            return PineCSharpExpressionScope.Equality(leftInteger, rightInteger, negated);
        }

        var fallback = "PineValueInProcess.AreEqual(" + Pine(left) + ", " + Pine(right) + ")";
        return negated ? "!(" + fallback + ")" : fallback;
    }

    private bool CanInlineCases(IReadOnlyList<PineValue> literals)
    {
        var index = 0;

        while (index < literals.Count)
        {
            var shape = PineCSharpConstantMatch.Shape(literals[index]);
            var end = index + 1;

            while (end < literals.Count && PineCSharpConstantMatch.Shape(literals[end]) == shape)
                ++end;

            if (!PineCSharpConstantMatch.CanInline(literals.Skip(index).Take(end - index)
                .Select(literal => new PineCSharpConstantMatch.Case(literal, _ => { })).ToArray(),
                _options.MaximumInlineMatchDifferingPositions))
            {
                return false;
            }

            index = end;
        }

        return literals.Count > 0;
    }

    private void EmitCursor(StringBuilder text, Function function, PineCSharpCursorPlan cursor)
    {
        var offsetParameter = cursor.OffsetParameter;
        var count = function.Prepared.Parameters.ParamsPaths.Count;
        var source = Project("argument_" + cursor.SourceParameter, cursor.SourcePath);
        Cancellation(text);
        text.AppendLine("PineValueInProcess source = " + source + ";");
        text.AppendLine("BigInteger? initialOffset = argument_" + offsetParameter + ".AsInteger();");
        text.AppendLine("if (initialOffset is not { } offset) {");
        EmitMiss();
        text.AppendLine("}");
        text.AppendLine("BigInteger initialByteOffset = offset * " + Integer(cursor.Multiplier) + ";");
        text.AppendLine("int firstByteOffset = initialByteOffset < 0 ? 0 : (int)initialByteOffset;");
        text.AppendLine("if (source.EvaluateIfBlob() is not { } sourceBlob) {");
        EmitMiss();
        text.AppendLine("}");
        text.AppendLine("if (sourceBlob.Bytes.Length - firstByteOffset < " + cursor.MinimumLength + ") {");
        EmitMiss();
        text.AppendLine("}");
        text.AppendLine("BlobMemory sourceBytes = sourceBlob.Bytes;");
        var scope = new PineCSharpExpressionScope();
        scope.SetValue("source", new("sourceBytes", PineCSharpExpressionType.BlobMemory));
        scope.SetInteger("offset", new("offset", PineCSharpExpressionType.BigInteger));
        text.AppendLine("bool offsetChanged = false;");
        var stateParameters = new HashSet<int>();
        CollectParameters(cursor.Body);

        for (var index = 0; index < count; index++)
        {
            if (index != offsetParameter && stateParameters.Contains(index))
            {
                _retainedLocals.Add("cursor_" + index);
                text.AppendLine("NativeValue cursor_" + index + " = NativeValue.FromValue(argument_" + index + ");");
            }
        }

        text.AppendLine("while (true) {");
        Cancellation(text);
        text.AppendLine("BigInteger byteOffsetValue = offset * " + Integer(cursor.Multiplier) + ";");
        text.AppendLine("int byteOffset = byteOffsetValue < 0 ? 0 : (int)byteOffsetValue;");
        text.AppendLine("if (sourceBytes.Length - byteOffset < " + cursor.MinimumLength + ") {");
        EmitReturn(cursor.Miss.Values);
        text.AppendLine("}");
        var suffix = 0;
        EmitDecision(cursor.Body);
        text.AppendLine("}");

        void CollectValue(CursorValue value)
        {
            switch (value)
            {
                case CursorValue.Parameter parameter:
                    stateParameters.Add(parameter.Index);
                    break;

                case CursorValue.Projection projection:
                    CollectValue(projection.Source);
                    break;

                case CursorValue.Add add:
                    CollectValue(add.Operand);
                    break;

                case CursorValue.Constant:
                    break;

                default:
                    throw new NotImplementedException("CollectValue does not handle: " + value.GetType().Name);
            }
        }

        void CollectParameters(CursorDecision decision)
        {
            switch (decision)
            {
                case CursorDecision.Return returned:
                    foreach (var value in returned.Values)
                        CollectValue(value);

                    break;

                case CursorDecision.Continue next:
                    for (var index = 0; index < next.Arguments.Length; index++)
                    {
                        if (next.Arguments[index] != new CursorValue.Parameter(index))
                        {
                            stateParameters.Add(index);
                            CollectValue(next.Arguments[index]);
                        }
                    }

                    break;

                case CursorDecision.Match match:
                    foreach (var item in match.Cases)
                        CollectParameters(item.Next);

                    CollectParameters(match.Otherwise);
                    break;

                default:
                    throw new NotImplementedException("CollectParameters does not handle: " + decision.GetType().Name);
            }
        }

        void EmitMiss() =>
            text.AppendLine(
                "return " +
                Tuple(
                    cursor.Miss.Values.Select(
                        value => value switch
                        {
                            CursorValue.Parameter parameter => "argument_" + parameter.Index,
                            CursorValue.Constant constant => Literal(constant.Literal),

                            CursorValue.Projection or CursorValue.Add =>
                            throw new InvalidOperationException("Cursor miss is not an original value."),

                            _ =>
                            throw new NotImplementedException("EmitMiss does not handle: " + value.GetType().Name)
                        })) + ";");

        string Number(CursorValue value) =>
            value switch
            {
                CursorValue.Parameter parameter when parameter.Index == offsetParameter => "offset",

                CursorValue.Add add =>
                "(" + Number(add.Operand) +
                (add.Amount.Sign < 0 ? " - " : " + ") + Integer(BigInteger.Abs(add.Amount)) + ")",

                CursorValue.Parameter or CursorValue.Constant or CursorValue.Projection =>
                throw new InvalidOperationException("Cursor offset is not a numeric induction expression."),

                _ =>
                throw new NotImplementedException("Number does not handle: " + value.GetType().Name)
            };

        string Native(CursorValue value) =>
            value switch
            {
                CursorValue.Parameter parameter =>
                parameter.Index == offsetParameter
                ?
                "NativeValue.FromInteger(offset)"
                :
                "cursor_" + parameter.Index,

                CursorValue.Constant constant => "NativeValue.FromValue(" + Literal(constant.Literal) + ")",

                CursorValue.Add add =>
                "NativeValue.Add(" + Native(add.Operand) + ", " + NativeInteger(add.Amount) + ")",

                CursorValue.Projection projection =>
                "NativeValue.FromValue((" + Native(projection.Source) +
                ").ToPineValue().GetElementAt(" + projection.Index + "))",

                _ =>
                throw new NotImplementedException("Native does not handle: " + value.GetType().Name)
            };

        string PineResult(CursorValue value) =>
            value is CursorValue.Parameter parameter && parameter.Index == offsetParameter
            ?
            "(offsetChanged ? PineValueInProcess.CreateInteger(offset) : argument_" + offsetParameter + ")"
            :
            "(" + Native(value) + ").ToPineValue()";

        void EmitReturn(IEnumerable<CursorValue> values) =>
            text.AppendLine("return " + Tuple(values.Select(PineResult)) + ";");

        void EmitDecision(CursorDecision decision)
        {
            switch (decision)
            {
                case CursorDecision.Return returned:
                    EmitReturn(returned.Values);
                    break;

                case CursorDecision.Continue next:
                    var destinations = new List<string>();
                    var arguments = new List<string>();

                    for (var index = 0; index < count; index++)
                    {
                        if (next.Arguments[index] == new CursorValue.Parameter(index))
                            continue;

                        destinations.Add(index == offsetParameter ? "offset" : "cursor_" + index);

                        arguments.Add(
                            index == offsetParameter ? Number(next.Arguments[index]) : Native(next.Arguments[index]));
                    }

                    EmitAssignments(
                        text,
                        destinations.Zip(arguments).Select(
                            pair =>
                            (Target: pair.First,
                            Expression: pair.Second,
                            Type: pair.First == "offset" ? "BigInteger" : "NativeValue")).ToArray());

                    if (next.Arguments[offsetParameter] != new CursorValue.Parameter(offsetParameter))
                        text.AppendLine("offsetChanged = true;");

                    text.AppendLine("continue;");
                    break;

                case CursorDecision.Match match:
                    var position = 0;

                    while (position < match.Cases.Length)
                    {
                        var length = PineCSharpConstantMatch.Shape(match.Cases[position].Literal).Length;
                        var end = position + 1;

                        while (end < match.Cases.Length &&
                            PineCSharpConstantMatch.Shape(match.Cases[end].Literal).Length == length)
                        {
                            ++end;
                        }

                        var cases =
                            match.Cases[position..end].Select(
                                item =>
                                new PineCSharpConstantMatch.Case(item.Literal, _ => EmitDecision(item.Next))).ToArray();

                        var directMiss =
                            position == 0 && end == match.Cases.Length &&
                            match.Otherwise is CursorDecision.Return;

                        if (directMiss)
                        {
                            if (length != cursor.MinimumLength)
                            {
                                text.AppendLine("if (sourceBytes.Length - byteOffset < " + length + ") {");
                                EmitDecision(match.Otherwise);
                                text.AppendLine("}");
                            }
                        }
                        else
                            text.AppendLine("if (sourceBytes.Length - byteOffset >= " + length + ") {");

                        PineCSharpConstantMatch.Emit(
                            text,
                            cases,
                            scope.Value("source")!,
                            "byteOffset",
                            "cursor_" + suffix++,
                            MaterializedLiteral,
                            directMiss ? _ => EmitDecision(match.Otherwise) : null);

                        if (!directMiss)
                            text.AppendLine("}");

                        if (directMiss)
                            return;

                        position = end;
                    }

                    EmitDecision(match.Otherwise);
                    break;

                default:
                    throw new NotImplementedException("EmitDecision does not handle: " + decision.GetType().Name);
            }
        }
    }

    private void EmitMatchCases(
        StringBuilder text,
        string source,
        IReadOnlyList<PineCSharpConstantMatch.Case> cases,
        string? offset = null)
    {
        var position = 0;

        while (position < cases.Count)
        {
            var shape = PineCSharpConstantMatch.Shape(cases[position].Literal);
            var end = position + 1;

            while (end < cases.Count && PineCSharpConstantMatch.Shape(cases[end].Literal) == shape)
                ++end;

            var group = cases.Skip(position).Take(end - position).ToArray();

            if (!PineCSharpConstantMatch.CanInline(group, _options.MaximumInlineMatchDifferingPositions))
            {
                foreach (var item in group)
                {
                    text.AppendLine(
                        "if (" +
                        (offset is null
                        ?
                        Equal(source, item.Literal)
                        :
                        Pine(source) + ".SliceSkipVarEqualConst(" + offset + ", " + MaterializedLiteral(item.Literal) +
                        ")") +
                        ") {");

                    item.Matched(text);
                    text.AppendLine("}");
                }

                position = end;
                continue;
            }

            var suffix = "match_" + _lookupIndex++;
            var localSource = "source_" + suffix;
            var memory = "memory_" + suffix;
            var branchScope = new PineCSharpExpressionScope(_expressionScope);

            var view =
                new PineCSharpExpression(
                    memory,
                    shape.Blob ? PineCSharpExpressionType.BlobMemory : PineCSharpExpressionType.ListMemory);

            branchScope.SetValue(source, view);
            text.AppendLine("{");
            var items = localSource + (shape.Blob ? ".Bytes" : ".Items");

            text.AppendLine(
                "if (" + Pine(source) + (shape.Blob ? ".EvaluateIfBlob()" : ".EvaluateIfList()") +
                " is { } " + localSource + " && " + items + ".Length " +
                (offset is null ? "== " : "- " + offset + " >= ") + shape.Length + ") {");

            text.AppendLine((shape.Blob ? "BlobMemory " : "ListMemory ") + memory + " = " + items + ";");

            PineCSharpConstantMatch.Emit(
                text,
                group,
                branchScope.Value(source)!,
                offset ?? "0",
                suffix,
                MaterializedLiteral);

            text.AppendLine("}");
            text.AppendLine("}");
            position = end;
        }
    }
}
