using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Function = Pine.Core.DotNet.PineCSharpCompiler.Function;
using Kind = Pine.Core.DotNet.PineCSharpLocalPlan.Kind;
using LocalType = Pine.Core.DotNet.PineCSharpLocalType;

namespace Pine.Core.DotNet;

internal sealed partial class PineCSharpEmitter
{
    private IReadOnlyDictionary<string, LocalType> _localTypes = new Dictionary<string, LocalType>();

    private IReadOnlyDictionary<string, LocalType> PlanLocals(IReadOnlyList<Function> group)
    {
        var plan = new PineCSharpLocalPlan();

        foreach (var function in group)
        {
            string V(PineVirtualValueId id) => Value(function, id);
            string L(int index) => Local(function, index);

            for (var index = 0; index < function.Prepared.Parameters.ParamsPaths.Count; index++)
                plan.Seed(L(index), Kind.Pine);

            foreach (var block in _plans[function].Blocks)
            {
                var stack = block.Parameters.ToList();

                foreach (var operation in block.Operations)
                {
                    var instruction = operation.Instruction;

                    switch (instruction.Kind)
                    {
                        case StackInstructionKind.Local_Get:
                            foreach (var (value, local) in operation.Results.Zip(instruction.LocalIndices))
                                plan.Copy(V(value), L(local));

                            break;

                        case StackInstructionKind.Local_Set:
                            for (var index = 0; index < instruction.LocalIndices.Length; index++)
                                plan.Copy(L(instruction.LocalIndices[index]), V(stack[stack.Count - index - 1]));

                            break;

                        case StackInstructionKind.Local_Set_Literal:
                            plan.Seed(
                                L(instruction.SingleLocalIndex),
                                PineCSharpLocalPlan.LiteralKind(instruction.Literal!.Evaluate()));

                            break;

                        case StackInstructionKind.Local_Int_Add_Const:
                            foreach (var index in instruction.LocalIndices)
                                plan.Seed(L(index), Kind.Integer | Kind.Empty);

                            break;

                        case StackInstructionKind.Local_Get_Skip_Head_Const:
                            foreach (var index in instruction.LocalIndices)
                                plan.ObservePine(L(index));

                            goto default;

                        default:
                            foreach (var result in operation.Results)
                                plan.Seed(V(result), ResultKind(instruction));

                            break;
                    }

                    for (var index = 0; index < operation.Inputs.Length; index++)
                    {
                        if (NeedsPineInput(instruction.Kind, index))
                            plan.ObservePine(V(operation.Inputs[index]));
                    }

                    stack.RemoveRange(stack.Count - operation.Inputs.Length, operation.Inputs.Length);
                    stack.AddRange(operation.Results);
                }

                if (block.Operations.LastOrDefault()?.Instruction.Kind == StackInstructionKind.Parse_Error_Const)
                    continue;

                if (_tailCalls[function].Contains(block.Id))
                {
                    var callee = function.Calls[block.Id];

                    var (instruction, inputs) =
                        block.Terminator switch
                        {
                            PineControlFlowTerminator.Invoke invoke => (invoke.Instruction, invoke.Inputs.ToArray()),

                            PineControlFlowTerminator.TailInvoke tail =>
                            (tail.InvokeInstruction,
                            stack.TakeLast(StackInstruction.GetStackEffect(tail.InvokeInstruction).PopCount).ToArray()),

                            PineControlFlowTerminator.Return or PineControlFlowTerminator.ConditionalJump or
                            PineControlFlowTerminator.Jump or PineControlFlowTerminator.Switch or
                            PineControlFlowTerminator.ValueMap =>
                            throw new InvalidOperationException("Expected a tail call."),

                            _ =>
                            throw new NotImplementedException(
                                "PlanLocals does not handle terminator: " + block.Terminator.GetType().Name)
                        };

                    if (_groupIndex[function] == _groupIndex[callee] &&
                        instruction.Kind != StackInstructionKind.Eval_Const)
                    {
                        for (var index = 0; index < inputs.Length; index++)
                            plan.Copy(Local(callee, index), V(inputs[index]));
                    }
                    else
                    {
                        foreach (var input in inputs)
                            plan.ObservePine(V(input));
                    }

                    continue;
                }

                if (block.Terminator is PineControlFlowTerminator.Invoke call)
                {
                    foreach (var input in call.Inputs)
                        plan.ObservePine(V(input));

                    var resultCount = StackInstruction.GetStackEffect(call.Instruction).PushCount;

                    foreach (var result in call.Arguments.TakeLast(resultCount))
                        plan.Seed(V(result), Kind.Pine);
                }

                if (block.Terminator is PineControlFlowTerminator.ValueMap map)
                {
                    plan.ObservePine(V(map.Input));
                    plan.Seed(V(map.Result), Kind.Pine);
                }

                foreach (var (target, arguments) in PineCSharpFunctionPlan.Edges(block.Terminator))
                {
                    foreach (var (parameter, argument) in function.Graph.Blocks[target.Value].Parameters.Zip(arguments))
                        plan.Copy(V(parameter), V(argument));
                }
            }
        }

        return plan.Finish();
    }

    private static Kind ResultKind(StackInstruction instruction) =>
        instruction.Kind switch
        {
            StackInstructionKind.Push_Literal => PineCSharpLocalPlan.LiteralKind(instruction.Literal!.Evaluate()),
            StackInstructionKind.Length => Kind.Integer,

            StackInstructionKind.Int_Add_Binary or StackInstructionKind.Int_Add_Const or
            StackInstructionKind.Int_Mul_Binary or StackInstructionKind.Int_Mul_Const or
            StackInstructionKind.Int_Mul_Const_Add_Binary =>
            Kind.Integer | Kind.Empty,

            StackInstructionKind.Int_Less_Than_Binary or StackInstructionKind.Int_Less_Than_Or_Equal_Binary or
            StackInstructionKind.Int_Less_Than_Const or StackInstructionKind.Int_Less_Than_Or_Equal_Const or
            StackInstructionKind.Int_Greater_Than_Or_Equal_Const =>
            Kind.Boolean | Kind.Empty,

            StackInstructionKind.Length_Equal_Const or StackInstructionKind.Equal_Binary or
            StackInstructionKind.Not_Equal_Binary or StackInstructionKind.Equal_Binary_Const or
            StackInstructionKind.Not_Equal_Binary_Const or StackInstructionKind.Is_List_Value or
            StackInstructionKind.Is_Blob_Value or StackInstructionKind.Logical_And_Binary or
            StackInstructionKind.Slice_Skip_Var_Equal_Const =>
            Kind.Boolean,

            _ =>
            Kind.Pine
        };

    private static bool NeedsPineInput(StackInstructionKind kind, int index) =>
        kind switch
        {
            StackInstructionKind.Local_Set or StackInstructionKind.Pop or
            StackInstructionKind.Int_Add_Binary or StackInstructionKind.Int_Add_Const or
            StackInstructionKind.Int_Mul_Binary or StackInstructionKind.Int_Mul_Const or
            StackInstructionKind.Int_Mul_Const_Add_Binary or StackInstructionKind.Int_Less_Than_Binary or
            StackInstructionKind.Int_Less_Than_Or_Equal_Binary or StackInstructionKind.Int_Less_Than_Const or
            StackInstructionKind.Int_Less_Than_Or_Equal_Const or StackInstructionKind.Int_Greater_Than_Or_Equal_Const or
            StackInstructionKind.Equal_Binary or StackInstructionKind.Not_Equal_Binary or
            StackInstructionKind.Equal_Binary_Const or StackInstructionKind.Not_Equal_Binary_Const or
            StackInstructionKind.Logical_And_Binary or StackInstructionKind.Negate =>
            false,

            StackInstructionKind.Skip_Binary or StackInstructionKind.Take_Binary or StackInstructionKind.Skip_Head_Binary or
            StackInstructionKind.Slice_Skip_Var_Take_Const or StackInstructionKind.Slice_Skip_Var_Take_Var or
            StackInstructionKind.Slice_Skip_Var_Equal_Const or StackInstructionKind.Bit_Shift_Left_Binary or
            StackInstructionKind.Bit_Shift_Right_Binary =>
            index == 0,

            _ =>
            true
        };

    private LocalType Storage(string variable) => _localTypes.GetValueOrDefault(variable);

    private string TypeOf(string variable) => TypeName(Storage(variable));

    private static string TypeName(LocalType type) =>
        type switch
        {
            LocalType.Pine => "PineValueInProcess",
            LocalType.Integer => "BigInteger",
            LocalType.OptionalInteger => "BigInteger?",
            LocalType.Boolean => "bool",
            LocalType.OptionalBoolean => "bool?",
            LocalType.IntegerWithOriginal => "(PineValueInProcess? Original, BigInteger? Number)",

            _ =>
            throw new ArgumentOutOfRangeException(nameof(type))
        };

    private string Empty(string variable) =>
        Storage(variable) == LocalType.Pine ? "PineValueInProcess.EmptyList" : "default";

    private string Pine(string variable) => AsPine(variable, Storage(variable));

    private string AsPine(string code, LocalType type)
    {
        string Construct(string factory, string argument) => "PineValueInProcess." + factory + "(" + argument + ")";

        string Optional(string factory, string argument)
        {
            var value = "scalar_" + _lookupIndex++;

            return
                "(" + argument + " is { } " + value + " ? " + Construct(factory, value) +
                " : PineValueInProcess.EmptyList)";
        }

        return type switch
        {
            LocalType.Pine => code,
            LocalType.Integer => Construct("CreateInteger", code),
            LocalType.OptionalInteger => Optional("CreateInteger", code),
            LocalType.Boolean => Construct("CreateBool", code),
            LocalType.OptionalBoolean => Optional("CreateBool", code),

            LocalType.IntegerWithOriginal =>
            "(" + code + ".Original ?? " + Optional("CreateInteger", code + ".Number") + ")",

            _ =>
            throw new ArgumentOutOfRangeException(nameof(type))
        };
    }

    private string Numeric(string variable) => AsNumber(variable, Storage(variable));

    private static string AsNumber(string code, LocalType type) =>
        type switch
        {
            LocalType.Pine => "(" + code + ").AsInteger()",
            LocalType.Integer or LocalType.OptionalInteger => code,
            LocalType.IntegerWithOriginal => "(" + code + ".Original?.AsInteger() ?? " + code + ".Number)",
            LocalType.Boolean or LocalType.OptionalBoolean => "(BigInteger?)null",

            _ =>
            throw new ArgumentOutOfRangeException(nameof(type))
        };

    private string ConvertValue(string destination, string code, LocalType type) =>
        ConvertValue(Storage(destination), code, type);

    private string ConvertValue(LocalType destination, string code, LocalType type)
    {
        if (destination == type)
            return code;

        return destination switch
        {
            LocalType.Pine => AsPine(code, type),

            LocalType.IntegerWithOriginal when type is LocalType.Integer or LocalType.OptionalInteger =>
            "(null, " + code + ")",

            LocalType.IntegerWithOriginal => "(" + AsPine(code, type) + ", null)",
            LocalType.OptionalInteger => AsNumber(code, type),
            LocalType.OptionalBoolean when type == LocalType.Boolean => code,
            LocalType.OptionalBoolean when code == "PineValueInProcess.EmptyList" => "null",

            LocalType.OptionalBoolean when type == LocalType.Pine =>
            "(" + code + ".IsBlob() ? (bool?)PineValueInProcess.AreEqual(" + code +
            ", PineValueInProcess.KernelTrueValue) : null)",

            _ =>
            throw new InvalidOperationException("Cannot copy " + type + " to " + destination + ": " + code)
        };
    }

    private void WriteLiteral(StringBuilder text, string destination, PineValue literal)
    {
        var kind = PineCSharpLocalPlan.LiteralKind(literal);
        var type = Storage(destination);

        if (type != LocalType.Pine && kind == Kind.Integer)
        {
            var integer = IntegerEncoding.ParseSignedIntegerStrict(literal).IsOkOrNullable()!.Value;
            WriteValue(text, destination, BigIntegerConstant(integer), LocalType.Integer);
        }
        else if (type != LocalType.Pine && kind == Kind.Boolean)
        {
            WriteValue(
                text,
                destination,
                literal.Equals(PineKernelValues.TrueValue) ? "true" : "false",
                LocalType.Boolean);
        }
        else
            WriteValue(text, destination, Literal(literal));

        _constantValues[destination] = literal;
    }

    private string Truth(string variable) =>
        Storage(variable) switch
        {
            LocalType.Boolean => variable,
            LocalType.OptionalBoolean => variable + " == true",

            _ =>
            Equal(variable, PineKernelValues.TrueValue)
        };

    private (string Code, LocalType Type)? IntegerComparison(
        StackInstruction instruction, string[] inputs, string result, bool? matchFalse = null)
    {
        var (op, complement) =
            instruction.Kind switch
            {
                StackInstructionKind.Int_Less_Than_Binary or StackInstructionKind.Int_Less_Than_Const => ("<", ">="),

                StackInstructionKind.Int_Less_Than_Or_Equal_Binary or StackInstructionKind.Int_Less_Than_Or_Equal_Const =>
                ("<=", ">"),

                StackInstructionKind.Int_Greater_Than_Or_Equal_Const => (">=", "<"),

                _ =>
                (null, null)
            };

        if (op is null)
            return null;

        var left = (inputs[0], Storage(inputs[0]));

        var right =
            instruction.IntegerLiteral is { } literal
            ?
            (BigIntegerConstant(literal), LocalType.Integer)
            :
            (inputs[1], Storage(inputs[1]));

        return
            (IntegerOperation(
                matchFalse == true ? complement! : op,
                left,
                right,
                result,
                comparison: true,
                branch: matchFalse is not null),
            left.Item2 == LocalType.Integer && right.Item2 == LocalType.Integer || matchFalse is not null
            ?
            LocalType.Boolean
            :
            LocalType.OptionalBoolean);
    }

    private static string IntegerOperation(
        string operation,
        (string Code, LocalType Type) left,
        (string Code, LocalType Type) right,
        string suffix,
        bool comparison = false,
        string? multiplier = null,
        bool branch = false)
    {
        var a = AsNumber(left.Code, left.Type);
        var b = AsNumber(right.Code, right.Type);

        string Apply(string first, string second) =>
            first + " " + operation + " " + (multiplier is null ? second : "(" + multiplier + " * " + second + ")");

        if (left.Type == LocalType.Integer && right.Type == LocalType.Integer)
            return Apply(a, b);

        // Lift arithmetic and branch tests when the right operand is already a scalar.
        // A lifted relational test is false on null, including when matching kernel False
        // via the complementary operator. Otherwise decode the right only after the left succeeds.
        if ((!comparison || branch) && right.Type is LocalType.Integer or LocalType.OptionalInteger)
            return Apply(a, b);

        var lhs = "left_" + suffix;
        var rhs = "right_" + suffix;
        var checks = new List<string>();

        if (left.Type != LocalType.Integer)
            checks.Add(a + " is { } " + lhs);

        else
            lhs = a;

        if (right.Type != LocalType.Integer)
            checks.Add(b + " is { } " + rhs);

        else
            rhs = b;

        if (branch)
            return string.Join(" && ", checks.Append(Apply(lhs, rhs)));

        return
            "(" + string.Join(" && ", checks) + " ? (" + (comparison ? "bool?" : "BigInteger?") + ")(" +
            Apply(lhs, rhs) + ") : null)";
    }
}
