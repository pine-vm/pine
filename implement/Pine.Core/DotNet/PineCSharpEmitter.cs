using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core.Addressing;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Numerics;
using System.Text;

using Function = Pine.Core.DotNet.PineCSharpCompiler.Function;
using LocalType = Pine.Core.DotNet.PineCSharpLocalType;

namespace Pine.Core.DotNet;

internal sealed partial class PineCSharpEmitter
{
    private readonly IReadOnlyList<Function> _functions;

    private readonly IReadOnlyList<Function> _roots;

    private readonly PineCSharpCompilationOptions _options;

    private readonly ConcurrentPineValueHashCache _canonicalHashCache;

    private readonly PineCSharpLiteralNaming _literalNaming;

    private readonly Dictionary<Function, string> _methodNames = [];

    private readonly string _dispatchMethodName;

    private readonly Dictionary<PineValue, string> _literalNames = [];

    private readonly List<PineValue> _literals = [];

    private readonly Dictionary<string, PineValue> _literalValues = [];

    private readonly Dictionary<string, PineValue> _constantValues = [];

    private readonly Dictionary<PineValue, (string Name, KeyValuePair<PineValue, int>[] Items)> _switchTables = [];

    private readonly Dictionary<PineValue, (string Name, KeyValuePair<PineValue, PineValue>[] Items)> _valueMaps = [];

    private readonly Dictionary<Function, HashSet<PineBlockId>> _tailCalls = [];

    private readonly Dictionary<Function, PineControlFlowLoopAnalysis> _loops = [];

    private readonly Dictionary<Function, PineCSharpFunctionPlan> _plans = [];

    private readonly HashSet<string> _retainedLocals = [];

    private readonly List<List<Function>> _groups;

    private readonly Dictionary<Function, int> _groupIndex = [];

    private int _lookupIndex;

    private int _assignmentIndex;

    public PineCSharpEmitter(
        IReadOnlyList<Function> functions,
        IReadOnlyList<Function> roots,
        PineCSharpCompilationOptions options,
        PineCSharpMethodNaming? methodNaming = null,
        ConcurrentPineValueHashCache? canonicalHashCache = null)
    {
        _functions = functions;
        _roots = roots;
        _options = options;
        _canonicalHashCache = canonicalHashCache ?? new();
        _literalNaming = new(_canonicalHashCache);
        methodNaming ??= PineCSharpMethodNaming.Default(_canonicalHashCache);
        var names = new HashSet<string>(StringComparer.Ordinal);

        string Register(string name)
        {
            PineCSharpCompiler.ValidateIdentifier(name, "method");

            if (name == options.ClassName ||
                name is "PineValue" or "PineValueInProcess" or "SliceOffset" or "BigInteger" or
                "BlobMemory" or "ListMemory" or "Kernel" or "Specialized" ||
                name.StartsWith("Loop_", StringComparison.Ordinal))
            {
                throw new ArgumentException("Method name conflicts with generated declarations: " + name);
            }

            if (!names.Add(name))
                throw new ArgumentException("Duplicate generated method name: " + name);

            return name;
        }

        _dispatchMethodName = Register(methodNaming.GetDispatchMethodName(roots[0].Identity.Expression));

        foreach (var function in functions)
        {
            _tailCalls.Add(function, [.. function.Calls.Keys.Where(block => IsTailCall(function, block))]);
            _loops.Add(function, function.Graph.AnalyzeLoops());

            _plans.Add(
                function,
                new(function, _tailCalls[function]));
        }

        var reachable = new HashSet<Function>();
        var pending = new Stack<Function>(roots);

        while (pending.TryPop(out var function))
        {
            if (reachable.Add(function))
            {
                foreach (var callee in _plans[function].Calls.Values)
                    pending.Push(callee);
            }
        }

        _functions = [.. functions.Where(reachable.Contains)];

        foreach (var function in _functions)
        {
            _methodNames.Add(
                function,
                Register(
                    methodNaming.GetMethodName(
                        function.Identity.Expression,
                        function.Identity.EnvironmentConstraint)));
        }

        _groups =
            Components(
                _functions,
                function =>
                _tailCalls[function].Where(_plans[function].Calls.ContainsKey).OrderBy(block => block.Value)
                .Select(block => function.Calls[block]));

        for (var index = 0; index < _groups.Count; index++)
        {
            foreach (var function in _groups[index])
                _groupIndex.Add(function, index);
        }
    }

    public PineCSharpCompilation Emit()
    {
        var methods =
            new StringBuilder(Header()).AppendLine("public static class " + _options.ClassName).AppendLine("{");

        EmitEntry(methods);

        foreach (var function in _functions)
        {
            var group = _groups[_groupIndex[function]];
            EmitContract(methods, function);

            if (group.Count == 1)
                EmitBody(methods, group, MethodName(function), function);

            else
            {
                methods.AppendLine(
                    "public static " + ReturnType(function.Prepared.Returns) + " " +
                    MethodName(function) +
                    "(" +
                    string.Join(", ", Parameters(function).Concat(TokenParameter())) +
                    ")");

                methods.AppendLine("{");
                var arguments = new List<string> { function.Identity.Index.ToString(CultureInfo.InvariantCulture) };
                var types = PlanLocals(group);

                arguments.AddRange(
                    Enumerable.Range(0, group.Max(member => member.Prepared.Parameters.ParamsPaths.Count))
                    .Select(
                        index =>
                        {
                            var input =
                                index < function.Prepared.Parameters.ParamsPaths.Count
                                ?
                                "argument_" + index
                                :
                                "PineValueInProcess.EmptyList";

                            return ConvertValue(types.GetValueOrDefault("state_" + index), input, LocalType.Pine);
                        }));

                arguments.AddRange(TokenArgument());

                methods.AppendLine(
                    "return " + MethodReference("Loop_" + _groupIndex[function]) +
                    "(" + string.Join(", ", arguments) + ");");

                methods.AppendLine("}");
            }
        }

        for (var index = 0; index < _groups.Count; index++)
        {
            if (_groups[index].Count > 1)
                EmitBody(methods, _groups[index], "Loop_" + index, null);
        }

        methods.AppendLine("}");

        var source =
            PineCSharpSourceSimplification.Format(methods.ToString(), _retainedLocals, _options.LiteralsTypeName);

        var literalsSource = EmitLiterals();

        var recursionComponents =
            Components(
                Enumerable.Range(0, _groups.Count).ToArray(),
                group =>
                _groups[group].SelectMany(
                    function => _plans[function].Calls.Select(
                        call => (Function: function, Block: call.Key, Callee: call.Value)))
                .Where(call => _groupIndex[call.Callee] != group || !_tailCalls[call.Function].Contains(call.Block))
                .Select(call => _groupIndex[call.Callee]).Distinct());

        var hasRecursion =
            recursionComponents.Any(
                component =>
                component.Count > 1 ||
                _groups[component[0]].Any(
                    function => _plans[function].Calls.Any(
                        call => _groupIndex[call.Value] == component[0] && !_tailCalls[function].Contains(call.Key))));

        var hasLoops =
            _groups.Any(group => group.Count > 1) ||
            _functions.Any(
                function => _tailCalls[function].Any(
                    block =>
                    _plans[function].Calls.GetValueOrDefault(block) == function)) ||
            _functions.Any(
                function => _plans[function].Blocks.Any(
                    block => block.Operations.Any(
                        operation => operation.Instruction.Kind == StackInstructionKind.Blob_Trim_Leading_Zeros))) ||
            _functions.Any(function => _plans[function].HasCycles);

        return
            new(
                source,
                literalsSource,
                _options,
                [
                    .. _functions.Select(
                        function => new PineCSharpMethod(
                            function.Identity,
                            MethodName(function),
                            function.Prepared.Parameters,
                            function.Prepared.Returns))
                ],
                new(hasLoops, hasRecursion, recursionComponents.Any(component => component.Count > 1)),
                _dispatchMethodName);
    }

    private void EmitEntry(StringBuilder text)
    {
        _localTypes = new Dictionary<string, LocalType>();

        text.AppendLine(
            "public static PineValueInProcess" +
            (_roots.All(root => root.Identity.EnvironmentConstraint is not null) ? "?" : "") +
            " " +
            _dispatchMethodName +
            "(" +
            string.Join(", ", new[] { "PineValueInProcess environment" }.Concat(TokenParameter())) + ")");

        text.AppendLine("{");
        Cancellation(text);

        foreach (var root in _roots)
        {
            if (root.Identity.EnvironmentConstraint is { } constraint)
            {
                var tests =
                    constraint.ParsedItems.Select(
                        item =>
                        "PineValueInProcess.ValueAtPathEquals(environment, [" +
                        string.Join(", ", item.Key) + "], " + Literal(item.Value) + ")");

                text.AppendLine("if (" + string.Join(" && ", tests.DefaultIfEmpty("true")) + ")");
                text.AppendLine("{");
            }

            var args = root.Prepared.Parameters.ParamsPaths.Select(path => Project("environment", path)).ToArray();
            var target = ForwardTarget(root);
            var call = Call(target, args);

            if (_plans[target].IsNoOp)
            {
                if (root.Identity.EnvironmentConstraint is not null)
                    Cancellation(text);

                text.AppendLine(
                    "return " +
                    Reconstruct(
                        root.Prepared.Returns,
                        _ =>
                        throw new InvalidOperationException("No-op worker has a return component.")) + ";");

                if (root.Identity.EnvironmentConstraint is not null)
                    text.AppendLine("}");

                continue;
            }

            if (root.Prepared.Returns.Equals(ReturnValueLayout.Canonical))
                text.AppendLine("return " + call + ";");

            else
            {
                text.AppendLine(
                    ReturnType(root.Prepared.Returns) + " result_" + root.Identity.Index + " = " + call + ";");

                EmitReconstructedReturn(
                    text,
                    root.Prepared.Returns,
                    index => Component("result_" + root.Identity.Index, root.Prepared.Returns, index));
            }

            if (root.Identity.EnvironmentConstraint is not null)
                text.AppendLine("}");
        }

        if (_roots.All(root => root.Identity.EnvironmentConstraint is not null))
            text.AppendLine("return null;");

        text.AppendLine("}");
    }

    private void EmitBody(StringBuilder text, List<Function> group, string name, Function? entry)
    {
        var returns = group[0].Prepared.Returns;
        _localTypes = PlanLocals(group);

        var parameters =
            entry is not null
            ?
            Parameters(entry).ToList()
            :
            [
            .. Enumerable.Range(0, group.Max(function => function.Prepared.Parameters.ParamsPaths.Count)).Select(
                index => TypeOf("state_" + index) + " state_" + index)
            ];

        if (entry is null)
            parameters.Insert(0, "int entry");

        parameters.AddRange(TokenParameter());

        text.AppendLine(
            (entry is null ? "private" : "public") + " static " + ReturnType(returns) + " " +
            name + "(" + string.Join(", ", parameters) + ")");

        text.AppendLine("{");

        if (entry is not null &&
            PineCSharpCursorPlan.TryCreate(entry, _options.MaximumInlineMatchDifferingPositions) is { } cursor)
        {
            EmitCursor(text, entry, cursor);
            text.AppendLine("}");
            return;
        }

        PrepareProjectionCaches(text, group);

        if (group.Any(
            function => _plans[function].Blocks.Any(
                block =>
                block.Terminator is PineControlFlowTerminator.Switch { Kind: PineSwitchKind.SliceSkipVarEqual } ||
                block.Operations.Any(
                    operation => operation.Instruction.Kind == StackInstructionKind.Slice_Skip_Var_Equal_Const))))
        {
            text.AppendLine("SliceOffset sliceOffset = default;");
        }

        if (entry is not null)
        {
            for (var index = 0; index < entry.Prepared.Parameters.ParamsPaths.Count; index++)
            {
                text.AppendLine(
                    TypeOf(Local(entry, index)) + " " + Local(entry, index) + " = " +
                    ConvertValue(Local(entry, index), "argument_" + index, LocalType.Pine) + ";");
            }
        }

        foreach (var function in group)
        {
            RetainLoopCarriedLocals(
                function,
                group.Count > 1 ||
                _tailCalls[function].Any(block => function.Calls[block] == function));

            var parameterCount = function.Prepared.Parameters.ParamsPaths.Count;

            var instructions =
                _plans[function].Blocks.SelectMany(block => block.Operations.Select(op => op.Instruction));

            var localCount = StackFrameInstructions.ComputeLocalsCount([.. instructions], function.Prepared.Parameters);

            for (var index = parameterCount; index < localCount; index++)
            {
                text.AppendLine(
                    TypeOf(Local(function, index)) + " " + Local(function, index) + " = " +
                    Empty(Local(function, index)) + ";");
            }

            var values =
                _plans[function].Blocks.SelectMany(
                    block =>
                    block.Parameters.Concat(block.Operations.SelectMany(op => op.Results)).Concat(
                        block.Terminator is PineControlFlowTerminator.Invoke invoke
                        ?
                        invoke.Arguments
                        :
                        block.Terminator is PineControlFlowTerminator.ValueMap map ? [map.Result] : []));

            foreach (var value in values.Distinct().OrderBy(value => value.Value))
            {
                text.AppendLine(
                    TypeOf(Value(function, value)) + " " + Value(function, value) + " = " +
                    Empty(Value(function, value)) + ";");
            }
        }

        if (entry is not null)
            text.AppendLine("goto " + Label(entry, entry.Graph.Entry) + ";");

        else
        {
            foreach (var function in group)
            {
                text.AppendLine(
                    "if (entry == " + function.Identity.Index + ") goto " + Label(function, function.Graph.Entry) + ";");
            }

            text.AppendLine("throw new global::System.ArgumentOutOfRangeException(nameof(entry));");
        }

        foreach (var function in group)
        {
            foreach (var block in _plans[function].Blocks)
            {
                _constantValues.Clear();

                text.AppendLine(Label(function, block.Id) + ":");
                text.AppendLine("{");

                if (block.Id == function.Graph.Entry)
                    Cancellation(text);

                var stack = block.Parameters.Select(value => Value(function, value)).ToList();
                var branchCondition = BranchCondition(function, block);

                for (var operationIndex = 0; operationIndex < block.Operations.Length; operationIndex++)
                {
                    if (EmitConcatChain(text, function, block, ref operationIndex, stack))
                        continue;

                    var operation = block.Operations[operationIndex];

                    if (operation != branchCondition?.Operation)
                        EmitOperation(text, function, block.Id, operationIndex, operation, stack);

                    if (operation.Instruction.Kind == StackInstructionKind.Parse_Error_Const)
                        break;

                    stack.RemoveRange(stack.Count - operation.Inputs.Length, operation.Inputs.Length);
                    stack.AddRange(operation.Results.Select(value => Value(function, value)));
                }

                if (!block.Operations.Any(operation => operation.Instruction.Kind == StackInstructionKind.Parse_Error_Const))
                    EmitTerminator(text, function, block, stack, branchCondition?.Code);

                text.AppendLine("}");
            }
        }

        text.AppendLine("}");
    }

    private bool EmitConcatChain(
        StringBuilder text, Function function, PineBasicBlock block, ref int index, List<string> stack)
    {
        var first = block.Operations[index];

        if (first.Instruction.Kind != StackInstructionKind.Concat_Binary)
            return false;

        var parts = first.Inputs.Select(value => Pine(Value(function, value))).ToList();
        var end = index;
        var result = first.Results[0];

        while (end + 1 < block.Operations.Length)
        {
            var next = block.Operations[end + 1];

            if (next.Instruction.Kind != StackInstructionKind.Concat_Binary || next.Inputs[1] != result ||
                block.Operations.Sum(operation => operation.Inputs.Count(input => input == result)) != 1)
            {
                break;
            }

            parts.Insert(0, Pine(Value(function, next.Inputs[0])));
            result = next.Results[0];
            ++end;
        }

        if (end == index)
            return false;

        WriteValue(
            text,
            Value(function, result),
            "PineValueInProcess.ConcatManyRight([" + string.Join(", ", parts) + "])");

        for (var operationIndex = index; operationIndex <= end; operationIndex++)
        {
            var operation = block.Operations[operationIndex];
            stack.RemoveRange(stack.Count - operation.Inputs.Length, operation.Inputs.Length);
            stack.AddRange(operation.Results.Select(value => Value(function, value)));
        }

        index = end;
        return true;
    }

    private void EmitTerminator(
        StringBuilder text, Function function, PineBasicBlock block, List<string> stack, string? branchCondition)
    {
        string V(PineVirtualValueId value) => Value(function, value);

        switch (block.Terminator)
        {
            case PineControlFlowTerminator.Return returned:
                text.AppendLine(
                    "return " +
                    Tuple(
                        stack.TakeLast(returned.ValueCount).Select(
                            variable =>
                            Storage(variable) != LocalType.Pine && ConstantValue(variable) is { } literal
                            ?
                            Literal(literal)
                            :
                            Pine(variable))) + ";");

                break;

            case PineControlFlowTerminator.Jump jump:
                Edge(text, function, block.Id, jump.Target, jump.Arguments.Select(V));
                break;

            case PineControlFlowTerminator.ConditionalJump branch:
                var condition = branchCondition ?? Equal(stack[^1], branch.Literal);

                text.AppendLine("if (" + condition + ")");
                text.AppendLine("{");
                Edge(text, function, block.Id, branch.Branch, branch.BranchArguments.Select(V));
                text.AppendLine("}");
                Edge(text, function, block.Id, branch.FallThrough, branch.FallThroughArguments.Select(V));
                break;

            case PineControlFlowTerminator.Switch choice:
                void EmitSwitchMatch(string source, string? offset = null) =>
                    EmitMatchCases(
                        text,
                        source,
                        [.. choice.Cases.Select(item => (item.Literal, item.Target))],
                        target => Edge(text, function, block.Id, target, choice.Arguments.Select(V)),
                        offset);

                if (choice.Kind == PineSwitchKind.Equal)
                {
                    if (CanInlineCases([.. choice.Cases.Select(item => item.Literal)]))
                    {
                        EmitSwitchMatch(stack[^1]);

                        Edge(text, function, block.Id, choice.FallThrough, choice.Arguments.Select(V));
                        break;
                    }

                    if (choice.Cases.Length <= 2)
                    {
                        foreach (var item in choice.Cases)
                        {
                            text.AppendLine("if (" + Equal(stack[^1], item.Literal) + ") {");
                            Edge(text, function, block.Id, item.Target, choice.Arguments.Select(V));
                            text.AppendLine("}");
                        }

                        Edge(text, function, block.Id, choice.FallThrough, choice.Arguments.Select(V));
                        break;
                    }

                    var table = SwitchTable(choice.Cases);
                    var selected = "case_" + function.Identity.Index + "_" + block.Id.Value;

                    text.AppendLine(
                        "if (" + table + ".TryGetValue(" + Pine(stack[^1]) + ".Evaluate(), out var " + selected + "))");

                    text.AppendLine("{");

                    if (choice.Cases.Select(item => item.Target).Distinct().Count() == 1)
                    {
                        Edge(text, function, block.Id, choice.Cases[0].Target, choice.Arguments.Select(V));
                        text.AppendLine("}");
                        Edge(text, function, block.Id, choice.FallThrough, choice.Arguments.Select(V));
                        break;
                    }

                    text.AppendLine("switch (" + selected + ")");
                    text.AppendLine("{");

                    foreach (var (item, index) in choice.Cases.Select((item, index) => (item, index)))
                    {
                        text.AppendLine("case " + index + ":");
                        Edge(text, function, block.Id, item.Target, choice.Arguments.Select(V));
                    }

                    text.AppendLine(
                        "default: throw new global::System.InvalidOperationException(\"Invalid switch case in function " +
                        function.Identity.Index + ", block " + block.Id.Value + ".\");");

                    text.AppendLine("}");
                    text.AppendLine("}");
                }
                else
                {
                    var suffix = "switch_" + function.Identity.Index + "_" + block.Id.Value;
                    var offset = "offset_" + suffix;

                    text.AppendLine(
                        "if (sliceOffset.Get(" + Numeric(stack[^1]) + ", " +
                        Integer(choice.SkipCountMultiplier) + ") is { } " + offset + ")");

                    text.AppendLine("{");

                    EmitSwitchMatch(stack[^2], offset);

                    text.AppendLine("}");

                    foreach (var item in choice.Cases)
                    {
                        if (item.Literal.Equals(PineValue.EmptyList))
                        {
                            text.AppendLine("else");
                            text.AppendLine("{");
                            Edge(text, function, block.Id, item.Target, choice.Arguments.Select(V));
                            text.AppendLine("}");
                            break;
                        }
                    }
                }

                Edge(text, function, block.Id, choice.FallThrough, choice.Arguments.Select(V));
                break;

            case PineControlFlowTerminator.ValueMap map:
                var mapped = "mapped_" + _lookupIndex++;

                text.AppendLine(
                    "if (" + ValueMap(map.Map) + ".TryGetValue(" + Pine(V(map.Input)) +
                    ".Evaluate(), out var " + mapped + "))");

                text.AppendLine("{");
                WriteValue(text, V(map.Result), mapped);
                Edge(text, function, block.Id, map.FallThrough, map.Arguments.Select(V).Append(V(map.Result)));
                text.AppendLine("}");
                Edge(text, function, block.Id, map.Fallback, map.Arguments.Select(V));
                break;

            case PineControlFlowTerminator.Invoke invoke:
                var callee = function.Calls[block.Id];
                var inputs = invoke.Inputs.Select(V).ToArray();

                if (_tailCalls[function].Contains(block.Id))
                {
                    TailCall(text, function, callee, invoke.Instruction, inputs);
                    break;
                }

                var result = "call_" + function.Identity.Index + "_" + block.Id.Value;

                text.AppendLine(
                    ReturnType(callee.Prepared.Returns) + " " + result + " = " +
                    Invoke(callee, invoke.Instruction, inputs) + ";");

                var count = StackInstruction.GetStackEffect(invoke.Instruction).PushCount;

                var results =
                    invoke.Instruction.Kind == StackInstructionKind.Eval_Const
                    ?
                    [Reconstruct(callee.Prepared.Returns, index => Component(result, callee.Prepared.Returns, index))]
                    :
                    Enumerable.Range(0, count)
                    .Select(index => Component(result, callee.Prepared.Returns, index)).ToArray();

                Edge(
                    text,
                    function,
                    block.Id,
                    invoke.Continuation,
                    invoke.Arguments.Take(invoke.Arguments.Length - count).Select(V).Concat(results));

                break;

            case PineControlFlowTerminator.TailInvoke tail:
                TailCall(
                    text,
                    function,
                    function.Calls[block.Id],
                    tail.InvokeInstruction,
                    [.. stack.TakeLast(StackInstruction.GetStackEffect(tail.InvokeInstruction).PopCount)]);

                break;

            default:
                throw new NotImplementedException("EmitTerminator does not handle: " + block.Terminator.GetType().Name);
        }
    }

    private void TailCall(
        StringBuilder text,
        Function caller,
        Function callee,
        StackInstruction instruction,
        string[] inputs)
    {
        var arguments =
            instruction.Kind == StackInstructionKind.Eval_Const
            ?
            [.. callee.Prepared.Parameters.ParamsPaths.Select(path => Project(Pine(inputs[0]), path))]
            :
            inputs;

        if (_groupIndex[caller] == _groupIndex[callee])
        {
            Assign(text, Enumerable.Range(0, arguments.Length).Select(index => Local(callee, index)), arguments);
            text.AppendLine("goto " + Label(callee, callee.Graph.Entry) + ";");
        }
        else
        {
            var call = Call(callee, arguments.Select(Pine));

            if (instruction.Kind == StackInstructionKind.Eval_Const &&
                !callee.Prepared.Returns.Equals(ReturnValueLayout.Canonical))
            {
                text.AppendLine("var tailResult = " + call + ";");

                text.AppendLine(
                    "return " +
                    Reconstruct(
                        callee.Prepared.Returns,
                        index => Component("tailResult", callee.Prepared.Returns, index)) + ";");
            }
            else
                text.AppendLine("return " + call + ";");
        }
    }

    private string Invoke(Function callee, StackInstruction instruction, string[] inputs) =>
        Call(
            callee,
            instruction.Kind == StackInstructionKind.Eval_Const
            ?
            callee.Prepared.Parameters.ParamsPaths.Select(path => Project(Pine(inputs[0]), path))
            :
            inputs.Select(Pine));

    private string Call(Function function, IEnumerable<string> arguments) =>
        MethodReference(MethodName(function)) + "(" + string.Join(", ", arguments.Concat(TokenArgument())) + ")";

    private string MethodReference(string name) => "global::" + _options.MethodsTypeName + "." + name;

    private void Edge(
        StringBuilder text, Function function, PineBlockId source, PineBlockId target, IEnumerable<string> arguments)
    {
        if (target != function.Graph.Entry &&
            (_loops[function].Loops.Any(loop => loop.Backedges.Contains(new(source, target))) ||
            (target.Value <= source.Value &&
            _loops[function].IrreducibleCycles.Any(
                cycle =>
                cycle.Blocks.Contains(source) && cycle.Blocks.Contains(target)))))
        {
            Cancellation(text);
        }

        Assign(text, function.Graph.Blocks[target.Value].Parameters.Select(value => Value(function, value)), arguments);
        text.AppendLine("goto " + Label(function, target) + ";");
    }

    private void RetainLoopCarriedLocals(Function function, bool hasTailLoop)
    {
        // A block-local initializer must not reset a VM local carried across reentry.
        var cyclicBlocks =
            _loops[function].Loops.SelectMany(loop => loop.Blocks)
            .Concat(_loops[function].IrreducibleCycles.SelectMany(cycle => cycle.Blocks)).ToHashSet();

        foreach (var block in function.Graph.Blocks)
        {
            if (!hasTailLoop && !cyclicBlocks.Contains(block.Id))
                continue;

            var written = new HashSet<int>();

            foreach (var operation in block.Operations)
            {
                var instruction = operation.Instruction;

                if (instruction.Kind is StackInstructionKind.Local_Get or StackInstructionKind.Local_Get_Skip_Head_Const or
                    StackInstructionKind.Local_Int_Add_Const)
                {
                    foreach (var index in instruction.LocalIndices)
                    {
                        if (!written.Contains(index))
                            _retainedLocals.Add(Local(function, index));
                    }
                }

                if (instruction.Kind is StackInstructionKind.Local_Set or StackInstructionKind.Local_Set_Literal or
                    StackInstructionKind.Local_Int_Add_Const)
                {
                    written.UnionWith(instruction.LocalIndices);
                }
            }
        }
    }

    private void Assign(StringBuilder text, IEnumerable<string> destinations, IEnumerable<string> sources)
    {
        var targets = destinations.ToArray();
        var values = sources.ToArray();

        if (targets.Length != values.Length)
            throw new InvalidOperationException("Mismatched spread assignment.");

        var assignments = targets.Zip(values).Where(pair => pair.First != pair.Second).ToArray();
        var constants = assignments.Select(pair => ConstantValue(pair.Second)).ToArray();

        for (var index = 0; index < assignments.Length; index++)
        {
            _constantValues.Remove(assignments[index].First);

            if (constants[index] is { } constant)
                _constantValues[assignments[index].First] = constant;

        }

        EmitAssignments(
            text,
            [
                .. assignments.Select(
                    pair =>
                    (Target: pair.First,
                    Expression: ConvertValue(pair.First, pair.Second, Storage(pair.Second)),
                    Type: TypeOf(pair.First)))
            ]);
    }

    private void EmitAssignments(
        StringBuilder text, IReadOnlyList<(string Target, string Expression, string Type)> assignments)
    {
        var written = new HashSet<string>();

        var capture =
            assignments.Any(
                assignment =>
                {
                    var dependsOnWrite =
                        SyntaxFactory.ParseExpression(assignment.Expression)
                        .DescendantNodesAndSelf().OfType<IdentifierNameSyntax>()
                        .Any(identifier => written.Contains(identifier.Identifier.ValueText));

                    written.Add(assignment.Target);
                    return dependsOnWrite;
                });

        if (capture)
        {
            var temporaries = new string[assignments.Count];

            for (var index = 0; index < assignments.Count; index++)
            {
                var assignment = assignments[index];
                temporaries[index] = "assignment_" + _assignmentIndex++;
                text.AppendLine(assignment.Type + " " + temporaries[index] + " = " + assignment.Expression + ";");
            }

            for (var index = 0; index < assignments.Count; index++)
                text.AppendLine(assignments[index].Target + " = " + temporaries[index] + ";");
        }
        else
        {
            foreach (var assignment in assignments)
                text.AppendLine(assignment.Target + " = " + assignment.Expression + ";");
        }
    }

    private void WriteValue(StringBuilder text, string destination, string source, LocalType type = LocalType.Pine)
    {
        var constant = ConstantValue(source);
        _constantValues.Remove(destination);

        if (constant is not null)
            _constantValues[destination] = constant;

        text.AppendLine(destination + " = " + ConvertValue(destination, source, type) + ";");
    }

    private PineValue? ConstantValue(string expression) =>
        _literalValues.GetValueOrDefault(expression) ?? _constantValues.GetValueOrDefault(expression);

    private string Equal(string input, PineValue literal, bool negated = false)
    {
        var type = Storage(input);
        var op = negated ? " != " : " == ";
        var empty = literal.Equals(PineValue.EmptyList);
        var matchTrue = literal.Equals(PineKernelValues.TrueValue);
        var matchFalse = literal.Equals(PineKernelValues.FalseValue);
        var mismatch = negated ? "true" : "false";

        string Structural(string value) =>
            (negated ? "!" : "") + "PineValueInProcess.AreEqual(" + value + ", " + Literal(literal) + ")";

        string CompareNumber(string number, bool optional) =>
            empty && optional
            ?
            number + op + "null"
            :
            Encodings.IntegerEncoding.ParseSignedIntegerStrict(literal).IsOkOrNullable() is { } value
            ?
            number + op + Integer(value)
            :
            mismatch;

        return type switch
        {
            LocalType.Integer or LocalType.OptionalInteger => CompareNumber(input, type == LocalType.OptionalInteger),

            LocalType.IntegerWithOriginal =>
            "(" + input + ".Original is null ? " + CompareNumber(input + ".Number", optional: true) +
            " : " + Structural(input + ".Original") + ")",

            LocalType.Boolean or LocalType.OptionalBoolean =>
            matchTrue || matchFalse
            ?
            input + op + (matchTrue ? "true" : "false")
            :
            empty && type == LocalType.OptionalBoolean ? input + op + "null" : mismatch,

            _ =>
            Structural(input)
        };
    }

    private void EmitContract(StringBuilder text, Function function)
    {
        text.AppendLine(
            "/// <summary>Function " + function.Identity.Index + "; " +
            (function.Identity.EnvironmentConstraint is null
            ?
            "unconstrained environment."
            :
            "specialized environment.") +
            " Arguments: " +
            string.Join(
                "; ",
                function.Prepared.Parameters.ParamsPaths.Select(
                    (path, index) =>
                    index + " = [" + string.Join(", ", path) + "]")) + ".</summary>");
    }

    private Function ForwardTarget(Function root)
    {
        var visited = new HashSet<Function>();

        while (visited.Add(root))
        {
            var plan = _plans[root];

            if (plan.Blocks.Count != 1 ||
                plan.Blocks[0].Terminator is not PineControlFlowTerminator.Invoke invoke ||
                !_tailCalls[root].Contains(plan.Blocks[0].Id) ||
                invoke.Instruction.Kind == StackInstructionKind.Eval_Const ||
                !plan.Blocks[0].Operations.All(operation => operation.Instruction.Kind == StackInstructionKind.Local_Get))
            {
                break;
            }

            var callee = plan.Calls[plan.Blocks[0].Id];

            var parameters =
                plan.Blocks[0].Operations.SelectMany(operation => operation.Instruction.LocalIndices).ToArray();

            if (!parameters.SequenceEqual(Enumerable.Range(0, root.Prepared.Parameters.ParamsPaths.Count)) ||
                invoke.Inputs.Length != parameters.Length ||
                !invoke.Inputs.SequenceEqual(plan.Blocks[0].Operations.SelectMany(operation => operation.Results)) ||
                callee.Prepared.Parameters.ParamsPaths.Count != parameters.Length)
            {
                break;
            }

            root = callee;
        }

        return root;
    }

    private void EmitOperation(
        StringBuilder text,
        Function function,
        PineBlockId blockId,
        int operationIndex,
        PineControlFlowOperation operation,
        List<string> stack)
    {
        var instruction = operation.Instruction;
        var inputs = operation.Inputs.Select(value => Value(function, value)).ToArray();
        var results = operation.Results.Select(value => Value(function, value)).ToArray();

        switch (instruction.Kind)
        {
            case StackInstructionKind.Local_Set:
                Assign(
                    text,
                    instruction.LocalIndices.Select(index => Local(function, index)),
                    Enumerable.Range(0, instruction.LocalIndices.Length).Select(index => stack[stack.Count - index - 1]));

                return;

            case StackInstructionKind.Local_Set_Literal:
                WriteLiteral(
                    text,
                    Local(function, instruction.SingleLocalIndex),
                    instruction.Literal!.Evaluate());

                return;

            case StackInstructionKind.Push_Literal:
                WriteLiteral(text, results[0], instruction.Literal!.Evaluate());
                return;

            case StackInstructionKind.Length:
                WriteValue(text, results[0], "new BigInteger(" + Pine(inputs[0]) + ".GetLength())", LocalType.Integer);
                return;

            case StackInstructionKind.Local_Get:
                Assign(text, results, instruction.LocalIndices.Select(index => Local(function, index)));
                return;

            case StackInstructionKind.Local_Get_Skip_Head_Const:
                Assign(
                    text,
                    results,
                    instruction.LocalIndices.Select(
                        index =>
                        LocalProjection(function, index, instruction.SkipCount!.Value)));

                return;

            case StackInstructionKind.Local_Int_Add_Const:
                foreach (var index in instruction.LocalIndices)
                {
                    var local = Local(function, index);

                    WriteValue(
                        text,
                        local,
                        Numeric(local) + " + " + BigIntegerConstant(instruction.IntegerLiteral!.Value),
                        LocalType.OptionalInteger);
                }

                return;

            case StackInstructionKind.Pop:
                return;

            case StackInstructionKind.Parse_Error_Const:
                text.AppendLine(
                    "throw new global::Pine.Core.CodeAnalysis.ParseExpressionException(" +
                    Microsoft.CodeAnalysis.CSharp.SymbolDisplay.FormatLiteral(instruction.ParseError!, quote: true) +
                    ");");

                return;

            case StackInstructionKind.Slice_Skip_Var_Equal_Const:
                var sliceSuffix = results[0];
                var offset = "offset_" + sliceSuffix;

                text.AppendLine(
                    "if (sliceOffset.Get(" + Numeric(inputs[1]) + ", BigInteger.One) is { } " + offset + ")");

                text.AppendLine("{");
                WriteValue(text, results[0], "false", LocalType.Boolean);

                EmitMatchCases<string>(
                    text,
                    inputs[0],
                    [(instruction.Literal!.Evaluate(), results[0])],
                    target => WriteValue(text, target, "true", LocalType.Boolean),
                    offset);

                text.AppendLine("}");
                text.AppendLine("else");
                text.AppendLine("{");

                WriteValue(
                    text,
                    results[0],
                    instruction.Literal!.Evaluate().Equals(PineValue.EmptyList) ? "true" : "false",
                    LocalType.Boolean);

                text.AppendLine("}");
                _constantValues.Remove(results[0]);
                return;

            case StackInstructionKind.Blob_Trim_Leading_Zeros:
                var suffix = results[0];

                text.AppendLine(
                    "if (" + Pine(inputs[0]) + ".Evaluate() is PineValue.BlobValue blob_" + suffix +
                    " && " + instruction.TakeCount + " <= blob_" + suffix + ".Bytes.Length)");

                text.AppendLine("{");
                text.AppendLine("var start_" + suffix + " = 0;");

                text.AppendLine(
                    "while (start_" + suffix + " < blob_" + suffix + ".Bytes.Length - " +
                    instruction.TakeCount + " && blob_" + suffix + ".Bytes.Span[start_" + suffix + "] == 0)");

                text.AppendLine("{");
                text.AppendLine("++start_" + suffix + ";");
                Cancellation(text);
                text.AppendLine("}");

                WriteValue(
                    text,
                    results[0],
                    "PineValueInProcess.Create(PineValue.Blob(blob_" + suffix +
                    ".Bytes[start_" + suffix + "..]))");

                text.AppendLine("}");
                text.AppendLine("else {");
                WriteValue(text, results[0], "PineValueInProcess.EmptyList");
                text.AppendLine("}");
                return;

            default:
                if (results.Length != 1)
                    throw new InvalidOperationException("Ordinary operation must produce one value.");

                if (ScalarExpression(instruction, inputs, results[0]) is { } scalar)
                    WriteValue(text, results[0], scalar.Code, scalar.Type);

                else
                    WriteValue(text, results[0], OperationExpression(instruction, inputs, results[0]));

                return;
        }
    }

    private string BigIntegerConstant(BigInteger integer) =>
        integer.IsZero
        ?
        "BigInteger.Zero"
        :
        integer.IsOne
        ?
        "BigInteger.One"
        :
        integer == BigInteger.MinusOne
        ?
        "BigInteger.MinusOne"
        :
        integer >= int.MinValue && integer <= int.MaxValue
        ?
        "new BigInteger(" + Integer(integer) + ")"
        :
        Integer(integer);

    private (string Code, LocalType Type)? ScalarExpression(
        StackInstruction instruction,
        string[] inputs,
        string result)
    {
        (string Code, LocalType Type) Input(int index) => (inputs[index], Storage(inputs[index]));

        (string Code, LocalType Type) Constant() =>
            (BigIntegerConstant(instruction.IntegerLiteral!.Value), LocalType.Integer);

        (string, LocalType) Boolean(string expression) => (expression, LocalType.Boolean);

        (string, LocalType) Math(string op, (string Code, LocalType Type) right, string? multiplier = null) =>
            (IntegerOperation(op, Input(0), right, result, multiplier: multiplier), LocalType.OptionalInteger);

        if (IntegerComparison(instruction, inputs, result) is { } comparison)
            return comparison;

        return instruction.Kind switch
        {
            StackInstructionKind.Int_Add_Binary => Math("+", Input(1)),
            StackInstructionKind.Int_Add_Const => Math("+", Constant()),
            StackInstructionKind.Int_Mul_Binary => Math("*", Input(1)),

            StackInstructionKind.Int_Mul_Const when instruction.IntegerLiteral == BigInteger.MinusOne =>
            ("-" + Numeric(inputs[0]), LocalType.OptionalInteger),

            StackInstructionKind.Int_Mul_Const => Math("*", Constant()),

            StackInstructionKind.Int_Mul_Const_Add_Binary when instruction.IntegerLiteral == BigInteger.MinusOne =>
            Math("-", Input(1)),

            StackInstructionKind.Int_Mul_Const_Add_Binary =>
            Math("+", Input(1), BigIntegerConstant(instruction.IntegerLiteral!.Value)),

            StackInstructionKind.Length_Equal_Const =>
            Boolean(LengthEqual(inputs[0], instruction.IntegerLiteral!.Value)),

            StackInstructionKind.Equal_Binary =>
            Boolean(BinaryEquality(inputs[0], inputs[1], false)),

            StackInstructionKind.Not_Equal_Binary =>
            Boolean(BinaryEquality(inputs[0], inputs[1], true)),

            StackInstructionKind.Equal_Binary_Const =>
            Boolean(Equal(inputs[0], instruction.Literal!.Evaluate())),

            StackInstructionKind.Not_Equal_Binary_Const =>
            Boolean(Equal(inputs[0], instruction.Literal!.Evaluate(), negated: true)),

            StackInstructionKind.Is_List_Value => Boolean(Pine(inputs[0]) + ".IsList()"),
            StackInstructionKind.Is_Blob_Value => Boolean(Pine(inputs[0]) + ".IsBlob()"),

            StackInstructionKind.Logical_And_Binary =>
            Boolean("(" + Truth(inputs[0]) + ") && (" + Truth(inputs[1]) + ")"),

            StackInstructionKind.Negate when Storage(inputs[0]) is LocalType.Boolean or LocalType.OptionalBoolean =>
            ("!" + inputs[0], Storage(inputs[0])),

            _ =>
            null
        };
    }

    private string OperationExpression(StackInstruction instruction, string[] inputs, string suffix)
    {
        var a = inputs.Length > 0 ? Pine(inputs[0]) : "";
        var b = inputs.Length > 1 ? Pine(inputs[1]) : "";
        var integerB = inputs.Length > 1 ? Numeric(inputs[1]) : "";
        string L() => Literal(instruction.Literal!.Evaluate());
        string Kernel(string name) => "PineValueInProcess.Create(Kernel." + name + "(" + a + ".Evaluate()))";

        string Binary(string name) =>
            "PineValueInProcess.Create(Specialized." + name +
            "(" + a + ".Evaluate(), " + b + ".Evaluate()))";

        string BitConst(string name) =>
            "PineValueInProcess.Create(Specialized." + name +
            "(" + a + ".Evaluate(), " + L() + ".Evaluate()))";

        string Shift(string name, bool constant) =>
            constant
            ?
            "PineValueInProcess.Create(Specialized." + name + "(" + instruction.ShiftCount + ", " + a + ".Evaluate()))"
            :
            integerB + " is { } shift_" + suffix + " ? PineValueInProcess.Create(Specialized." + name +
            "(shift_" + suffix + ", " + a + ".Evaluate())) : PineValueInProcess.EmptyList";

        string CountOperation(string name) =>
            integerB + " is { } count_" + suffix +
            " ? PineValueInProcess." +
            name +
            "((int)count_" +
            suffix +
            ", " +
            a +
            ") : PineValueInProcess.EmptyList";

        string Const(string op, bool comparison = false, bool unsigned = false) =>
            IntegerUnary(a, instruction.IntegerLiteral!.Value, op, comparison, suffix, unsigned);

        return instruction.Kind switch
        {
            StackInstructionKind.Concat_Generic => "PineValueInProcess.Concat(" + a + ")",
            StackInstructionKind.Concat_Binary => "PineValueInProcess.ConcatBinary(" + a + ", " + b + ")",
            StackInstructionKind.Build_List => List(inputs.Select(Pine)),

            StackInstructionKind.Build_List_With_Prefix =>
            List(
                instruction.Literal!.ListItemsOrNull()!.Select(item => Literal(item.Evaluate())).Concat(
                    inputs.Select(Pine))),

            StackInstructionKind.Prepend_List_Items =>
            inputs.Length == 2
            ?
            "PineValueInProcess.PrependItem(" + a + ", " + b + ")"
            :
            Pine(inputs[^1]) + ".IsList() ? PineValueInProcess.ConcatBinary(" +
            List(inputs[..^1].Select(Pine)) + ", " + Pine(inputs[^1]) + ") : PineValueInProcess.EmptyList",

            StackInstructionKind.Append_List_Items =>
            a + ".IsList() ? PineValueInProcess.ConcatBinary(" +
            a + ", " + List(inputs[1..].Select(Pine)) + ") : PineValueInProcess.EmptyList",

            StackInstructionKind.Skip_Generic => Kernel("skip"),
            StackInstructionKind.Take_Generic => Kernel("take"),
            StackInstructionKind.Skip_Binary => CountOperation("Skip"),
            StackInstructionKind.Take_Binary => CountOperation("Take"),
            StackInstructionKind.Skip_Const => "PineValueInProcess.Skip(" + instruction.SkipCount + ", " + a + ")",
            StackInstructionKind.Take_Const => "PineValueInProcess.Take(" + instruction.TakeCount + ", " + a + ")",

            StackInstructionKind.Take_Last_Const =>
            "PineValueInProcess.TakeLast(" + instruction.TakeCount + ", " + a + ")",

            StackInstructionKind.Skip_Head_Const => a + ".GetElementAt(" + instruction.SkipCount + ")",

            StackInstructionKind.Skip_Head_Binary =>
            integerB + " is { } index_" + suffix +
            " ? " + a + ".GetElementAt((int)index_" + suffix + ") : PineValueInProcess.EmptyList",

            StackInstructionKind.Head_Generic => a + ".GetElementAt(0)",
            StackInstructionKind.Reverse => "PineValueInProcess.Reverse(" + a + ")",

            StackInstructionKind.Slice_Skip_Var_Take_Const =>
            integerB + " is { } skip_" + suffix +
            " ? PineValueInProcess.Slice(skip_" + suffix + " < 0 ? 0 : (int)skip_" + suffix + ", " +
            instruction.TakeCount + ", " + a + ") : PineValueInProcess.EmptyList",

            StackInstructionKind.Slice_Skip_Var_Take_Var =>
            integerB + " is { } skip_" + suffix + " && " +
            Numeric(inputs[2]) + " is { } take_" + suffix + " ? PineValueInProcess.Slice((int)skip_" + suffix +
            ", (int)take_" + suffix + ", " + a + ") : PineValueInProcess.EmptyList",

            StackInstructionKind.Equal_Generic => Kernel("equal"),
            StackInstructionKind.Negate => Kernel("negate"),
            StackInstructionKind.Int_Is_Sorted_Asc_Generic => Kernel("int_is_sorted_asc"),
            StackInstructionKind.Int_Add_Generic => Kernel("int_add"),
            StackInstructionKind.Int_Mul_Generic => Kernel("int_mul"),
            StackInstructionKind.Int_Unsigned_Add_Const => Const("+", unsigned: true),
            StackInstructionKind.Int_Unsigned_Less_Than_Or_Equal_Const => Const("<=", true, true),
            StackInstructionKind.Int_Unsigned_Greater_Than_Or_Equal_Const => Const(">=", true, true),
            StackInstructionKind.Bit_And_Generic => Kernel("bit_and"),
            StackInstructionKind.Bit_Or_Generic => Kernel("bit_or"),
            StackInstructionKind.Bit_Xor_Generic => Kernel("bit_xor"),
            StackInstructionKind.Bit_Not => Kernel("bit_not"),
            StackInstructionKind.Bit_And_Binary => Binary("bit_and"),
            StackInstructionKind.Bit_Or_Binary => Binary("bit_or"),
            StackInstructionKind.Bit_Xor_Binary => Binary("bit_xor"),
            StackInstructionKind.Bit_And_Const => BitConst("bit_and"),
            StackInstructionKind.Bit_Or_Const => BitConst("bit_or"),
            StackInstructionKind.Bit_Shift_Left_Generic => Kernel("bit_shift_left"),
            StackInstructionKind.Bit_Shift_Right_Generic => Kernel("bit_shift_right"),
            StackInstructionKind.Bit_Shift_Left_Binary => Shift("bit_shift_left", false),
            StackInstructionKind.Bit_Shift_Right_Binary => Shift("bit_shift_right", false),
            StackInstructionKind.Bit_Shift_Left_Const => Shift("bit_shift_left", true),
            StackInstructionKind.Bit_Shift_Right_Const => Shift("bit_shift_right", true),

            StackInstructionKind.Select_Value_Const =>
            MapExpression(
                a,
                instruction.ValueMap!,
                Literal(instruction.Literal!.Evaluate())),

            _ =>
            throw new NotImplementedException("OperationExpression does not handle: " + instruction.Kind)
        };
    }

    internal static bool Supports(StackInstructionKind kind) =>
        kind switch
        {
            StackInstructionKind.Push_Literal or StackInstructionKind.Parse_Error_Const or
            StackInstructionKind.Local_Set or StackInstructionKind.Local_Get or
            StackInstructionKind.Local_Get_Skip_Head_Const or StackInstructionKind.Local_Set_Literal or
            StackInstructionKind.Local_Int_Add_Const or StackInstructionKind.Pop or
            StackInstructionKind.Length or StackInstructionKind.Length_Equal_Const or
            StackInstructionKind.Concat_Generic or StackInstructionKind.Concat_Binary or
            StackInstructionKind.Prepend_List_Items or StackInstructionKind.Append_List_Items or
            StackInstructionKind.Slice_Skip_Var_Take_Var or StackInstructionKind.Slice_Skip_Var_Take_Const or
            StackInstructionKind.Skip_Generic or StackInstructionKind.Skip_Binary or StackInstructionKind.Skip_Const or
            StackInstructionKind.Take_Generic or StackInstructionKind.Take_Binary or StackInstructionKind.Take_Const or
            StackInstructionKind.Take_Last_Const or StackInstructionKind.Skip_Head_Binary or
            StackInstructionKind.Skip_Head_Const or StackInstructionKind.Head_Generic or StackInstructionKind.Reverse or
            StackInstructionKind.Build_List or StackInstructionKind.Build_List_With_Prefix or
            StackInstructionKind.Equal_Binary or StackInstructionKind.Not_Equal_Binary or
            StackInstructionKind.Equal_Binary_Const or StackInstructionKind.Not_Equal_Binary_Const or
            StackInstructionKind.Equal_Generic or StackInstructionKind.Is_List_Value or StackInstructionKind.Is_Blob_Value or
            StackInstructionKind.Negate or StackInstructionKind.Int_Is_Sorted_Asc_Generic or
            StackInstructionKind.Int_Less_Than_Binary or StackInstructionKind.Int_Less_Than_Or_Equal_Binary or
            StackInstructionKind.Int_Less_Than_Const or StackInstructionKind.Int_Less_Than_Or_Equal_Const or
            StackInstructionKind.Int_Greater_Than_Or_Equal_Const or
            StackInstructionKind.Int_Unsigned_Less_Than_Or_Equal_Const or
            StackInstructionKind.Int_Unsigned_Greater_Than_Or_Equal_Const or
            StackInstructionKind.Int_Add_Binary or StackInstructionKind.Int_Add_Const or
            StackInstructionKind.Int_Unsigned_Add_Const or StackInstructionKind.Int_Add_Generic or
            StackInstructionKind.Int_Mul_Const_Add_Binary or StackInstructionKind.Int_Mul_Binary or
            StackInstructionKind.Int_Mul_Const or StackInstructionKind.Int_Mul_Generic or
            StackInstructionKind.Bit_And_Generic or StackInstructionKind.Bit_And_Binary or StackInstructionKind.Bit_And_Const or
            StackInstructionKind.Bit_Or_Generic or StackInstructionKind.Bit_Or_Binary or StackInstructionKind.Bit_Or_Const or
            StackInstructionKind.Bit_Xor_Generic or StackInstructionKind.Bit_Xor_Binary or StackInstructionKind.Bit_Not or
            StackInstructionKind.Bit_Shift_Left_Binary or StackInstructionKind.Bit_Shift_Left_Const or
            StackInstructionKind.Bit_Shift_Left_Generic or StackInstructionKind.Bit_Shift_Right_Binary or
            StackInstructionKind.Bit_Shift_Right_Const or StackInstructionKind.Bit_Shift_Right_Generic or
            StackInstructionKind.Logical_And_Binary or StackInstructionKind.Blob_Trim_Leading_Zeros or
            StackInstructionKind.Slice_Skip_Var_Equal_Const or StackInstructionKind.Select_Value_Const =>
            true,

            _ =>
            false
        };

    private string IntegerUnary(
        string a, BigInteger value, string op, bool comparison, string suffix, bool unsigned = false) =>
        (unsigned ? "Kernel.UnsignedIntegerFromValueRelaxed(" + a + ".Evaluate())" : a + ".AsInteger()") +
        " is { } integer_" + suffix + " ? PineValueInProcess." + (comparison ? "CreateBool" : "CreateInteger") +
        "(integer_" + suffix + " " + op + " " + Integer(value) + ") : PineValueInProcess.EmptyList";

    private string Integer(BigInteger value) =>
        value >= int.MinValue && value <= int.MaxValue
        ?
        value.ToString(CultureInfo.InvariantCulture)
        :
        Literal(Encodings.IntegerEncoding.EncodeSignedInteger(value)) + ".AsInteger()!.Value";

    private void EmitReconstructedReturn(StringBuilder text, ReturnValueLayout layout, Func<int, string> component)
    {
        if (layout.Recipe is not ReturnValueLayout.Node.Variant variant)
        {
            text.AppendLine("return " + Reconstruct(layout, component) + ";");
            return;
        }

        for (var index = 0; index < variant.Cases.Length; index++)
        {
            text.AppendLine(
                "if (PineValueInProcess.AreEqual(" + component(variant.Index) + ", " +
                Literal(layout.Slots[variant.Index].Keys[index]) + ")) {");

            text.AppendLine("return " + Reconstruct(layout, component, variant.Cases[index]) + ";");
            text.AppendLine("}");
        }

        text.AppendLine(
            "throw new global::System.InvalidOperationException(\"Invalid return discriminant in slot " +
            variant.Index + ".\");");
    }

    private string Reconstruct(
        ReturnValueLayout layout,
        Func<int, string> component,
        ReturnValueLayout.Node? recipe = null)
    {
        string Build(ReturnValueLayout.Node node) =>
            node switch
            {
                ReturnValueLayout.Node.Fixed fixedValue => Literal(fixedValue.Value),

                ReturnValueLayout.Node.Component item =>
                layout.Slots[item.Index].IsMapped
                ?
                MapExpression(
                    component(item.Index),
                    item.Values.Select(
                        (value, index) =>
                        KeyValuePair.Create(layout.Slots[item.Index].Keys[index], value)),
                    "throw new global::System.InvalidOperationException(\"Invalid return discriminant in slot " +
                    item.Index +
                    ".\")")
                :
                component(item.Index),

                ReturnValueLayout.Node.List list =>
                FixedTree(list) is { } literal ? Literal(literal) : List(list.Items.Select(Build)),

                ReturnValueLayout.Node.Variant variant => Choice(variant),

                _ =>
                throw new NotImplementedException("Reconstruct does not handle: " + node.GetType().Name)
            };

        string Choice(ReturnValueLayout.Node.Variant variant)
        {
            var result =
                "throw new global::System.InvalidOperationException(\"Invalid return discriminant in slot " +
                variant.Index + ".\")";

            for (var index = variant.Cases.Length - 1; index >= 0; index--)
            {
                result =
                    "(PineValueInProcess.AreEqual(" + component(variant.Index) + ", " +
                    Literal(layout.Slots[variant.Index].Keys[index]) + ") ? " + Build(variant.Cases[index]) +
                    " : " + result + ")";
            }

            return result;
        }

        return Build(recipe ?? layout.Recipe);
    }

    private static PineValue? FixedTree(ReturnValueLayout.Node node)
    {
        switch (node)
        {
            case ReturnValueLayout.Node.Fixed fixedValue:
                return fixedValue.Value;

            case ReturnValueLayout.Node.List list:
                var items = list.Items.Select(FixedTree).ToArray();
                return items.All(item => item is not null) ? PineValue.List([.. items.Select(item => item!)]) : null;

            case ReturnValueLayout.Node.Component:
            case ReturnValueLayout.Node.Variant:
                return null;

            default:
                throw new NotImplementedException("FixedTree does not handle: " + node.GetType().Name);
        }
    }

    private string MapExpression(string input, IEnumerable<KeyValuePair<PineValue, PineValue>> map, string fallback)
    {
        var items = Ordered(map).ToArray();

        if (items.Length <= 2)
        {
            var result = fallback;

            foreach (var item in items.Reverse())
            {
                result =
                    "(PineValueInProcess.AreEqual(" + input + ", " + Literal(item.Key) + ") ? " +
                    Literal(item.Value) + " : " + result + ")";
            }

            return result;
        }

        var selected = "mapped_" + _lookupIndex++;

        return
            "(" + ValueMap(items) + ".TryGetValue(" + input + ".Evaluate(), out var " + selected +
            ") ? " + selected + " : " + fallback + ")";
    }

    private IEnumerable<KeyValuePair<PineValue, PineValue>> Ordered(
        IEnumerable<KeyValuePair<PineValue, PineValue>> map) =>
        map.OrderBy(
            item => Convert.ToHexStringLower(_canonicalHashCache.GetHash(item.Key).Span),
            StringComparer.Ordinal);

    private string SwitchTable(IEnumerable<PineSwitchCase> cases)
    {
        var items =
            cases.Select((item, index) => KeyValuePair.Create(item.Literal, index))
            .DistinctBy(item => item.Key).ToArray();

        var identity =
            PineValue.List(
                [
                    .. items.Select(
                        item => PineValue.List(
                            [item.Key, Encodings.IntegerEncoding.EncodeSignedInteger(item.Value)]))
                ]);

        if (!_switchTables.TryGetValue(identity, out var table))
        {
            foreach (var item in items)
                Literal(item.Key);

            table = (_literalNaming.TableName("Switch", identity), items);
            _switchTables.Add(identity, table);
        }

        return "global::" + _options.LiteralsTypeName + "." + table.Name;
    }

    private string ValueMap(IEnumerable<KeyValuePair<PineValue, PineValue>> map)
    {
        var items = Ordered(map).DistinctBy(item => item.Key).ToArray();

        var identity =
            PineValue.List([.. items.Select(item => PineValue.List([item.Key, item.Value]))]);

        if (!_valueMaps.TryGetValue(identity, out var table))
        {
            foreach (var item in items)
            {
                Literal(item.Key);
                Literal(item.Value);
            }

            table = (_literalNaming.TableName("Map", identity), items);
            _valueMaps.Add(identity, table);
        }

        return "global::" + _options.LiteralsTypeName + "." + table.Name;
    }

    private string MaterializedLiteral(PineValue value)
    {
        Literal(value);

        return
            value.Equals(PineValue.EmptyList)
            ?
            "PineValue.EmptyList"
            :
            value.Equals(PineKernelValues.TrueValue)
            ?
            "global::Pine.Core.PineLanguage.PineKernelValues.TrueValue"
            :
            value.Equals(PineKernelValues.FalseValue)
            ?
            "global::Pine.Core.PineLanguage.PineKernelValues.FalseValue"
            :
            "global::" + _options.LiteralsTypeName + ".Materialized_" + _literalNames[value];
    }

    private string Literal(PineValue value)
    {
        var singleton =
            value.Equals(PineValue.EmptyList)
            ?
            "PineValueInProcess.EmptyList"
            :
            value.Equals(PineKernelValues.TrueValue)
            ?
            "PineValueInProcess.KernelTrueValue"
            :
            value.Equals(PineKernelValues.FalseValue) ? "PineValueInProcess.KernelFalseValue" : null;

        if (singleton is not null)
        {
            _literalValues[singleton] = value;
            return singleton;
        }

        if (!_literalNames.TryGetValue(value, out var name))
        {
            if (value is PineValue.ListValue list)
            {
                foreach (var item in list.Items.Span)
                    Literal(item);
            }

            name = _literalNaming.ValueName(value);
            _literalNames.Add(value, name);
            _literals.Add(value);
        }

        var expression = "global::" + _options.LiteralsTypeName + "." + name;
        _literalValues[expression] = value;
        return expression;
    }

    private string EmitLiterals()
    {
        var text =
            new StringBuilder(Header()).AppendLine("public static class " + _options.LiteralsClassName).AppendLine("{");

        foreach (var value in _literals)
        {
            var name = _literalNames[value];

            var materialized =
                value switch
                {
                    PineValue.BlobValue blob =>
                    blob.Bytes.Length % 4 is 0 &&
                    Encodings.StringEncoding.StringFromBlobValue(blob.Bytes).IsOkOrNull() is { } stringValue
                    ?
                    "global::Pine.Core.Encodings.StringEncoding.ValueFromString(" +
                    SymbolDisplay.FormatLiteral(stringValue, quote: true) + ")"
                    :
                    "PineValue.Blob(new byte[] { " + string.Join(", ", blob.Bytes.ToArray()) + " })",

                    PineValue.ListValue list =>
                    "PineValue.List([" +
                    string.Join(
                        ", ",
                        list.Items.ToArray().Select(MaterializedLiteral)) + "])",

                    _ =>
                    throw new NotImplementedException("EmitLiterals does not handle: " + value.GetType().Name)
                };

            text.AppendLine("public static readonly PineValue Materialized_" + name + " = " + materialized + ";");
            text.AppendLine("public static PineValueInProcess " + name + " => Literal_" + name + ".Value;");
            text.AppendLine("private static class Literal_" + name);
            text.AppendLine("{");

            var children =
                value is PineValue.ListValue parent
                ?
                ", [" +
                string.Join(
                    ", ",
                    parent.Items.ToArray().Select(
                        child =>
                        _literalNames.TryGetValue(child, out var childName)
                        ?
                        childName
                        :
                        child.Equals(PineValue.EmptyList)
                        ?
                        "PineValueInProcess.EmptyList"
                        :
                        child.Equals(PineKernelValues.TrueValue)
                        ?
                        "PineValueInProcess.KernelTrueValue"
                        :
                        "PineValueInProcess.KernelFalseValue")) + "]"
                :
                "";

            text.AppendLine(
                "internal static readonly PineValueInProcess Value = PineValueInProcess.CreateFullyRepresented(" +
                "Materialized_" + name + children + ");");

            text.AppendLine("}");
        }

        foreach (var table in _switchTables.Values)
        {
            text.AppendLine(
                "public static readonly global::System.Collections.Frozen.FrozenDictionary<PineValue, int> " +
                table.Name + " = global::System.Collections.Frozen.FrozenDictionary.ToFrozenDictionary(" +
                "new global::System.Collections.Generic.Dictionary<PineValue, int>");

            text.AppendLine("{");

            foreach (var item in table.Items)
                text.AppendLine("[" + MaterializedLiteral(item.Key) + "] = " + item.Value + ",");

            text.AppendLine("});");
        }

        foreach (var table in _valueMaps.Values)
        {
            text.AppendLine(
                "public static readonly global::System.Collections.Frozen.FrozenDictionary<PineValue, PineValueInProcess> " +
                table.Name + " = global::System.Collections.Frozen.FrozenDictionary.ToFrozenDictionary(" +
                "new global::System.Collections.Generic.Dictionary<PineValue, PineValueInProcess>");

            text.AppendLine("{");

            foreach (var item in table.Items)
                text.AppendLine("[" + MaterializedLiteral(item.Key) + "] = " + Literal(item.Value) + ",");

            text.AppendLine("});");
        }

        return PineCSharpSourceSimplification.Format(text.AppendLine("}").ToString(), new HashSet<string>());
    }

    private string Header() =>
        "#nullable enable\n" +
        (_options.ContainerNamespace is null ? "" : "\nnamespace " + _options.ContainerNamespace + ";\n\n") +
        "using global::System.Numerics;\n" +
        "using PineValue = global::Pine.Core.PineLanguage.PineValue;\n" +
        "using PineValueInProcess = global::Pine.Core.Internal.PineValueInProcess;\n" +
        "using SliceOffset = global::Pine.Core.DotNet.PineCSharpSliceOffset;\n" +
        "using Kernel = global::Pine.Core.PineLanguage.BuiltinFunction;\n" +
        "using Specialized = global::Pine.Core.Internal.BuiltinFunctionSpecialized;\n" +
        "using BlobMemory = global::System.ReadOnlyMemory<byte>;\n" +
        "using ListMemory = global::System.ReadOnlyMemory<global::Pine.Core.PineLanguage.PineValue>;\n" +
        string.Concat(
            _functions.Select(function => function.Prepared.Returns.SlotCount).Where(count => count > 1)
            .Distinct().Order().Select(
                count => "using Return_" + count + " = (" +
                    string.Join(
                        ", ",
                        Enumerable.Range(0, count).Select(
                            index =>
                            "global::Pine.Core.Internal.PineValueInProcess item" + index)) + ");\n"));

    private void Cancellation(StringBuilder text)
    {
        if (_options.EnableCancellation)
            text.AppendLine("cancellationToken.ThrowIfCancellationRequested();");
    }

    private IEnumerable<string> TokenParameter() =>
        _options.EnableCancellation ? ["global::System.Threading.CancellationToken cancellationToken"] : [];

    private IEnumerable<string> TokenArgument() => _options.EnableCancellation ? ["cancellationToken"] : [];

    private static string Project(string source, IEnumerable<int> path) =>
        path.Aggregate(source, (current, index) => current + ".GetElementAt(" + index + ")");

    private static string Local(Function function, int index) =>
        index < function.Prepared.Parameters.ParamsPaths.Count
        ?
        "state_" + index
        :
        "local_" + function.Identity.Index + "_" + index;

    private static string Value(Function function, PineVirtualValueId value) =>
        "value_" + function.Identity.Index + "_" + value.Value;

    private static string Label(Function function, PineBlockId block) =>
        "function_" + function.Identity.Index + "_block_" + block.Value;

    private string MethodName(Function function) => _methodNames[function];

    private static IEnumerable<string> Parameters(Function function) =>
        Enumerable.Range(0, function.Prepared.Parameters.ParamsPaths.Count)
        .Select(index => "PineValueInProcess argument_" + index);

    private static string ReturnType(ReturnValueLayout layout) =>
        layout.SlotCount switch
        {
            0 => "global::System.ValueTuple",
            1 => "PineValueInProcess",

            _ =>
            "Return_" + layout.SlotCount
        };

    private static string Component(string result, ReturnValueLayout layout, int index) =>
        layout.SlotCount == 1 ? result : result + ".item" + index;

    private static string Tuple(IEnumerable<string> values)
    {
        var array = values.ToArray();

        return array.Length switch
        {
            0 =>
            "new global::System.ValueTuple()",

            1 =>
            array[0],

            _ =>
            "(" + string.Join(", ", array) + ")"
        };
    }

    private string List(IEnumerable<string> values)
    {
        var items = values.ToArray();
        var constants = items.Select(ConstantValue).ToArray();

        return
            constants.All(item => item is not null)
            ?
            Literal(PineValue.List([.. constants.Select(item => item!)]))
            :
            "PineValueInProcess.CreateList(new PineValueInProcess[] { " + string.Join(", ", items) + " })";
    }

    private static bool IsTailCall(Function function, PineBlockId blockId)
    {
        var callee = function.Calls[blockId];

        if (!function.Prepared.Returns.Equals(callee.Prepared.Returns))
            return false;

        if (function.Graph.Blocks[blockId.Value].Terminator is PineControlFlowTerminator.TailInvoke)
            return true;

        if (function.Graph.Blocks[blockId.Value].Terminator is not PineControlFlowTerminator.Invoke invoke ||
            invoke.Arguments.Length != function.Prepared.Returns.SlotCount ||
            (invoke.Instruction.Kind == StackInstructionKind.Eval_Const &&
            !callee.Prepared.Returns.Equals(ReturnValueLayout.Canonical)))
        {
            return false;
        }

        var values = invoke.Arguments.ToList();
        var original = values.ToArray();
        var locals = new Dictionary<int, PineVirtualValueId>();
        var visited = new HashSet<PineBlockId>();
        var next = invoke.Continuation;

        while (visited.Add(next))
        {
            var block = function.Graph.Blocks[next.Value];
            var aliases = block.Parameters.Zip(values).ToDictionary(item => item.First, item => item.Second);

            foreach (var operation in block.Operations)
            {
                switch (operation.Instruction.Kind)
                {
                    case StackInstructionKind.Local_Set:
                        for (var index = 0; index < operation.Instruction.LocalIndices.Length; index++)
                            locals[operation.Instruction.LocalIndices[index]] = values[values.Count - index - 1];

                        values.RemoveRange(values.Count - operation.Inputs.Length, operation.Inputs.Length);
                        break;

                    case StackInstructionKind.Local_Get:
                        for (var index = 0; index < operation.Results.Length; index++)
                        {
                            if (!locals.TryGetValue(operation.Instruction.LocalIndices[index], out var value))
                                return false;

                            aliases[operation.Results[index]] = value;
                            values.Add(value);
                        }

                        break;

                    case StackInstructionKind.Pop:
                        values.RemoveRange(values.Count - operation.Inputs.Length, operation.Inputs.Length);
                        break;

                    default:
                        return false;
                }
            }

            switch (block.Terminator)
            {
                case PineControlFlowTerminator.Return returned:
                    return
                        returned.ValueCount == original.Length &&
                        values.TakeLast(returned.ValueCount).SequenceEqual(original);

                case PineControlFlowTerminator.Jump jump:
                    values = [.. jump.Arguments.Select(value => aliases[value])];
                    next = jump.Target;
                    break;

                case PineControlFlowTerminator.ConditionalJump:
                case PineControlFlowTerminator.Switch:
                case PineControlFlowTerminator.Invoke:
                case PineControlFlowTerminator.ValueMap:
                case PineControlFlowTerminator.TailInvoke:
                    return false;

                default:
                    throw new NotImplementedException("IsTailCall does not handle: " + block.Terminator.GetType().Name);
            }
        }

        return false;
    }

    private static List<List<T>> Components<T>(IReadOnlyList<T> nodes, Func<T, IEnumerable<T>> successors) where T : notnull
    {
        var indices = new Dictionary<T, int>();
        var low = new Dictionary<T, int>();
        var active = new HashSet<T>();
        var stack = new Stack<T>();
        var result = new List<List<T>>();

        void Visit(T node)
        {
            indices[node] = low[node] = indices.Count;
            stack.Push(node);
            active.Add(node);

            foreach (var next in successors(node))
            {
                if (!indices.ContainsKey(next))
                {
                    Visit(next);
                    low[node] = Math.Min(low[node], low[next]);
                }
                else if (active.Contains(next))
                    low[node] = Math.Min(low[node], indices[next]);
            }

            if (low[node] != indices[node])
                return;

            var component = new List<T>();
            T item;

            do
            {
                item = stack.Pop();
                active.Remove(item);
                component.Add(item);
            }
            while (!EqualityComparer<T>.Default.Equals(node, item));

            component.Reverse();
            result.Add(component);
        }

        foreach (var node in nodes)
        {
            if (!indices.ContainsKey(node))
                Visit(node);
        }

        return result;
    }
}
