using System;
using System.Collections.Immutable;
using System.Globalization;
using System.Linq;
using System.Text.Json;

namespace Pine.Core.Interpreter.IntermediateVM.Semantic;

/// <summary>Lossless literal rendering and deterministic, layout-independent graph diagnostics.</summary>
public static class GraphRendering
{
    /// <summary>Renders every block, including unreachable blocks, sorted by identity.</summary>
    public static string Render(FunctionGraph graph) =>
        "function " + Function(graph.Id) + " " + Signature(graph.Signature) +
        " entry " + Block(graph.Entry) + "\n" +
        string.Concat(graph.Blocks.OrderBy(pair => pair.Key.Value).Select(pair =>
            "block " + Block(pair.Key) +
            (pair.Key == pair.Value.Id ? "" : " [stored id " + Block(pair.Value.Id) + "]") +
            "(" + string.Join(", ", pair.Value.Parameters.Select(Definition)) + "):\n" +
            string.Concat(pair.Value.Operations.Select(operation => "  " + RenderOperation(operation) + "\n")) +
            "  " + RenderTerminator(pair.Value.Terminator) + "\n"));

    /// <summary>Preserves every blob byte and every list boundary without language-level decoding.</summary>
    public static string RenderLiteral(LiteralValue literal) =>
        literal switch
        {
            LiteralValue.Blob blob =>
                "blob(" + string.Concat(blob.Bytes.Select(item => item.ToString("x2", CultureInfo.InvariantCulture))) + ")",
            LiteralValue.List list => "list(" + string.Join(", ", list.Items.Select(RenderLiteral)) + ")",
            _ => throw new NotImplementedException(
                "RenderLiteral does not handle literal variant: " + literal.GetType().Name),
        };

    private static string RenderOperation(Operation operation) =>
        operation switch
        {
            Operation.Literal literal => Definition(literal.Result) + " = " + RenderLiteral(literal.Value),
            Operation.MakeList list => Definition(list.Result) + " = make-list " + Values(list.Items),
            Operation.Project project =>
                Definition(project.Result) + " = project " + Value(project.Source) + Path(project.Path),
            Operation.Builtin builtin =>
                Definition(builtin.Result) + " = builtin " + JsonSerializer.Serialize(builtin.Name) +
                " " + Value(builtin.Argument),
            _ => throw new NotImplementedException(
                "RenderOperation does not handle operation variant: " + operation.GetType().Name),
        };

    private static string RenderTerminator(Terminator terminator) =>
        terminator switch
        {
            Terminator.Return ret => "return " + Values(ret.Values),
            Terminator.Jump jump => "jump " + RenderEdge(jump.Edge),
            Terminator.Branch branch =>
                "branch " + Value(branch.TestedValue) + " == " + RenderLiteral(branch.Literal) +
                " then " + RenderEdge(branch.IfEqual) + " else " + RenderEdge(branch.IfNotEqual),
            Terminator.Switch selection =>
                "switch " + Value(selection.Selector) + " {" +
                string.Join("; ", selection.Cases.Select(@case =>
                    RenderLiteral(@case.Value) + " => " + RenderEdge(@case.Edge))) +
                "} default " + RenderEdge(selection.Default),
            Terminator.Invoke invoke =>
                "invoke " + RenderCall(invoke.Call) + " continue " + Block(invoke.Continuation.Target) +
                "(" + string.Join(", ", invoke.Continuation.Bindings.Select(RenderBinding)) + ")",
            Terminator.TailInvoke tail => "tail-invoke " + RenderCall(tail.Call),
            _ => throw new NotImplementedException(
                "RenderTerminator does not handle terminator variant: " + terminator.GetType().Name),
        };

    private static string RenderBinding(ContinuationBinding binding) =>
        binding switch
        {
            ContinuationBinding.CallerValue caller => "caller " + Value(caller.Value),
            ContinuationBinding.ReturnedResult returned => "returned[" + Number(returned.Index) + "]",
            _ => throw new NotImplementedException(
                "RenderBinding does not handle binding variant: " + binding.GetType().Name),
        };

    private static string RenderCall(Call call) =>
        "c" + Number(call.Site.Value) + " " + RenderTarget(call.Target) +
        Values(call.Arguments) + " " + Signature(call.Signature);

    private static string RenderTarget(CallTarget target) =>
        target switch
        {
            CallTarget.Dynamic dynamicTarget => "dynamic " + Value(dynamicTarget.EncodedExpression),
            CallTarget.Known known => "known " + Function(known.Function),
            _ => throw new NotImplementedException(
                "RenderTarget does not handle target variant: " + target.GetType().Name),
        };

    private static string Signature(FunctionSignature signature) =>
        "(" + string.Join(", ", signature.Parameters.Select(parameter =>
            Path(parameter.Path) + ":" + TypeName(parameter.Type))) +
        ") -> (" + string.Join(", ", signature.Results.Select(TypeName)) + ")";

    private static string TypeName(ValueType type) =>
        type switch
        {
            ValueType.PineValue => "pine",
            _ => throw new NotImplementedException("TypeName does not handle value type: " + type),
        };

    private static string Definition(ValueDefinition definition) =>
        Value(definition.Id) + ":" + TypeName(definition.Type);

    private static string RenderEdge(Edge edge) => Block(edge.Target) + Values(edge.Arguments);

    private static string Values(ImmutableList<PineVirtualValueId> values) =>
        "(" + string.Join(", ", values.Select(Value)) + ")";

    private static string Path(EnvironmentPath path) =>
        "[" + string.Join(",", path.Indices.Select(Number)) + "]";

    private static string Value(PineVirtualValueId value) => "v" + Number(value.Value);
    private static string Block(PineBlockId block) => "b" + Number(block.Value);
    private static string Function(FunctionId function) => "f" + Number(function.Value);
    private static string Number(int number) => number.ToString(CultureInfo.InvariantCulture);
}
