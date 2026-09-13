using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

namespace Pine.Core.Interpreter.IntermediateVM.Semantic;

/// <summary>
/// Evidence of structural validation for exactly these immutable inputs. This is deliberately not
/// a record: callers cannot construct evidence or replace its graph using a record copy expression.
/// Validation does not prove termination, successful target parsing, or successful Pine evaluation.
/// </summary>
public sealed class ValidatedFunctionGraph
{
    /// <summary>The original graph, retained without rewriting.</summary>
    public FunctionGraph Graph { get; }

    /// <summary>The validated known-function contracts, using ordinary ID equality.</summary>
    public ImmutableDictionary<FunctionId, FunctionSignature> KnownFunctionSignatures { get; }

    private ValidatedFunctionGraph(
        FunctionGraph graph,
        ImmutableDictionary<FunctionId, FunctionSignature> knownFunctionSignatures)
    {
        Graph = graph;
        KnownFunctionSignatures = knownFunctionSignatures;
    }

    /// <summary>
    /// Checks all blocks, including unreachable blocks, without imposing any layout, reachability,
    /// numeric backedge, or cycle restrictions. Diagnostics are ordered by signature-table function ID,
    /// then root contract, then block ID and source position. All mutable working state is local.
    /// The root signature resolves recursive calls; an explicit table entry for it must agree.
    /// </summary>
    public static Result<ImmutableList<GraphDiagnostic>, ValidatedFunctionGraph> ValidateGraph(
        FunctionGraph graph,
        ImmutableDictionary<FunctionId, FunctionSignature> knownFunctionSignatures)
    {
        return Validate();

        Result<ImmutableList<GraphDiagnostic>, ValidatedFunctionGraph> Validate()
        {
            var diagnostics = ImmutableList.CreateBuilder<GraphDiagnostic>();
            var definitions = new HashSet<PineVirtualValueId>();
            var callSites = new HashSet<CallSiteId>();
            var rootLocation = new GraphLocation(graph?.Id);

            void Error(GraphDiagnosticCode code, GraphLocation location, string reason) =>
                diagnostics.Add(new(code, location, reason));

            bool Present([NotNullWhen(true)] object? value, GraphLocation location)
            {
                if (value is not null)
                    return true;

                Error(GraphDiagnosticCode.MissingData, location, "Required data is absent.");
                return false;
            }

            bool CheckType(ValueType type, GraphLocation location)
            {
                if (type is ValueType.PineValue)
                    return true;

                Error(GraphDiagnosticCode.InvalidType, location, "Unsupported semantic value type.");
                return false;
            }

            void MatchType(ValueType? actual, ValueType? expected, GraphLocation location)
            {
                if (actual is not null && expected is not null && actual != expected)
                    Error(GraphDiagnosticCode.TypeMismatch, location, "Value type differs from the required type.");
            }

            bool CheckPath(EnvironmentPath? path, GraphLocation location)
            {
                if (!Present(path, location) || !Present(path!.Indices, location))
                    return false;

                if (path.Indices.Any(index => index < 0))
                {
                    Error(GraphDiagnosticCode.InvalidPath, location, "Projection indices must be nonnegative.");
                    return false;
                }

                return true;
            }

            bool CheckSignature(FunctionSignature? signature, GraphLocation location)
            {
                if (!Present(signature, location))
                    return false;

                var valid = true;

                if (Present(signature!.Parameters, location with { Member = location.Member + ".parameters" }))
                {
                    for (var index = 0; index < signature.Parameters.Count; ++index)
                    {
                        var parameter = signature.Parameters[index];
                        var parameterLocation = location with { Member = location.Member + $".parameters[{index}]" };

                        if (!Present(parameter, parameterLocation))
                        {
                            valid = false;
                            continue;
                        }

                        valid = CheckType(parameter.Type, parameterLocation) & valid;
                        valid = CheckPath(parameter.Path, parameterLocation with { Member = parameterLocation.Member + ".path" }) & valid;
                    }
                }
                else
                    valid = false;

                if (Present(signature.Results, location with { Member = location.Member + ".results" }))
                {
                    for (var index = 0; index < signature.Results.Count; ++index)
                        valid = CheckType(signature.Results[index], location with { Member = location.Member + $".results[{index}]" }) & valid;
                }
                else
                    valid = false;

                return valid;
            }

            bool CheckLiteral(LiteralValue? literal, GraphLocation location)
            {
                if (!Present(literal, location))
                    return false;

                switch (literal)
                {
                    case LiteralValue.Blob blob:
                        return Present(blob.Bytes, location with { Member = location.Member + ".bytes" });

                    case LiteralValue.List list:
                        if (!Present(list.Items, location with { Member = location.Member + ".items" }))
                            return false;

                        var valid = true;
                        for (var index = 0; index < list.Items.Count; ++index)
                            valid = CheckLiteral(list.Items[index], location with { Member = location.Member + $".items[{index}]" }) & valid;
                        return valid;

                    default:
                        throw new NotImplementedException(
                            "ValidateGraph does not handle literal variant: " + literal!.GetType().Name);
                }
            }

            void CheckArity(int actual, int? expected, GraphLocation location)
            {
                if (expected is not null && actual != expected)
                    Error(GraphDiagnosticCode.ArityMismatch, location, "Positional value count differs from the contract.");
            }

            if (!Present(graph, rootLocation) ||
                !Present(knownFunctionSignatures, rootLocation with { Member = "knownFunctions" }))
                return Result<ImmutableList<GraphDiagnostic>, ValidatedFunctionGraph>.err(diagnostics.ToImmutable());

            var signatures = knownFunctionSignatures.WithComparers(
                EqualityComparer<FunctionId>.Default, EqualityComparer<FunctionSignature>.Default);
            var validSignatures = signatures.OrderBy(pair => pair.Key.Value)
                .Where(pair => CheckSignature(pair.Value, new(pair.Key, Member: "knownFunctions.signature")))
                .Select(pair => pair.Key)
                .ToImmutableHashSet();

            var rootSignatureValid = CheckSignature(graph.Signature, rootLocation with { Member = "signature" });

            if (rootSignatureValid && validSignatures.Contains(graph.Id) &&
                signatures[graph.Id] != graph.Signature)
                Error(GraphDiagnosticCode.SignatureMismatch, rootLocation with { Member = "signature" },
                    "Root signature differs from its known-function table entry.");

            if (!Present(graph.Blocks, rootLocation with { Member = "blocks" }))
                return Result<ImmutableList<GraphDiagnostic>, ValidatedFunctionGraph>.err(diagnostics.ToImmutable());

            if (!graph.Blocks.ContainsKey(graph.Entry))
                Error(GraphDiagnosticCode.MissingEntry, rootLocation with { Block = graph.Entry, Member = "entry" },
                    "Entry block does not exist.");

            foreach (var (blockId, block) in graph.Blocks.OrderBy(pair => pair.Key.Value))
                CheckBlock(blockId, block);

            return diagnostics.Count is 0
                ? Result<ImmutableList<GraphDiagnostic>, ValidatedFunctionGraph>.ok(new(graph, signatures))
                : Result<ImmutableList<GraphDiagnostic>, ValidatedFunctionGraph>.err(diagnostics.ToImmutable());

            void CheckBlock(PineBlockId blockId, BasicBlock? block)
            {
                var location = rootLocation with { Block = blockId };
                var scope = new Dictionary<PineVirtualValueId, ValueType>();

                if (!Present(block, location))
                    return;

                if (block!.Id != blockId)
                    Error(GraphDiagnosticCode.BlockIdMismatch, location, "Block ID differs from its dictionary key.");

                void Define(ValueDefinition? definition, GraphLocation definitionLocation)
                {
                    if (!Present(definition, definitionLocation))
                        return;

                    var valueLocation = definitionLocation with { Value = definition!.Id };
                    CheckType(definition.Type, valueLocation);
                    if (!definitions.Add(definition.Id))
                        Error(GraphDiagnosticCode.DuplicateDefinition, valueLocation, "Value ID is already defined in this function.");
                    scope.TryAdd(definition.Id, definition.Type);
                }

                ValueType? Use(PineVirtualValueId value, GraphLocation useLocation, ValueType? expected = ValueType.PineValue)
                {
                    var valueLocation = useLocation with { Value = value };
                    if (!scope.TryGetValue(value, out var type))
                    {
                        Error(GraphDiagnosticCode.UndefinedValue, valueLocation, "Value is not defined earlier in this block.");
                        return null;
                    }

                    MatchType(type, expected, valueLocation);
                    return type;
                }

                ImmutableList<ValueDefinition>? Target(PineBlockId target, GraphLocation edgeLocation)
                {
                    if (!graph.Blocks.TryGetValue(target, out var targetBlock))
                    {
                        Error(GraphDiagnosticCode.MissingTarget, edgeLocation, "Edge target block does not exist.");
                        return null;
                    }

                    // Missing target data is reported at the target block, not once per incoming edge.
                    return targetBlock?.Parameters;
                }

                void CheckEdge(Edge? edge, GraphLocation edgeLocation)
                {
                    if (!Present(edge, edgeLocation))
                        return;

                    var parameters = Target(edge!.Target, edgeLocation);
                    if (!Present(edge.Arguments, edgeLocation with { Member = "arguments" }))
                        return;

                    CheckArity(edge.Arguments.Count, parameters?.Count, edgeLocation with { Member = "arguments" });
                    for (var index = 0; index < edge.Arguments.Count; ++index)
                        Use(edge.Arguments[index], edgeLocation with { Member = $"arguments[{index}]" },
                            parameters is not null && index < parameters.Count ? parameters[index]?.Type : null);
                }

                void CheckCall(Call? call)
                {
                    if (!Present(call, location with { Member = "call" }))
                        return;

                    if (!callSites.Add(call!.Site))
                        Error(GraphDiagnosticCode.DuplicateCallSite, location with { Member = "call.site" },
                            "Call site ID is already used in this function.");

                    var signatureValid = CheckSignature(call.Signature, location with { Member = "call.signature" });
                    if (Present(call.Target, location with { Member = "call.target" }))
                    {
                        switch (call.Target)
                        {
                            case CallTarget.Dynamic dynamic:
                                Use(dynamic.EncodedExpression, location with { Member = "call.target" });
                                if (signatureValid && call.Signature != FunctionSignature.Canonical)
                                    Error(GraphDiagnosticCode.SignatureMismatch, location with { Member = "call.signature" },
                                        "Dynamic calls require the canonical signature.");
                                break;

                            case CallTarget.Known known:
                                if (known.Function == graph.Id)
                                {
                                    if (signatureValid && rootSignatureValid && call.Signature != graph.Signature)
                                        Error(GraphDiagnosticCode.SignatureMismatch, location with { Member = "call.signature" },
                                            "Recursive call signature differs from the root signature.");
                                }
                                else if (!signatures.TryGetValue(known.Function, out var knownSignature))
                                    Error(GraphDiagnosticCode.UnknownFunction, location with { Member = "call.target" },
                                        "Known call target is absent from the signature table.");
                                else if (signatureValid && validSignatures.Contains(known.Function) && call.Signature != knownSignature)
                                    Error(GraphDiagnosticCode.SignatureMismatch, location with { Member = "call.signature" },
                                        "Call signature differs from the known-function contract.");
                                break;

                            default:
                                throw new NotImplementedException(
                                    "ValidateGraph does not handle call target variant: " + call.Target.GetType().Name);
                        }
                    }

                    if (Present(call.Arguments, location with { Member = "call.arguments" }))
                    {
                        var parameters = call.Signature?.Parameters;
                        CheckArity(call.Arguments.Count, parameters?.Count, location with { Member = "call.arguments" });
                        for (var index = 0; index < call.Arguments.Count; ++index)
                            Use(call.Arguments[index], location with { Member = $"call.arguments[{index}]" },
                                parameters is not null && index < parameters.Count ? parameters[index]?.Type : null);
                    }
                }

                if (Present(block.Parameters, location with { Member = "parameters" }))
                {
                    for (var index = 0; index < block.Parameters.Count; ++index)
                        Define(block.Parameters[index], location with { Member = $"parameters[{index}]" });

                    if (blockId == graph.Entry && graph.Signature?.Parameters is { } entryParameters)
                    {
                        CheckArity(block.Parameters.Count, entryParameters.Count, location with { Member = "parameters" });
                        for (var index = 0; index < Math.Min(block.Parameters.Count, entryParameters.Count); ++index)
                            MatchType(block.Parameters[index]?.Type, entryParameters[index]?.Type,
                                location with { Value = block.Parameters[index]?.Id, Member = $"parameters[{index}]" });
                    }
                }

                if (Present(block.Operations, location with { Member = "operations" }))
                {
                    for (var index = 0; index < block.Operations.Count; ++index)
                    {
                        var operation = block.Operations[index];
                        var operationLocation = location with { Operation = index };
                        if (!Present(operation, operationLocation))
                            continue;

                        // Inputs are checked before the result enters the block-local scope.
                        switch (operation)
                        {
                            case Operation.Literal literal:
                                CheckLiteral(literal.Value, operationLocation with { Member = "literal" });
                                Define(literal.Result, operationLocation with { Member = "result" });
                                MatchType(literal.Result?.Type, ValueType.PineValue, operationLocation with { Value = literal.Result?.Id, Member = "result" });
                                break;

                            case Operation.MakeList list:
                                if (Present(list.Items, operationLocation with { Member = "items" }))
                                    for (var item = 0; item < list.Items.Count; ++item)
                                        Use(list.Items[item], operationLocation with { Member = $"items[{item}]" });
                                Define(list.Result, operationLocation with { Member = "result" });
                                MatchType(list.Result?.Type, ValueType.PineValue, operationLocation with { Value = list.Result?.Id, Member = "result" });
                                break;

                            case Operation.Project project:
                                Use(project.Source, operationLocation with { Member = "source" });
                                CheckPath(project.Path, operationLocation with { Member = "path" });
                                Define(project.Result, operationLocation with { Member = "result" });
                                MatchType(project.Result?.Type, ValueType.PineValue, operationLocation with { Value = project.Result?.Id, Member = "result" });
                                break;

                            case Operation.Builtin builtin:
                                Use(builtin.Argument, operationLocation with { Member = "argument" });
                                if (!IsBuiltinName(builtin.Name))
                                    Error(GraphDiagnosticCode.InvalidBuiltin, operationLocation with { Member = "name" },
                                        "Name is not a supported Pine builtin.");
                                Define(builtin.Result, operationLocation with { Member = "result" });
                                MatchType(builtin.Result?.Type, ValueType.PineValue, operationLocation with { Value = builtin.Result?.Id, Member = "result" });
                                break;

                            default:
                                throw new NotImplementedException(
                                    "ValidateGraph does not handle operation variant: " + operation.GetType().Name);
                        }
                    }
                }

                if (!Present(block.Terminator, location with { Member = "terminator" }))
                    return;

                switch (block.Terminator)
                {
                    case Terminator.Return result:
                        if (Present(result.Values, location with { Member = "return.values" }))
                        {
                            var results = graph.Signature?.Results;
                            CheckArity(result.Values.Count, results?.Count, location with { Member = "return.values" });
                            for (var index = 0; index < result.Values.Count; ++index)
                                Use(result.Values[index], location with { Member = $"return.values[{index}]" },
                                    results is not null && index < results.Count ? results[index] : null);
                        }
                        break;

                    case Terminator.Jump jump:
                        CheckEdge(jump.Edge, location with { Edge = 0 });
                        break;

                    case Terminator.Branch branch:
                        Use(branch.TestedValue, location with { Member = "testedValue" });
                        CheckLiteral(branch.Literal, location with { Member = "literal" });
                        CheckEdge(branch.IfEqual, location with { Edge = 0 });
                        CheckEdge(branch.IfNotEqual, location with { Edge = 1 });
                        break;

                    case Terminator.Switch selection:
                        Use(selection.Selector, location with { Member = "selector" });
                        if (Present(selection.Cases, location with { Member = "cases" }))
                        {
                            var literals = new HashSet<LiteralValue>();
                            for (var index = 0; index < selection.Cases.Count; ++index)
                            {
                                var @case = selection.Cases[index];
                                var caseLocation = location with { Edge = index };
                                if (!Present(@case, caseLocation))
                                    continue;

                                if (CheckLiteral(@case.Value, caseLocation with { Member = "literal" }) && !literals.Add(@case.Value))
                                    Error(GraphDiagnosticCode.DuplicateCase, caseLocation with { Member = "literal" },
                                        "Switch literal duplicates an earlier case.");
                                CheckEdge(@case.Edge, caseLocation);
                            }
                        }
                        CheckEdge(selection.Default, location with { Edge = selection.Cases?.Count, Member = "default" });
                        break;

                    case Terminator.Invoke invoke:
                        CheckCall(invoke.Call);
                        var continuationLocation = location with { Edge = 0 };
                        if (Present(invoke.Continuation, continuationLocation))
                        {
                            var continuation = invoke.Continuation;
                            var parameters = Target(continuation.Target, continuationLocation);
                            if (Present(continuation.Bindings, continuationLocation with { Member = "bindings" }))
                            {
                                CheckArity(continuation.Bindings.Count, parameters?.Count, continuationLocation with { Member = "bindings" });
                                for (var index = 0; index < continuation.Bindings.Count; ++index)
                                {
                                    var binding = continuation.Bindings[index];
                                    var bindingLocation = continuationLocation with { Member = $"bindings[{index}]" };
                                    var expected = parameters is not null && index < parameters.Count ? parameters[index]?.Type : null;
                                    if (!Present(binding, bindingLocation))
                                        continue;

                                    switch (binding)
                                    {
                                        case ContinuationBinding.CallerValue caller:
                                            Use(caller.Value, bindingLocation, expected);
                                            break;

                                        case ContinuationBinding.ReturnedResult returned:
                                            var results = invoke.Call?.Signature?.Results;
                                            if (returned.Index < 0 || (results is not null && returned.Index >= results.Count))
                                                Error(GraphDiagnosticCode.InvalidReturnedResult, bindingLocation,
                                                    "Returned result index is outside the callee result vector.");
                                            else if (results is not null)
                                                MatchType(results[returned.Index], expected, bindingLocation);
                                            break;

                                        default:
                                            throw new NotImplementedException(
                                                "ValidateGraph does not handle continuation binding variant: " + binding.GetType().Name);
                                    }
                                }
                            }
                        }
                        break;

                    case Terminator.TailInvoke tail:
                        CheckCall(tail.Call);
                        if (tail.Call?.Signature?.Results is { } callResults && graph.Signature?.Results is { } rootResults &&
                            !callResults.SequenceEqual(rootResults))
                            Error(GraphDiagnosticCode.SignatureMismatch, location with { Member = "call.signature.results" },
                                "Tail call results differ from the enclosing function's results.");
                        break;

                    default:
                        throw new NotImplementedException(
                            "ValidateGraph does not handle terminator variant: " + block.Terminator.GetType().Name);
                }
            }
        }
    }

    private static bool IsBuiltinName(string? name) =>
        name is
        nameof(BuiltinFunction.equal) or
        nameof(BuiltinFunction.length) or
        nameof(BuiltinFunction.head) or
        nameof(BuiltinFunction.skip) or
        nameof(BuiltinFunction.take) or
        nameof(BuiltinFunction.concat) or
        nameof(BuiltinFunction.reverse) or
        nameof(BuiltinFunction.negate) or
        nameof(BuiltinFunction.int_add) or
        nameof(BuiltinFunction.int_mul) or
        nameof(BuiltinFunction.int_is_sorted_asc) or
        nameof(BuiltinFunction.bit_and) or
        nameof(BuiltinFunction.bit_or) or
        nameof(BuiltinFunction.bit_xor) or
        nameof(BuiltinFunction.bit_not) or
        nameof(BuiltinFunction.bit_shift_left) or
        nameof(BuiltinFunction.bit_shift_right);
}
