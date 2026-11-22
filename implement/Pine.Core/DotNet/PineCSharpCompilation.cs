using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Linq;

using InternalExpression = Pine.Core.Runtime.InternalExpression.Expression;
using Conversion = Pine.Core.Runtime.InternalExpression.ExpressionConversion;

namespace Pine.Core.DotNet;

/// <summary>Options for compiling an evaluation-closed graph, without an interpreter fallback.</summary>
/// <param name="EnableCancellation">Whether to emit cancellation support in the generated methods.</param>
/// <param name="IncludeGenericEntry">Whether to generate a root entry point without environment constraints.</param>
/// <param name="ClassName">Name of the generated methods class.</param>
/// <param name="LiteralsClassName">Name of the generated literal pool class.</param>
/// <param name="MaximumFunctionCount">Maximum number of functions in the compiled graph.</param>
/// <param name="ContainerNamespace">Namespace for the generated classes, or <c>null</c> for the global namespace.</param>
/// <param name="MaximumInlineMatchDifferingPositions">
/// Maximum varying byte/item positions in an inlined constant-match group. Zero disables inlining.
/// Common positions and length/type guards are shared independently of literal length.
/// </param>
public sealed record PineCSharpCompilationOptions(
    bool EnableCancellation = true,
    bool IncludeGenericEntry = true,
    string ClassName = "CompiledPine",
    string LiteralsClassName = "PineLiterals",
    int MaximumFunctionCount = 1024,
    string? ContainerNamespace = null,
    int MaximumInlineMatchDifferingPositions = 4)
{
    /// <summary>Fully qualified metadata name of the generated methods class.</summary>
    public string MethodsTypeName => Qualify(ClassName);

    /// <summary>Fully qualified metadata name of the generated literal pool.</summary>
    public string LiteralsTypeName => Qualify(LiteralsClassName);

    private string Qualify(string name) => ContainerNamespace is null ? name : ContainerNamespace + "." + name;
}

/// <summary>Properties of control flow in the emitted C#, not a termination or asymptotic-time proof.</summary>
public sealed record PineCSharpComplexity(bool HasLoops, bool HasRecursion, bool HasMutualRecursion);

/// <summary>A function's identity. Canonical expression conversion and hashing are demand-driven.</summary>
public sealed class PineCSharpFunction
{
    private readonly Lazy<Expression> _expression;

    private readonly Lazy<string> _expressionHash;

    /// <summary>Index in this compilation's method graph.</summary>
    public int Index { get; }

    /// <summary>Original expression, before constraint substitution.</summary>
    public Expression Expression => _expression.Value;

    /// <summary>Assumptions used to instantiate the expression.</summary>
    public PineValueClass? EnvironmentConstraint { get; }

    /// <summary>Stable encoded-expression hash, computed only when requested.</summary>
    public string ExpressionHash => _expressionHash.Value;

    internal PineCSharpFunction(
        int index,
        InternalExpression expression,
        PineValueClass? constraint,
        ConcurrentPineValueHashCache? canonicalHashCache = null)
    {
        Index = index;
        EnvironmentConstraint = constraint;
        _expression = new(() => Conversion.ToCanonicalCached(expression));

        _expressionHash =
            new(
                () =>
                Convert.ToHexStringLower(
                    (canonicalHashCache ??= new()).GetHash(
                        Runtime.InternalExpression.ExpressionValueEncoding.EncodeExpressionAsValue(expression)).Span));
    }
}

/// <summary>A persistent, cheap call path from a requested root to a function.</summary>
public sealed record PineCSharpCallPath(
    PineCSharpFunction Function,
    PineCSharpCallPath? Parent,
    PineBlockId? CallerBlock,
    int RootIndex)
{
    /// <summary>Renders graph indices and call-site blocks; hashing is opt-in.</summary>
    public string Describe(bool includeExpressionHashes = false)
    {
        var segments = new Stack<PineCSharpCallPath>();

        for (var path = this; path is not null; path = path.Parent)
            segments.Push(path);

        return
            "root " + RootIndex + ": " +
            string.Join(
                " -> ",
                segments.Select(
                    path => (path.CallerBlock is { } block ? "block " + block.Value + " / " : "") +
                        "function " + path.Function.Index +
                        (includeExpressionHashes ? " (" + path.Function.ExpressionHash + ")" : "")));
    }
}

/// <summary>Why a reachable graph cannot be compiled as a leaf.</summary>
public enum PineCSharpRejectionKind
{
    /// <summary>A runtime expression remains unresolved.</summary>
    DynamicEvaluation,
    /// <summary>A constant eval has a malformed target or unsupported template form.</summary>
    UnsupportedConstantEvaluation,
    /// <summary>The C# backend does not implement an operation.</summary>
    UnsupportedOperation,
    /// <summary>Instantiation exceeded the configured graph bound.</summary>
    FunctionLimit,
    /// <summary>A static call's established interface does not match its target.</summary>
    IncompatibleInterface,
}

/// <summary>
/// A rejection retains identities, a call path, and the offending instruction, not eagerly formatted
/// expressions or derived hashes. Callers can inspect these to improve upstream specialization.
/// </summary>
public sealed record PineCSharpRejection(
    PineCSharpRejectionKind Kind,
    PineCSharpCallPath Path,
    PineBlockId Block,
    int? OperationIndex,
    StackInstruction? Instruction)
{
    /// <summary>The optimized graph containing the offending operation, retained without formatting.</summary>
    public PineControlFlowGraph? Graph { get; init; }

    /// <summary>Formats a diagnostic, optionally deriving stable expression hashes.</summary>
    public string Describe(bool includeExpressionHashes = false) =>
        Kind + " at " + Path.Describe(includeExpressionHashes) +
        " / block " + Block.Value +
        (OperationIndex is { } index ? " / operation " + index : " / terminator") +
        (Instruction is { } instruction ? ": " + instruction.Kind : "");

    /// <inheritdoc/>
    public override string ToString() => Describe();
}

/// <summary>A spread worker interface available in the generated class.</summary>
public sealed record PineCSharpMethod(
    PineCSharpFunction Function,
    string Name,
    StaticFunctionInterface Parameters,
    ReturnValueLayout Returns);

/// <summary>Separate compilation units for methods and reusable, fully represented literals.</summary>
public sealed record PineCSharpCompilation(
    string MethodsSource,
    string LiteralsSource,
    PineCSharpCompilationOptions Options,
    IReadOnlyList<PineCSharpMethod> Methods,
    PineCSharpComplexity Complexity,
    string DispatchMethodName);

/// <summary>Compiles specialized CFGs to C# only after proving their transitive evaluation closure.</summary>
public static class PineCSharpCompiler
{
    internal sealed class Function(
        PineCSharpFunction identity,
        InternalExpressionCompilation.StaticFunction prepared,
        PineCSharpCallPath path)
    {
        public PineCSharpFunction Identity { get; } = identity;

        public InternalExpressionCompilation.StaticFunction Prepared { get; } = prepared;

        public PineCSharpCallPath Path { get; } = path;

        public PineControlFlowGraph Graph { get; } = prepared.CreateGraph();

        public Dictionary<PineBlockId, Function> Calls { get; } = [];
    }

    /// <summary>
    /// Compiles the generic entry and/or explicitly requested specializations. With no generic entry,
    /// The dispatch method returns null on a constraint mismatch, allowing selection outside the compiled leaf.
    /// Every requested variant must be evaluation-closed; rejection never hides a VM fallback.
    /// </summary>
    public static Result<PineCSharpRejection, PineCSharpCompilation> Compile(
        Expression expression,
        IReadOnlyList<PineValueClass>? specializations = null,
        PineCSharpCompilationOptions? options = null,
        PineVMParseCache? parseCache = null,
        ConcurrentPineValueHashCache? canonicalHashCache = null)
    {
        canonicalHashCache ??= new();

        return
            Compile(
                PineCSharpMethodNaming.Default(canonicalHashCache),
                expression,
                specializations,
                options,
                parseCache,
                canonicalHashCache);
    }

    /// <summary>
    /// Compiles using caller-supplied declaration naming. Reuse the naming policy and canonical
    /// hash-tree cache across incremental refinements; namespace options scope each emitted module.
    /// Names must be unique C# identifiers, distinct from the containing class and private loop helpers.
    /// </summary>
    public static Result<PineCSharpRejection, PineCSharpCompilation> Compile(
        PineCSharpMethodNaming methodNaming,
        Expression expression,
        IReadOnlyList<PineValueClass>? specializations = null,
        PineCSharpCompilationOptions? options = null,
        PineVMParseCache? parseCache = null,
        ConcurrentPineValueHashCache? canonicalHashCache = null)
    {
        ArgumentNullException.ThrowIfNull(methodNaming);
        ArgumentNullException.ThrowIfNull(methodNaming.GetMethodName);
        ArgumentNullException.ThrowIfNull(methodNaming.GetDispatchMethodName);
        canonicalHashCache ??= new();
        options ??= new();
        ArgumentOutOfRangeException.ThrowIfLessThan(options.MaximumFunctionCount, 1);
        ArgumentOutOfRangeException.ThrowIfNegative(options.MaximumInlineMatchDifferingPositions);
        ValidateName(options.ClassName);
        ValidateName(options.LiteralsClassName);

        if (options.ContainerNamespace is { } containerNamespace)
        {
            foreach (var segment in containerNamespace.Split('.'))
                ValidateIdentifier(segment, "namespace segment");
        }

        if (options.ClassName == options.LiteralsClassName)
            throw new ArgumentException("Methods and literals must use different class names.", nameof(options));

        specializations ??= [];

        if (!options.IncludeGenericEntry && specializations.Count == 0)
            throw new ArgumentException("At least one entry variant must be requested.", nameof(specializations));

        parseCache ??= new();
        var root = Conversion.FromCanonicalCached(expression);

        var dispatch =
            new InternalExpressionCompilation.StaticDispatchCompilation(
                root,
                specializations,
                parseCache,
                disableReduction: false,
                enableTailRecursionOptimization: true,
                skipInlining: (_, _) => true,
                reducedExpressionCache: null,
                InternalExpressionCompilation.DefaultPathMaxLowExclusive,
                InternalExpressionCompilation.DefaultPathMaxHighInclusive,
                disableGenericApplicationChainConsolidation: false,
                enableDirectInvocation: true,
                skipDirectInvocation: null,
                compilationEnvClasses: null,
                requireStaticClosure: true);

        var functions = new List<Function>();
        var byKey = new Dictionary<(InternalExpression, PineValueClass?), Function>();
        var roots = new List<Function>();

        Function Add(
            InternalExpression body,
            PineValueClass? constraint,
            PineCSharpCallPath? parent,
            PineBlockId? block,
            int rootIndex)
        {
            if (byKey.TryGetValue((body, constraint), out var existing))
                return existing;

            var identity = new PineCSharpFunction(functions.Count, body, constraint, canonicalHashCache);
            var path = new PineCSharpCallPath(identity, parent, block, rootIndex);
            var function = new Function(identity, dispatch.GetFunction(body, constraint), path);
            byKey.Add((body, constraint), function);
            functions.Add(function);
            return function;
        }

        foreach (var constraint in specializations.Where(constraint => constraint.ParsedItems.Count > 0)
            .Distinct().OrderDescending(PineValueClassSpecificityComparer.Instance))
        {
            roots.Add(Add(root, constraint, null, null, roots.Count));
        }

        if (options.IncludeGenericEntry || specializations.Any(constraint => constraint.ParsedItems.Count == 0))
            roots.Add(Add(root, null, null, null, roots.Count));

        for (var index = 0; index < functions.Count; index++)
        {
            var function = functions[index];

            if (functions.Count > options.MaximumFunctionCount)
                return Reject(PineCSharpRejectionKind.FunctionLimit, function, function.Graph.Entry, null, null);

            foreach (var block in function.Graph.Blocks)
            {
                for (var operationIndex = 0; operationIndex < block.Operations.Length; operationIndex++)
                {
                    var instruction = block.Operations[operationIndex].Instruction;

                    if (!PineCSharpEmitter.Supports(instruction.Kind))
                    {
                        return
                            Reject(
                                PineCSharpRejectionKind.UnsupportedOperation,
                                function,
                                block.Id,
                                operationIndex,
                                instruction);
                    }
                }

                var instructionAtCall =
                    block.Terminator switch
                    {
                        PineControlFlowTerminator.Invoke invoke => invoke.Instruction,
                        PineControlFlowTerminator.TailInvoke tail => tail.InvokeInstruction,

                        PineControlFlowTerminator.Return or PineControlFlowTerminator.Jump or
                        PineControlFlowTerminator.ConditionalJump or PineControlFlowTerminator.Switch or
                        PineControlFlowTerminator.ValueMap =>
                        null,

                        _ =>
                        throw new NotImplementedException(
                            "Compile does not handle terminator: " + block.Terminator.GetType().Name)
                    };

                if (instructionAtCall is null)
                    continue;

                InternalExpression target;
                PineValueClass? constraint;

                switch (instructionAtCall.Kind)
                {
                    case StackInstructionKind.Eval_Binary:
                    case StackInstructionKind.Eval_Multi:
                        return
                            Reject(
                                PineCSharpRejectionKind.DynamicEvaluation,
                                function,
                                block.Id,
                                null,
                                instructionAtCall);

                    case StackInstructionKind.Eval_Const:
                        if (instructionAtCall.TemplatePlan is not null || instructionAtCall.TemplateProducer ||
                            instructionAtCall.Literal is not { } literal ||
                            ExpressionTemplatePlan.TryParseTemplate(literal.Evaluate(), parseCache) is not null ||
                            parseCache.ParseInternalExpression(literal.Evaluate()).IsOkOrNull() is not { } parsed)
                        {
                            return
                                Reject(
                                    PineCSharpRejectionKind.UnsupportedConstantEvaluation,
                                    function,
                                    block.Id,
                                    null,
                                    instructionAtCall);
                        }

                        target = parsed;
                        constraint = null;
                        break;

                    case StackInstructionKind.Invoke_StackFrame_Const:
                        var direct =
                            instructionAtCall.OptimizedInvocation ??
                            throw new InvalidOperationException("Static call without target.");

                        target = direct.InternalExpression;
                        constraint = direct.EnvironmentConstraint;
                        break;

                    default:
                        return
                            Reject(
                                PineCSharpRejectionKind.UnsupportedOperation,
                                function,
                                block.Id,
                                null,
                                instructionAtCall);
                }

                if (!byKey.ContainsKey((target, constraint)) && functions.Count >= options.MaximumFunctionCount)
                    return Reject(PineCSharpRejectionKind.FunctionLimit, function, block.Id, null, instructionAtCall);

                var callee = Add(target, constraint, function.Path, block.Id, function.Path.RootIndex);

                if (instructionAtCall.OptimizedInvocation is { } invocation &&
                    (!invocation.InvocationInterface.Equals(callee.Prepared.Parameters) ||
                    !invocation.ReturnLayout.Equals(callee.Prepared.Returns)))
                {
                    return
                        Reject(
                            PineCSharpRejectionKind.IncompatibleInterface,
                            function,
                            block.Id,
                            null,
                            instructionAtCall);
                }

                function.Calls.Add(block.Id, callee);
            }
        }

        return
            Result<PineCSharpRejection, PineCSharpCompilation>.ok(
                new PineCSharpEmitter(functions, roots, options, methodNaming, canonicalHashCache).Emit());
    }

    private static Result<PineCSharpRejection, PineCSharpCompilation> Reject(
        PineCSharpRejectionKind kind,
        Function function,
        PineBlockId block,
        int? operation,
        StackInstruction? instruction) =>
        Result<PineCSharpRejection, PineCSharpCompilation>.err(
            new(kind, function.Path, block, operation, instruction) { Graph = function.Graph });

    private static void ValidateName(string name)
    {
        ValidateIdentifier(name, "class");

        if (name is "Pine" or "System" or "PineValue" or "PineValueInProcess" or "SliceOffset" or "BigInteger" or
            "BlobMemory" or "ListMemory" or "Kernel" or "Specialized" or "Evaluate" ||
            name.StartsWith("Function_", StringComparison.Ordinal) ||
            name.StartsWith("Loop_", StringComparison.Ordinal) ||
            name.StartsWith("Map_", StringComparison.Ordinal) ||
            name.StartsWith("Switch_", StringComparison.Ordinal) ||
            name.StartsWith("Return_", StringComparison.Ordinal) ||
            name.StartsWith("Literal_Value_", StringComparison.Ordinal) ||
            name.StartsWith("Value_", StringComparison.Ordinal) ||
            name.StartsWith("Materialized_Value_", StringComparison.Ordinal))
        {
            throw new ArgumentException("Invalid C# class identifier: " + name);
        }
    }

    internal static void ValidateIdentifier(string name, string kind)
    {
        if (!Microsoft.CodeAnalysis.CSharp.SyntaxFacts.IsValidIdentifier(name) ||
            name.StartsWith('@') ||
            Microsoft.CodeAnalysis.CSharp.SyntaxFacts.GetKeywordKind(name) !=
            Microsoft.CodeAnalysis.CSharp.SyntaxKind.None)
        {
            throw new ArgumentException("Invalid C# " + kind + " identifier: " + name);
        }
    }
}
