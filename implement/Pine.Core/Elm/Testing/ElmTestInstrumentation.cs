using Pine.Core.Addressing;
using Pine.Core.CLI;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Internal;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Threading;

using InstrumentedVM = Pine.Core.Interpreter.IntermediateVM.PineVM;

namespace Pine.Core.Elm.Testing;

/// <summary>Shared execution limits plus optional diagnostic sampling and VM optimization controls.</summary>
public sealed record ElmTestInstrumentationOptions : ElmTestEvaluationOptions
{
    /// <summary>Minimum interval between VM-event-driven samples; zero disables periodic sampling.</summary>
    public TimeSpan SnapshotInterval { get; init; } = TimeSpan.FromSeconds(5);

    /// <summary>Maximum number of frames retained in each sampled stack trace.</summary>
    public int StackTraceDepth { get; init; } = 20;

    /// <summary>Maximum retained samples, including the final stopped stack; older samples are discarded.</summary>
    public int MaxSamples { get; init; } = 200;

    /// <summary>Records input paths and materialized values without forcing lazy values.</summary>
    public bool IncludeInputs { get; init; }

    /// <summary>Queries and records locals only when a stack trace is captured.</summary>
    public bool IncludeLocals { get; init; }

    /// <summary>Disables native precompiled leaves to observe pure Pine evaluation.</summary>
    public bool DisablePrecompiledLeaves { get; init; }

    /// <summary>Disables invocation-result caching for instrumented VMs.</summary>
    public bool DisableInvocationCache { get; init; }

    /// <summary>Disables tail-call frame replacement; compiler-generated backward jumps may remain.</summary>
    public bool DisableTailRecursion { get; init; }

    /// <summary>Disables expression reduction during VM compilation.</summary>
    public bool DisableReduction { get; init; }

    /// <summary>Preserves expression call boundaries for stack attribution; tail-loop lowering can still occur.</summary>
    public bool DisableInlining { get; init; }

    /// <summary>Disables direct invocation, generic application consolidation and simple eval/template shortcuts.</summary>
    public bool DisableApplicationFastPaths { get; init; }

    /// <inheritdoc/>
    public override void Validate()
    {
        base.Validate();

        if (SnapshotInterval < TimeSpan.Zero || StackTraceDepth <= 0)
            throw new ArgumentException("Snapshot interval must be nonnegative; stack depths must be positive.");

        if (MaxSamples <= 0)
            throw new ArgumentException("Maximum retained samples must be positive.");
    }
}

/// <summary>Cheap snapshot of the outcome, current Elm context, elapsed time and aggregate VM work.</summary>
public sealed record ElmTestProfileSummary(
    string Outcome, string? StopReason, string Phase, string Context,
    double ElapsedMilliseconds, PerformanceCounters Counters, string? ProfileFilter = null);

/// <summary>A captured value's DAG hash or bounded nonmaterializing structural preview.</summary>
public sealed record ElmTestProfileValue(string? ValueHash, string Preview)
{
    /// <summary>Bounded, directly available children of an unmaterialized list; never forces the parent.</summary>
    public IReadOnlyList<ElmTestProfileValue>? Items { get; init; }

    /// <summary>Full directly available child count, which can exceed the retained preview.</summary>
    public int? ItemCount { get; init; }
}

/// <summary>A sampled VM frame with optional input paths, materialized input values and locals.</summary>
public sealed record ElmTestProfileFrame(
    string ExpressionHash, int InstructionPointer,
    IReadOnlyList<int[]>? ParameterPaths,
    IReadOnlyList<ElmTestProfileValue>? Inputs,
    IReadOnlyList<ElmTestProfileValue>? Locals)
{
    /// <summary>Elm declarations associated with this expression, if known.</summary>
    public IReadOnlyList<string> Declarations { get; init; } = [];

    /// <summary>Frame identity within one evaluation; tail replacements receive a new identity.</summary>
    public long? FrameIndex { get; init; }

    /// <summary>Exclusive instructions executed so far by this frame.</summary>
    public long InstructionCount { get; init; }

    /// <summary>Backward jumps executed so far by this frame.</summary>
    public long LoopIterationCount { get; init; }

    /// <summary>Key into the report's compiled instruction listings.</summary>
    public string? CompiledFrameId { get; init; }
}

/// <summary>A summary and the stack captured at the same evaluation boundary, current frame first.</summary>
public sealed record ElmTestProfileSample(ElmTestProfileSummary Summary, IReadOnlyList<ElmTestProfileFrame> StackTrace);

/// <summary>Observed entry counts and exclusive work for a content-addressed Pine expression.</summary>
public sealed record ElmTestExpressionProfile(
    [property: JsonPropertyOrder(-2)] string Hash,
    [property: JsonPropertyOrder(-1)] IReadOnlyList<string> Declarations,
    [property: JsonPropertyOrder(0)] long Invocations,
    [property: JsonPropertyOrder(2)] long Instructions,
    [property: JsonPropertyOrder(1)] long LoopIterations,
    [property: JsonPropertyOrder(3)] string Description);

/// <summary>A content-addressed DAG node containing either base64 blob bytes or child node hashes.</summary>
public sealed record ElmTestProfileValueNode(string? BlobBase64, IReadOnlyList<string>? Items);

/// <summary>Exact backward-jump count at a compiled instruction offset, including nonreturning frames.</summary>
public sealed record ElmTestLoopProfile(
    string ExpressionHash, string CompiledFrameId, int InstructionPointer, long Iterations);

/// <summary>Versioned, self-contained profile with complete expression/value graphs and captured diagnostics.</summary>
public sealed record ElmTestProfileReport(
    int SchemaVersion, ElmTestInstrumentationOptions Options, string? SelectedTest,
    ElmTestProfileSummary Summary, IReadOnlyList<ElmTestProfileSample> Samples,
    IReadOnlyList<ElmTestExpressionProfile> Expressions,
    IReadOnlyDictionary<string, ElmTestProfileValueNode> Values)
{
    /// <summary>Project, filter, seed and resolution metadata needed to interpret the run.</summary>
    public IReadOnlyDictionary<string, string> Metadata { get; init; } = new Dictionary<string, string>();

    /// <summary>Compiled body identifiers mapped to instruction listings with jump destinations.</summary>
    public IReadOnlyDictionary<string, string> CompiledFrames { get; init; } = new Dictionary<string, string>();

    /// <summary>Exact backward-jump counts grouped by compiled body and instruction offset.</summary>
    public IReadOnlyList<ElmTestLoopProfile> LoopSites { get; init; } = [];

    /// <summary>Number of older samples evicted by the retention limit; aggregate counters are unaffected.</summary>
    public long DroppedSamples { get; init; }

    /// <summary>Serializes the report with readable indentation and numeric counters.</summary>
    public string ToJson() => JsonSerializer.Serialize(this, new JsonSerializerOptions { WriteIndented = true });
}

/// <summary>Stops test preparation or execution after a command-wide limit or cancellation.</summary>
public sealed class ElmTestInstrumentationStoppedException(string message) : Exception(message);

/// <summary>A discovered test and a filter that unambiguously selects it.</summary>
public sealed record ElmTestSelectionSuggestion(ListedTest Test, string Filter);

/// <summary>Selection diagnostics when profiling cannot execute exactly one runnable Elm test.</summary>
public sealed class ElmTestInstrumentationSelectionException(
    int foundCount, int remainingCount, string? filter,
    IReadOnlyList<ElmTestSelectionSuggestion> suggestions,
    bool suggestionsFromRemaining = true)
    : Exception("Profiling requires exactly one runnable Elm test.")
{
    /// <summary>Number of nonempty test entries discovered before selection.</summary>
    public int FoundCount { get; } = foundCount;

    /// <summary>Number of nonempty entries after Test.only semantics and the optional filter.</summary>
    public int RemainingCount { get; } = remainingCount;

    /// <summary>The user-supplied filter, or null when omitted.</summary>
    public string? Filter { get; } = filter;

    /// <summary>Ready-to-use selectors for runnable candidate tests.</summary>
    public IReadOnlyList<ElmTestSelectionSuggestion> Suggestions { get; } = suggestions;

    /// <summary>Whether candidates come from the current selection rather than a zero-match fallback.</summary>
    public bool SuggestionsFromRemaining { get; } = suggestionsFromRemaining;
}

/// <summary>
/// Opt-in instrumentation across test preparation and execution. Frame and backward-jump
/// hooks avoid per-instruction callbacks. Expensive stack/local queries run only when sampled.
/// Diagnostic recording requires serial evaluation; nonprofiling budgets support parallel VMs.
/// </summary>
public sealed class ElmTestInstrumentation : IDisposable
{
    private sealed class Entry(Expression expression)
    {
        public Expression Expression { get; } = expression;

        public long _invocations;

        public long _instructions;

        public long _loops;

        public HashSet<string> Names { get; } = new(StringComparer.Ordinal);
    }

    private sealed record Scope(string Phase, string Context, string? ProfileFilter = null);

    private sealed record CapturedValue(
        PineValue? Value, string Preview, CapturedValue[]? Items = null, int? ItemCount = null);

    private sealed record CapturedFrame(
        Expression Expression, int Pointer, int[][]? Paths,
        CapturedValue[]? Inputs, CapturedValue[]? Locals,
        StackFrameInstructions? Instructions, long? FrameIndex, long InstructionCount, long LoopIterationCount);

    private sealed record CapturedSample(ElmTestProfileSummary Summary, CapturedFrame[] Frames);

    private sealed class FrameKeyComparer :
        IEqualityComparer<(Expression Expression, StackFrameInstructions Instructions)>,
        IEqualityComparer<(Expression Expression, StackFrameInstructions Instructions, int Pointer)>
    {
        public bool Equals(
            (Expression Expression, StackFrameInstructions Instructions) x,
            (Expression Expression, StackFrameInstructions Instructions) y) =>
            ReferenceEquals(x.Instructions, y.Instructions) && x.Expression.Equals(y.Expression);

        public int GetHashCode((Expression Expression, StackFrameInstructions Instructions) key) =>
            HashCode.Combine(key.Expression.GetHashCode(), RuntimeHelpers.GetHashCode(key.Instructions));

        public bool Equals(
            (Expression Expression, StackFrameInstructions Instructions, int Pointer) x,
            (Expression Expression, StackFrameInstructions Instructions, int Pointer) y) =>
            x.Pointer == y.Pointer && Equals((x.Expression, x.Instructions), (y.Expression, y.Instructions));

        public int GetHashCode((Expression Expression, StackFrameInstructions Instructions, int Pointer) key) =>
            HashCode.Combine(GetHashCode((key.Expression, key.Instructions)), key.Pointer);
    }

    private sealed class ScopeLease(Action restore) : IDisposable
    {
        public void Dispose() => restore();
    }

    private readonly Dictionary<Expression, Entry> _entries = [];

    private readonly Queue<CapturedSample> _samples = [];

    private readonly Dictionary<(Expression Expression, StackFrameInstructions Instructions, int Pointer), long> _loopSites =
        new(new FrameKeyComparer());

    private readonly Dictionary<(Expression Expression, StackFrameInstructions Instructions), string> _compiledFrameIds =
        new(new FrameKeyComparer());

    private long _droppedSamples;

    private readonly CancellationTokenSource _cancellation = new();

    private readonly Stopwatch _elapsed = Stopwatch.StartNew();

    private readonly IReadOnlyDictionary<PineValue, PrecompiledLeaf>? _precompiledLeaves;

    private readonly Func<IReadOnlyDictionary<PineValue, PrecompiledLeaf>>? _precompiledLeavesProvider;

    private readonly AsyncLocal<Scope?> _scope = new();

    private readonly Lock _budgetLock = new();

    private Scope _lastScope = new("initialization", "");

    private PerformanceCounters _completed;

    private Func<PerformanceCounters>? _loadCurrentCounters;

    private long _nextSampleTicks;

    private string? _stopReason;

    private string _outcome = "running";

    private bool _stopAnnounced;

    /// <summary>Full path of the selected test after discovery, or null before selection.</summary>
    public string? SelectedTest { get; private set; }

    /// <summary>Run metadata populated during discovery and copied into the saved report.</summary>
    public IDictionary<string, string> Metadata { get; } = new Dictionary<string, string>(StringComparer.Ordinal);

    /// <summary>Whether the user explicitly requested cancellation rather than a quota or timeout.</summary>
    public bool CancelledByUser { get; private set; }

    /// <summary>Whether expression statistics, sampled stacks and captured values are collected.</summary>
    public bool RecordDiagnostics { get; }

    /// <summary>Effective immutable limits and diagnostic controls for this run.</summary>
    public ElmTestInstrumentationOptions Options { get; }

    /// <summary>Cooperative cancellation shared by preparation and all execution VMs.</summary>
    public CancellationToken CancellationToken => _cancellation.Token;

    /// <summary>Called on scope entry, and on periodic samples when OnSample is absent; contains no serialized graphs.</summary>
    public Action<ElmTestProfileSummary>? OnProgress { get; init; }

    /// <summary>Called during evaluation with a frozen sample, without serializing complete value graphs.</summary>
    public Action<ElmTestProfileSample>? OnSample { get; init; }

    /// <summary>Called once with immediately available statistics before detailed report serialization.</summary>
    public Action<ElmTestProfileSummary>? OnStopped { get; init; }

    /// <summary>Creates command-wide tracking and starts the optional cooperative timeout.</summary>
    /// <param name="options">Validated limits and diagnostic controls.</param>
    /// <param name="precompiledLeaves">Explicit native leaves, if supplied.</param>
    /// <param name="precompiledLeavesProvider">Lazy native-leaf provider used when no explicit leaves are supplied.</param>
    /// <param name="recordDiagnostics">False tracks only aggregate work and permits parallel VM execution.</param>
    public ElmTestInstrumentation(
        ElmTestInstrumentationOptions options,
        IReadOnlyDictionary<PineValue, PrecompiledLeaf>? precompiledLeaves = null,
        Func<IReadOnlyDictionary<PineValue, PrecompiledLeaf>>? precompiledLeavesProvider = null,
        bool recordDiagnostics = true)
    {
        options.Validate();
        Options = options;
        RecordDiagnostics = recordDiagnostics;
        _precompiledLeaves = precompiledLeaves;
        _precompiledLeavesProvider = precompiledLeavesProvider;
        _nextSampleTicks = options.SnapshotInterval.Ticks;

        if (options.Timeout is { } timeout)
            _cancellation.CancelAfter(timeout);
    }

    /// <summary>Attributes work on the current async execution flow until the returned scope is disposed.</summary>
    public IDisposable EnterScope(string phase, string context, string? profileFilter = null)
    {
        ThrowIfStopped();
        var previous = _scope.Value;
        _scope.Value = new Scope(phase, context, profileFilter);

        lock (_budgetLock)
        {
            _lastScope = _scope.Value;
        }

        OnProgress?.Invoke(GetSummary());
        return new ScopeLease(() => _scope.Value = previous);
    }

    /// <summary>Records the selected test's full path for the saved report.</summary>
    public void SelectTest(string path) => SelectedTest = path;

    /// <summary>Sets the completed run's outcome and freezes elapsed execution time.</summary>
    public void SetOutcome(string outcome)
    {
        _outcome = outcome;
        _elapsed.Stop();
    }

    /// <summary>Announces cancellation observed outside VM evaluation, such as during dependency resolution.</summary>
    public void NotifyCancellation() => AnnounceStop(_stopReason ?? "Time budget exhausted.");

    /// <summary>Requests user cancellation without serializing or querying VM state.</summary>
    public void Cancel()
    {
        CancelledByUser = true;
        _stopReason = "User cancellation requested.";
        _cancellation.Cancel();
    }

    /// <summary>Returns aggregate statistics without constructing stack traces or encoded value graphs.</summary>
    public ElmTestProfileSummary GetSummary()
    {
        lock (_budgetLock)
        {
            return
                new(
                    _stopReason is null ? _outcome : CancelledByUser ? "cancelled" : "stopped",
                    _stopReason,
                    _lastScope.Phase,
                    _lastScope.Context,
                    _elapsed.Elapsed.TotalMilliseconds,
                    PerformanceCounters.Add(
                        _completed,
                        RecordDiagnostics ? _loadCurrentCounters?.Invoke() ?? default : default),
                    _lastScope.ProfileFilter);
        }
    }

    /// <summary>Associates compiled function expressions with their Elm declaration names for rankings.</summary>
    public void RegisterDeclarations(ElmInteractiveEnvironment.ParsedInteractiveEnvironment environment)
    {
        if (!RecordDiagnostics)
            return;

        var cache = new PineVMParseCache();

        foreach (var module in environment.Modules)
            foreach (var (name, value) in module.moduleContent.FunctionDeclarations)
            {
                // Zero-parameter roots are executable expression wrappers, not necessarily tagged functions.
                if (cache.ParseExpression(value).IsOkOrNull() is { } wrapper)
                    GetEntry(wrapper).Names.Add(module.moduleName + "." + name);

                if (FunctionRecord.ParseFunctionRecordTagged(value, cache).IsOkOrNull() is { ParameterCount: > 0 } function)
                    GetEntry(function.InnerFunction).Names.Add(module.moduleName + "." + name);
            }
    }

    /// <summary>Creates a VM sharing this run's limits and optional recording; profiling VMs must execute serially.</summary>
    public IPineVM CreateVm(IInvocationCacheAccess invocationCache, PineVMSharedCaches caches)
    {
        ThrowIfStopped();
        var vm = new EvaluationContext(this, invocationCache, caches);
        ThrowIfStopped();
        return vm;
    }

    private Entry GetEntry(Expression expression)
    {
        if (!_entries.TryGetValue(expression, out var entry))
            _entries.Add(expression, entry = new Entry(expression));

        return entry;
    }

    private void AnnounceStop(string reason)
    {
        bool announce;

        lock (_budgetLock)
        {
            _stopReason ??= reason;
            _lastScope = _scope.Value ?? _lastScope;
            announce = !_stopAnnounced;
            _stopAnnounced = true;
        }

        if (announce)
            OnStopped?.Invoke(GetSummary());
    }

    private void ObserveWork(PerformanceCounters current, ref PerformanceCounters previous)
    {
        lock (_budgetLock)
        {
            _completed =
                PerformanceCounters.Add(
                    _completed,
                    new PerformanceCounters(
                        current.InvocationCount - previous.InvocationCount,
                        current.BuildListCount - previous.BuildListCount,
                        current.LoopIterationCount - previous.LoopIterationCount,
                        current.InstructionCount - previous.InstructionCount,
                        current.CurriedFunctionPlanParseCount - previous.CurriedFunctionPlanParseCount,
                        current.PartialApplicationAllocationCount - previous.PartialApplicationAllocationCount,
                        current.DirectSaturatedApplicationCount - previous.DirectSaturatedApplicationCount,
                        current.PartialApplicationMaterializationCount -
                        previous.PartialApplicationMaterializationCount));

            previous = current;

            if (Options.InvocationBudget is { } inv && _completed.InvocationCount > inv)
            {
                _stopReason ??=
                    $"InvocationCount budget exhausted: command limit {CommandLineInterface.FormatIntegerForDisplay(inv)}.";
            }

            if (Options.LoopBudget is { } loops && _completed.LoopIterationCount > loops)
            {
                _stopReason ??=
                    $"LoopIterationCount budget exhausted: command limit {CommandLineInterface.FormatIntegerForDisplay(loops)}.";
            }

            if (_stopReason is not null)
                _cancellation.Cancel();
        }
    }

    private void ThrowIfStopped()
    {
        if (_cancellation.IsCancellationRequested)
        {
            AnnounceStop(_stopReason ?? "Time budget exhausted.");
            throw new ElmTestInstrumentationStoppedException(_stopReason!);
        }
    }

    private void Capture(Func<IEnumerable<EvaluationStackTraceFrame>> loadStack)
    {
        var remainingNodes = 4096;

        CapturedValue Freeze(PineValueInProcess? value, int depth = 0)
        {
            if (value is null)
                return new(null, "(uninitialized)");

            if (value.EvaluatedOrNull is { } evaluated)
                return new(evaluated, DescribeValue(evaluated));

            if (value.IntegerOrNull is { } integer)
            {
                return
                    new(
                        IntegerEncoding.EncodeSignedInteger(integer),
                        "integer " + integer + " (cached; not forced)");
            }

            if (depth < 3 && remainingNodes > 0 && value.UnevaluatedStructuralItemsOrNull() is { } items)
            {
                var children = new List<CapturedValue>();

                for (var index = 0; index < Math.Min(16, items.Count) && remainingNodes > 0; index++)
                {
                    remainingNodes--;
                    children.Add(Freeze(items[index], depth + 1));
                }

                return
                    new(
                        null,
                        $"unmaterialized list ({items.Count} items; bounded structural preview)",
                        [.. children],
                        items.Count);
            }

            return new(null, "(unevaluated; not forced)");
        }
        // Do not force lazy values: previews and already materialized values are enough to diagnose a loop.
        var frames =
            loadStack().Take(Options.StackTraceDepth).Select(
                frame =>
                new CapturedFrame(
                    frame.Expression,
                    frame.InstructionPointer,
                    Options.IncludeInputs && frame.Input is { } input
                    ?
                    [.. input.Parameters.ParamsPaths.Select(path => path.ToArray())]
                    :
                    null,
                    Options.IncludeInputs ? frame.Input?.Arguments.Select(value => Freeze(value)).ToArray() : null,
                    Options.IncludeLocals ? frame.LoadLocals?.Invoke().Select(value => Freeze(value)).ToArray() : null,
                    frame.Instructions,
                    frame.FrameIndex,
                    frame.FrameInstructionCount,
                    frame.FrameLoopIterationCount)).ToArray();

        var sample = new CapturedSample(GetSummary(), frames);

        if (_samples.Count >= Options.MaxSamples)
        {
            _samples.Dequeue();
            _droppedSamples++;
        }

        _samples.Enqueue(sample);

        if (OnSample is { } report)
        {
            // Do not keep a strong value-hash cache alive after samples have been evicted.
            var hashes = new ConcurrentPineValueHashCache();
            report(MaterializeSample(sample, value => Convert.ToHexStringLower(hashes.GetHash(value).Span)));
        }
    }

    private string CompiledFrameId(Expression expression, StackFrameInstructions instructions)
    {
        var key = (expression, instructions);

        if (!_compiledFrameIds.TryGetValue(key, out var id))
        {
            // The suffix distinguishes bodies with the same source/constraint but different compilation settings.
            id =
                StackInstructionTraceRenderer.RenderStackFrameIdentifier(expression, instructions) +
                "-" + _compiledFrameIds.Count;

            _compiledFrameIds.Add(key, id);
        }

        return id;
    }

    private ElmTestProfileSample MaterializeSample(CapturedSample sample, Func<PineValue, string> store)
    {
        ElmTestProfileValue Describe(CapturedValue value) =>
            new(value.Value is { } evaluated ? store(evaluated) : null, value.Preview)
            {
                Items = value.Items?.Select(Describe).ToArray(),
                ItemCount = value.ItemCount,
            };

        return
            new(
                sample.Summary,
                [
                .. sample.Frames.Select(
                    frame => new ElmTestProfileFrame(
                        store(ExpressionEncoding.EncodeExpressionAsValue(frame.Expression)),
                        frame.Pointer,
                        frame.Paths,
                        frame.Inputs?.Select(Describe).ToArray(),
                        frame.Locals?.Select(Describe).ToArray())
                    {
                        Declarations = [.. GetEntry(frame.Expression).Names.Order(StringComparer.Ordinal)],
                        FrameIndex = frame.FrameIndex,
                        InstructionCount = frame.InstructionCount,
                        LoopIterationCount = frame.LoopIterationCount,
                        CompiledFrameId =
                        frame.Instructions is { } instructions
                        ?
                        CompiledFrameId(frame.Expression, instructions)
                        :
                        null,
                    })
                ]);
    }

    /// <summary>Materializes a complete content-addressed report after serial diagnostic recording has stopped.</summary>
    public ElmTestProfileReport GetReport()
    {
        var hashes = new ConcurrentPineValueHashCache();
        var values = new Dictionary<string, ElmTestProfileValueNode>(StringComparer.Ordinal);

        string Store(PineValue value)
        {
            var rootHash = Convert.ToHexStringLower(hashes.GetHash(value).Span);
            var pending = new Stack<PineValue>();
            pending.Push(value);

            while (pending.TryPop(out var node))
            {
                var hash = Convert.ToHexStringLower(hashes.GetHash(node).Span);

                if (values.ContainsKey(hash))
                    continue;

                switch (node)
                {
                    case PineValue.BlobValue blob:
                        values.Add(hash, new(Convert.ToBase64String(blob.Bytes.Span), null));
                        break;

                    case PineValue.ListValue list:
                        var items = new string[list.Items.Length];

                        for (var index = 0; index < items.Length; index++)
                        {
                            var child = list.Items.Span[index];
                            items[index] = Convert.ToHexStringLower(hashes.GetHash(child).Span);
                            pending.Push(child);
                        }

                        values.Add(hash, new(null, items));
                        break;

                    default:
                        throw new NotImplementedException(
                            "GetReport does not handle Pine value variant: " + node.GetType().Name);
                }
            }

            return rootHash;
        }

        string ExpressionHash(Expression expression) => Store(ExpressionEncoding.EncodeExpressionAsValue(expression));

        var expressions =
            _entries.Values.Where(entry => entry._invocations > 0 || entry._instructions > 0 || entry._loops > 0)
            .Select(
                entry => new ElmTestExpressionProfile(
                    ExpressionHash(entry.Expression),
                    [.. entry.Names.Order(StringComparer.Ordinal)],
                    entry._invocations,
                    entry._instructions,
                    entry._loops,
                    DescribeExpression(entry.Expression)))
            .OrderBy(entry => entry.Hash, StringComparer.Ordinal).ToArray();

        var samples = _samples.Select(sample => MaterializeSample(sample, Store)).ToArray();

        var loops =
            _loopSites.Select(
                pair => new ElmTestLoopProfile(
                    ExpressionHash(pair.Key.Expression),
                    CompiledFrameId(pair.Key.Expression, pair.Key.Instructions),
                    pair.Key.Pointer,
                    pair.Value))
            .OrderByDescending(site => site.Iterations).ThenBy(site => site.CompiledFrameId, StringComparer.Ordinal)
            .ThenBy(site => site.InstructionPointer).ToArray();

        return
            new(2, Options, SelectedTest, GetSummary(), samples, expressions, values)
            {
                Metadata = new Dictionary<string, string>(Metadata, StringComparer.Ordinal),
                DroppedSamples = _droppedSamples,
                LoopSites = loops,
                CompiledFrames =
                _compiledFrameIds.ToDictionary(
                    pair => pair.Value,
                    pair => StackInstructionTraceRenderer.RenderStackFrameInstructions(pair.Key.Instructions),
                    StringComparer.Ordinal),
            };
    }

    private static string DescribeExpression(Expression expression)
    {
        var nodes = 200;

        string Render(Expression current)
        {
            if (--nodes < 0)
                return "...";

            return current switch
            {
                Expression.Environment => "environment",
                Expression.Litral literal => DescribeValue(literal.Value),

                Expression.List list =>
                "[" + string.Join(", ", list.Items.Take(50).Select(Render)) +
                (list.Items.Count > 50 ? ", ..." : "") + "]",

                Expression.Builtin builtin => builtin.Function + "(" + Render(builtin.Input) + ")",
                Expression.Eval eval => "eval(" + Render(eval.Encoded) + ", " + Render(eval.Environment) + ")",

                Expression.Conditional conditional =>
                "if " + Render(conditional.Condition) +
                " then " + Render(conditional.TrueBranch) + " else " + Render(conditional.FalseBranch),

                _ =>
                throw new NotImplementedException(
                    nameof(DescribeExpression) +
                    " does not handle expression variant: " + current.GetType().Name),
            };
        }

        return Render(expression);
    }

    private static string DescribeValue(PineValue value)
    {
        if (value is PineValue.BlobValue blob)
        {
            if (IntegerEncoding.ParseSignedIntegerStrict(value).IsOkOrNullable() is { } integer)
                return "integer " + integer;

            return
                $"blob ({blob.Bytes.Length} bytes; " +
                Convert.ToHexStringLower(blob.Bytes.Span[..Math.Min(32, blob.Bytes.Length)]) +
                (blob.Bytes.Length > 32 ? "...)" : ")");
        }

        if (value is PineValue.ListValue list)
            return $"list ({list.Items.Length} items)";

        throw new NotImplementedException("DescribeValue does not handle Pine value variant: " + value.GetType().Name);
    }

    private string DescribeStopReason(EvaluationErrorReason? reason) =>
        reason switch
        {
            EvaluationErrorReason.QuotaExhausted quota =>
            $"{quota.QuotaKind} budget exhausted: command limit " +
            CommandLineInterface.FormatIntegerForDisplay(
                (quota.QuotaKind switch
                {
                    EvaluationQuotaKind.InvocationCount => Options.InvocationBudget,
                    EvaluationQuotaKind.LoopIterationCount => Options.LoopBudget,
                    EvaluationQuotaKind.StackDepth => Options.StackDepthLimit,

                    _ =>
                    throw new NotImplementedException("DescribeStopReason does not handle quota kind: " + quota.QuotaKind),
                }) ?? quota.Limit) +
            $" (VM remaining limit {CommandLineInterface.FormatIntegerForDisplay(quota.Limit)}).",

            EvaluationErrorReason.CancellationRequested => _stopReason ?? "Time budget exhausted.",
            EvaluationErrorReason.ParseExpressionFailed parse => "Evaluation failed: " + parse.ParseError,
            EvaluationErrorReason.InstructionPointerOutOfBounds => "Evaluation instruction pointer out of bounds.",
            null => "Evaluation stopped.",

            _ =>
            throw new NotImplementedException(
                "DescribeStopReason does not handle evaluation reason: " + reason.GetType().Name),
        };

    /// <summary>Releases the command-wide cancellation timer.</summary>
    public void Dispose() => _cancellation.Dispose();

    private sealed class EvaluationContext : ICancellablePineVM
    {
        private readonly ElmTestInstrumentation _owner;

        private readonly InstrumentedVM _vm;

        private readonly Dictionary<long, (Entry Entry, long Instructions, long Loops)> _active = [];

        private PerformanceCounters _observed;

        public EvaluationContext(ElmTestInstrumentation owner, IInvocationCacheAccess cache, PineVMSharedCaches caches)
        {
            _owner = owner;

            _vm =
                InstrumentedVM.CreateCustom(
                    evalCache: null,
                    evaluationConfigDefault: null,
                    reportFunctionApplication: null,
                    compilationEnvClasses: null,
                    disableReductionInCompilation: owner.Options.DisableReduction,
                    selectPrecompiled: null,
                    skipInlineForExpression: _ => owner.Options.DisableInlining,
                    enableTailRecursionOptimization: !owner.Options.DisableTailRecursion,
                    parseCache: caches.ParsedExpressions,
                    precompiledLeaves: owner.Options.DisablePrecompiledLeaves
                    ?
                    new Dictionary<PineValue, PrecompiledLeaf>()
                    :
                    owner._precompiledLeaves ??
                    owner._precompiledLeavesProvider?.Invoke() ?? IntermediateVM.SetupVM.DefaultPrecompiledLeaves,
                    reportEnterPrecompiledLeaf: null,
                    reportExitPrecompiledLeaf: owner.RecordDiagnostics ? NativeExit : null,
                    optimizationParametersSerial: null,
                    cacheFileStore: null,
                    invocationCache: owner.Options.DisableInvocationCache ? null : cache,
                    tryGetExpressionCompilation: caches.ExpressionCompilations.TryGet,
                    getOrAddExpressionCompilation: caches.ExpressionCompilations.GetOrAdd,
                    expressionEncodingCache: caches.EncodedExpressions,
                    reducedExpressionCache: caches.ReducedExpressions,
                    disableGenericApplicationChainConsolidation: owner.Options.DisableApplicationFastPaths,
                    disableDirectContinueForSimpleEval: owner.Options.DisableApplicationFastPaths,
                    disableDirectEvalForSimpleTemplate: owner.Options.DisableApplicationFastPaths,
                    disableDirectInvocation: owner.Options.DisableApplicationFastPaths,
                    reportEvaluationEvent: Event);
        }

        private void NativeExit(PineValue encoded, PineValueInProcess input, PineValueInProcess? result)
        {
            if (result is not null && _vm.ParseCache.ParseExpression(encoded).IsOkOrNull() is { } expression)
                _owner.GetEntry(expression)._invocations++;
        }

        private void Event(in EvaluationEvent evaluationEvent)
        {
            if (!_owner.RecordDiagnostics)
            {
                _owner.ObserveWork(evaluationEvent.LoadCounters(), ref _observed);

                if (evaluationEvent.Kind is EvaluationEventKind.EvaluationStopped &&
                    evaluationEvent.StopReason is EvaluationErrorReason.QuotaExhausted or EvaluationErrorReason.CancellationRequested)
                {
                    _owner.AnnounceStop(
                        _owner._stopReason ?? _owner.DescribeStopReason(evaluationEvent.StopReason));
                }

                return;
            }

            var entry = _owner.GetEntry(evaluationEvent.Expression);
            _owner._loadCurrentCounters = evaluationEvent.LoadCounters;

            switch (evaluationEvent.Kind)
            {
                case EvaluationEventKind.FrameEntered:
                    entry._invocations++;
                    _active[evaluationEvent.FrameIndex] = (entry, 0, 0);
                    break;

                case EvaluationEventKind.FrameExited:
                    AttributeWork(
                        evaluationEvent.FrameIndex,
                        entry,
                        evaluationEvent.FrameInstructionCount,
                        evaluationEvent.FrameLoopIterationCount);

                    _active.Remove(evaluationEvent.FrameIndex);
                    break;

                case EvaluationEventKind.BackwardJump:
                    AttributeWork(
                        evaluationEvent.FrameIndex,
                        entry,
                        evaluationEvent.FrameInstructionCount,
                        evaluationEvent.FrameLoopIterationCount);

                    if (evaluationEvent.Instructions is { } instructions)
                    {
                        var site = (evaluationEvent.Expression, instructions, evaluationEvent.InstructionPointer);
                        _owner._loopSites.TryGetValue(site, out var count);
                        _owner._loopSites[site] = count + 1;
                    }

                    break;

                case EvaluationEventKind.EvaluationStopped:
                    _owner.AnnounceStop(_owner.DescribeStopReason(evaluationEvent.StopReason));

                    foreach (var frame in evaluationEvent.LoadStackTrace())
                        if (frame.FrameIndex is { } index && _active.ContainsKey(index))
                        {
                            AttributeWork(
                                index,
                                _owner.GetEntry(frame.Expression),
                                frame.FrameInstructionCount,
                                frame.FrameLoopIterationCount);
                        }

                    _owner.Capture(evaluationEvent.LoadStackTrace);
                    break;

                default:
                    throw new NotImplementedException("Event does not handle event kind: " + evaluationEvent.Kind);
            }

            var now = _owner._elapsed.Elapsed.Ticks;

            if (evaluationEvent.Kind is not EvaluationEventKind.EvaluationStopped &&
                _owner.Options.SnapshotInterval > TimeSpan.Zero && now >= _owner._nextSampleTicks)
            {
                _owner._nextSampleTicks =
                    now > long.MaxValue - _owner.Options.SnapshotInterval.Ticks
                    ?
                    long.MaxValue
                    :
                    now + _owner.Options.SnapshotInterval.Ticks;

                _owner.Capture(evaluationEvent.LoadStackTrace);

                if (_owner.OnSample is null)
                    _owner.OnProgress?.Invoke(_owner.GetSummary());
            }
        }

        private void AttributeWork(long frameIndex, Entry entry, long instructions, long loops)
        {
            _active.TryGetValue(frameIndex, out var previous);
            entry._instructions += instructions - previous.Instructions;
            entry._loops += loops - previous.Loops;
            _active[frameIndex] = (entry, instructions, loops);
        }

        public Result<string, PineValue> EvaluateExpression(Expression expression, PineValue environment) =>
            EvaluateExpression(expression, environment, default);

        public Result<string, PineValue> EvaluateExpression(
            Expression expression,
            PineValue environment,
            CancellationToken token)
        {
            _owner.ThrowIfStopped();
            _active.Clear();
            _observed = default;
            using var linked = CancellationTokenSource.CreateLinkedTokenSource(_owner.CancellationToken, token);
            PerformanceCounters completed;

            lock (_owner._budgetLock)
            {
                completed = _owner._completed;
            }

            var config =
                new InstrumentedVM.EvaluationConfig(
                    _owner.Options.InvocationBudget is { } inv
                    ?
                    (int)Math.Max(0, inv - completed.InvocationCount)
                    :
                    null,
                    _owner.Options.LoopBudget is { } loop
                    ?
                    (int)Math.Max(0, loop - completed.LoopIterationCount)
                    :
                    null,
                    _owner.Options.StackDepthLimit);

            var result = _vm.EvaluateExpressionOnCustomStack(expression, environment, config, linked.Token);

            _active.Clear();
            var counters = result.IsOkOrNull()?.Counters ?? result.IsErrOrNull()!.Counters;

            if (_owner.RecordDiagnostics)
                _owner._completed = PerformanceCounters.Add(_owner._completed, counters);

            else
                _owner.ObserveWork(counters, ref _observed);

            _owner._loadCurrentCounters = null;

            if (result.IsErrOrNull() is { } error)
            {
                if (error.Reason is EvaluationErrorReason.QuotaExhausted or EvaluationErrorReason.CancellationRequested)
                {
                    if (_owner._stopReason is null)
                    {
                        _owner.AnnounceStop(
                            error.Reason is EvaluationErrorReason.CancellationRequested
                            ?
                            "Cancellation requested before VM execution."
                            :
                            error.ToString());
                    }

                    throw new ElmTestInstrumentationStoppedException(_owner._stopReason!);
                }

                return error.ToString();
            }

            _owner.ThrowIfStopped();
            return result.Map(report => report.ReturnValue.Evaluate()).MapError(error => error.ToString());
        }
    }
}
