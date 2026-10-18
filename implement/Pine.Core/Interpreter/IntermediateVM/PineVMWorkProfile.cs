using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text.Json;
using System.Threading;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Direct work-site counters and bounded constructor-return inferences, not a global heap census.
/// Serial, opt-in instrumentation;
/// snapshots contain only numeric measurements and caller-provided scenario labels.
/// </summary>
public sealed class PineVMWorkProfile : IDisposable
{
    /// <summary>Sites covered by this profiler; allocations elsewhere are not counted as objects.</summary>
    public enum Metric
    {
        EvalDirectCalls, EvalDirectEntries,
        /// <summary>Private evaluator invocations rejected before accepting a body entry.</summary>
        EvalDirectBudgetRejectedEntries,
        EvalDirectRecursiveEntries, EvalDirectLiteral, EvalDirectList, EvalDirectEnvironment,
        EvalDirectConditional, EvalDirectBuiltin, EvalDirectStringTag, EvalDirectUnsupported,
        EvalDirectListArrays, EvalDirectListItems, LiteralWrapperCalls,
        /// <summary>Successful invocation-local pure constructor result reuses; other direct counters remain physical.</summary>
        DirectConstructorMemoHits,
        /// <summary>Literal body entries that returned an invocation-local memoized result without allocating a wrapper.</summary>
        DirectConstructorMemoLiteralHits,
        /// <summary>List body entries that returned an invocation-local memoized result without constructing a list.</summary>
        DirectConstructorMemoListHits,
        /// <summary>Environment body entries that returned an invocation-local memoized result.</summary>
        DirectConstructorMemoEnvironmentHits,
        /// <summary>Logical instructions in successfully completed constructor invocations, including memo replay.</summary>
        ConstructorCompletedLogicalInstructions,
        /// <summary>Constructor invocations rejected before beginning by the optional invocation budget.</summary>
        ConstructorBudgetRejectedInvocations,
        ConstructorEntries, ConstructorDistinctReferences, ConstructorRepeatedReferences,
        ConstructorRepeatedLists, ConstructorRepeatedListItems, ConstructorCoverageCapEvents,
        ConstructorEligibleInvocations, ConstructorIneligibleInvocations,
        ConstructorInvocationsStarted, ConstructorInvocationsCompleted, ConstructorInvocationsIncomplete,
        ConstructorRepeatFrontiers, ConstructorFrontierAvoidableListArrays, ConstructorFrontierAvoidableListItems,
        ConstructorFrontierAvoidableWrappers, ConstructorFrontierAvoidableLiteralWrappers, ConstructorFrontierAvoidableListWrappers,
        ConstructorFrontierAvoidableNodeEntries,
        ConstructorCompletedRepeatFrontiers, ConstructorCompletedFrontierAvoidableNodeEntries,
        ConstructorCompletedFrontierAvoidableListArrays, ConstructorCompletedFrontierAvoidableListItems,
        ConstructorCompletedFrontierAvoidableWrappers, ConstructorCompletedFrontierAvoidableLiteralWrappers,
        ConstructorCompletedFrontierAvoidableListWrappers,
        /// <summary>Completed invocations with proven fresh wrapper/array reachability from the returned root.</summary>
        ConstructorCompletedRootRetentionProvenInvocations,
        /// <summary>Completed invocations outside the conservative constructor-root retention proof.</summary>
        ConstructorCompletedRootRetentionUnknownInvocations,
        /// <summary>Completed invocations with proven root retention and exhaustive reference opportunity coverage.</summary>
        ConstructorCompletedRootAvoidableOpportunityProvenInvocations,
        /// <summary>Completed invocations without a complete root-retained sharing-opportunity measurement.</summary>
        ConstructorCompletedRootAvoidableOpportunityUnknownInvocations,
        /// <summary>Sum across distinct return lifetimes; never a simultaneous GC-live object count.</summary>
        ConstructorCompletedRootRetainedFreshObjectExposures,
        /// <summary>Gross sharing opportunity over return lifetimes with complete identity coverage, excluding memo overhead.</summary>
        ConstructorCompletedRootRetainedAvoidableObjectExposures,
        TrackingSets, TrackingReferences, TrackingAllocatedBytes,
        CreateCalls, CreateReuses, WrapperAllocations, CreateListCalls, CreateListReuses,
        InProcessChildArrays, InProcessChildItems,
        EvaluateCalls, EvaluateCached, EvaluateUncached, ConcreteChildArrays, ConcreteChildItems,
        MaterializedListCalls, MaterializedListRootReturn, MaterializedListInvocationCache,
        MaterializedListRecognition, MaterializedListOther,
        PineListCalls, PineListPreallocationReuses, PineListPostallocationReuses, ListValueConstructorAllocations,
        DeferredProducerHits, DeferredValuesCreated, DeferredMaterializations, DeferredMaterializedEnvironmentSlots,
        DeferredMaterializedRootReturn, DeferredMaterializedInvocationCache, DeferredMaterializedRecognition, DeferredMaterializedOther,
        TemplateEnvironmentStagesCollected, RecipeBindings, RecipeBindCacheHits, RecipeResolveCacheHits, RecipeChildArrays,
        RecipeInstantiateCalls, RecipeInstantiateSharedCalls, RecipeInstantiateSharedMemoHits,
        RecipeInstantiateSharedChildArrays, RecipeInstantiateSharedChildItems, RecipeConstantChildArrays,
        RecognizerStages, RecognizerConstructorStages, RecognizerRecipeChildArrays, RecognizerRecipeChildItems,
        RecognizerTerminalParseFailures, RecognizerRejectedByBudget, RecognizerMalformedOrUnsupportedFailures,
        TemplateCacheProbes, TemplateCacheHits, TemplateCacheMisses, TemplateCacheNullResults,
        TemplateCacheFactoryRuns, RecognizerVisits, RecognizerBudgetFailures,
        Count
    }

    internal enum MaterializationCause
    {
        Other, RootReturn, InvocationCache, Recognition
    }

    [ThreadStatic]
    internal static PineVMWorkProfile? Current;

    private readonly long[] _counts = new long[(int)Metric.Count];

    private readonly Stopwatch _wall = Stopwatch.StartNew();

    private readonly long _allocated = GC.GetTotalAllocatedBytes(precise: true);

    private readonly TimeSpan _pause = GC.GetTotalPauseDuration();

    private readonly int[] _collections = [GC.CollectionCount(0), GC.CollectionCount(1), GC.CollectionCount(2)];

    private readonly Process _process = Process.GetCurrentProcess();

    private readonly TimeSpan _cpu;

    private readonly TextWriter? _writer;

    private readonly bool _ownsWriter;

    private CancellationToken _token;

    private long _nextSnapshotTicks;

    private long _checkpointEntries;

    private MaterializationCause _cause;

    private PineVMExpressionTemplatePlanCache? _cache;

    private string _phase = "initialization";

    private string _scenario = "";

    private bool _disposed;

    private int _positiveCacheEntries;

    private int _negativeCacheEntries;

    private bool _hasCacheSnapshot;

    private long _snapshotAllocatedBytes;

    private long _nextMemoryCheckTicks;

    private ConstructorOpportunities _constructorOpportunities;

    private long _nextConstructorBoundarySnapshotEntries = 4096;

    private int _constructorBoundarySnapshots;

    private long _completedConstructorPrivateCallTicks;

    private long _incompleteConstructorPrivateCallTicks;

    private double _completedConstructorPrivateCallGcPauseMilliseconds;

    private double _incompleteConstructorPrivateCallGcPauseMilliseconds;

    private long _completedConstructorPrivateCallWindows;

    private long _incompleteConstructorPrivateCallWindows;

    private long _constructorPrivateCallWindowsExcludedNested;

    private long _constructorPrivateCallWindowsExcludedMissing;

    internal double ConstructorCompletedPrivateCallWallMilliseconds =>
        _completedConstructorPrivateCallTicks * 1000.0 / Stopwatch.Frequency;

    internal double ConstructorIncompletePrivateCallWallMilliseconds =>
        _incompleteConstructorPrivateCallTicks * 1000.0 / Stopwatch.Frequency;

    internal bool ConstructorPrivateCallWindowCoverageComplete =>
        _completedConstructorPrivateCallWindows + _incompleteConstructorPrivateCallWindows ==
        this[Metric.ConstructorInvocationsStarted];

    internal struct ConstructorOpportunities
    {
        public ConstructorBoundarySnapshot BoundaryStart;

        public long RepeatFrontiers;

        public long NodeEntries;

        public long ListArrays;

        public long ListItems;

        public long Wrappers;

        public long LiteralWrappers;

        public long ListWrappers;
    }

    /// <summary>
    /// Numeric, invocation-local observations at the direct constructor return boundary.
    /// Root retention covers only fresh wrappers and direct child arrays, not preexisting
    /// literal/environment descendants, later ownership, or the process's GC-live object count.
    /// Allocation/time/GC windows bracket the private evaluator call, including observer work
    /// inside that call but excluding constructor tracking-set setup. GC observations are process-wide.
    /// </summary>
    internal readonly record struct ConstructorBoundarySnapshot
    {
        public long InvocationOrdinal { get; init; }

        public bool Completed { get; init; }

        public bool? ResultAlreadyMaterialized { get; init; }

        public long StartStopwatchTimestamp { get; init; }

        public long EndStopwatchTimestamp { get; init; }

        public long StartAcceptedBodyEntries { get; init; }

        public long EndAcceptedBodyEntries { get; init; }

        public long AcceptedBodyEntries { get; init; }

        public long? LogicalInstructionCount { get; init; }

        public long? LogicalBuildListCount { get; init; }

        public long? LogicalBuildListItemCount { get; init; }

        public long BudgetRejectedBodyEntries { get; init; }

        public long OuterEvalDirectCalls { get; init; }

        public long PrivateEvaluatorMethodInvocations => AcceptedBodyEntries + BudgetRejectedBodyEntries;

        public long LiteralEntries { get; init; }

        public long ListEntries { get; init; }

        public long EnvironmentEntries { get; init; }

        public long MemoHits { get; init; }

        public long LiteralMemoHits { get; init; }

        public long ListMemoHits { get; init; }

        public long EnvironmentMemoHits { get; init; }

        public long LabelEntries { get; init; }

        public long ConditionalEntries { get; init; }

        public long BuiltinEntries { get; init; }

        public long UnsupportedEntries { get; init; }

        public long FreshWrappers { get; init; }

        public long FreshLiteralWrappers { get; init; }

        public long FreshListWrappers { get; init; }

        public long ListArrays { get; init; }

        public long ListReferenceSlots { get; init; }

        public long InProcessChildArrays { get; init; }

        public long InProcessChildSlots { get; init; }

        public long CreateCalls { get; init; }

        public long LiteralWrapperCalls { get; init; }

        public long CreateReuses { get; init; }

        public long CreateListCalls { get; init; }

        public long CreateListReuses { get; init; }

        public long EvaluateCalls { get; init; }

        public long CoverageCapEvents { get; init; }

        /// <summary>Tracked distinct reference identities; exhaustive only with complete opportunity coverage.</summary>
        public long UniqueReferences { get; init; }

        public int ReferenceCapacity { get; init; }

        public long TrackingAllocatedBytes { get; init; }

        public long SetupTrackingAllocatedBytes { get; init; }

        public long SnapshotAllocatedBytes { get; init; }

        public long ThreadAllocatedBytes { get; init; }

        public bool PrivateCallWindowCaptured { get; init; }

        public double WallMilliseconds { get; init; }

        public double GcPauseMilliseconds { get; init; }

        public int Gen0Collections { get; init; }

        public int Gen1Collections { get; init; }

        public int Gen2Collections { get; init; }

        public long RepeatFrontiers { get; init; }

        public long AvoidableDescendantBodyEntries { get; init; }

        public long AvoidableListArrays { get; init; }

        public long AvoidableListReferenceSlots { get; init; }

        public long AvoidableWrappers { get; init; }

        public long AvoidableLiteralWrappers { get; init; }

        public long AvoidableListWrappers { get; init; }

        public bool ReferenceTrackingEnabled { get; init; }

        public bool OpportunityCoverageComplete => Completed && ReferenceTrackingEnabled && CoverageCapEvents is 0;

        public bool PureListLiteralEnvironmentGrammar { get; init; }

        public bool NoNestedConstructorInvocations { get; init; }

        public bool AllocationConservationVerified { get; init; }

        public bool RootOwnedFreshSubsetKnown =>
            Completed && PureListLiteralEnvironmentGrammar && NoNestedConstructorInvocations &&
            AllocationConservationVerified &&
            OuterEvalDirectCalls is 1 &&
            EvaluateCalls is 0 &&
            PrivateCallWindowCaptured;

        public long? RootOwnedFreshLiteralWrappers => RootOwnedFreshSubsetKnown ? FreshLiteralWrappers : null;

        public long? RootOwnedFreshListWrappers => RootOwnedFreshSubsetKnown ? FreshListWrappers : null;

        public long? RootOwnedFreshListArrays => RootOwnedFreshSubsetKnown ? FreshListWrappers : null;

        public long? UnretainedEmptyListArrays => RootOwnedFreshSubsetKnown ? CreateListReuses : null;

        public bool RootRetainedFreshObjectsKnown => RootOwnedFreshSubsetKnown && CreateListReuses is 0;

        public long? RootRetainedFreshObjects => RootRetainedFreshObjectsKnown ? FreshWrappers + ListArrays : null;

        public long? RootRetainedReferenceSlots => RootRetainedFreshObjectsKnown ? ListReferenceSlots : null;

        public long? RootRetainedAvoidableObjects =>
            RootRetainedFreshObjectsKnown && OpportunityCoverageComplete
            ?
            AvoidableWrappers + AvoidableListArrays
            :
            null;

        public long? RequiredFirstUniqueWrappers =>
            RootRetainedFreshObjectsKnown && OpportunityCoverageComplete ? FreshWrappers - AvoidableWrappers : null;

        public long? RequiredFirstUniqueLiteralWrappers =>
            RootRetainedFreshObjectsKnown && OpportunityCoverageComplete
            ?
            FreshLiteralWrappers - AvoidableLiteralWrappers
            :
            null;

        public long? RequiredFirstUniqueListWrappers =>
            RootRetainedFreshObjectsKnown && OpportunityCoverageComplete
            ?
            FreshListWrappers - AvoidableListWrappers
            :
            null;

        public long? RequiredFirstUniqueListArrays =>
            RootRetainedFreshObjectsKnown && OpportunityCoverageComplete ? ListArrays - AvoidableListArrays : null;

        public long? RequiredFirstUniqueListReferenceSlots =>
            RootRetainedFreshObjectsKnown && OpportunityCoverageComplete
            ?
            ListReferenceSlots - AvoidableListReferenceSlots
            :
            null;

        public long? RootOwnedObjectsBeforeMemo => RootRetainedFreshObjects;

        public long? CounterfactualRootOwnedObjectsAfterMemo =>
            RequiredFirstUniqueWrappers + RequiredFirstUniqueListArrays;
    }

    /// <summary>Last constructor attempt, including incomplete attempts; no output graph is retained.</summary>
    internal ConstructorBoundarySnapshot? LastConstructorInvocation { get; private set; }

    /// <summary>One completed invocation, not the sum or an independently combined set of maxima.</summary>
    internal ConstructorBoundarySnapshot? LargestCompletedConstructorByAcceptedBodyEntries { get; private set; }

    internal ConstructorBoundarySnapshot? LargestCompletedConstructorByLogicalInstructions { get; private set; }

    /// <summary>Largest proven simultaneous fresh wrapper/array graph at one completed return.</summary>
    internal ConstructorBoundarySnapshot? LargestCompletedConstructorByRootRetainedFreshObjects { get; private set; }

    /// <summary>One completed graph's gross sharing opportunity; memo overhead is not subtracted.</summary>
    internal ConstructorBoundarySnapshot? LargestCompletedConstructorByRootRetainedAvoidableObjects { get; private set; }

    /// <summary>Maximum identities retained in each constructor-only direct invocation.</summary>
    public int ReferenceCapacity { get; }

    /// <summary>Optional explicit process working-set guard, checked only at coarse checkpoints.</summary>
    public long? WorkingSetLimitBytes { get; }

    /// <summary>Optional command-wide maximum accepted direct body entries, including invocation roots.</summary>
    public long? DirectVisitBudget { get; }

    /// <summary>Optional command-wide maximum constructor invocations, checked before starting the next invocation.</summary>
    public long? ConstructorInvocationBudget { get; }

    /// <summary>Enables normal constructor sharing; false selects a diagnostic baseline only in this profile.</summary>
    public bool PreserveConstructorSharing { get; }

    /// <summary>Explicit profiling guard stop, separate from the VM's ordinary evaluation quotas.</summary>
    public string? StopReason { get; private set; }

    /// <summary>Creates counters; writing snapshots is optional and never retains Pine values.</summary>
    public PineVMWorkProfile(
        TextWriter? writer = null,
        int referenceCapacity = 4096,
        long? workingSetLimitBytes = null,
        long? directVisitBudget = null,
        long? constructorInvocationBudget = null,
        bool preserveConstructorSharing = true)
    {
        if (referenceCapacity < 0 || workingSetLimitBytes is <= 0)
            throw new ArgumentOutOfRangeException(nameof(referenceCapacity));

        if (directVisitBudget is <= 0)
            throw new ArgumentOutOfRangeException(nameof(directVisitBudget));

        if (constructorInvocationBudget is <= 0)
            throw new ArgumentOutOfRangeException(nameof(constructorInvocationBudget));

        _writer = writer;
        ReferenceCapacity = referenceCapacity;
        WorkingSetLimitBytes = workingSetLimitBytes;
        DirectVisitBudget = directVisitBudget;
        ConstructorInvocationBudget = constructorInvocationBudget;
        PreserveConstructorSharing = preserveConstructorSharing;
        _cpu = _process.TotalProcessorTime;
    }

    private PineVMWorkProfile(
        string path,
        long? workingSetLimitBytes,
        long? directVisitBudget,
        int referenceCapacity,
        long? constructorInvocationBudget,
        bool preserveConstructorSharing)
        : this(new StreamWriter(Path.GetFullPath(path), append: false),
              referenceCapacity: referenceCapacity, workingSetLimitBytes: workingSetLimitBytes,
              directVisitBudget: directVisitBudget, constructorInvocationBudget: constructorInvocationBudget,
              preserveConstructorSharing: preserveConstructorSharing)
    {
        _ownsWriter = true;
        WriteSnapshot("started");
    }

    /// <summary>Opens a flushed NDJSON stream, including an initial snapshot before evaluation starts.</summary>
    public static PineVMWorkProfile Open(
        string path,
        long? workingSetLimitBytes = null,
        long? directVisitBudget = null,
        int referenceCapacity = 4096,
        long? constructorInvocationBudget = null,
        bool preserveConstructorSharing = true)
    {
        if (workingSetLimitBytes is <= 0)
            throw new ArgumentOutOfRangeException(nameof(workingSetLimitBytes));

        if (directVisitBudget is <= 0)
            throw new ArgumentOutOfRangeException(nameof(directVisitBudget));

        if (referenceCapacity < 0)
            throw new ArgumentOutOfRangeException(nameof(referenceCapacity));

        if (constructorInvocationBudget is <= 0)
            throw new ArgumentOutOfRangeException(nameof(constructorInvocationBudget));

        return
            new(
                path,
                workingSetLimitBytes,
                directVisitBudget,
                referenceCapacity,
                constructorInvocationBudget,
                preserveConstructorSharing);
    }

    /// <summary>Reads one direct-site counter.</summary>
    public long this[Metric metric] => _counts[(int)metric];

    /// <summary>
    /// Sum of disjoint covered runtime allocation sites, excluding profiling metadata.
    /// Includes wrappers, child/recipe arrays, bound recipes, and explicit concrete-list constructors.
    /// This is neither a global heap object count nor a retained-object count.
    /// </summary>
    public long KnownAllocationObjects =>
        this[Metric.WrapperAllocations] +
        this[Metric.InProcessChildArrays] +
        this[Metric.ConcreteChildArrays] +
        this[Metric.RecipeChildArrays] +
        this[Metric.RecipeConstantChildArrays] +
        this[Metric.RecognizerRecipeChildArrays] +
        this[Metric.RecipeBindings] +
        this[Metric.ListValueConstructorAllocations];

    internal void Count(Metric metric, long amount = 1) => _counts[(int)metric] += amount;

    internal void DirectConstructorMemoHit(Expression expression)
    {
        Count(Metric.DirectConstructorMemoHits);

        switch (expression)
        {
            case Expression.Litral:
                Count(Metric.DirectConstructorMemoLiteralHits);
                break;

            case Expression.List:
                Count(Metric.DirectConstructorMemoListHits);
                break;

            case Expression.Environment:
                Count(Metric.DirectConstructorMemoEnvironmentHits);
                break;

            case Expression.Label:
            case Expression.Conditional:
            case Expression.Builtin:
            case Expression.Eval:
                throw new InvalidOperationException("Only pure constructor nodes can have memo hits.");

            default:
                throw new NotImplementedException(
                    "DirectConstructorMemoHit does not handle expression variant: " + expression.GetType().Name);
        }
    }

    internal ConstructorOpportunities BeginConstructorInvocation()
    {
        if (ConstructorInvocationBudget is { } budget && this[Metric.ConstructorInvocationsStarted] >= budget)
        {
            Count(Metric.ConstructorBudgetRejectedInvocations);
            StopReason = "Work-profile constructor invocation budget exhausted.";
            WriteSnapshot("constructor-invocation-limit");
            throw new OperationCanceledException(StopReason);
        }

        var previous = _constructorOpportunities;
        _constructorOpportunities = default;
        Count(Metric.ConstructorInvocationsStarted);
        _constructorOpportunities.BoundaryStart = CaptureConstructorBoundary();
        return previous;
    }

    internal void BeginConstructorBody()
    {
        var start = _constructorOpportunities.BoundaryStart;

        _constructorOpportunities.BoundaryStart =
            start with
            {
                SetupTrackingAllocatedBytes = this[Metric.TrackingAllocatedBytes] - start.TrackingAllocatedBytes,
                TrackingAllocatedBytes = this[Metric.TrackingAllocatedBytes],
                EndStopwatchTimestamp = Stopwatch.GetTimestamp(),
                ThreadAllocatedBytes = GC.GetAllocatedBytesForCurrentThread(),
                GcPauseMilliseconds = GC.GetTotalPauseDuration().TotalMilliseconds,
                Gen0Collections = GC.CollectionCount(0),
                Gen1Collections = GC.CollectionCount(1),
                Gen2Collections = GC.CollectionCount(2)
            };
    }

    internal void EndConstructorBody()
    {
        var allocatedAtReturn = GC.GetAllocatedBytesForCurrentThread();
        var timestampAtReturn = Stopwatch.GetTimestamp();
        var start = _constructorOpportunities.BoundaryStart;

        _constructorOpportunities.BoundaryStart =
            start with
            {
                PrivateCallWindowCaptured = true,
                StartStopwatchTimestamp = start.EndStopwatchTimestamp,
                EndStopwatchTimestamp = timestampAtReturn,
                ThreadAllocatedBytes = allocatedAtReturn - start.ThreadAllocatedBytes,
                GcPauseMilliseconds = GC.GetTotalPauseDuration().TotalMilliseconds - start.GcPauseMilliseconds,
                Gen0Collections = GC.CollectionCount(0) - start.Gen0Collections,
                Gen1Collections = GC.CollectionCount(1) - start.Gen1Collections,
                Gen2Collections = GC.CollectionCount(2) - start.Gen2Collections
            };
    }

    internal void EndConstructorInvocation(
        ConstructorOpportunities previous,
        bool completed,
        bool? resultAlreadyMaterialized = null,
        PerformanceCounters? logicalCounters = null)
    {
        var boundary =
            CompleteConstructorBoundary(completed, resultAlreadyMaterialized) with
            {
                LogicalInstructionCount = logicalCounters?.InstructionCount,
                LogicalBuildListCount = logicalCounters?.BuildListCount,
                LogicalBuildListItemCount = logicalCounters?.BuildListItemCount
            };

        LastConstructorInvocation = boundary;
        CountConstructorPrivateCallWindow(boundary);
        var newRetainedOpportunityMaximum = false;
        var newLogicalWorkMaximum = false;

        if (completed)
        {
            Count(Metric.ConstructorInvocationsCompleted);
            Count(Metric.ConstructorCompletedLogicalInstructions, logicalCounters?.InstructionCount ?? 0);

            if (boundary.LogicalInstructionCount is { } logicalInstructions &&
                (LargestCompletedConstructorByLogicalInstructions is not { } largestLogical ||
                logicalInstructions > largestLogical.LogicalInstructionCount))
            {
                LargestCompletedConstructorByLogicalInstructions = boundary;
                newLogicalWorkMaximum = true;
            }

            Count(Metric.ConstructorCompletedRepeatFrontiers, _constructorOpportunities.RepeatFrontiers);
            Count(Metric.ConstructorCompletedFrontierAvoidableNodeEntries, _constructorOpportunities.NodeEntries);
            Count(Metric.ConstructorCompletedFrontierAvoidableListArrays, _constructorOpportunities.ListArrays);
            Count(Metric.ConstructorCompletedFrontierAvoidableListItems, _constructorOpportunities.ListItems);
            Count(Metric.ConstructorCompletedFrontierAvoidableWrappers, _constructorOpportunities.Wrappers);

            Count(
                Metric.ConstructorCompletedFrontierAvoidableLiteralWrappers,
                _constructorOpportunities.LiteralWrappers);

            Count(Metric.ConstructorCompletedFrontierAvoidableListWrappers, _constructorOpportunities.ListWrappers);

            if (LargestCompletedConstructorByAcceptedBodyEntries is not { } largest ||
                boundary.AcceptedBodyEntries > largest.AcceptedBodyEntries)
            {
                LargestCompletedConstructorByAcceptedBodyEntries = boundary;
            }

            Count(
                boundary.RootRetainedFreshObjectsKnown
                ?
                Metric.ConstructorCompletedRootRetentionProvenInvocations
                :
                Metric.ConstructorCompletedRootRetentionUnknownInvocations);

            if (boundary.RootRetainedFreshObjects is { } freshObjects)
            {
                // These are exposures at distinct return boundaries, never a simultaneous live count.
                Count(Metric.ConstructorCompletedRootRetainedFreshObjectExposures, freshObjects);

                if (LargestCompletedConstructorByRootRetainedFreshObjects is not { } largestFresh ||
                    freshObjects > largestFresh.RootRetainedFreshObjects)
                {
                    LargestCompletedConstructorByRootRetainedFreshObjects = boundary;
                }
            }

            Count(
                boundary.RootRetainedAvoidableObjects.HasValue
                ?
                Metric.ConstructorCompletedRootAvoidableOpportunityProvenInvocations
                :
                Metric.ConstructorCompletedRootAvoidableOpportunityUnknownInvocations);

            if (boundary.RootRetainedAvoidableObjects is { } avoidableObjects)
            {
                Count(Metric.ConstructorCompletedRootRetainedAvoidableObjectExposures, avoidableObjects);

                if (LargestCompletedConstructorByRootRetainedAvoidableObjects is not { } largestOpportunity ||
                    avoidableObjects > largestOpportunity.RootRetainedAvoidableObjects)
                {
                    LargestCompletedConstructorByRootRetainedAvoidableObjects = boundary;
                    newRetainedOpportunityMaximum = avoidableObjects > 0;
                }
            }
        }
        else
            Count(Metric.ConstructorInvocationsIncomplete);

        _constructorOpportunities = previous;

        // At most sixteen boundary reports, with at least a doubling of physical or logical work.
        // Smaller record maxima remain available in every ordinary snapshot without additional writes.
        var boundaryWork = Math.Max(boundary.AcceptedBodyEntries, boundary.LogicalInstructionCount ?? 0);

        if (_writer is not null && (newRetainedOpportunityMaximum || newLogicalWorkMaximum) &&
            _constructorBoundarySnapshots < 16 && boundaryWork >= _nextConstructorBoundarySnapshotEntries)
        {
            ++_constructorBoundarySnapshots;

            _nextConstructorBoundarySnapshotEntries =
                boundaryWork <= long.MaxValue / 2 ? boundaryWork * 2 : long.MaxValue;

            WriteSnapshot("constructor-completed-maximum");
        }
    }

    private void CountConstructorPrivateCallWindow(ConstructorBoundarySnapshot boundary)
    {
        if (!boundary.NoNestedConstructorInvocations)
        {
            ++_constructorPrivateCallWindowsExcludedNested;
            return;
        }

        if (!boundary.PrivateCallWindowCaptured)
        {
            ++_constructorPrivateCallWindowsExcludedMissing;
            return;
        }
        // Including only non-nested windows avoids double counting. If a parent window
        // is excluded, its unmeasured exclusive time remains outside these sums.
        var elapsedTicks = boundary.EndStopwatchTimestamp - boundary.StartStopwatchTimestamp;

        if (boundary.Completed)
        {
            ++_completedConstructorPrivateCallWindows;
            _completedConstructorPrivateCallTicks += elapsedTicks;
            _completedConstructorPrivateCallGcPauseMilliseconds += boundary.GcPauseMilliseconds;
        }
        else
        {
            ++_incompleteConstructorPrivateCallWindows;
            _incompleteConstructorPrivateCallTicks += elapsedTicks;
            _incompleteConstructorPrivateCallGcPauseMilliseconds += boundary.GcPauseMilliseconds;
        }
    }

    private ConstructorBoundarySnapshot CaptureConstructorBoundary() =>
        new()
        {
            InvocationOrdinal = this[Metric.ConstructorInvocationsStarted],
            EndStopwatchTimestamp = Stopwatch.GetTimestamp(),
            AcceptedBodyEntries = this[Metric.EvalDirectEntries],
            BudgetRejectedBodyEntries = this[Metric.EvalDirectBudgetRejectedEntries],
            OuterEvalDirectCalls = this[Metric.EvalDirectCalls],
            LiteralEntries = this[Metric.EvalDirectLiteral],
            ListEntries = this[Metric.EvalDirectList],
            EnvironmentEntries = this[Metric.EvalDirectEnvironment],
            MemoHits = this[Metric.DirectConstructorMemoHits],
            LiteralMemoHits = this[Metric.DirectConstructorMemoLiteralHits],
            ListMemoHits = this[Metric.DirectConstructorMemoListHits],
            EnvironmentMemoHits = this[Metric.DirectConstructorMemoEnvironmentHits],
            LabelEntries = this[Metric.EvalDirectStringTag],
            ConditionalEntries = this[Metric.EvalDirectConditional],
            BuiltinEntries = this[Metric.EvalDirectBuiltin],
            UnsupportedEntries = this[Metric.EvalDirectUnsupported],
            FreshWrappers = this[Metric.WrapperAllocations],
            FreshLiteralWrappers = this[Metric.CreateCalls] - this[Metric.CreateReuses],
            FreshListWrappers = this[Metric.CreateListCalls] - this[Metric.CreateListReuses],
            ListArrays = this[Metric.EvalDirectListArrays],
            ListReferenceSlots = this[Metric.EvalDirectListItems],
            InProcessChildArrays = this[Metric.InProcessChildArrays],
            InProcessChildSlots = this[Metric.InProcessChildItems],
            CreateCalls = this[Metric.CreateCalls],
            LiteralWrapperCalls = this[Metric.LiteralWrapperCalls],
            CreateReuses = this[Metric.CreateReuses],
            CreateListCalls = this[Metric.CreateListCalls],
            CreateListReuses = this[Metric.CreateListReuses],
            EvaluateCalls = this[Metric.EvaluateCalls],
            CoverageCapEvents = this[Metric.ConstructorCoverageCapEvents],
            UniqueReferences = this[Metric.ConstructorDistinctReferences],
            ReferenceCapacity = ReferenceCapacity,
            TrackingAllocatedBytes = this[Metric.TrackingAllocatedBytes],
            SnapshotAllocatedBytes = _snapshotAllocatedBytes,
            ThreadAllocatedBytes = GC.GetAllocatedBytesForCurrentThread(),
            GcPauseMilliseconds = GC.GetTotalPauseDuration().TotalMilliseconds,
            Gen0Collections = GC.CollectionCount(0),
            Gen1Collections = GC.CollectionCount(1),
            Gen2Collections = GC.CollectionCount(2)
        };

    private ConstructorBoundarySnapshot CompleteConstructorBoundary(bool completed, bool? resultAlreadyMaterialized)
    {
        var start = _constructorOpportunities.BoundaryStart;
        var end = CaptureConstructorBoundary();

        var boundary =
            end with
            {
                InvocationOrdinal = start.InvocationOrdinal,
                Completed = completed,
                ResultAlreadyMaterialized = resultAlreadyMaterialized,
                StartStopwatchTimestamp =
                start.PrivateCallWindowCaptured ? start.StartStopwatchTimestamp : start.EndStopwatchTimestamp,
                EndStopwatchTimestamp =
                start.PrivateCallWindowCaptured ? start.EndStopwatchTimestamp : end.EndStopwatchTimestamp,
                StartAcceptedBodyEntries = start.AcceptedBodyEntries,
                EndAcceptedBodyEntries = end.AcceptedBodyEntries,
                AcceptedBodyEntries = end.AcceptedBodyEntries - start.AcceptedBodyEntries,
                BudgetRejectedBodyEntries = end.BudgetRejectedBodyEntries - start.BudgetRejectedBodyEntries,
                OuterEvalDirectCalls = end.OuterEvalDirectCalls - start.OuterEvalDirectCalls + 1,
                LiteralEntries = end.LiteralEntries - start.LiteralEntries,
                ListEntries = end.ListEntries - start.ListEntries,
                EnvironmentEntries = end.EnvironmentEntries - start.EnvironmentEntries,
                MemoHits = end.MemoHits - start.MemoHits,
                LiteralMemoHits = end.LiteralMemoHits - start.LiteralMemoHits,
                ListMemoHits = end.ListMemoHits - start.ListMemoHits,
                EnvironmentMemoHits = end.EnvironmentMemoHits - start.EnvironmentMemoHits,
                LabelEntries = end.LabelEntries - start.LabelEntries,
                ConditionalEntries = end.ConditionalEntries - start.ConditionalEntries,
                BuiltinEntries = end.BuiltinEntries - start.BuiltinEntries,
                UnsupportedEntries = end.UnsupportedEntries - start.UnsupportedEntries,
                FreshWrappers = end.FreshWrappers - start.FreshWrappers,
                FreshLiteralWrappers = end.FreshLiteralWrappers - start.FreshLiteralWrappers,
                FreshListWrappers = end.FreshListWrappers - start.FreshListWrappers,
                ListArrays = end.ListArrays - start.ListArrays,
                ListReferenceSlots = end.ListReferenceSlots - start.ListReferenceSlots,
                InProcessChildArrays = end.InProcessChildArrays - start.InProcessChildArrays,
                InProcessChildSlots = end.InProcessChildSlots - start.InProcessChildSlots,
                CreateCalls = end.CreateCalls - start.CreateCalls,
                LiteralWrapperCalls = end.LiteralWrapperCalls - start.LiteralWrapperCalls,
                CreateReuses = end.CreateReuses - start.CreateReuses,
                CreateListCalls = end.CreateListCalls - start.CreateListCalls,
                CreateListReuses = end.CreateListReuses - start.CreateListReuses,
                EvaluateCalls = end.EvaluateCalls - start.EvaluateCalls,
                CoverageCapEvents = end.CoverageCapEvents - start.CoverageCapEvents,
                UniqueReferences = end.UniqueReferences - start.UniqueReferences,
                TrackingAllocatedBytes = end.TrackingAllocatedBytes - start.TrackingAllocatedBytes,
                SetupTrackingAllocatedBytes = start.SetupTrackingAllocatedBytes,
                SnapshotAllocatedBytes = end.SnapshotAllocatedBytes - start.SnapshotAllocatedBytes,
                ThreadAllocatedBytes =
                start.PrivateCallWindowCaptured
                ?
                start.ThreadAllocatedBytes
                :
                end.ThreadAllocatedBytes - start.ThreadAllocatedBytes,
                PrivateCallWindowCaptured = start.PrivateCallWindowCaptured,
                WallMilliseconds =
                start.PrivateCallWindowCaptured
                ?
                (start.EndStopwatchTimestamp - start.StartStopwatchTimestamp) * 1000.0 / Stopwatch.Frequency
                :
                (end.EndStopwatchTimestamp - start.EndStopwatchTimestamp) * 1000.0 / Stopwatch.Frequency,
                GcPauseMilliseconds =
                start.PrivateCallWindowCaptured
                ?
                start.GcPauseMilliseconds
                :
                end.GcPauseMilliseconds - start.GcPauseMilliseconds,
                Gen0Collections =
                start.PrivateCallWindowCaptured ? start.Gen0Collections : end.Gen0Collections - start.Gen0Collections,
                Gen1Collections =
                start.PrivateCallWindowCaptured ? start.Gen1Collections : end.Gen1Collections - start.Gen1Collections,
                Gen2Collections =
                start.PrivateCallWindowCaptured ? start.Gen2Collections : end.Gen2Collections - start.Gen2Collections,
                RepeatFrontiers = _constructorOpportunities.RepeatFrontiers,
                AvoidableDescendantBodyEntries = _constructorOpportunities.NodeEntries,
                AvoidableListArrays = _constructorOpportunities.ListArrays,
                AvoidableListReferenceSlots = _constructorOpportunities.ListItems,
                AvoidableWrappers = _constructorOpportunities.Wrappers,
                AvoidableLiteralWrappers = _constructorOpportunities.LiteralWrappers,
                AvoidableListWrappers = _constructorOpportunities.ListWrappers,
                ReferenceTrackingEnabled = ReferenceCapacity > 0,
                NoNestedConstructorInvocations = start.InvocationOrdinal == end.InvocationOrdinal
            };

        return
            boundary with
            {
                PureListLiteralEnvironmentGrammar =
                boundary.LabelEntries is 0 && boundary.ConditionalEntries is 0 &&
                boundary.BuiltinEntries is 0 && boundary.UnsupportedEntries is 0 &&
                boundary.AcceptedBodyEntries ==
                boundary.LiteralEntries + boundary.ListEntries + boundary.EnvironmentEntries,
                AllocationConservationVerified =
                boundary.MemoHits == boundary.LiteralMemoHits + boundary.ListMemoHits + boundary.EnvironmentMemoHits &&
                boundary.EnvironmentMemoHits <= boundary.EnvironmentEntries &&
                boundary.CreateCalls + boundary.LiteralMemoHits == boundary.LiteralEntries &&
                boundary.LiteralWrapperCalls + boundary.LiteralMemoHits == boundary.LiteralEntries &&
                boundary.CreateListCalls + boundary.ListMemoHits == boundary.ListEntries &&
                boundary.FreshWrappers == boundary.FreshLiteralWrappers + boundary.FreshListWrappers &&
                boundary.ListArrays + boundary.ListMemoHits == boundary.ListEntries &&
                boundary.ListArrays == boundary.FreshListWrappers + boundary.CreateListReuses &&
                boundary.InProcessChildArrays == boundary.ListArrays &&
                boundary.InProcessChildSlots == boundary.ListReferenceSlots &&
                boundary.AvoidableWrappers == boundary.AvoidableLiteralWrappers + boundary.AvoidableListWrappers &&
                boundary.AvoidableWrappers <= boundary.FreshWrappers &&
                boundary.AvoidableListArrays <= boundary.ListArrays &&
                boundary.AvoidableListReferenceSlots <= boundary.ListReferenceSlots
            };
    }

    internal void CountConstructorOpportunity(Metric metric, long amount = 1)
    {
        Count(metric, amount);

        switch (metric)
        {
            case Metric.ConstructorRepeatFrontiers:
                _constructorOpportunities.RepeatFrontiers += amount;
                break;

            case Metric.ConstructorFrontierAvoidableNodeEntries:
                _constructorOpportunities.NodeEntries += amount;
                break;

            case Metric.ConstructorFrontierAvoidableListArrays:
                _constructorOpportunities.ListArrays += amount;
                break;

            case Metric.ConstructorFrontierAvoidableListItems:
                _constructorOpportunities.ListItems += amount;
                break;

            case Metric.ConstructorFrontierAvoidableWrappers:
                _constructorOpportunities.Wrappers += amount;
                break;

            case Metric.ConstructorFrontierAvoidableLiteralWrappers:
                _constructorOpportunities.LiteralWrappers += amount;
                break;

            case Metric.ConstructorFrontierAvoidableListWrappers:
                _constructorOpportunities.ListWrappers += amount;
                break;

            default:
                throw new ArgumentOutOfRangeException(nameof(metric));
        }
    }

    internal void ChildArray(int items)
    {
        Count(Metric.InProcessChildArrays);
        Count(Metric.InProcessChildItems, items);
    }

    /// <summary>
    /// Activates counters for synchronous VM/value work. Always dispose on the same thread;
    /// nesting restores both the previous sink and its cancellation token.
    /// </summary>
    public EvaluationScope Enter(CancellationToken token = default) =>
        EnterEvaluation(this, token, null);

    internal static EvaluationScope EnterEvaluation(
        PineVMWorkProfile? profile, CancellationToken token, PineVMExpressionTemplatePlanCache? cache) =>
        new(profile, token, cache);

    /// <summary>Allocation-free synchronous scope; do not box or dispose copied scopes out of order.</summary>
    public readonly struct EvaluationScope : IDisposable
    {
        private readonly PineVMWorkProfile? _previous;

        private readonly PineVMWorkProfile? _profile;

        private readonly CancellationToken _previousToken;

        private readonly PineVMExpressionTemplatePlanCache? _previousCache;

        internal EvaluationScope(
            PineVMWorkProfile? profile, CancellationToken token, PineVMExpressionTemplatePlanCache? cache)
        {
            _previous = Current;
            _profile = profile;
            _previousToken = profile?._token ?? default;
            _previousCache = profile?._cache;

            if (_previous is not null || profile is not null)
                Current = profile;

            if (profile is not null)
            {
                profile._token = token;
                profile._cache = cache ?? _previousCache;
            }
        }

        public void Dispose()
        {
            if (_profile is not null)
            {
                _profile._token = _previousToken;
                _profile._cache = _previousCache;
            }

            Current = _previous;
        }
    }

    internal CauseScope WithCause(MaterializationCause cause) => new(this, cause);

    internal readonly struct CauseScope : IDisposable
    {
        private readonly PineVMWorkProfile _profile;

        private readonly MaterializationCause _previous;

        internal CauseScope(PineVMWorkProfile profile, MaterializationCause cause)
        {
            _profile = profile;
            _previous = profile._cause;
            profile._cause = cause;
        }

        public void Dispose() => _profile._cause = _previous;
    }

    internal void MaterializedList()
    {
        Count(Metric.MaterializedListCalls);

        Count(
            _cause switch
            {
                MaterializationCause.RootReturn => Metric.MaterializedListRootReturn,
                MaterializationCause.InvocationCache => Metric.MaterializedListInvocationCache,
                MaterializationCause.Recognition => Metric.MaterializedListRecognition,

                _ =>
                Metric.MaterializedListOther
            });
    }

    internal void DeferredMaterialized(int environmentSlots)
    {
        Count(Metric.DeferredMaterializations);
        Count(Metric.DeferredMaterializedEnvironmentSlots, environmentSlots);

        Count(
            _cause switch
            {
                MaterializationCause.RootReturn => Metric.DeferredMaterializedRootReturn,
                MaterializationCause.InvocationCache => Metric.DeferredMaterializedInvocationCache,
                MaterializationCause.Recognition => Metric.DeferredMaterializedRecognition,

                _ =>
                Metric.DeferredMaterializedOther
            });
    }

    internal bool DirectEntry(
        Expression expression, HashSet<Expression>? references, bool isRoot, bool underRepeatFrontier)
    {
        if (DirectVisitBudget is { } budget && this[Metric.EvalDirectEntries] >= budget)
        {
            Count(Metric.EvalDirectBudgetRejectedEntries);
            StopReason = "Work-profile direct node-entry budget exhausted.";
            WriteSnapshot("direct-visit-limit");
            throw new OperationCanceledException(StopReason);
        }

        Count(Metric.EvalDirectEntries);

        if (!isRoot)
            Count(Metric.EvalDirectRecursiveEntries);

        if (underRepeatFrontier)
            CountConstructorOpportunity(Metric.ConstructorFrontierAvoidableNodeEntries);

        Count(
            expression switch
            {
                Expression.Litral => Metric.EvalDirectLiteral,
                Expression.List => Metric.EvalDirectList,
                Expression.Environment => Metric.EvalDirectEnvironment,
                Expression.Conditional => Metric.EvalDirectConditional,
                Expression.Builtin => Metric.EvalDirectBuiltin,
                Expression.Label => Metric.EvalDirectStringTag,
                Expression.Eval => Metric.EvalDirectUnsupported,

                _ =>
                throw new NotImplementedException(
                    "DirectEntry does not handle expression variant: " + expression.GetType().Name)
            });

        var repeatFrontier = false;

        if (references is not null &&
            expression is Expression.Litral or Expression.List or Expression.Environment)
        {
            Count(Metric.ConstructorEntries);

            if (references.Contains(expression))
            {
                Count(Metric.ConstructorRepeatedReferences);

                if (!underRepeatFrontier)
                {
                    repeatFrontier = true;
                    CountConstructorOpportunity(Metric.ConstructorRepeatFrontiers);
                }

                if (expression is Expression.List list)
                {
                    Count(Metric.ConstructorRepeatedLists);
                    Count(Metric.ConstructorRepeatedListItems, list.Items.Count);
                }
            }
            else if (references.Count < ReferenceCapacity)
            {
                var allocatedBefore = GC.GetAllocatedBytesForCurrentThread();
                references.Add(expression);
                Count(Metric.TrackingAllocatedBytes, GC.GetAllocatedBytesForCurrentThread() - allocatedBefore);
                Count(Metric.ConstructorDistinctReferences);
                Count(Metric.TrackingReferences);
            }
            else
                Count(Metric.ConstructorCoverageCapEvents);
        }

        Checkpoint();
        return underRepeatFrontier || repeatFrontier;
    }

    internal void Checkpoint(bool force = false)
    {
        if (!force && (++_checkpointEntries & 4095) is not 0)
            return;

        if (_wall.ElapsedTicks >= _nextSnapshotTicks)
        {
            WriteSnapshot("running");
            _nextSnapshotTicks = _wall.ElapsedTicks + Stopwatch.Frequency;
        }

        _token.ThrowIfCancellationRequested();

        if (WorkingSetLimitBytes is { } limit && _wall.ElapsedTicks >= _nextMemoryCheckTicks)
        {
            _nextMemoryCheckTicks = _wall.ElapsedTicks + Stopwatch.Frequency / 20;
            _process.Refresh();

            if (_process.WorkingSet64 > limit)
            {
                StopReason = "Explicit work-profile working-set limit exceeded.";
                WriteSnapshot("working-set-limit");
                throw new OperationCanceledException(StopReason);
            }
        }
    }

    /// <summary>Flushes the preceding phase before changing the attribution label.</summary>
    public void SetScenario(string phase, string scenario)
    {
        WriteSnapshot("phase-boundary");
        _phase = phase;
        _scenario = scenario;
    }

    /// <summary>
    /// Cumulative process deltas include profiler overhead and unrelated process work. GC heap sizes
    /// describe the last GC, not retained objects attributable to a particular VM or phase.
    /// </summary>
    public void WriteSnapshot(string outcome)
    {
        if (_writer is null)
            return;

        var allocatedBefore = GC.GetAllocatedBytesForCurrentThread();
        _process.Refresh();
        var info = GC.GetGCMemoryInfo();
        var numeric = new Dictionary<string, long>((int)Metric.Count);

        for (var index = 0; index < (int)Metric.Count; ++index)
            numeric[((Metric)index).ToString()] = _counts[index];

        if (_cache is { } cache)
        {
            _positiveCacheEntries = _negativeCacheEntries = 0;

            foreach (var entry in cache.Entries)
            {
                if (entry.Value.Plan is null)
                    ++_negativeCacheEntries;

                else
                    ++_positiveCacheEntries;
            }

            _hasCacheSnapshot = true;
        }

        var stopwatchTimestamp = Stopwatch.GetTimestamp();
        var snapshotUnixTimeMilliseconds = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds();

        _writer.WriteLine(
            JsonSerializer.Serialize(
                new
                {
                    SchemaVersion = 2,
                    Phase = _phase,
                    Scenario = _scenario,
                    Outcome = outcome,
                    StopwatchTimestamp = stopwatchTimestamp,
                    StopwatchFrequency = Stopwatch.Frequency,
                    SnapshotUnixTimeMilliseconds = snapshotUnixTimeMilliseconds,
                    UnixTimestampMilliseconds = snapshotUnixTimeMilliseconds,
                    WallMilliseconds = _wall.Elapsed.TotalMilliseconds,
                    CpuMilliseconds = (_process.TotalProcessorTime - _cpu).TotalMilliseconds,
                    AllocatedBytes = GC.GetTotalAllocatedBytes(precise: true) - _allocated,
                    WorkingSetBytes = _process.WorkingSet64,
                    LastGcHeapBytes = info.HeapSizeBytes,
                    LastGcFragmentedBytes = info.FragmentedBytes,
                    LastGcCommittedBytes = info.TotalCommittedBytes,
                    GcPauseMilliseconds = (GC.GetTotalPauseDuration() - _pause).TotalMilliseconds,
                    Gen0Collections = GC.CollectionCount(0) - _collections[0],
                    Gen1Collections = GC.CollectionCount(1) - _collections[1],
                    Gen2Collections = GC.CollectionCount(2) - _collections[2],
                    ReferenceCapacity,
                    WorkingSetLimitBytes,
                    DirectVisitBudget,
                    ConstructorInvocationBudget,
                    PreserveConstructorSharing,
                    ConstructorReferenceTrackingEnabled = ReferenceCapacity > 0,
                    ConstructorEligibilityExcludesEvalAndBuiltin = true,
                    ConstructorReferenceSubsetIsListLiteralEnvironment = true,
                    DirectEntryCountersIncludeMemoHits = true,
                    ConstructorLogicalCountersIncludeMemoReplay = true,
                    ConstructorRetentionUsesMemoAwareAllocationConservation = true,
                    ConstructorCounterfactualMeasuresRemainingExecutionOpportunities = true,
                    ConstructorCounterfactualExcludesReusedWrappers = true,
                    ConstructorCoverageComplete =
                    ReferenceCapacity > 0 && this[Metric.ConstructorCoverageCapEvents] is 0,
                    ConstructorCounterfactualCoverageComplete =
                    ReferenceCapacity > 0 && this[Metric.ConstructorCoverageCapEvents] is 0 &&
                    this[Metric.ConstructorInvocationsStarted] == this[Metric.ConstructorInvocationsCompleted],
                    ConstructorInvocationsPending =
                    this[Metric.ConstructorInvocationsStarted] -
                    this[Metric.ConstructorInvocationsCompleted] -
                    this[Metric.ConstructorInvocationsIncomplete],
                    LastConstructorInvocation,
                    LargestCompletedConstructorByAcceptedBodyEntries,
                    LargestCompletedConstructorByLogicalInstructions,
                    LargestCompletedConstructorByRootRetainedFreshObjects,
                    LargestCompletedConstructorByRootRetainedAvoidableObjects,
                    ConstructorBoundarySnapshotCount = _constructorBoundarySnapshots,
                    ConstructorBoundarySnapshotLimit = 16,
                    ConstructorCompletedPrivateCallStopwatchTicks = _completedConstructorPrivateCallTicks,
                    ConstructorIncompletePrivateCallStopwatchTicks = _incompleteConstructorPrivateCallTicks,
                    ConstructorCompletedPrivateCallWallMilliseconds,
                    ConstructorIncompletePrivateCallWallMilliseconds,
                    ConstructorCompletedPrivateCallGcPauseMilliseconds =
                    _completedConstructorPrivateCallGcPauseMilliseconds,
                    ConstructorIncompletePrivateCallGcPauseMilliseconds =
                    _incompleteConstructorPrivateCallGcPauseMilliseconds,
                    ConstructorCompletedPrivateCallWindows = _completedConstructorPrivateCallWindows,
                    ConstructorIncompletePrivateCallWindows = _incompleteConstructorPrivateCallWindows,
                    ConstructorPrivateCallWindowsExcludedNested = _constructorPrivateCallWindowsExcludedNested,
                    ConstructorPrivateCallWindowsExcludedMissing = _constructorPrivateCallWindowsExcludedMissing,
                    ConstructorPrivateCallWindowCoverageComplete,
                    ConstructorPrivateCallWindowsIncludeObserverWork = true,
                    ConstructorPrivateCallGcPauseWindowsAreProcessWide = true,
                    ConstructorPrivateCallGcPauseWindowsProveCausality = false,
                    ConstructorRetainedObjectExposuresAreNotSimultaneous = true,
                    ConstructorRootRetentionExcludesPreexistingObjects = true,
                    ConstructorRootRetentionExcludesMemoOverhead = true,
                    StructuralProducerEquivalenceKnown = false,
                    RetainedVmObjectCountKnown = false,
                    KnownAllocationObjects,
                    KnownAllocationObjectsIsLowerBound = true,
                    KnownAllocationObjectsExcludesProfilingMetadata = true,
                    PreviousSnapshotsAllocatedBytes = _snapshotAllocatedBytes,
                    TemplateCachePositiveEntries = _positiveCacheEntries,
                    TemplateCacheNegativeEntries = _negativeCacheEntries,
                    TemplateCacheSnapshotAvailable = _hasCacheSnapshot,
                    TemplateCacheSnapshotCurrent = _cache is not null,
                    Counters = numeric
                }));

        _writer.Flush();
        _snapshotAllocatedBytes += GC.GetAllocatedBytesForCurrentThread() - allocatedBefore;
    }

    /// <summary>Writes a final snapshot and closes an owned stream; counter-only instances allocate no report.</summary>
    public void Dispose()
    {
        if (_disposed)
            return;

        _disposed = true;
        WriteSnapshot("disposed");

        if (_ownsWriter)
            _writer?.Dispose();

        _process.Dispose();
    }
}
