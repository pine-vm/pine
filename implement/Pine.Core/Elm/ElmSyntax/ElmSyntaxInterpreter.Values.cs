using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Internal;
using Pine.Core.PineVM;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

namespace Pine.Core.Elm.ElmSyntax;

public partial class ElmSyntaxInterpreter
{
    /// <summary>
    /// In-process representation of an Elm function closure.
    /// <para>
    /// The interpreter operates on <see cref="PineValueInProcess"/> for <em>all</em> runtime
    /// values. Ordinary Elm data (integers, characters, strings, lists, tuples, records and
    /// tags) is represented by the standard <see cref="PineValueInProcess"/> instances backed by
    /// a concrete <see cref="PineValue"/>. Function values, however, have no concrete
    /// <see cref="PineValue"/> encoding (they capture Elm AST nodes and a local-binding
    /// environment), so they are represented by this dedicated subtype which carries the closure
    /// state directly.
    /// </para>
    /// </summary>
    internal sealed class ElmClosureInProcess(
        ElmClosureInProcess.SourceRef source,
        int parameterCount,
        IReadOnlyList<PineValueInProcess> argumentsAlreadyCollected,
        IReadOnlyDictionary<string, PineValueInProcess> capturedBindings,
        DeclQualifiedName capturedTopLevel)
        : PineValueInProcess
    {
        public SourceRef Source { get; } = source;

        public int ParameterCount { get; } = parameterCount;

        public IReadOnlyList<PineValueInProcess> ArgumentsAlreadyCollected { get; } = argumentsAlreadyCollected;

        public IReadOnlyDictionary<string, PineValueInProcess> CapturedBindings { get; } = capturedBindings;

        public DeclQualifiedName CapturedTopLevel { get; } = capturedTopLevel;

        public ElmClosureInProcess With(
            IReadOnlyList<PineValueInProcess> argumentsAlreadyCollected) =>
            new(
                Source,
                ParameterCount,
                argumentsAlreadyCollected,
                CapturedBindings,
                CapturedTopLevel);

        /// <summary>
        /// Identifies the body that will be evaluated once the closure is fully applied.
        /// </summary>
        public abstract record SourceRef
        {
            /// <summary>
            /// A reference to a user-defined function declaration. <see cref="Name"/> is the
            /// fully-qualified name used for stack-trace rendering and the infinite-recursion
            /// detector; <see cref="Implementation"/> carries the parameter patterns and body.
            /// </summary>
            public sealed record Declared(
                DeclQualifiedName Name,
                ElmSyntaxAbstract.FunctionImplementation Implementation)
                : SourceRef;

            /// <summary>
            /// An anonymous lambda expression. Stack traces and the infinite-recursion detector
            /// use a fixed synthetic name (the abstract syntax model carries no source location).
            /// </summary>
            public sealed record Lambda(
                ElmSyntaxAbstract.Expression.LambdaExpression LambdaExpression)
                : SourceRef;

            public override string ToString()
            {
                return RenderSourceRef(this);
            }
        }
    }

    internal sealed class ElmChoiceTagConstructorInProcess(
        DeclQualifiedName typeName,
        string tagName,
        int totalArity,
        ImmutableList<PineValueInProcess> arguments)
        : PineValueInProcess
    {
        public DeclQualifiedName TypeName { get; } = typeName;

        public string TagName { get; } = tagName;

        public int TotalArity { get; } = totalArity;

        public ImmutableList<PineValueInProcess> Arguments { get; } = arguments;

        public ElmChoiceTagConstructorInProcess WithArgumentsApplied(IReadOnlyList<PineValueInProcess> arguments) =>
            new(
                TypeName,
                TagName,
                TotalArity,
                Arguments.AddRange(arguments));
    }

    internal sealed class ElmRecordTypeConstructorInProcess(
        DeclQualifiedName typeName,
        ImmutableArray<(string FieldName, PineValue FieldNameValue)> fieldNames,
        ImmutableList<PineValueInProcess> arguments)
        : PineValueInProcess
    {
        public DeclQualifiedName TypeName { get; } = typeName;

        public ImmutableArray<(string FieldName, PineValue FieldNameValue)> FieldNames { get; } = fieldNames;

        public ImmutableList<PineValueInProcess> Arguments { get; } = arguments;

        public ElmRecordTypeConstructorInProcess WithArgumentsApplied(IReadOnlyList<PineValueInProcess> arguments) =>
            new(
                TypeName,
                FieldNames,
                Arguments.AddRange(arguments));
    }

    internal sealed class ElmRecordAccessChainInProcess(
        ImmutableArray<(string FieldName, PineValue FieldNameValue)> fieldNames)
        : PineValueInProcess
    {
        public ImmutableArray<(string FieldName, PineValue FieldNameValue)> FieldNames { get; } = fieldNames;

        public static ElmRecordAccessChainInProcess CreateFromFieldNames(IReadOnlyList<string> fieldNames) =>
            new([.. fieldNames.Select(fn => (fn, StringEncoding.ValueFromString(fn)))]);
    }

    /// <summary>
    /// True for in-process values that carry no concrete <see cref="PineValue"/> backing
    /// (closures and boxed function-bearing values); such values must not be passed to the
    /// structural <see cref="PineValueInProcess"/> operations (Evaluate / AsInteger / AreEqual).
    /// </summary>
    private static bool IsOpaque(PineValueInProcess value) =>
        value is ElmClosureInProcess or ElmChoiceTagConstructorInProcess or ElmRecordTypeConstructorInProcess or ElmRecordAccessChainInProcess;

    /// <summary>
    /// True when <paramref name="value"/> is itself opaque (see <see cref="IsOpaque"/>) or embeds an
    /// opaque value somewhere within its unevaluated list/tagged structure. Such values cannot be
    /// fully materialized to a concrete <see cref="PineValue"/> and must be converted back to an
    /// <see cref="ElmValue"/> structurally (see <see cref="ToElmStructural"/>).
    /// </summary>
    private static bool ContainsOpaque(PineValueInProcess value)
    {
        if (IsOpaque(value))
            return true;

        if (value.UnevaluatedStructuralItemsOrNull() is { } items)
        {
            for (var i = 0; i < items.Count; i++)
            {
                if (ContainsOpaque(items[i]))
                    return true;
            }
        }

        return false;
    }

    /// <summary>
    /// Converts an <see cref="ElmValue"/> (used at the public resolver boundary) into the
    /// interpreter's in-process value representation. Function values are mapped to
    /// <see cref="ElmClosureInProcess"/>; every other value is encoded to its concrete
    /// <see cref="PineValue"/> form.
    /// </summary>
    internal static PineValueInProcess ToProcess(ElmValue value)
    {
        return PineValueInProcess.Create(ElmValueEncoding.ElmValueAsPineValue(value));
    }

    /// <summary>
    /// Converts an in-process value back to an <see cref="ElmValue"/> for use at the public
    /// resolver/result boundary (and for rendering values in runtime-error messages).
    /// </summary>
    internal static ElmValue ToElm(PineValueInProcess value) =>
        ToElm(value, visiting: null);

    /// <summary>
    /// Core of <see cref="ToElm(PineValueInProcess)"/>. <paramref name="visiting"/> tracks the
    /// closures currently being expanded so that a reference cycle (for example a let-recursive
    /// closure that captures itself in its own environment) is broken instead of recursing forever.
    /// It is allocated lazily on the first closure encountered and threaded through every nested
    /// conversion. Identity is by reference (<see cref="ElmClosureInProcess"/> uses reference equality).
    /// </summary>
    private static ElmValue ToElm(
        PineValueInProcess value,
        HashSet<ElmClosureInProcess>? visiting)
    {
        if (value is ElmClosureInProcess closure)
        {
            throw new System.NotSupportedException(
                "Cannot convert a closure to an ElmValue. Closures cannot be returned from resolvers or captured in ElmValues; they can only be applied to arguments within the resolver's execution.");
        }

        if (ContainsOpaque(value))
        {
            return ToElmStructural(value, visiting);
        }

        return
            ElmValueEncoding.PineValueAsElmValue(value.Evaluate(), null, null)
            .Extract(
                err =>
                throw new System.InvalidOperationException(
                    "Failed decoding in-process value as Elm value: " + err));
    }

    /// <summary>
    /// Reconstructs an <see cref="ElmValue"/> from an in-process value that embeds one or more
    /// opaque (function-bearing) child values and therefore has no concrete <see cref="PineValue"/>
    /// encoding. Only the structural Elm forms that can carry functions are possible here: a list, a
    /// tuple (encoded as a list), a record or a custom-type tag. String/Float/Bytes/Int values carry
    /// only concrete leaves and never reach this method. The decoding mirrors
    /// <see cref="ElmValueEncoding.PineListValueAsElmValue"/> but recurses through <see cref="ToElm(PineValueInProcess)"/>
    /// so that opaque leaves are recovered as their original <see cref="ElmValue"/> form.
    /// </summary>
    private static ElmValue ToElmStructural(
        PineValueInProcess value,
        HashSet<ElmClosureInProcess>? visiting)
    {
        var items = AsListItems(value);

        if (items is null)
        {
            // Unreachable in practice: a value flagged as opaque-containing is either a box, a
            // closure (both handled by the caller) or an unevaluated list/tagged structure.
            return ToElm(value, visiting);
        }

        // Record (new flat format): [recordTag, fieldName0, fieldValue0, fieldName1, fieldValue1, ...].
        if (items.Count >= 1 &&
            (items.Count & 1) == 1 &&
            !IsOpaque(items[0]) &&
            items[0].Evaluate() == ElmValue.ElmRecordTypeTagNameAsValue)
        {
            var fieldCount = (items.Count - 1) / 2;

            var recordFields = new (string fieldName, ElmValue fieldValue)[fieldCount];

            for (var i = 0; i < fieldCount; i++)
            {
                var fieldName =
                    StringEncoding.StringFromValue(items[1 + 2 * i].Evaluate())
                    .Extract(
                        err =>
                        throw new System.InvalidOperationException(
                            "Failed decoding record field name: " + err));

                recordFields[i] = (fieldName, ToElm(items[2 + 2 * i], visiting));
            }

            return new ElmValue.ElmRecord(recordFields);
        }

        // Custom-type tag: [tagName, [tagArgs...]].
        if (items.Count is 2 &&
            !IsOpaque(items[0]) &&
            AsListItems(items[1]) is { } tagArgs &&
            StringEncoding.StringFromValue(items[0].Evaluate()).IsOkOrNull() is { } tagName &&
            ElmValueEncoding.StringIsValidTagName(tagName))
        {
            var tagArguments = new ElmValue[tagArgs.Count];

            for (var i = 0; i < tagArguments.Length; i++)
            {
                tagArguments[i] = ToElm(tagArgs[i], visiting);
            }

            return ElmValue.TagInstance(tagName, tagArguments);
        }

        // Ordinary list or tuple.
        var elements = new ElmValue[items.Count];

        for (var i = 0; i < elements.Length; i++)
        {
            elements[i] = ToElm(items[i], visiting);
        }

        return new ElmValue.ElmList(elements);
    }

    /// <summary>
    /// Returns the child items of an in-process value that evaluates to a list, without forcing
    /// evaluation of those items. Handles both the lazily constructed list/tagged forms and an
    /// already-evaluated <see cref="PineValue.ListValue"/>. Returns <c>null</c> when the value is not
    /// a list.
    /// </summary>
    private static IReadOnlyList<PineValueInProcess>? AsListItems(PineValueInProcess value)
    {
        if (value.UnevaluatedStructuralItemsOrNull() is { } unevaluated)
            return unevaluated;

        if (value.EvaluatedOrNull is PineValue.ListValue listValue)
        {
            var wrapped = new PineValueInProcess[listValue.Items.Length];

            for (var i = 0; i < wrapped.Length; i++)
            {
                wrapped[i] = PineValueInProcess.Create(listValue.Items.Span[i]);
            }

            return wrapped;
        }

        return null;
    }

    /// <summary>
    /// Converts a list of in-process values back to <see cref="ElmValue"/> instances.
    /// </summary>
    internal static IReadOnlyList<ElmValue> ToElmList(IReadOnlyList<PineValueInProcess> values)
    {
        var result = new ElmValue[values.Count];

        for (var i = 0; i < result.Length; i++)
        {
            result[i] = ToElm(values[i]);
        }

        return result;
    }

    /// <summary>
    /// Tests whether the in-process <paramref name="value"/> is the Elm <c>True</c> value
    /// (encoded as the kernel boolean true value).
    /// </summary>
    private static bool IsElmTrue(PineValueInProcess value)
    {
        if (ContainsOpaque(value))
            return false;

        return PineValueInProcess.AreEqual(value, PineKernelValues.TrueValue);
    }

    /// <summary>
    /// Structural equality between two in-process values, comparing them directly as
    /// <see cref="PineValueInProcess"/> instances without projecting either operand to an
    /// <see cref="ElmValue"/>.
    /// <para>
    /// Fully Pine-encodable values are compared via
    /// <see cref="PineValueInProcess.AreEqual(PineValueInProcess, PineValueInProcess)"/>. Values
    /// that embed one of the interpreter's opaque variants — closures
    /// (<see cref="ElmClosureInProcess"/>), boxed function-bearing values
    /// (<see cref="ElmValueBox"/>), partially-applied tag/record constructors
    /// (<see cref="ElmChoiceTagConstructorInProcess"/>, <see cref="ElmRecordTypeConstructorInProcess"/>)
    /// and record-access chains (<see cref="ElmRecordAccessChainInProcess"/>) — are compared
    /// structurally: composite list/tagged values are descended into element-wise so that opaque
    /// leaves are reached and compared by their variant-specific identity. An opaque leaf only
    /// equals another opaque leaf of the same variant; it never equals a concrete or composite
    /// value occupying the same position.
    /// </para>
    /// </summary>
    internal static bool ValuesEqualInProcess(PineValueInProcess left, PineValueInProcess right) =>
        ValuesEqualInProcess(left, right, visiting: null);

    private static bool ValuesEqualInProcess(
        PineValueInProcess left,
        PineValueInProcess right,
        List<(ElmClosureInProcess Left, ElmClosureInProcess Right)>? visiting)
    {
        if (ReferenceEquals(left, right))
            return true;

        var leftContainsOpaque = ContainsOpaque(left);
        var rightContainsOpaque = ContainsOpaque(right);

        if (!leftContainsOpaque && !rightContainsOpaque)
        {
            // Neither value embeds an opaque variant, so both are fully Pine-encodable and the
            // general equality predictor can compare them without forcing wasteful evaluation.
            return PineValueInProcess.AreEqual(left, right);
        }

        if (leftContainsOpaque != rightContainsOpaque)
        {
            // A value that embeds an opaque variant can only equal another value that embeds an
            // opaque variant in the same positions.
            return false;
        }

        // Both operands embed at least one opaque variant. Compare them structurally, descending
        // through lists / tagged values and comparing opaque leaves by their variant identity.

        if (IsOpaque(left) || IsOpaque(right))
        {
            if (!IsOpaque(left) || !IsOpaque(right))
                return false;

            return OpaqueLeavesEqual(left, right, visiting);
        }

        var leftItems = AsListItems(left);
        var rightItems = AsListItems(right);

        // Both contain opaque leaves but neither is an opaque leaf, so both must be composite
        // list/tagged structures.
        if (leftItems is null || rightItems is null)
            return false;

        if (leftItems.Count != rightItems.Count)
            return false;

        for (var i = 0; i < leftItems.Count; i++)
        {
            if (!ValuesEqualInProcess(leftItems[i], rightItems[i], visiting))
                return false;
        }

        return true;
    }

    /// <summary>
    /// Compares two opaque in-process leaves (see <see cref="IsOpaque"/>) by their variant. A leaf
    /// only equals another leaf of the same variant with structurally equal payload; otherwise the
    /// result is <see langword="false"/>.
    /// </summary>
    private static bool OpaqueLeavesEqual(
        PineValueInProcess left,
        PineValueInProcess right,
        List<(ElmClosureInProcess Left, ElmClosureInProcess Right)>? visiting)
    {
        switch (left)
        {
            case ElmClosureInProcess leftClosure:
                return
                    right is ElmClosureInProcess rightClosure
                    && ClosuresEqualInProcess(leftClosure, rightClosure, visiting);

            case ElmChoiceTagConstructorInProcess leftTag:
                return
                    right is ElmChoiceTagConstructorInProcess rightTag
                    && ChoiceTagConstructorsEqualInProcess(leftTag, rightTag, visiting);

            case ElmRecordTypeConstructorInProcess leftCtor:
                return
                    right is ElmRecordTypeConstructorInProcess rightCtor
                    && RecordTypeConstructorsEqualInProcess(leftCtor, rightCtor, visiting);

            case ElmRecordAccessChainInProcess leftChain:
                return
                    right is ElmRecordAccessChainInProcess rightChain
                    && FieldNamesEqual(leftChain.FieldNames, rightChain.FieldNames);

            default:
                return false;
        }
    }

    /// <summary>
    /// Structural equality for two closures. Compares the body reference, parameter count, captured
    /// top-level name, already-collected arguments and captured bindings. <paramref name="visiting"/>
    /// breaks reference cycles: a let-recursive closure can capture itself in its own environment, so
    /// a pair already being compared is treated as equal (co-inductively) once its other fields match.
    /// </summary>
    private static bool ClosuresEqualInProcess(
        ElmClosureInProcess left,
        ElmClosureInProcess right,
        List<(ElmClosureInProcess Left, ElmClosureInProcess Right)>? visiting)
    {
        if (ReferenceEquals(left, right))
            return true;

        if (!left.Source.Equals(right.Source))
            return false;

        if (left.ParameterCount != right.ParameterCount)
            return false;

        if (!left.CapturedTopLevel.Equals(right.CapturedTopLevel))
            return false;

        if (left.ArgumentsAlreadyCollected.Count != right.ArgumentsAlreadyCollected.Count)
            return false;

        if (left.CapturedBindings.Count != right.CapturedBindings.Count)
            return false;

        // Break reference cycles: a let-recursive closure can capture itself in its own
        // environment, so a pair already being compared is treated as equal (co-inductively)
        // once its other fields match. Reference identity is used here because
        // PineValueInProcess (the base type of ElmClosureInProcess) intentionally throws from
        // its instance Equals, which rules out hash-set / value-tuple membership checks.
        visiting ??= [];

        for (var i = 0; i < visiting.Count; i++)
        {
            if (ReferenceEquals(visiting[i].Left, left) && ReferenceEquals(visiting[i].Right, right))
                return true;
        }

        visiting.Add((left, right));

        try
        {
            for (var i = 0; i < left.ArgumentsAlreadyCollected.Count; i++)
            {
                if (!ValuesEqualInProcess(
                        left.ArgumentsAlreadyCollected[i],
                        right.ArgumentsAlreadyCollected[i],
                        visiting))
                    return false;
            }

            foreach (var binding in left.CapturedBindings)
            {
                if (!right.CapturedBindings.TryGetValue(binding.Key, out var rightBound))
                    return false;

                if (!ValuesEqualInProcess(binding.Value, rightBound, visiting))
                    return false;
            }

            return true;
        }
        finally
        {
            visiting.RemoveAt(visiting.Count - 1);
        }
    }

    /// <summary>
    /// Structural equality for two partially-applied custom-type tag constructors: the declaring
    /// type, tag name, total arity and the arguments collected so far must all match.
    /// </summary>
    private static bool ChoiceTagConstructorsEqualInProcess(
        ElmChoiceTagConstructorInProcess left,
        ElmChoiceTagConstructorInProcess right,
        List<(ElmClosureInProcess Left, ElmClosureInProcess Right)>? visiting)
    {
        if (!left.TypeName.Equals(right.TypeName))
            return false;

        if (!string.Equals(left.TagName, right.TagName, System.StringComparison.Ordinal))
            return false;

        if (left.TotalArity != right.TotalArity)
            return false;

        if (left.Arguments.Count != right.Arguments.Count)
            return false;

        for (var i = 0; i < left.Arguments.Count; i++)
        {
            if (!ValuesEqualInProcess(left.Arguments[i], right.Arguments[i], visiting))
                return false;
        }

        return true;
    }

    /// <summary>
    /// Structural equality for two partially-applied record-type constructors: the declaring type,
    /// the ordered field names and the arguments collected so far must all match.
    /// </summary>
    private static bool RecordTypeConstructorsEqualInProcess(
        ElmRecordTypeConstructorInProcess left,
        ElmRecordTypeConstructorInProcess right,
        List<(ElmClosureInProcess Left, ElmClosureInProcess Right)>? visiting)
    {
        if (!left.TypeName.Equals(right.TypeName))
            return false;

        if (!FieldNamesEqual(left.FieldNames, right.FieldNames))
            return false;

        if (left.Arguments.Count != right.Arguments.Count)
            return false;

        for (var i = 0; i < left.Arguments.Count; i++)
        {
            if (!ValuesEqualInProcess(left.Arguments[i], right.Arguments[i], visiting))
                return false;
        }

        return true;
    }

    /// <summary>
    /// Ordinal element-wise equality of two ordered field-name sequences.
    /// </summary>
    private static bool FieldNamesEqual(
        ImmutableArray<(string FieldName, PineValue FieldNameValue)> left,
        ImmutableArray<(string FieldName, PineValue FieldNameValue)> right)
    {
        if (left.Length != right.Length)
            return false;

        for (var i = 0; i < left.Length; i++)
        {
            if (!string.Equals(left[i].FieldName, right[i].FieldName, System.StringComparison.Ordinal))
                return false;
        }

        return true;
    }

    /// <summary>
    /// Builds an in-process integer value.
    /// </summary>
    private static PineValueInProcess MakeInteger(BigInteger value) =>
        PineValueInProcess.CreateInteger(value);

    private static readonly PineValueInProcess s_elmRecordTypeTagNameAsValueInProcess =
        PineValueInProcess.Create(ElmValue.ElmRecordTypeTagNameAsValue);

    /// <summary>
    /// Renders an in-process value as an Elm expression string together with a flag indicating
    /// whether it must be parenthesised when used as a function-application argument (for example
    /// in a call-stack frame rendering). Used by <see cref="ElmCallStackFrame.ToString"/>.
    /// </summary>
    internal static (string rendered, bool needsParens) RenderArgumentForError(PineValueInProcess value)
    {
        if (value is ElmRecordAccessChainInProcess recordAccessChain)
        {
            return ("record-access: ." + string.Join('.', recordAccessChain.FieldNames), needsParens: true);
        }

        return ElmValue.RenderAsElmExpression(ToElm(value));
    }

    /// <summary>
    /// Renders an in-process value as an Elm expression string together with a flag indicating
    /// whether it must be parenthesised when used as a function-application argument (for example
    /// in a call-stack frame rendering). Used by <see cref="ElmCallStackFrame.ToString"/>.
    /// </summary>
    public static (string expressionString, bool needsParens) RenderAsElmExpression(PineValueInProcess valueInProcess)
    {
        if (valueInProcess is ElmClosureInProcess closure)
        {
            return ($"<closure({RenderSourceRef(closure.Source)})>", needsParens: true);
        }

        var asElmValue = ToElm(valueInProcess);

        return ElmValue.RenderAsElmExpression(asElmValue);
    }

    internal static string RenderSourceRef(ElmClosureInProcess.SourceRef sourceRef)
    {
        if (sourceRef is ElmClosureInProcess.SourceRef.Declared declared)
        {
            return $"declared:{declared.Name}";
        }

        if (sourceRef is ElmClosureInProcess.SourceRef.Lambda lambda)
        {
            return $"lambda:{lambda.LambdaExpression}";
        }

        throw new System.NotImplementedException(
            $"Unknown SourceRef type: {sourceRef.GetType().FullName}");
    }
}
