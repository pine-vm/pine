using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Internal;
using Pine.Core.PineVM;
using System.Collections.Generic;
using System.Collections.Immutable;
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
    /// state directly. It mirrors <see cref="ElmValue.ElmFunction"/> but stores its already
    /// collected arguments and captured bindings as <see cref="PineValueInProcess"/>.
    /// </para>
    /// </summary>
    internal sealed class ElmClosureInProcess : PineValueInProcess
    {
        public ElmValue.ElmFunction.SourceRef Source { get; }

        public int ParameterCount { get; }

        public IReadOnlyList<PineValueInProcess> ArgumentsAlreadyCollected { get; }

        public IReadOnlyDictionary<string, PineValueInProcess> CapturedBindings { get; }

        public DeclQualifiedName CapturedTopLevel { get; }

        public ElmClosureInProcess(
            ElmValue.ElmFunction.SourceRef source,
            int parameterCount,
            IReadOnlyList<PineValueInProcess> argumentsAlreadyCollected,
            IReadOnlyDictionary<string, PineValueInProcess> capturedBindings,
            DeclQualifiedName capturedTopLevel)
        {
            Source = source;
            ParameterCount = parameterCount;
            ArgumentsAlreadyCollected = argumentsAlreadyCollected;
            CapturedBindings = capturedBindings;
            CapturedTopLevel = capturedTopLevel;
        }

        public ElmClosureInProcess With(
            IReadOnlyList<PineValueInProcess> argumentsAlreadyCollected) =>
            new(
                Source,
                ParameterCount,
                argumentsAlreadyCollected,
                CapturedBindings,
                CapturedTopLevel);
    }

    /// <summary>
    /// Fallback in-process representation for an <see cref="ElmValue"/> that has no concrete
    /// <see cref="PineValue"/> encoding because it embeds one or more function values (for
    /// example a list, tuple or record whose elements/fields are functions). The original
    /// <see cref="ElmValue"/> is retained verbatim and recovered at the boundary via
    /// <see cref="ToElm(PineValueInProcess)"/>.
    /// </summary>
    internal sealed class ElmValueBox : PineValueInProcess
    {
        public ElmValue Value { get; }

        public ElmValueBox(ElmValue value)
        {
            Value = value;
        }
    }

    /// <summary>
    /// True for in-process values that carry no concrete <see cref="PineValue"/> backing
    /// (closures and boxed function-bearing values); such values must not be passed to the
    /// structural <see cref="PineValueInProcess"/> operations (Evaluate / AsInteger / AreEqual).
    /// </summary>
    private static bool IsOpaque(PineValueInProcess value) =>
        value is ElmClosureInProcess or ElmValueBox;

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
    /// <see cref="PineValue"/> form, falling back to <see cref="ElmValueBox"/> when the value
    /// embeds a function and therefore has no concrete encoding.
    /// </summary>
    internal static PineValueInProcess ToProcess(ElmValue value)
    {
        if (value is ElmValue.ElmFunction function)
        {
            var collected = new PineValueInProcess[function.ArgumentsAlreadyCollected.Count];

            for (var i = 0; i < collected.Length; i++)
            {
                collected[i] = ToProcess(function.ArgumentsAlreadyCollected[i]);
            }

            IReadOnlyDictionary<string, PineValueInProcess> capturedBindings;

            if (function.CapturedBindings.Count is 0)
            {
                capturedBindings = ImmutableDictionary<string, PineValueInProcess>.Empty;
            }
            else
            {
                var builder = new Dictionary<string, PineValueInProcess>(function.CapturedBindings.Count);

                foreach (var (name, bound) in function.CapturedBindings)
                {
                    builder[name] = ToProcess(bound);
                }

                capturedBindings = builder;
            }

            return
                new ElmClosureInProcess(
                    source: function.Source,
                    parameterCount: function.ParameterCount,
                    argumentsAlreadyCollected: collected,
                    capturedBindings: capturedBindings,
                    capturedTopLevel: function.CapturedTopLevel);
        }

        try
        {
            return PineValueInProcess.Create(ElmValueEncoding.ElmValueAsPineValue(value));
        }
        catch (System.NotImplementedException)
        {
            // The value embeds a function (e.g. a list/record/tuple of functions) and has no
            // concrete Pine encoding; retain it verbatim in a box.
            return new ElmValueBox(value);
        }
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
        if (value is ElmValueBox box)
        {
            return box.Value;
        }

        if (value is ElmClosureInProcess closure)
        {
            return ClosureToElm(closure, visiting);
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
    /// Converts an <see cref="ElmClosureInProcess"/> to an <see cref="ElmValue.ElmFunction"/>,
    /// breaking reference cycles via <paramref name="visiting"/>. A recursive occurrence of a
    /// closure already being expanded is represented as a function with an empty captured
    /// environment, which is a faithful finite rendering of the otherwise-infinite self-reference.
    /// </summary>
    private static ElmValue ClosureToElm(
        ElmClosureInProcess closure,
        HashSet<ElmClosureInProcess>? visiting)
    {
        if (visiting is not null && visiting.Contains(closure))
        {
            return
                new ElmValue.ElmFunction(
                    Source: closure.Source,
                    ParameterCount: closure.ParameterCount,
                    ArgumentsAlreadyCollected: [],
                    CapturedBindings: ImmutableDictionary<string, ElmValue>.Empty,
                    CapturedTopLevel: closure.CapturedTopLevel);
        }

        visiting ??= new HashSet<ElmClosureInProcess>(ReferenceEqualityComparer.Instance);

        visiting.Add(closure);

        try
        {
            var collected = new ElmValue[closure.ArgumentsAlreadyCollected.Count];

            for (var i = 0; i < collected.Length; i++)
            {
                collected[i] = ToElm(closure.ArgumentsAlreadyCollected[i], visiting);
            }

            IReadOnlyDictionary<string, ElmValue> capturedBindings;

            if (closure.CapturedBindings.Count is 0)
            {
                capturedBindings = ImmutableDictionary<string, ElmValue>.Empty;
            }
            else
            {
                var builder = new Dictionary<string, ElmValue>(closure.CapturedBindings.Count);

                foreach (var (name, bound) in closure.CapturedBindings)
                {
                    builder[name] = ToElm(bound, visiting);
                }

                capturedBindings = builder;
            }

            return
                new ElmValue.ElmFunction(
                    Source: closure.Source,
                    ParameterCount: closure.ParameterCount,
                    ArgumentsAlreadyCollected: collected,
                    CapturedBindings: capturedBindings,
                    CapturedTopLevel: closure.CapturedTopLevel);
        }
        finally
        {
            visiting.Remove(closure);
        }
    }

    /// <summary>
    /// Reconstructs an <see cref="ElmValue"/> from an in-process value that embeds one or more
    /// opaque (function-bearing) child values and therefore has no concrete <see cref="PineValue"/>
    /// encoding. Only the structural Elm forms that can carry functions are possible here: a list, a
    /// tuple (encoded as a list), a record or a custom-type tag. String/Float/Bytes/Int values carry
    /// only concrete leaves and never reach this method. The decoding mirrors
    /// <see cref="ElmValueEncoding.PineListValueAsElmValue"/> but recurses through <see cref="ToElm"/>
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
    /// Converts a list of <see cref="ElmValue"/> arguments to their in-process representation.
    /// </summary>
    internal static PineValueInProcess[] ToProcessList(IReadOnlyList<ElmValue> values)
    {
        var result = new PineValueInProcess[values.Count];

        for (var i = 0; i < result.Length; i++)
        {
            result[i] = ToProcess(values[i]);
        }

        return result;
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
    /// Structural equality between two in-process values, accounting for opaque values (closures and
    /// boxed function-bearing values, possibly nested within lists/tuples/records/tags) which the
    /// general <see cref="PineValueInProcess.AreEqual(PineValueInProcess, PineValueInProcess)"/>
    /// cannot evaluate.
    /// </summary>
    private static bool ValuesEqual(PineValueInProcess left, PineValueInProcess right)
    {
        if (ReferenceEquals(left, right))
            return true;

        var leftContainsOpaque = ContainsOpaque(left);
        var rightContainsOpaque = ContainsOpaque(right);

        if (leftContainsOpaque || rightContainsOpaque)
        {
            // A value that embeds a function can only equal another value that embeds a function in
            // the same positions; comparing structurally via the ElmValue projection handles both.
            if (leftContainsOpaque != rightContainsOpaque)
                return false;

            return ToElm(left).Equals(ToElm(right));
        }

        return PineValueInProcess.AreEqual(left, right);
    }

    /// <summary>
    /// Builds an in-process integer value.
    /// </summary>
    private static PineValueInProcess MakeInteger(BigInteger value) =>
        PineValueInProcess.CreateInteger(value);

    /// <summary>
    /// Builds an in-process Elm record value from already-evaluated field values.
    /// </summary>
    private static PineValueInProcess BuildRecordValue(
        IReadOnlyList<(string FieldName, PineValueInProcess Value)> fields)
    {
        var elmFields = new (string FieldName, ElmValue Value)[fields.Count];

        for (var i = 0; i < elmFields.Length; i++)
        {
            elmFields[i] = (fields[i].FieldName, ToElm(fields[i].Value));
        }

        return ToProcess(MakeElmRecord(elmFields));
    }

    /// <summary>
    /// Renders an in-process value as an Elm expression string for runtime-error messages.
    /// </summary>
    private static string RenderValueForError(PineValueInProcess value) =>
        ElmValue.RenderAsElmExpression(ToElm(value)).expressionString;
}
