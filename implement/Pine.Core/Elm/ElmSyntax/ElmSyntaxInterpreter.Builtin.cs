using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using Pine.Core.Internal;
using System.Collections.Immutable;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// Builtins for some Elm kernel and core functions.
/// <para>
/// Each entry maps a fully-qualified declaration name to a delegate that computes the
/// function's result directly on the interpreter's value model
/// (<see cref="PineValueInProcess"/>). The interpreter consults these builtins (via
/// <see cref="ApplicationResolver(System.Collections.Generic.IReadOnlyDictionary{DeclQualifiedName, System.Func{ImmutableList{PineValueInProcess}, PineValueInProcess}})"/>)
/// before falling back to the user-defined Elm declarations, so a registered builtin
/// short-circuits the recursive Elm implementation while preserving its observable
/// semantics. This keeps the dispatch general enough to add further builtins (for example
/// for equality checks, <c>Dict.get</c>, <c>List.member</c>, or <c>String.split</c>) as
/// runtime-efficiency optimizations.
/// </para>
/// </summary>
public partial class ElmSyntaxInterpreter
{
    private static readonly ImmutableDictionary<DeclQualifiedName, System.Func<ImmutableList<PineValueInProcess>, PineValueInProcess?>> s_builtinFunctionResolvers =
        BuildBuiltinFunctionResolvers();

    private static ImmutableDictionary<DeclQualifiedName, System.Func<ImmutableList<PineValueInProcess>, PineValueInProcess?>> BuildBuiltinFunctionResolvers()
    {
        var builder =
            ImmutableDictionary.CreateBuilder<DeclQualifiedName, System.Func<ImmutableList<PineValueInProcess>, PineValueInProcess?>>();

        builder.Add(
            DeclQualifiedName.Create(["Basics"], "compare"),
            ResolveBasicsCompare);

        builder.Add(
            DeclQualifiedName.Create(["Basics"], "eq"),
            ResolveBasicsEq);

        builder.Add(
            DeclQualifiedName.Create(["Basics"], "neq"),
            ResolveBasicsNeq);

        builder.Add(
            DeclQualifiedName.Create(["Basics"], "idiv"),
            ResolveBasicsIdiv);

        builder.Add(
            DeclQualifiedName.Create(["Basics"], "mul"),
            ResolveBasicsMul);

        builder.Add(
            DeclQualifiedName.Create(["Basics"], "modBy"),
            ResolveBasicsModBy);

        builder.Add(
            DeclQualifiedName.Create(["Json", "Decode"], "parseValue"),
            ResolveJsonDecodeParseValue);

        builder.Add(
            DeclQualifiedName.Create(["String"], "split"),
            ResolveStringSplit);

        builder.Add(
            DeclQualifiedName.Create(["String"], "join"),
            ResolveStringJoin);

        builder.Add(
            DeclQualifiedName.Create(["String"], "toList"),
            ResolveStringToList);

        builder.Add(
            DeclQualifiedName.Create(["String"], "fromList"),
            ResolveStringFromList);

        builder.Add(
            DeclQualifiedName.Create(["String"], "reverse"),
            ResolveStringReverse);

        builder.Add(
            DeclQualifiedName.Create(["String"], "toInt"),
            ResolveStringToInt);

        builder.Add(
            DeclQualifiedName.Create(["String"], "fromInt"),
            ResolveStringFromInt);

        builder.Add(
            DeclQualifiedName.Create(["String"], "slice"),
            ResolveStringSlice);

        builder.Add(
            DeclQualifiedName.Create(["String"], "lines"),
            ResolveStringLines);

        builder.Add(
            DeclQualifiedName.Create(["String"], "contains"),
            ResolveStringContains);

        builder.Add(
            DeclQualifiedName.Create(["String"], "toFloat"),
            ResolveStringToFloat);

        builder.Add(
            DeclQualifiedName.Create(["String"], "fromFloat"),
            ResolveStringFromFloat);

        builder.Add(
            DeclQualifiedName.Create(["String"], "trimLeftCountBytesTrimmed"),
            ResolveStringTrimLeftCountBytesTrimmed);

        builder.Add(
            DeclQualifiedName.Create(["String"], "trimRightCountBytesRemaining"),
            ResolveStringTrimRightCountBytesRemaining);

        builder.Add(
            DeclQualifiedName.Create(["Dict"], "get"),
            ResolveDictGet);

        builder.Add(
            DeclQualifiedName.Create(["Dict"], "values"),
            ResolveDictValues);

        builder.Add(
            DeclQualifiedName.Create(["Dict"], "toList"),
            ResolveDictToList);

        builder.Add(
            DeclQualifiedName.Create(["Dict"], "insertHelp"),
            ResolveDictInsertHelp);

        builder.Add(
            DeclQualifiedName.Create(["Dict"], "sizeHelp"),
            ResolveDictSizeHelp);

        builder.Add(
            DeclQualifiedName.Create(["Bytes", "Encode"], "encodeCharsAsBlob"),
            ResolveBytesEncodeEncodeCharsAsBlob);

        builder.Add(
            DeclQualifiedName.Create(["Bytes", "Encode"], "encodeBlob"),
            ResolveBytesEncodeEncodeBlob);

        builder.Add(
            DeclQualifiedName.Create(["Bytes", "Decode"], "decodeBlobAsCharsRec"),
            ResolveBytesDecodeDecodeBlobAsCharsRec);

        builder.Add(
            DeclQualifiedName.Create(["Base64", "Encode"], "toBytes"),
            ResolveBase64EncodeToBytes);

        builder.Add(
            DeclQualifiedName.Create(["Base64", "Decode"], "fromBytes"),
            ResolveBase64DecodeFromBytes);

        return builder.ToImmutable();
    }

    /// <summary>
    /// Builtin implementation of <c>Basics.compare</c> directly on the interpreter's value
    /// model. The two operands are converted to <see cref="PineValue"/> and compared by
    /// <see cref="CoreBasicsPrecompiledLeaves.BasicsCompare(PineValue, PineValue)"/>, which
    /// mirrors Elm's <c>compare</c> semantics (numbers, chars, strings, lists, and tuples,
    /// including structurally nested values). Converting to <see cref="PineValue"/> keeps the
    /// implementation simple and reuses the already-validated comparison logic.
    /// <para>
    /// As in Elm, comparing values that contain functions is not supported and surfaces as a
    /// runtime exception (raised while evaluating the operands or by the comparison itself).
    /// </para>
    /// </summary>
    private static PineValueInProcess? ResolveBasicsCompare(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 2)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var left = arguments[0].Evaluate();
        var right = arguments[1].Evaluate();

        return PineValueInProcess.Create(CoreBasicsPrecompiledLeaves.BasicsCompare(left, right));
    }

    /// <summary>
    /// Builtin implementation of <c>Basics.eq</c> (the <c>==</c> operator) directly on the
    /// interpreter's value model, mirroring the recursive Elm implementation in
    /// <c>elm-kernel-modules/Basics.elm</c>.
    /// <para>
    /// Equality is <em>not</em> plain structural equality of the underlying <see cref="PineValue"/>:
    /// two <c>Dict</c> values that hold the same key/value pairs compare equal even when different
    /// insertion orders produced structurally different red-black trees (the values are normalized
    /// through an in-order traversal first). <c>Set</c> values are normalized the same way, and
    /// <c>Float</c> values are compared up to their numerator/denominator ratio.
    /// </para>
    /// <para>
    /// As in Elm, comparing values that contain functions is not supported and surfaces as a runtime
    /// exception (raised while evaluating the operands).
    /// </para>
    /// </summary>
    private static PineValueInProcess? ResolveBasicsEq(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 2)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        return
            PineValueInProcess.CreateBool(
                ElmValuesEqual(arguments[0].Evaluate(), arguments[1].Evaluate()));
    }

    /// <summary>
    /// Builtin implementation of <c>Basics.neq</c> (the <c>/=</c> operator): the negation of
    /// <see cref="ResolveBasicsEq"/>.
    /// </summary>
    private static PineValueInProcess? ResolveBasicsNeq(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 2)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        return
            PineValueInProcess.CreateBool(
                !ElmValuesEqual(arguments[0].Evaluate(), arguments[1].Evaluate()));
    }

    private static readonly PineValue s_integerOneValue =
        IntegerEncoding.EncodeSignedInteger(1);

    /// <summary>
    /// Structural Elm equality on <see cref="PineValue"/>, faithfully mirroring <c>eq</c> from
    /// <c>elm-kernel-modules/Basics.elm</c>: plain structural equality first, then float
    /// numerator/denominator comparison, and finally a length-guarded recursive comparison that
    /// normalizes <c>Dict</c> (<c>RBNode_elm_builtin</c>) and <c>Set</c> (<c>Set_elm_builtin</c>)
    /// values to insertion-order-independent lists before comparing them.
    /// </summary>
    private static bool ElmValuesEqual(PineValue a, PineValue b)
    {
        if (a == b)
        {
            return true;
        }

        if (IsElmFloat(a, out var numA, out var denomA))
        {
            return numA == b && denomA == s_integerOneValue;
        }

        if (IsElmFloat(b, out var numB, out var denomB))
        {
            return a == numB && denomB == s_integerOneValue;
        }

        // isPineBlob a: blobs that were not already equal can never be equal here.
        if (a is not PineValue.ListValue listA)
        {
            return false;
        }

        if (b is not PineValue.ListValue listB || listA.Items.Length != listB.Items.Length)
        {
            return false;
        }

        if (listA.Items.Length is 0)
        {
            // Two empty lists would have been caught by the structural equality above.
            return true;
        }

        var tagA = listA.Items.Span[0];

        if (tagA == ElmValue.ElmStringTypeTagNameAsValue)
        {
            // Two distinct strings are never equal (equal strings are caught above).
            return false;
        }

        if (tagA == ElmValue.ElmDictNotEmptyTagNameAsValue)
        {
            return DictToListPine(a) == DictToListPine(b);
        }

        if (tagA == ElmValue.ElmSetTypeTagNameAsValue)
        {
            return
                DictKeysPine(a.ValueFromPathOrEmptyList([1, 0])) ==
                DictKeysPine(b.ValueFromPathOrEmptyList([1, 0]));
        }

        // Default: element-wise recursive comparison (lists, tuples, records, custom tags).
        for (var i = 0; i < listA.Items.Length; ++i)
        {
            if (!ElmValuesEqual(listA.Items.Span[i], listB.Items.Span[i]))
            {
                return false;
            }
        }

        return true;
    }

    /// <summary>
    /// Tests whether <paramref name="value"/> is an <c>Elm_Float</c> tagged value and, if so, extracts
    /// its numerator and denominator.
    /// </summary>
    private static bool IsElmFloat(
        PineValue value,
        out PineValue numerator,
        out PineValue denominator)
    {
        numerator = PineValue.EmptyList;
        denominator = PineValue.EmptyList;

        if (value is not PineValue.ListValue list ||
            list.Items.Length is not 2 ||
            list.Items.Span[0] != ElmValue.ElmFloatTypeTagNameAsValue ||
            list.Items.Span[1] is not PineValue.ListValue tagArgs ||
            tagArgs.Items.Length is not 2)
        {
            return false;
        }

        numerator = tagArgs.Items.Span[0];
        denominator = tagArgs.Items.Span[1];

        return true;
    }

    /// <summary>
    /// In-order traversal of a <c>Dict</c> red-black tree, collecting <c>(key, value)</c> pairs in
    /// ascending key order — the normalization used by <c>eq</c> so that dicts built from different
    /// insertion orders compare equal.
    /// </summary>
    private static PineValue DictToListPine(PineValue dict)
    {
        var pairs = new System.Collections.Generic.List<PineValue>();

        CollectDictPairsPine(dict, pairs);

        return PineValue.List([.. pairs]);
    }

    private static void CollectDictPairsPine(
        PineValue dict,
        System.Collections.Generic.List<PineValue> accumulator)
    {
        if (dict is not PineValue.ListValue { Items: { Length: 2 } items } ||
            items.Span[0] != ElmValue.ElmDictNotEmptyTagNameAsValue)
        {
            // RBEmpty_elm_builtin (or any non-node) contributes nothing.
            return;
        }

        // RBNode_elm_builtin arguments: [ color, key, value, left, right ].
        if (items.Span[1] is not PineValue.ListValue { Items: { Length: 5 } nodeArgs })
        {
            return;
        }

        CollectDictPairsPine(nodeArgs.Span[3], accumulator);
        accumulator.Add(PineValue.List([nodeArgs.Span[1], nodeArgs.Span[2]]));
        CollectDictPairsPine(nodeArgs.Span[4], accumulator);
    }

    /// <summary>
    /// In-order traversal of a <c>Dict</c> red-black tree, collecting keys in ascending order — the
    /// normalization used by <c>eq</c> for <c>Set</c> values.
    /// </summary>
    private static PineValue DictKeysPine(PineValue dict)
    {
        var keys = new System.Collections.Generic.List<PineValue>();

        CollectDictKeysPine(dict, keys);

        return PineValue.List([.. keys]);
    }

    private static void CollectDictKeysPine(
        PineValue dict,
        System.Collections.Generic.List<PineValue> accumulator)
    {
        if (dict is not PineValue.ListValue { Items: { Length: 2 } items } ||
            items.Span[0] != ElmValue.ElmDictNotEmptyTagNameAsValue)
        {
            return;
        }

        if (items.Span[1] is not PineValue.ListValue { Items: { Length: 5 } nodeArgs })
        {
            return;
        }

        CollectDictKeysPine(nodeArgs.Span[3], accumulator);
        accumulator.Add(nodeArgs.Span[1]);
        CollectDictKeysPine(nodeArgs.Span[4], accumulator);
    }

    /// <summary>
    /// Builtin implementation of <c>Basics.idiv</c> (the <c>//</c> operator): integer division that
    /// truncates toward zero, mirroring <c>elm-kernel-modules/Basics.elm</c>. Division by zero yields
    /// <c>0</c>, as in Elm.
    /// </summary>
    private static PineValueInProcess? ResolveBasicsIdiv(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 2)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(arguments[0].Evaluate()).IsOkOrNullable() is not { } dividend ||
            IntegerEncoding.ParseSignedIntegerRelaxed(arguments[1].Evaluate()).IsOkOrNullable() is not { } divisor)
        {
            // Operands are not plain integers: defer to the user-defined implementation.
            return null;
        }

        if (divisor.IsZero)
        {
            return PineValueInProcess.CreateInteger(0);
        }

        // BigInteger division truncates toward zero, matching Elm's idiv.
        return PineValueInProcess.CreateInteger(dividend / divisor);
    }

    /// <summary>
    /// Builtin implementation of <c>Basics.mul</c> (the <c>*</c> operator) for integer operands,
    /// mirroring <c>elm-kernel-modules/Basics.elm</c>. When either operand is not a plain integer (for
    /// example a <c>Float</c>), the builtin defers to the user-defined implementation, which handles
    /// the float cases.
    /// </summary>
    private static PineValueInProcess? ResolveBasicsMul(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 2)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var left = arguments[0].Evaluate();
        var right = arguments[1].Evaluate();

        if (left is not PineValue.BlobValue ||
            right is not PineValue.BlobValue ||
            IntegerEncoding.ParseSignedIntegerRelaxed(left).IsOkOrNullable() is not { } leftInteger ||
            IntegerEncoding.ParseSignedIntegerRelaxed(right).IsOkOrNullable() is not { } rightInteger)
        {
            // At least one operand is not a plain integer (for example a Float): defer.
            return null;
        }

        return PineValueInProcess.CreateInteger(leftInteger * rightInteger);
    }

    /// <summary>
    /// Builtin implementation of <c>Basics.modBy</c> directly on the interpreter's value model,
    /// mirroring the recursive Elm implementation in <c>elm-kernel-modules/Basics.elm</c>
    /// (<c>modBy divisor dividend</c>). The modulo always carries the sign of the divisor, so it
    /// differs from <c>remainderBy</c> for operands of opposite sign. When either operand is not a
    /// plain integer the builtin defers to the user-defined implementation.
    /// </summary>
    private static PineValueInProcess? ResolveBasicsModBy(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 2)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var divisorValue = arguments[0].Evaluate();
        var dividendValue = arguments[1].Evaluate();

        if (divisorValue is not PineValue.BlobValue ||
            dividendValue is not PineValue.BlobValue ||
            IntegerEncoding.ParseSignedIntegerRelaxed(divisorValue).IsOkOrNullable() is not { } divisor ||
            IntegerEncoding.ParseSignedIntegerRelaxed(dividendValue).IsOkOrNullable() is not { } dividend)
        {
            // At least one operand is not a plain integer: defer.
            return null;
        }

        // modBy 1 _ == 0 (matches the explicit guard in the Elm implementation).
        if (divisor == System.Numerics.BigInteger.One)
        {
            return PineValueInProcess.CreateInteger(0);
        }

        // remainderBy divisor dividend == dividend - divisor * idiv(dividend, divisor).
        // idiv yields 0 when the divisor is zero, so remainderBy then equals the dividend.
        var remainder =
            divisor.IsZero
            ?
            dividend
            :
            // BigInteger division truncates toward zero, matching Elm's idiv.
            dividend - divisor * (dividend / divisor);

        // modBy returns a non-negative-toward-divisor result: add the divisor back when the
        // remainder fell on the opposite side of zero.
        if (remainder.Sign >= 0)
        {
            return PineValueInProcess.CreateInteger(remainder);
        }

        return PineValueInProcess.CreateInteger(remainder + divisor);
    }

    // Tag name of the Elm 'String' wrapper from elm-kernel-modules/String.elm.
    private static readonly PineValueInProcess s_stringTagName =
        PineValueInProcess.Create(ElmValue.ElmStringTypeTagNameAsValue);

    /// <summary>
    /// Extracts the raw UTF-32 characters blob from an Elm <c>String</c> value
    /// (<c>String charsBlob</c>), or throws when the value is not a well-formed string.
    /// </summary>
    private static System.ReadOnlyMemory<byte> AsStringCharsBytes(PineValue value, string operationName)
    {
        if (value is PineValue.ListValue { Items: { Length: 2 } items } &&
            items.Span[0] == ElmValue.ElmStringTypeTagNameAsValue &&
            items.Span[1] is PineValue.ListValue { Items: { Length: 1 } charsArg })
        {
            return charsArg.Span[0] switch
            {
                PineValue.BlobValue blob => blob.Bytes,

                _ when charsArg.Span[0] == PineValue.EmptyList =>
                System.ReadOnlyMemory<byte>.Empty,

                _ =>
                throw new System.InvalidOperationException(
                    operationName + ": expected the String characters to be a blob."),
            };
        }

        throw new System.InvalidOperationException(
            operationName + ": expected an Elm String value.");
    }

    /// <summary>Builds an Elm <c>String</c> value (<c>String charsBlob</c>) from a UTF-32 bytes blob.</summary>
    private static PineValueInProcess MakeElmString(System.ReadOnlyMemory<byte> charsBytes) =>
        PineValueInProcess.CreateTagged(
            s_stringTagName,
            [PineValueInProcess.Create(PineValue.Blob(charsBytes))]);

    /// <summary>
    /// Builtin implementation of <c>String.toList</c> directly on the interpreter's value model,
    /// mirroring <c>elm-kernel-modules/String.elm</c>: splits the UTF-32 characters blob into one
    /// four-byte <c>Char</c> per code point.
    /// </summary>
    private static PineValueInProcess? ResolveStringToList(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 1)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var charsBytes = AsStringCharsBytes(arguments[0].Evaluate(), "String.toList");

        var chars = new System.Collections.Generic.List<PineValueInProcess>(charsBytes.Length / 4);

        for (var offset = 0; offset + 4 <= charsBytes.Length; offset += 4)
        {
            chars.Add(PineValueInProcess.Create(PineValue.Blob(charsBytes.Slice(offset, 4))));
        }

        return PineValueInProcess.CreateList(chars);
    }

    /// <summary>
    /// Builtin implementation of <c>String.fromList</c> directly on the interpreter's value model,
    /// mirroring <c>elm-kernel-modules/String.elm</c>: concatenates the four-byte <c>Char</c> blobs
    /// into a single Elm <c>String</c>.
    /// </summary>
    private static PineValueInProcess? ResolveStringFromList(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 1)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var charItems = AsListItems(arguments[0]);

        if (charItems is null)
        {
            throw new System.InvalidOperationException(
                "String.fromList: expected a list of characters.");
        }

        var builder = new System.IO.MemoryStream();

        foreach (var charItem in charItems)
        {
            if (charItem.Evaluate() is not PineValue.BlobValue charBlob)
            {
                throw new System.InvalidOperationException(
                    "String.fromList: expected each character to be a blob.");
            }

            builder.Write(charBlob.Bytes.Span);
        }

        return MakeElmString(builder.ToArray());
    }

    /// <summary>
    /// Builtin implementation of <c>String.reverse</c> directly on the interpreter's value model,
    /// mirroring <c>elm-kernel-modules/String.elm</c>: reverses the order of the four-byte code points
    /// in the characters blob.
    /// </summary>
    private static PineValueInProcess? ResolveStringReverse(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 1)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var charsBytes = AsStringCharsBytes(arguments[0].Evaluate(), "String.reverse").Span;

        var reversed = new byte[charsBytes.Length];

        var charCount = charsBytes.Length / 4;

        for (var i = 0; i < charCount; ++i)
        {
            charsBytes.Slice(i * 4, 4).CopyTo(System.MemoryExtensions.AsSpan(reversed, (charCount - 1 - i) * 4));
        }

        return MakeElmString(reversed);
    }

    /// <summary>
    /// Builtin implementation of <c>String.split</c> directly on the interpreter's value model,
    /// mirroring <c>elm-kernel-modules/String.elm</c>: splits the string at every occurrence of the
    /// separator (keeping empty segments). An empty separator yields the list of single-character
    /// strings.
    /// </summary>
    private static PineValueInProcess? ResolveStringSplit(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 2)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var separatorBytes = AsStringCharsBytes(arguments[0].Evaluate(), "String.split");
        var stringBytes = AsStringCharsBytes(arguments[1].Evaluate(), "String.split");

        var segments = new System.Collections.Generic.List<PineValueInProcess>();

        if (separatorBytes.Length is 0)
        {
            // Empty separator: one single-character string per code point.
            for (var offset = 0; offset + 4 <= stringBytes.Length; offset += 4)
            {
                segments.Add(MakeElmString(stringBytes.Slice(offset, 4)));
            }

            // An empty string still yields a single empty segment in Elm.
            if (stringBytes.Length is 0)
            {
                segments.Add(MakeElmString(System.ReadOnlyMemory<byte>.Empty));
            }

            return PineValueInProcess.CreateList(segments);
        }

        var separatorSpan = separatorBytes.Span;

        var lastStart = 0;
        var offset0 = 0;

        while (offset0 + separatorBytes.Length <= stringBytes.Length)
        {
            if (System.MemoryExtensions.SequenceEqual(stringBytes.Span.Slice(offset0, separatorBytes.Length), separatorSpan))
            {
                segments.Add(MakeElmString(stringBytes.Slice(lastStart, offset0 - lastStart)));

                offset0 += separatorBytes.Length;
                lastStart = offset0;
            }
            else
            {
                offset0 += 4;
            }
        }

        segments.Add(MakeElmString(stringBytes.Slice(lastStart)));

        return PineValueInProcess.CreateList(segments);
    }

    /// <summary>
    /// Builtin implementation of <c>String.join</c> directly on the interpreter's value model,
    /// mirroring <c>elm-kernel-modules/String.elm</c>: concatenates the chunk strings with the
    /// separator placed between consecutive chunks.
    /// </summary>
    private static PineValueInProcess? ResolveStringJoin(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 2)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var separatorBytes = AsStringCharsBytes(arguments[0].Evaluate(), "String.join");

        var chunkItems = AsListItems(arguments[1]);

        if (chunkItems is null)
        {
            throw new System.InvalidOperationException(
                "String.join: expected a list of strings.");
        }

        var builder = new System.IO.MemoryStream();

        for (var i = 0; i < chunkItems.Count; ++i)
        {
            if (0 < i)
            {
                builder.Write(separatorBytes.Span);
            }

            builder.Write(AsStringCharsBytes(chunkItems[i].Evaluate(), "String.join").Span);
        }

        return MakeElmString(builder.ToArray());
    }

    /// <summary>
    /// Builtin implementation of <c>String.fromInt</c> directly on the interpreter's value model,
    /// mirroring <c>elm-kernel-modules/String.elm</c>: renders the integer in base ten, with a leading
    /// <c>-</c> for negative values.
    /// </summary>
    private static PineValueInProcess? ResolveStringFromInt(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 1)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(arguments[0].Evaluate()).IsOkOrNullable() is not { } integer)
        {
            throw new System.InvalidOperationException(
                "String.fromInt: expected an integer.");
        }

        return
            MakeElmString(
                StringEncoding.BlobValueFromString(
                    integer.ToString(System.Globalization.CultureInfo.InvariantCulture)).Bytes);
    }

    /// <summary>
    /// Builtin implementation of <c>String.toInt</c> directly on the interpreter's value model,
    /// mirroring <c>elm-kernel-modules/String.elm</c>: parses an optionally-signed sequence of decimal
    /// digits, yielding <c>Just</c> the integer or <c>Nothing</c> for any malformed input.
    /// </summary>
    private static PineValueInProcess? ResolveStringToInt(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 1)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var charsBytes = AsStringCharsBytes(arguments[0].Evaluate(), "String.toInt").Span;

        var charCount = charsBytes.Length / 4;

        var index = 0;
        var negative = false;

        if (charCount is not 0)
        {
            var firstCodePoint = System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(charsBytes);

            if (firstCodePoint is '-')
            {
                negative = true;
                index = 1;
            }
            else if (firstCodePoint is '+')
            {
                index = 1;
            }
        }

        // At least one digit must follow the optional sign.
        if (index == charCount)
        {
            return s_maybeNothingValue;
        }

        System.Numerics.BigInteger accumulator = 0;

        for (; index < charCount; ++index)
        {
            var codePoint =
                System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(charsBytes[(index * 4)..]);

            if (codePoint is < '0' or > '9')
            {
                return s_maybeNothingValue;
            }

            accumulator = (accumulator * 10) + (int)(codePoint - '0');
        }

        if (negative)
        {
            accumulator = -accumulator;
        }

        return
            PineValueInProcess.CreateTagged(
                s_maybeJustTagNameValue,
                [PineValueInProcess.CreateInteger(accumulator)]);
    }

    /// <summary>
    /// Returns the first <paramref name="count"/> bytes of <paramref name="bytes"/>, matching the
    /// clamping behavior of the <c>Pine_kernel.take</c> primitive (negative counts yield an empty
    /// slice; counts past the end yield the whole sequence).
    /// </summary>
    private static System.ReadOnlyMemory<byte> TakeBytes(
        System.ReadOnlyMemory<byte> bytes,
        System.Numerics.BigInteger count)
    {
        if (count >= bytes.Length)
        {
            return bytes;
        }

        if (count <= 0)
        {
            return System.ReadOnlyMemory<byte>.Empty;
        }

        return bytes[..(int)count];
    }

    /// <summary>
    /// Skips the first <paramref name="count"/> bytes of <paramref name="bytes"/>, matching the
    /// clamping behavior of the <c>Pine_kernel.skip</c> primitive (non-positive counts return the
    /// whole sequence; counts past the end yield an empty slice).
    /// </summary>
    private static System.ReadOnlyMemory<byte> SkipBytes(
        System.ReadOnlyMemory<byte> bytes,
        System.Numerics.BigInteger count)
    {
        if (count <= 0)
        {
            return bytes;
        }

        if (count >= bytes.Length)
        {
            return System.ReadOnlyMemory<byte>.Empty;
        }

        return bytes[(int)count..];
    }

    /// <summary>
    /// Builtin implementation of <c>String.slice</c> directly on the interpreter's value model,
    /// mirroring <c>elm-kernel-modules/String.elm</c>: takes the substring between a start and end
    /// index, where negative indexes count from the end of the string.
    /// </summary>
    private static PineValueInProcess? ResolveStringSlice(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 3)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(arguments[0].Evaluate()).IsOkOrNullable() is not { } start ||
            IntegerEncoding.ParseSignedIntegerRelaxed(arguments[1].Evaluate()).IsOkOrNullable() is not { } end)
        {
            throw new System.InvalidOperationException(
                "String.slice: expected the start and end indexes to be integers.");
        }

        var charsBytes = AsStringCharsBytes(arguments[2].Evaluate(), "String.slice");

        if (0 <= start && start <= end)
        {
            // Both indexes are non-negative and in order: slice [start, end) directly.
            var sliceLength = end - start;

            return
                MakeElmString(
                    TakeBytes(SkipBytes(charsBytes, start * 4), sliceLength * 4));
        }

        // Resolve a (byte) index that may be relative to the end of the string.
        var length = charsBytes.Length;

        System.Numerics.BigInteger AbsoluteIndex(System.Numerics.BigInteger relativeIndex) =>
            relativeIndex < 0 ? relativeIndex + length : relativeIndex;

        var absoluteStart = AbsoluteIndex(start * 4);

        var absoluteSliceLength = AbsoluteIndex(end * 4) - absoluteStart;

        return
            MakeElmString(
                TakeBytes(SkipBytes(charsBytes, absoluteStart), absoluteSliceLength));
    }

    // Code points whose four-byte big-endian encoding terminates a line for String.lines.
    private const uint s_lineFeedCodePoint = '\n';

    private const uint s_carriageReturnCodePoint = '\r';

    /// <summary>
    /// Reads the four-byte big-endian UTF-32 code point at <paramref name="charOffset"/>, or returns
    /// <see langword="null"/> when fewer than four bytes remain (i.e. the end of the string).
    /// </summary>
    private static uint? CodePointAt(System.ReadOnlySpan<byte> charsBytes, int charOffset)
    {
        if (charOffset < 0 || charOffset + 4 > charsBytes.Length)
        {
            return null;
        }

        return System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(charsBytes[charOffset..]);
    }

    /// <summary>
    /// Builtin implementation of <c>String.lines</c> directly on the interpreter's value model,
    /// mirroring <c>elm-kernel-modules/String.elm</c>: splits the string into lines at every
    /// <c>\n</c>, <c>\r</c>, or <c>\r\n</c> sequence (the separators are removed).
    /// </summary>
    private static PineValueInProcess? ResolveStringLines(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 1)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var charsBytes = AsStringCharsBytes(arguments[0].Evaluate(), "String.lines");

        var span = charsBytes.Span;

        var lines = new System.Collections.Generic.List<PineValueInProcess>();

        var currentLineStart = 0;
        var offset = 0;

        while (true)
        {
            var nextChar = CodePointAt(span, offset);

            if (nextChar is null)
            {
                // End of input: emit the final (possibly empty) line.
                lines.Add(MakeElmString(charsBytes[currentLineStart..]));
                break;
            }

            // '\r\n' is treated as a single line separator.
            if (nextChar is s_carriageReturnCodePoint && CodePointAt(span, offset + 4) is s_lineFeedCodePoint)
            {
                lines.Add(MakeElmString(charsBytes[currentLineStart..offset]));
                offset += 8;
                currentLineStart = offset;
            }
            else if (nextChar is s_lineFeedCodePoint || nextChar is s_carriageReturnCodePoint)
            {
                lines.Add(MakeElmString(charsBytes[currentLineStart..offset]));
                offset += 4;
                currentLineStart = offset;
            }
            else
            {
                offset += 4;
            }
        }

        return PineValueInProcess.CreateList(lines);
    }

    /// <summary>
    /// Builtin implementation of <c>String.contains</c> directly on the interpreter's value model,
    /// mirroring <c>elm-kernel-modules/String.elm</c>: reports whether the second string contains the
    /// first as a substring. The empty pattern is contained in every string.
    /// </summary>
    private static PineValueInProcess? ResolveStringContains(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 2)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var patternBytes = AsStringCharsBytes(arguments[0].Evaluate(), "String.contains");
        var stringBytes = AsStringCharsBytes(arguments[1].Evaluate(), "String.contains");

        if (patternBytes.Length is 0)
        {
            return PineValueInProcess.CreateBool(true);
        }

        var patternSpan = patternBytes.Span;
        var stringSpan = stringBytes.Span;

        for (var offset = 0; offset + patternBytes.Length <= stringBytes.Length; offset += 4)
        {
            if (System.MemoryExtensions.SequenceEqual(
                stringSpan.Slice(offset, patternBytes.Length),
                patternSpan))
            {
                return PineValueInProcess.CreateBool(true);
            }
        }

        return PineValueInProcess.CreateBool(false);
    }

    /// <summary>
    /// Returns <see langword="true"/> when the given four-byte UTF-32 code point is removed by
    /// <c>String.trim</c> / <c>trimLeft</c> / <c>trimRight</c>, mirroring <c>isCharRemovedOnTrim</c>
    /// from <c>elm-kernel-modules/String.elm</c>.
    /// </summary>
    private static bool IsCharRemovedOnTrim(uint codePoint) =>
        codePoint is ' ' or '\t' or '\n' or '\r' or '\u00A0';

    /// <summary>
    /// Extracts the raw characters blob from an argument that is either a bare blob (the inner value
    /// of an Elm <c>String</c>) or the wrapping <c>String</c> value itself. The internal trim helpers
    /// are invoked with the bare blob, so both shapes are accepted defensively.
    /// </summary>
    private static System.ReadOnlyMemory<byte> AsCharsBytes(PineValue value, string operationName)
    {
        if (value is PineValue.BlobValue blob)
        {
            return blob.Bytes;
        }

        if (value == PineValue.EmptyList || value == PineValue.EmptyBlob)
        {
            return System.ReadOnlyMemory<byte>.Empty;
        }

        return AsStringCharsBytes(value, operationName);
    }

    /// <summary>
    /// Builtin implementation of <c>String.trimLeftCountBytesTrimmed</c> directly on the interpreter's
    /// value model, mirroring <c>elm-kernel-modules/String.elm</c>: counts the number of leading bytes
    /// (starting from <c>offset</c>) that belong to whitespace characters removed by <c>trim</c>.
    /// </summary>
    private static PineValueInProcess? ResolveStringTrimLeftCountBytesTrimmed(
        ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 2)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(arguments[0].Evaluate()).IsOkOrNullable() is not { } offsetBig)
        {
            throw new System.InvalidOperationException(
                "String.trimLeftCountBytesTrimmed: expected the offset to be an integer.");
        }

        var charsBytes = AsCharsBytes(arguments[1].Evaluate(), "String.trimLeftCountBytesTrimmed").Span;

        var offset = offsetBig;

        while (true)
        {
            if (CodePointAt(charsBytes, (int)offset) is not { } codePoint)
            {
                // No further character (end of the blob): stop trimming.
                return PineValueInProcess.CreateInteger(offset);
            }

            if (!IsCharRemovedOnTrim(codePoint))
            {
                return PineValueInProcess.CreateInteger(offset);
            }

            offset += 4;
        }
    }

    /// <summary>
    /// Builtin implementation of <c>String.trimRightCountBytesRemaining</c> directly on the
    /// interpreter's value model, mirroring <c>elm-kernel-modules/String.elm</c>: shrinks
    /// <c>remainingLength</c> past any trailing whitespace characters removed by <c>trim</c>.
    /// </summary>
    private static PineValueInProcess? ResolveStringTrimRightCountBytesRemaining(
        ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 2)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(arguments[0].Evaluate()).IsOkOrNullable() is not { } remainingLengthBig)
        {
            throw new System.InvalidOperationException(
                "String.trimRightCountBytesRemaining: expected the remaining length to be an integer.");
        }

        var charsBytes = AsCharsBytes(arguments[1].Evaluate(), "String.trimRightCountBytesRemaining").Span;

        var remainingLength = remainingLengthBig;

        while (true)
        {
            if (remainingLength <= 0)
            {
                return PineValueInProcess.CreateInteger(0);
            }

            if (CodePointAt(charsBytes, (int)(remainingLength - 4)) is not { } codePoint ||
                !IsCharRemovedOnTrim(codePoint))
            {
                return PineValueInProcess.CreateInteger(remainingLength);
            }

            remainingLength -= 4;
        }
    }

    /// <summary>
    /// Builtin implementation of <c>String.toFloat</c> directly on the interpreter's value model,
    /// mirroring <c>elm-kernel-modules/String.elm</c>: parses an optionally negative decimal (with
    /// optional <c>e</c>/<c>E</c> exponent) into <c>Just</c> an <c>Elm_Float</c> rational, or
    /// <c>Nothing</c> for malformed input. The produced numerator/denominator are not normalized,
    /// matching the Elm implementation byte-for-byte.
    /// </summary>
    private static PineValueInProcess? ResolveStringToFloat(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 1)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var charsBytes = AsStringCharsBytes(arguments[0].Evaluate(), "String.toFloat");

        if (charsBytes.Length is 0)
        {
            return s_maybeNothingValue;
        }

        var firstCodePoint = System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(charsBytes.Span);

        if (firstCodePoint is '-')
        {
            var (negNumerator, negDenominator) = ToRationalComponentsLessSign(charsBytes[4..]);

            if (negDenominator is null)
            {
                return s_maybeNothingValue;
            }

            return MakeJustElmFloat(-negNumerator, negDenominator.Value);
        }

        var (num, denom) = ToRationalComponentsLessSign(charsBytes);

        if (denom is null)
        {
            return s_maybeNothingValue;
        }

        return MakeJustElmFloat(num, denom.Value);
    }

    /// <summary>Wraps an <c>Elm_Float numerator denominator</c> in a <c>Just</c>.</summary>
    private static PineValueInProcess MakeJustElmFloat(
        System.Numerics.BigInteger numerator,
        System.Numerics.BigInteger denominator) =>
        PineValueInProcess.CreateTagged(
            s_maybeJustTagNameValue,
            [
                PineValueInProcess.CreateTagged(
                    s_elmFloatTagName,
                    [
                        PineValueInProcess.CreateInteger(numerator),
                        PineValueInProcess.CreateInteger(denominator),
                    ]),
            ]);

    private static readonly PineValueInProcess s_elmFloatTagName =
        PineValueInProcess.Create(ElmValue.ElmFloatTypeTagNameAsValue);

    /// <summary>
    /// Parses a sign-less decimal (with optional <c>e</c>/<c>E</c> exponent) into rational
    /// numerator/denominator components, mirroring <c>toRationalComponentsLessSign</c> from
    /// <c>elm-kernel-modules/String.elm</c>. Returns <c>(0, null)</c> to signal a parse failure.
    /// </summary>
    private static (System.Numerics.BigInteger Numerator, System.Numerics.BigInteger? Denominator) ToRationalComponentsLessSign(
        System.ReadOnlyMemory<byte> charsBytes)
    {
        var lower = ParseWithExponent(charsBytes, 'e');

        if (lower.Denominator is not null)
        {
            return lower;
        }

        var upper = ParseWithExponent(charsBytes, 'E');

        if (upper.Denominator is not null)
        {
            return upper;
        }

        return ToRationalComponentsWithoutExponent(charsBytes);
    }

    /// <summary>
    /// Implements the exponent branch of <c>toRationalComponentsLessSign</c>: splits on the given
    /// exponent character and combines the mantissa with the power-of-ten exponent.
    /// </summary>
    private static (System.Numerics.BigInteger Numerator, System.Numerics.BigInteger? Denominator) ParseWithExponent(
        System.ReadOnlyMemory<byte> charsBytes,
        uint exponentChar)
    {
        var segments = SplitOnCodePoint(charsBytes, exponentChar);

        if (segments.Count is not 2)
        {
            return (0, null);
        }

        var mantissa = segments[0];
        var exponent = segments[1];

        if (mantissa.Length is 0 || exponent.Length is 0)
        {
            return (0, null);
        }

        var mantissaComponents = ToRationalComponentsWithoutExponent(mantissa);

        if (mantissaComponents.Denominator is not { } denom)
        {
            return (0, null);
        }

        if (ParseInt(exponent) is not { } exponentInt)
        {
            return (0, null);
        }

        var exponentIsNonPositive = exponentInt <= 0;

        var exponentMagnitude = exponentIsNonPositive ? -exponentInt : exponentInt;

        var powTen = System.Numerics.BigInteger.Pow(10, (int)exponentMagnitude);

        if (exponentIsNonPositive)
        {
            return (mantissaComponents.Numerator, denom * powTen);
        }

        return (mantissaComponents.Numerator * powTen, denom);
    }

    /// <summary>
    /// Implements <c>toRationalComponentsWithoutExponent</c> from
    /// <c>elm-kernel-modules/String.elm</c>: parses an unsigned decimal with an optional fractional
    /// part into numerator/denominator components. Returns <c>(0, null)</c> on failure.
    /// </summary>
    private static (System.Numerics.BigInteger Numerator, System.Numerics.BigInteger? Denominator) ToRationalComponentsWithoutExponent(
        System.ReadOnlyMemory<byte> charsBytes)
    {
        var segments = SplitOnCodePoint(charsBytes, '.');

        if (segments.Count is 1)
        {
            if (ParseUnsignedInt(segments[0]) is not { } whole)
            {
                return (0, null);
            }

            return (whole, 1);
        }

        if (segments.Count is not 2)
        {
            return (0, null);
        }

        var beforeSep = segments[0];
        var afterSep = segments[1];

        if (afterSep.Length is 0)
        {
            if (beforeSep.Length is 0)
            {
                return (0, null);
            }

            if (ParseUnsignedInt(beforeSep) is not { } beforeOnly)
            {
                return (0, null);
            }

            return (beforeOnly, 1);
        }

        if (ParseUnsignedInt(beforeSep) is not { } beforeSepInt ||
            ParseUnsignedInt(afterSep) is not { } afterSepInt)
        {
            return (0, null);
        }

        // The denominator is 10^(number of fractional digits); the Elm code only tabulates lengths
        // up to ten fractional digits and falls back to a denominator of 1 beyond that.
        var fractionalDigits = afterSep.Length / 4;

        var denom =
            fractionalDigits is >= 1 and <= 10
            ?
            System.Numerics.BigInteger.Pow(10, fractionalDigits)
            :
            System.Numerics.BigInteger.One;

        var numerator = (beforeSepInt * denom) + afterSepInt;

        return (numerator, denom);
    }

    /// <summary>
    /// Splits the UTF-32 characters blob at every occurrence of the given (single) code point,
    /// mirroring <c>splitHelperOnBlob</c> for a one-character separator (empty segments are kept).
    /// </summary>
    private static System.Collections.Generic.List<System.ReadOnlyMemory<byte>> SplitOnCodePoint(
        System.ReadOnlyMemory<byte> charsBytes,
        uint codePoint)
    {
        var segments = new System.Collections.Generic.List<System.ReadOnlyMemory<byte>>();

        var span = charsBytes.Span;

        var lastStart = 0;

        for (var offset = 0; offset + 4 <= charsBytes.Length; offset += 4)
        {
            if (System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(span[offset..]) == codePoint)
            {
                segments.Add(charsBytes[lastStart..offset]);
                lastStart = offset + 4;
            }
        }

        segments.Add(charsBytes[lastStart..]);

        return segments;
    }

    /// <summary>
    /// Parses an optionally signed decimal integer from a UTF-32 characters blob, mirroring
    /// <c>parseInt</c> from <c>elm-kernel-modules/String.elm</c>. Returns <see langword="null"/> on
    /// malformed input.
    /// </summary>
    private static System.Numerics.BigInteger? ParseInt(System.ReadOnlyMemory<byte> charsBytes)
    {
        if (charsBytes.Length is 0)
        {
            return null;
        }

        var firstCodePoint = System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(charsBytes.Span);

        if (firstCodePoint is '-')
        {
            if (ParseUnsignedInt(charsBytes[4..]) is not { } magnitude)
            {
                return null;
            }

            return -magnitude;
        }

        if (firstCodePoint is '+')
        {
            return ParseUnsignedInt(charsBytes[4..]);
        }

        return ParseUnsignedInt(charsBytes);
    }

    /// <summary>
    /// Parses an unsigned decimal integer from a UTF-32 characters blob, mirroring
    /// <c>parseUnsignedInt</c> from <c>elm-kernel-modules/String.elm</c>: at least one decimal digit
    /// must be present and every character must be a digit. Returns <see langword="null"/> otherwise.
    /// </summary>
    private static System.Numerics.BigInteger? ParseUnsignedInt(System.ReadOnlyMemory<byte> charsBytes)
    {
        if (charsBytes.Length is 0)
        {
            return null;
        }

        var span = charsBytes.Span;

        System.Numerics.BigInteger accumulator = 0;

        for (var offset = 0; offset + 4 <= charsBytes.Length; offset += 4)
        {
            var codePoint = System.Buffers.Binary.BinaryPrimitives.ReadUInt32BigEndian(span[offset..]);

            if (codePoint is < '0' or > '9')
            {
                return null;
            }

            accumulator = (accumulator * 10) + (int)(codePoint - '0');
        }

        return accumulator;
    }

    /// <summary>
    /// Builtin implementation of <c>String.fromFloat</c> directly on the interpreter's value model,
    /// mirroring <c>elm-kernel-modules/String.elm</c>: renders an <c>Elm_Float</c> rational (or a
    /// plain integer) as a decimal string with up to sixteen fractional digits, rounding half up and
    /// trimming trailing zeros.
    /// </summary>
    private static PineValueInProcess? ResolveStringFromFloat(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 1)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var value = arguments[0].Evaluate();

        if (IsElmFloat(value, out var numeratorValue, out var denominatorValue))
        {
            if (IntegerEncoding.ParseSignedIntegerRelaxed(numeratorValue).IsOkOrNullable() is not { } numerator ||
                IntegerEncoding.ParseSignedIntegerRelaxed(denominatorValue).IsOkOrNullable() is not { } denominator)
            {
                throw new System.InvalidOperationException(
                    "String.fromFloat: expected the float components to be integers.");
            }

            return
                MakeElmString(
                    StringEncoding.BlobValueFromString(FromFloatDecimal(16, numerator, denominator)).Bytes);
        }

        // A plain integer Float reduces to String.fromInt.
        if (IntegerEncoding.ParseSignedIntegerRelaxed(value).IsOkOrNullable() is not { } integer)
        {
            throw new System.InvalidOperationException(
                "String.fromFloat: expected a Float or integer value.");
        }

        return
            MakeElmString(
                StringEncoding.BlobValueFromString(
                    integer.ToString(System.Globalization.CultureInfo.InvariantCulture)).Bytes);
    }

    /// <summary>
    /// Renders a rational number as a decimal string, mirroring <c>fromFloatDecimal</c> from
    /// <c>elm-kernel-modules/String.elm</c>.
    /// </summary>
    private static string FromFloatDecimal(
        int decimalPlacesMax,
        System.Numerics.BigInteger numerator,
        System.Numerics.BigInteger denom)
    {
        if (denom == 1)
        {
            return numerator.ToString(System.Globalization.CultureInfo.InvariantCulture);
        }

        if (denom == 0)
        {
            return numerator < 0 ? "-Infinity" : "Infinity";
        }

        var isNegative = numerator < 0;

        var signStr = isNegative ? "-" : "";

        var absNum = System.Numerics.BigInteger.Abs(numerator);

        var intPart = absNum / denom;

        var remainder = absNum % denom;

        if (remainder == 0 || decimalPlacesMax == 0)
        {
            return signStr + intPart.ToString(System.Globalization.CultureInfo.InvariantCulture);
        }

        var scale = System.Numerics.BigInteger.Pow(10, decimalPlacesMax);

        var scaledVal = remainder * scale;

        var scaledInt = scaledVal / denom;

        var leftover = scaledVal % denom;

        // Round half up: increment when twice the leftover reaches the denominator.
        var scaledIntRounded = denom <= leftover * 2 ? scaledInt + 1 : scaledInt;

        var scaledStr = scaledIntRounded.ToString(System.Globalization.CultureInfo.InvariantCulture);

        var overflowed = scale <= scaledIntRounded;

        System.Numerics.BigInteger newIntPart;
        string fractionDigits;

        if (overflowed)
        {
            // Carried into the integer part; drop the leading digit of the fractional part.
            newIntPart = intPart + 1;
            fractionDigits = scaledStr[1..];
        }
        else
        {
            // Left-pad the fractional part with zeros to the requested width.
            var neededZeros = decimalPlacesMax - scaledStr.Length;
            newIntPart = intPart;
            fractionDigits = new string('0', neededZeros < 0 ? 0 : neededZeros) + scaledStr;
        }

        var trimmedFraction = fractionDigits.TrimEnd('0');

        if (trimmedFraction.Length is 0)
        {
            return signStr + newIntPart.ToString(System.Globalization.CultureInfo.InvariantCulture);
        }

        return signStr + newIntPart.ToString(System.Globalization.CultureInfo.InvariantCulture) + "." + trimmedFraction;
    }

    // Tag-name encodings of the Dict constructors from elm-kernel-modules/Dict.elm.
    private static readonly PineValue s_dictRBNodeTagNameValue =
        StringEncoding.ValueFromString("RBNode_elm_builtin");

    private static readonly PineValue s_dictRBEmptyTagNameValue =
        StringEncoding.ValueFromString("RBEmpty_elm_builtin");

    private static readonly PineValueInProcess s_dictRBNodeTagName =
        PineValueInProcess.Create(s_dictRBNodeTagNameValue);

    // The empty red-black tree (RBEmpty_elm_builtin with no arguments).
    private static readonly PineValueInProcess s_dictRBEmpty =
        PineValueInProcess.CreateTagged(
            PineValueInProcess.Create(s_dictRBEmptyTagNameValue),
            []);

    // Tag-name encoding of the Red NColor constructor from elm-kernel-modules/Dict.elm.
    private static readonly PineValue s_dictColorRedTagNameValue =
        StringEncoding.ValueFromString("Red");

    // The NColor constructors, encoded as no-argument tagged values.
    private static readonly PineValueInProcess s_dictColorRed =
        PineValueInProcess.CreateTagged(
            PineValueInProcess.Create(s_dictColorRedTagNameValue),
            []);

    private static readonly PineValueInProcess s_dictColorBlack =
        PineValueInProcess.CreateTagged(
            PineValueInProcess.Create(StringEncoding.ValueFromString("Black")),
            []);


    // Order tags returned by Basics.compare, mirrored from CoreBasicsPrecompiledLeaves.
    private static readonly PineValue s_orderLTValue =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("LT", []));

    private static readonly PineValue s_orderGTValue =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("GT", []));

    private static readonly PineValueInProcess s_maybeJustTagNameValue =
        PineValueInProcess.Create(StringEncoding.ValueFromString("Just"));

    private static readonly PineValueInProcess s_maybeNothingValue =
        PineValueInProcess.CreateTagged(
            PineValueInProcess.Create(StringEncoding.ValueFromString("Nothing")),
            []);

    /// <summary>
    /// Builtin implementation of <c>Dict.get</c> directly on the interpreter's value model,
    /// mirroring the recursive Elm implementation in <c>elm-kernel-modules/Dict.elm</c>: it walks
    /// the red-black tree, comparing the target key against each node's key via
    /// <see cref="CoreBasicsPrecompiledLeaves.BasicsCompare(PineValue, PineValue)"/> and descending
    /// left (<c>LT</c>) or right (<c>GT</c>) until the key matches (<c>EQ</c>) or an empty subtree is
    /// reached.
    /// <para>
    /// Unlike a naive conversion, this implementation never calls <see cref="PineValueInProcess.Evaluate"/>
    /// on the stored <em>values</em>; only the <em>keys</em> are evaluated (to reuse the existing
    /// comparison logic). Elm's semantics do not require equality on a <c>Dict</c>'s value type, so the
    /// values may carry function closures (<see cref="ElmClosureInProcess"/>) that have no concrete
    /// <see cref="PineValue"/> encoding. The matched value is returned wrapped in <c>Just</c> exactly as
    /// it was stored; a missing key yields <c>Nothing</c>.
    /// </para>
    /// </summary>
    internal static PineValueInProcess? ResolveDictGet(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 2)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var targetKey = arguments[0].Evaluate();

        var currentDict = arguments[1];

        while (true)
        {
            var dictItems = AsListItems(currentDict);

            if (dictItems is null || dictItems.Count is not 2)
            {
                throw new System.InvalidOperationException(
                    "Dict.get: expected a Dict value (a two-element tagged value).");
            }

            var tagNameValue = dictItems[0].Evaluate();

            if (tagNameValue == s_dictRBEmptyTagNameValue)
            {
                return s_maybeNothingValue;
            }

            if (tagNameValue != s_dictRBNodeTagNameValue)
            {
                throw new System.InvalidOperationException(
                    "Dict.get: expected an RBNode_elm_builtin or RBEmpty_elm_builtin tag.");
            }

            // RBNode_elm_builtin arguments: [ color, key, value, left, right ].
            var nodeArgs = AsListItems(dictItems[1]);

            if (nodeArgs is null || nodeArgs.Count is not 5)
            {
                throw new System.InvalidOperationException(
                    "Dict.get: malformed RBNode_elm_builtin (expected 5 arguments).");
            }

            var order = CoreBasicsPrecompiledLeaves.BasicsCompare(targetKey, nodeArgs[1].Evaluate());

            if (order == s_orderLTValue)
            {
                currentDict = nodeArgs[3];
            }
            else if (order == s_orderGTValue)
            {
                currentDict = nodeArgs[4];
            }
            else
            {
                return PineValueInProcess.CreateTagged(s_maybeJustTagNameValue, [nodeArgs[2]]);
            }
        }
    }

    /// <summary>
    /// The destructured fields of a non-empty red-black tree node
    /// (<c>RBNode_elm_builtin color key value left right</c>) on the interpreter's value model.
    /// </summary>
    private readonly record struct DictNode(
        PineValueInProcess Color,
        PineValueInProcess Key,
        PineValueInProcess Value,
        PineValueInProcess Left,
        PineValueInProcess Right);

    /// <summary>
    /// Destructures an in-process <c>Dict</c> value: returns the node fields for an
    /// <c>RBNode_elm_builtin</c>, or <c>null</c> for the empty tree (<c>RBEmpty_elm_builtin</c>).
    /// Only the tag (and never the stored <em>value</em>) is forced, so nodes whose values are
    /// function closures (<see cref="ElmClosureInProcess"/>) are destructured without evaluation.
    /// </summary>
    private static DictNode? AsDictNode(PineValueInProcess dict, string operationName)
    {
        var dictItems = AsListItems(dict);

        if (dictItems is null || dictItems.Count is not 2)
        {
            throw new System.InvalidOperationException(
                operationName + ": expected a Dict value (a two-element tagged value).");
        }

        var tagNameValue = dictItems[0].Evaluate();

        if (tagNameValue == s_dictRBEmptyTagNameValue)
        {
            return null;
        }

        if (tagNameValue != s_dictRBNodeTagNameValue)
        {
            throw new System.InvalidOperationException(
                operationName + ": expected an RBNode_elm_builtin or RBEmpty_elm_builtin tag.");
        }

        // RBNode_elm_builtin arguments: [ color, key, value, left, right ].
        var nodeArgs = AsListItems(dictItems[1]);

        if (nodeArgs is null || nodeArgs.Count is not 5)
        {
            throw new System.InvalidOperationException(
                operationName + ": malformed RBNode_elm_builtin (expected 5 arguments).");
        }

        return new DictNode(nodeArgs[0], nodeArgs[1], nodeArgs[2], nodeArgs[3], nodeArgs[4]);
    }

    /// <summary>
    /// Builds an <c>RBNode_elm_builtin color key value left right</c> in-process, without forcing
    /// any of the fields (so the value may be a closure).
    /// </summary>
    private static PineValueInProcess MakeDictNode(
        PineValueInProcess color,
        PineValueInProcess key,
        PineValueInProcess value,
        PineValueInProcess left,
        PineValueInProcess right) =>
        PineValueInProcess.CreateTagged(
            s_dictRBNodeTagName,
            [color, key, value, left, right]);

    /// <summary>Tests whether an in-process <c>NColor</c> value is the <c>Red</c> constructor.</summary>
    private static bool IsDictColorRed(PineValueInProcess color)
    {
        var items = AsListItems(color);

        if (items is null || items.Count < 1)
        {
            return false;
        }

        return items[0].Evaluate() == s_dictColorRedTagNameValue;
    }

    /// <summary>
    /// Builtin implementation of <c>Dict.values</c> directly on the interpreter's value model,
    /// mirroring <c>elm-kernel-modules/Dict.elm</c>: collects all values in ascending key order by an
    /// in-order traversal of the red-black tree.
    /// <para>
    /// Like <see cref="ResolveDictGet"/>, the stored <em>values</em> are never evaluated, so a
    /// <c>Dict</c> whose values are function closures (<see cref="ElmClosureInProcess"/>) is supported;
    /// each value is placed into the resulting list exactly as it was stored.
    /// </para>
    /// </summary>
    internal static PineValueInProcess? ResolveDictValues(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 1)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var collected = new System.Collections.Generic.List<PineValueInProcess>();

        CollectDictValues(arguments[0], collected);

        return PineValueInProcess.CreateList(collected);
    }

    private static void CollectDictValues(
        PineValueInProcess dict,
        System.Collections.Generic.List<PineValueInProcess> accumulator)
    {
        if (AsDictNode(dict, "Dict.values") is not { } node)
        {
            return;
        }

        CollectDictValues(node.Left, accumulator);
        accumulator.Add(node.Value);
        CollectDictValues(node.Right, accumulator);
    }

    /// <summary>
    /// Builtin implementation of <c>Dict.toList</c> directly on the interpreter's value model,
    /// mirroring <c>elm-kernel-modules/Dict.elm</c>: collects all key-value pairs (as Elm tuples,
    /// encoded as two-element lists) in ascending key order by an in-order traversal.
    /// <para>
    /// As with <see cref="ResolveDictValues"/>, the stored values are never evaluated, so values that
    /// are function closures (<see cref="ElmClosureInProcess"/>) pass through unchanged.
    /// </para>
    /// </summary>
    internal static PineValueInProcess? ResolveDictToList(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 1)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var collected = new System.Collections.Generic.List<PineValueInProcess>();

        CollectDictPairs(arguments[0], collected);

        return PineValueInProcess.CreateList(collected);
    }

    private static void CollectDictPairs(
        PineValueInProcess dict,
        System.Collections.Generic.List<PineValueInProcess> accumulator)
    {
        if (AsDictNode(dict, "Dict.toList") is not { } node)
        {
            return;
        }

        CollectDictPairs(node.Left, accumulator);
        accumulator.Add(PineValueInProcess.CreateList([node.Key, node.Value]));
        CollectDictPairs(node.Right, accumulator);
    }

    /// <summary>
    /// Builtin implementation of <c>Dict.insertHelp</c> directly on the interpreter's value model,
    /// mirroring <c>elm-kernel-modules/Dict.elm</c>: inserts the key-value pair into the red-black
    /// tree, descending by comparing the inserted key against each node's key, then rebalancing on the
    /// way back up via <see cref="BalanceDict"/>.
    /// <para>
    /// Only the <em>keys</em> are evaluated (to reuse the existing comparison logic); the inserted and
    /// existing <em>values</em> are never forced, so values that are function closures
    /// (<see cref="ElmClosureInProcess"/>) are inserted and preserved unchanged.
    /// </para>
    /// </summary>
    internal static PineValueInProcess? ResolveDictInsertHelp(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 3)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        var key = arguments[0];
        var value = arguments[1];
        var dict = arguments[2];

        return InsertHelp(key.Evaluate(), key, value, dict);
    }

    private static PineValueInProcess InsertHelp(
        PineValue keyEvaluated,
        PineValueInProcess key,
        PineValueInProcess value,
        PineValueInProcess dict)
    {
        if (AsDictNode(dict, "Dict.insertHelp") is not { } node)
        {
            // New nodes are always red; any violation is fixed when balancing.
            return MakeDictNode(s_dictColorRed, key, value, s_dictRBEmpty, s_dictRBEmpty);
        }

        var order = CoreBasicsPrecompiledLeaves.BasicsCompare(keyEvaluated, node.Key.Evaluate());

        if (order == s_orderLTValue)
        {
            return
                BalanceDict(
                    node.Color,
                    node.Key,
                    node.Value,
                    InsertHelp(keyEvaluated, key, value, node.Left),
                    node.Right);
        }

        if (order == s_orderGTValue)
        {
            return
                BalanceDict(
                    node.Color,
                    node.Key,
                    node.Value,
                    node.Left,
                    InsertHelp(keyEvaluated, key, value, node.Right));
        }

        // EQ: replace the value, keeping the existing node's key, color, and children.
        return MakeDictNode(node.Color, node.Key, value, node.Left, node.Right);
    }

    /// <summary>
    /// In-process port of the <c>balance</c> helper from <c>elm-kernel-modules/Dict.elm</c>, factored
    /// into a dedicated method for readability. It restores the red-black tree invariants after an
    /// insertion by rotating and recoloring nodes, never forcing any stored value.
    /// </summary>
    private static PineValueInProcess BalanceDict(
        PineValueInProcess color,
        PineValueInProcess key,
        PineValueInProcess value,
        PineValueInProcess left,
        PineValueInProcess right)
    {
        if (AsDictNode(right, "Dict.insertHelp") is { } rightNode && IsDictColorRed(rightNode.Color))
        {
            if (AsDictNode(left, "Dict.insertHelp") is { } leftRedNode && IsDictColorRed(leftRedNode.Color))
            {
                return
                    MakeDictNode(
                        s_dictColorRed,
                        key,
                        value,
                        MakeDictNode(
                            s_dictColorBlack,
                            leftRedNode.Key,
                            leftRedNode.Value,
                            leftRedNode.Left,
                            leftRedNode.Right),
                        MakeDictNode(
                            s_dictColorBlack,
                            rightNode.Key,
                            rightNode.Value,
                            rightNode.Left,
                            rightNode.Right));
            }

            return
                MakeDictNode(
                    color,
                    rightNode.Key,
                    rightNode.Value,
                    MakeDictNode(s_dictColorRed, key, value, left, rightNode.Left),
                    rightNode.Right);
        }

        if (AsDictNode(left, "Dict.insertHelp") is { } leftNode &&
            IsDictColorRed(leftNode.Color) &&
            AsDictNode(leftNode.Left, "Dict.insertHelp") is { } leftLeftNode &&
            IsDictColorRed(leftLeftNode.Color))
        {
            return
                MakeDictNode(
                    s_dictColorRed,
                    leftNode.Key,
                    leftNode.Value,
                    MakeDictNode(
                        s_dictColorBlack,
                        leftLeftNode.Key,
                        leftLeftNode.Value,
                        leftLeftNode.Left,
                        leftLeftNode.Right),
                    MakeDictNode(s_dictColorBlack, key, value, leftNode.Right, right));
        }

        return MakeDictNode(color, key, value, left, right);
    }

    /// <summary>
    /// Builtin implementation of <c>Dict.sizeHelp</c> directly on the interpreter's value model,
    /// mirroring <c>elm-kernel-modules/Dict.elm</c>: returns the accumulator plus the number of
    /// key-value pairs in the dictionary.
    /// <para>
    /// Only the tree structure is inspected; the stored values are never evaluated, so a <c>Dict</c>
    /// whose values are function closures (<see cref="ElmClosureInProcess"/>) is counted without
    /// failing.
    /// </para>
    /// </summary>
    internal static PineValueInProcess? ResolveDictSizeHelp(ImmutableList<PineValueInProcess> arguments)
    {
        if (arguments.Count is not 2)
        {
            // Defer to regular currying / the user-defined implementation when not saturated.
            return null;
        }

        if (IntegerEncoding.ParseSignedIntegerRelaxed(arguments[0].Evaluate()).IsOkOrNullable() is not { } accumulator)
        {
            throw new System.InvalidOperationException(
                "Dict.sizeHelp: expected an integer accumulator.");
        }

        return PineValueInProcess.CreateInteger(accumulator + CountDictNodes(arguments[1]));
    }

    private static System.Numerics.BigInteger CountDictNodes(PineValueInProcess dict)
    {
        System.Numerics.BigInteger count = 0;

        var pending = new System.Collections.Generic.Stack<PineValueInProcess>();

        pending.Push(dict);

        while (pending.Count > 0)
        {
            if (AsDictNode(pending.Pop(), "Dict.sizeHelp") is not { } node)
            {
                continue;
            }

            count += 1;

            pending.Push(node.Left);
            pending.Push(node.Right);
        }

        return count;
    }
}
