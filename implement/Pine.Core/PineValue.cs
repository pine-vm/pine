using Pine.ElmInteractive;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Linq;

namespace Pine;

/// <summary>
/// The <see cref="PineValue"/> type describes values processed by Pine programs.
/// It is a choice type with two cases, <see cref="ListValue"/> and <see cref="BlobValue"/>.
/// <para/>
/// Other kinds of data, like text, images, or a file system directory, are encoded based on these primitives.
/// For conversion between the generic value type and common data types, see <see cref="PineValueAsInteger"/>, <see cref="PineValueAsString"/>, and <see cref="PineValueComposition"/>.
/// <para/>
/// There is also a standard representation of program code and expressions as Pine values, 
/// and you can see a reference implementation of this encoding at <see cref="PineVM.PineVM.ParseExpressionFromValue"/>.
/// </summary>
public abstract record PineValue : IEquatable<PineValue>
{
    /*
     * C# 12 does not support mapping from collection expression to ReadOnlyMemory<byte>,
     * therefore provide an additional overload.
     * */
    public static PineValue Blob(byte[] bytes) =>
        Blob((ReadOnlyMemory<byte>)bytes);

    public static PineValue Blob(ReadOnlyMemory<byte> bytes) =>
        bytes.Length is 0
        ?
        EmptyBlob
        :
        bytes.Length is 1
        ?
        InternedBlobSingle[bytes.Span[0]]
        :
        bytes.Length is 2
        ?
        InternedBlobTuple[bytes.Span[0] * 256 + bytes.Span[1]]
        :
        new BlobValue(bytes);

    public static PineValue List(IReadOnlyList<PineValue> elements)
    {
        if (elements.Count is 0)
            return EmptyList;

        var newInstance = new ListValue(elements);

        if (InternedListsDict?.TryGetValue(newInstance, out var interned) ?? false)
            return interned;

        return newInstance;
    }

    public static readonly PineValue EmptyList = new ListValue([]);

    public static readonly PineValue EmptyBlob = new BlobValue(ReadOnlyMemory<byte>.Empty);

    private static readonly PineValue[] InternedBlobSingle =
        [..Enumerable.Range(0, 256)
        .Select(i => new BlobValue(new byte[] { (byte)i }))];

    private static readonly PineValue[] InternedBlobTuple =
        [..Enumerable.Range(0, 256)
        .SelectMany(i => Enumerable.Range(0, 256).Select(j => new BlobValue(new byte[] { (byte)i, (byte)j })))];

    private static readonly PineValue[] InternedBlobs =
        [EmptyBlob, .. InternedBlobSingle, .. InternedBlobTuple];

    private static readonly ListValue Interned_Pine_string_String = (ListValue)PineValueAsString.ValueFromString("String");

    private static readonly IEnumerable<string> InternedStrings =
        [
        "Bool",
        "True",
        "False",

        "EQ",
        "LT",
        "GT",

        "Nothing",
        "Just",
        "Err",
        "Ok",

        "Basics",
        "List",
        "Maybe",
        "Result",
        "Tuple",
        "Char",
        "String",
        "Platform",
        "Array",
        "Dict",
        "Set",
        "Json",
        "Regex",
        "Time",
        "Debug",
        "Process",


        // From the Elm core Dict module
        "Red",
        "Black",
        "RBNode_elm_builtin",
        "RBEmpty_elm_builtin",

        // From the Pine module
        "Pine",
        "Value",
        "ListValue",
        "BlobValue",

        "Expression",
        "LiteralExpression",
        "ListExpression",
        "ParseAndEvalExpression",
        "ConditionalExpression",
        "FunctionExpression",
        "KernelApplicationExpression",
        "StringTagExpression",

        "Literal",
        "List",
        "ParseAndEval",
        "Conditional",
        "Environment",
        "Function",
        "KernelApplication",
        "StringTag",

        "equal",
        "length",
        "list_head",
        "skip",
        "take",
        "reverse",
        "negate",
        "concat",
        "add_int",
        "mul_int",
        "is_sorted_ascending_int",

        "Literal",
        "List",
        "ParseAndEval",
        "Conditional",
        "Environment",
        "Function",
        "KernelApplication",
        "StringTag",

        "functionName",
        "argument",
        "condition",
        "ifTrue",
        "ifFalse",
        "environment",
        "function",
        "expression",

        // From the FirCompiler module
        "FirCompiler",
        "FunctionApplicationExpression",
        "ReferenceExpression",
        "DeclarationBlockExpression",
        "PineFunctionApplicationExpression",

        "Deconstruction",
        "ListItemDeconstruction",
        "SkipItemsDeconstruction",
        "PineFunctionApplicationDeconstruction",

        "FunctionEnvironment",
        "LocalEnvironment",
        "ImportedEnvironment",
        "IndependentEnvironment",


        // From the Elm syntax library
        "Module",
        "File",
        "Import",

        "Declaration",
        "FunctionDeclaration",
        "AliasDeclaration",
        "CustomTypeDeclaration",
        "PortDeclaration",
        "InfixDeclaration",
        "Destructuring",

        ];

    private static readonly IReadOnlyList<ListValue> InternedStringsLists =
        [..InternedStrings
        .Distinct()
        .Select(s => new ListValue(PineValueAsString.ListValueFromString(s)))];

    private static readonly IReadOnlyList<ListValue> InternedElmStringsLists =
        [..InternedStringsLists
        .Distinct()
        .Select(s => new ListValue([Interned_Pine_string_String, s]))];

    private static readonly IReadOnlyList<ListValue> InternedBlobsInCompiler =
        [..InternedBlobs
        .Select(selector: b =>
        (ListValue)
        ElmValueEncoding.ElmValueAsPineValue(
            ElmValueInterop.PineValueEncodedAsInElmCompiler(b)))
        ];

    private static readonly IReadOnlyList<ListValue> InternedListsInCompiler =
        [..InternedStringsLists.Prepend(EmptyList)
        .Distinct()
        .Select(selector: s =>
        (ListValue)
        ElmValueEncoding.ElmValueAsPineValue(
            ElmValueInterop.PineValueEncodedAsInElmCompiler(s)))];

    private static readonly IReadOnlyList<ListValue> InternedLists =
        [
        Interned_Pine_string_String,
        ..InternedStringsLists,
        ..InternedElmStringsLists,
        ..InternedBlobsInCompiler,
        ..InternedListsInCompiler
        ];

    private static readonly FrozenDictionary<ListValue, ListValue> InternedListsDict =
        InternedLists
        .Distinct()
        .ToFrozenDictionary(
            keySelector: value => value,
            elementSelector: value => value);

    /// <summary>
    /// A <see cref="PineValue"/> that is a list of <see cref="PineValue"/>s.
    /// </summary>
    public record ListValue : PineValue, IEquatable<ListValue>
    {
        private readonly int slimHashCode;

        public IReadOnlyList<PineValue> Elements { get; }

        public ListValue(IReadOnlyList<PineValue> elements)
        {
            Elements = elements;

            var hash = new HashCode();

            foreach (var item in elements)
            {
                hash.Add(item.GetHashCode());
            }

            slimHashCode = hash.ToHashCode();
        }

        public virtual bool Equals(ListValue? other)
        {
            if (other is null)
                return false;

            if (ReferenceEquals(this, other))
                return true;

            if (slimHashCode != other.slimHashCode || Elements.Count != other.Elements.Count)
                return false;

            for (int i = 0; i < Elements.Count; i++)
            {
                var selfElement = Elements[i];
                var otherElement = other.Elements[i];

                if (selfElement is ListValue selfList && otherElement is ListValue otherList)
                {
                    if (!selfList.Equals(otherList))
                        return false;
                }
                else
                {
                    if (!selfElement.Equals(otherElement))
                        return false;
                }
            }

            return true;
        }

        public override int GetHashCode() => slimHashCode;
    }

    /// <summary>
    /// A <see cref="PineValue"/> that is a sequence of bytes.
    /// </summary>
    public record BlobValue : PineValue, IEquatable<BlobValue>
    {
        private readonly int slimHashCode;

        public ReadOnlyMemory<byte> Bytes { get; }

        public BlobValue(ReadOnlyMemory<byte> bytes)
        {
            Bytes = bytes;

            var hash = new HashCode();

            hash.AddBytes(bytes.Span);

            slimHashCode = hash.ToHashCode();
        }

        public virtual bool Equals(BlobValue? other)
        {
            if (other is null)
                return false;

            return ReferenceEquals(this, other)
                ||
                (slimHashCode == other.slimHashCode &&
                Bytes.Span.SequenceEqual(other.Bytes.Span));
        }

        public override int GetHashCode() => slimHashCode;
    }

    /// <summary>
    /// Determines whether the given value is present within a nested ListValue, considering transitive containment.
    /// </summary>
    /// <param name="pineValue">The value to search for within the nested lists.</param>
    /// <returns>
    /// Returns true if the provided PineValue is found within the nested ListValue, including in any sublists;
    /// returns false if the current instance is a BlobValue, as it does not contain any elements.
    /// </returns>
    public bool ContainsInListTransitive(PineValue pineValue) =>
        this switch
        {
            BlobValue => false,

            ListValue list =>
            list.Elements.Any(e => e.Equals(pineValue) || e.ContainsInListTransitive(pineValue)),

            _ =>
            throw new NotImplementedException()
        };
}

