using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Reflection;
using Ast = Pine.Core.Elm.Syntax.Abstract.Expression;
using OriginalPine = AllocationBaseline.ValueEncodingBinaryDeterministic;
using OriginalSyntax = AllocationBaseline.ElmSyntaxAbstractBinary;

if (args.Contains("--emit-hashes"))
{
    var constructorType = typeof(ElmSyntaxAbstractBinary).GetNestedType("Constructor", BindingFlags.NonPublic)!;
    var method = typeof(ElmSyntaxAbstractBinary).GetMethod("CanonicalConstructor", BindingFlags.Static | BindingFlags.NonPublic)!;
    var hashes = Enum.GetValues(constructorType).Cast<object>().Select(constructor =>
    {
        var expression = (Pine.Core.PineLanguage.Expression)method.Invoke(null, [constructor])!;
        return (Constructor: constructor, Hash: Pine.Core.Addressing.PineValueHashTree.ComputeHash(
            ExpressionEncoding.EncodeExpressionAsValue(expression)));
    }).ToArray();
    Console.WriteLine("using System;\n\nnamespace Pine.Core.Elm.Syntax.Abstract;\n\npublic static partial class ElmSyntaxAbstractBinary\n{");
    Console.WriteLine("    // Generated from CanonicalConstructor; the general-Pine oracle verifies every hash.");
    Console.WriteLine("    private static ReadOnlySpan<byte> ConstructorHashBytes =>\n    [");
    foreach (var (constructor, hash) in hashes)
    {
        Console.WriteLine("        // " + constructor);
        for (var offset = 0; offset < 32; offset += 16)
            Console.WriteLine("        " + string.Join(", ", hash.Slice(offset, 16).ToArray().Select(b => $"0x{b:x2}")) + ",");
    }
    Console.WriteLine("    ];\n}");
    return;
}

static (long Total, long Minimum, long Maximum) Measure(Action action, int repetitions = 256, bool warm = true)
{
    if (warm)
        for (var index = 0; index < 64; ++index)
            action();

    long total = 0, minimum = long.MaxValue, maximum = 0;
    for (var index = 0; index < repetitions; ++index)
    {
        var before = GC.GetAllocatedBytesForCurrentThread();
        action();
        var allocated = GC.GetAllocatedBytesForCurrentThread() - before;
        total += allocated;
        minimum = Math.Min(minimum, allocated);
        maximum = Math.Max(maximum, allocated);
    }
    return (total, minimum, maximum);
}

static byte[] Bytes(Action<Stream> encode)
{
    using var stream = new MemoryStream();
    encode(stream);
    return stream.ToArray();
}

static Ast Int(BigInteger i) => new Ast.IntegerLiteral(i, IntegerEncoding.EncodeSignedInteger(i));
Action<ReadOnlySpan<byte>> discard = static _ => { };

var pine = new Dictionary<string, PineValue>
{
    ["empty-list"] = PineValue.EmptyList,
    ["blob-1MB"] = PineValue.Blob(new byte[1_000_000]),
    ["mixed"] = PineValue.List([
        IntegerEncoding.EncodeSignedInteger(123456), StringEncoding.ValueFromString("Acme.Tree"),
        PineValue.List([PineValue.EmptyBlob, PineValue.EmptyList])]),
    ["wide-1024"] = PineValue.List(Enumerable.Range(0, 1024).Select(i => IntegerEncoding.EncodeSignedInteger(100000 + i)).ToArray()),
};
PineValue nested = PineValue.EmptyList;
for (var index = 0; index < 128; ++index)
    nested = PineValue.List([nested]);
pine["depth-128"] = nested;
pine["shared-1024"] = PineValue.List(Enumerable.Repeat(pine["mixed"], 1024).ToArray());
pine["distinct-shared-1024"] = PineValue.List(
    ((PineValue.ListValue)pine["wide-1024"]).Items.ToArray()
    .Concat(((PineValue.ListValue)pine["wide-1024"]).Items.ToArray().Reverse()).ToArray());

var ast = new Dictionary<string, Ast>
{
    ["unit"] = Ast.UnitExpr.Instance,
    ["application"] = new Ast.Application(Ast.Identifier.Create(["Acme", "Tree"], "f"), [Int(1), Int(2), Int(3)]),
    ["record"] = new Ast.RecordExpr([
        new("a", StringEncoding.ValueFromString("a"), Int(123456)),
        new("b", StringEncoding.ValueFromString("b"), Ast.StringLiteral.Create("escaped\ntext\u00e9\U0001f600"))]),
    ["payload-1MB"] = new Ast.TupledExpression([new Ast.GLSLExpression(new string('x', 1_000_000)), Int(BigInteger.One << 80000)]),
    ["wide-1024"] = new Ast.ListExpr(Enumerable.Range(0, 1024).Select(i => Int(100000 + i)).ToArray()),
};
ast["shared-1024"] = new Ast.ListExpr(Enumerable.Repeat(ast["application"], 1024).ToArray());
ast["distinct-shared-1024"] = new Ast.ListExpr(
    ((Ast.ListExpr)ast["wide-1024"]).Elements.Concat(((Ast.ListExpr)ast["wide-1024"]).Elements.Reverse()).ToArray());
Ast deep = Ast.UnitExpr.Instance;
for (var index = 0; index < 128; ++index)
    deep = new Ast.Negation(deep);
ast["depth-128"] = deep;

if (args.Contains("--encoder-tables"))
{
    var wide = (Ast.ListExpr)ast["wide-1024"];
    Action fill = () =>
    {
        using var encoder = new ValueEncodingBinaryDeterministic.TreeEncoder<object>(discard, reuseReferences: true);
        encoder.TryReference(wide);
        for (var index = 0; index < wide.Elements.Count; ++index)
        {
            var expression = wide.Elements[index];
            encoder.TryReference(expression);
            encoder.TryBlobReference(((Ast.IntegerLiteral)expression).ValueAsPineValue);
        }
    };
    var first = Measure(fill, repetitions: 1, warm: false);
    var warmed = Measure(fill);
    Console.WriteLine("fixture,operation,repetitions,total_bytes,mean_bytes,min_bytes,max_bytes");
    Console.WriteLine(string.Join(",", "wide-1024", "encoder-tables-cold", 1, first.Total, first.Total, first.Minimum, first.Maximum));
    Console.WriteLine(string.Join(",", "wide-1024", "encoder-tables-warmed", 256, warmed.Total,
        ((decimal)warmed.Total / 256).ToString(CultureInfo.InvariantCulture), warmed.Minimum, warmed.Maximum));
    return;
}

if (args.Contains("--index-only"))
{
    var name = args.Last();
    var bytes = Bytes(stream => ElmSyntaxAbstractBinary.Encode(stream, ast[name]));
    Action index = () =>
    {
        using var decoder = new ValueEncodingBinaryDeterministic.TreeDecoder<object>(bytes);
    };
    var first = Measure(index, repetitions: 1, warm: false);
    var warmed = Measure(index);
    Console.WriteLine("fixture,operation,repetitions,total_bytes,mean_bytes,min_bytes,max_bytes");
    Console.WriteLine(string.Join(",", name, "index-cold", 1, first.Total, first.Total, first.Minimum, first.Maximum));
    Console.WriteLine(string.Join(",", name, "index-warmed", 256, warmed.Total,
        ((decimal)warmed.Total / 256).ToString(CultureInfo.InvariantCulture), warmed.Minimum, warmed.Maximum));
    return;
}

var cold = args.Contains("--cold");
var coldRoute = args.SkipWhile(argument => argument != "--route").Skip(1).FirstOrDefault();
var selected = args.LastOrDefault() ?? "";
if (cold)
{
    pine = pine.Where(pair => pair.Key == selected).ToDictionary();
    ast = ast.Where(pair => pair.Key == selected).ToDictionary();
}

var failures = 0;
Console.WriteLine("kind,fixture,route,operation,wire_bytes,repetitions,total_bytes,mean_bytes,min_bytes,max_bytes");

void Compare(string kind, string fixture, string operation, int oldSize, int newSize, Action original, Action shared)
{
    var repetitions = cold ? 1 : 256;
    if (cold && coldRoute is not null)
    {
        var routeAction = coldRoute switch
        {
            "specialized" => original,
            "shared" => shared,
            _ => throw new ArgumentException("Unknown measurement route: " + coldRoute)
        };
        var result = Measure(routeAction, repetitions: 1, warm: false);
        Console.WriteLine(string.Join(",", kind, fixture, coldRoute, operation,
            coldRoute == "specialized" ? oldSize : newSize, 1, result.Total, result.Total, result.Minimum, result.Maximum));
        return;
    }
    var old = Measure(original, repetitions, warm: !cold);
    var current = Measure(shared, repetitions, warm: !cold);
    void Print(string route, int size, (long Total, long Minimum, long Maximum) value) =>
        Console.WriteLine(string.Join(",", kind, fixture, route, operation, size, repetitions,
            value.Total, ((decimal)value.Total / repetitions).ToString(CultureInfo.InvariantCulture), value.Minimum, value.Maximum));
    Print("specialized", oldSize, old);
    Print("shared", newSize, current);
    if (!cold && current.Total > old.Total)
        ++failures;
}

foreach (var (name, value) in pine)
{
    // Encode first-use costs must be measured before preparing decode inputs.
    Compare("Pine", name, "encode", 0, 0,
        () => OriginalPine.Encode(discard, value), () => ValueEncodingBinaryDeterministic.Encode(discard, value));
    var original = Bytes(stream => OriginalPine.Encode(stream, value));
    var shared = Bytes(stream => ValueEncodingBinaryDeterministic.Encode(stream, value));
    if (!original.AsSpan().SequenceEqual(shared))
        throw new Exception("Pine wire format changed: " + name);
    Compare("Pine", name, "decode", original.Length, shared.Length,
        () => GC.KeepAlive(OriginalPine.DecodeRoot(original)), () => GC.KeepAlive(ValueEncodingBinaryDeterministic.DecodeRoot(shared)));
}

foreach (var (name, value) in ast)
{
    Compare("AST", name, "encode", 0, 0,
        () => OriginalSyntax.Encode(discard, value), () => ElmSyntaxAbstractBinary.Encode(discard, value));
    var original = Bytes(stream => OriginalSyntax.Encode(stream, value));
    var shared = Bytes(stream => ElmSyntaxAbstractBinary.Encode(stream, value));
    Compare("AST", name, "decode", original.Length, shared.Length,
        () => GC.KeepAlive(OriginalSyntax.DecodeExpression(original, out _)),
        () => GC.KeepAlive(ElmSyntaxAbstractBinary.DecodeExpression(shared, out _)));
    if (!OriginalSyntax.DecodeExpression(original, out _).Equals(ElmSyntaxAbstractBinary.DecodeExpression(shared, out _)))
        throw new Exception("Syntax model changed: " + name);
}

if (!cold)
{
    // Check dictionary growth boundaries, not just favorable representative workloads.
    for (var blobs = 0; blobs <= 20; ++blobs)
        for (var lists = 0; lists <= 20; ++lists)
        {
            var children = Enumerable.Range(0, blobs).Select(i => IntegerEncoding.EncodeSignedInteger(100000 + i))
                .Concat(Enumerable.Range(0, lists).Select(i => PineValue.List([IntegerEncoding.EncodeSignedInteger(200000 + i)])))
                .ToArray();
            var value = PineValue.List(children);
            var old = Measure(() => OriginalPine.Encode(discard, value), repetitions: 16);
            var current = Measure(() => ValueEncodingBinaryDeterministic.Encode(discard, value), repetitions: 16);
            if (current.Total > old.Total)
                throw new Exception($"Pine allocation regression at {blobs} blobs / {lists} lists: {old.Total} -> {current.Total}");
        }
    Console.WriteLine("Pine dictionary-boundary checks: 441 passed");
}

if (failures != 0)
    throw new Exception($"Allocation regression in {failures} measured operations.");
