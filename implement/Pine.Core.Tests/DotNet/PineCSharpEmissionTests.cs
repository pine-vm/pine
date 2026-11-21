using AwesomeAssertions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core.DotNet;
using Pine.Core.Encodings;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Reflection;
using System.Threading;
using Xunit;

namespace Pine.Core.Tests.DotNet;

public class PineCSharpEmissionTests
{
    [Fact]
    public void Pine_CSharp_spread_call_results_do_not_use_forwarding_locals()
    {
        var compilation = Compile("spread-call");
        var syntax = SyntaxFactory.ParseCompilationUnit(compilation.MethodsSource);

        foreach (var variable in syntax.DescendantNodes().OfType<VariableDeclaratorSyntax>().Where(
            variable =>
            variable.Initializer?.Value is MemberAccessExpressionSyntax
            { Expression: IdentifierNameSyntax receiver } &&
            receiver.Identifier.ValueText.StartsWith("call_", StringComparison.Ordinal)))
        {
            var name = variable.Identifier.ValueText;

            syntax.DescendantNodes().OfType<AssignmentExpressionSyntax>().Any(
                assignment =>
                assignment.Right is IdentifierNameSyntax source && source.Identifier.ValueText == name)
                .Should().BeFalse("call result components should be assigned directly to their continuation bindings");
        }

        compilation.MethodsSource.Should().Contain("call_0_0.item0").And.Contain("call_0_0.item1");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Pine_CSharp_gotos_to_return_or_throw_chains_are_inlined(bool throws)
    {
        var source =
            TestMethod(
                """
                if (source.LengthEqual(0)) goto first;
                return source;
                first: goto second;
                second: goto third;
                third:
                {
                """ +
                (throws ? """throw new System.InvalidOperationException("expected");""" : "return source;") +
                "\n}");

        var optimized = PineCSharpSourceSimplification.Format(source, new HashSet<string>());

        optimized.Should().NotContain("goto ").And.NotContain("first:").And.NotContain("second:").And.NotContain(
            "third:");

        var compilation =
            new PineCSharpCompilation(
                source,
                "",
                new(EnableCancellation: false),
                [],
                new(false, false, false),
                "Run");

        using var originalAssembly = Load(compilation);
        using var optimizedAssembly = Load(compilation with { MethodsSource = optimized });
        var nonempty = PineValueInProcess.Create(PineValue.Blob([1]));
        optimizedAssembly.Evaluate(nonempty).Should().BeSameAs(originalAssembly.Evaluate(nonempty));

        if (throws)
        {
            Action original = () => originalAssembly.Evaluate(PineValueInProcess.EmptyList);
            Action rewritten = () => optimizedAssembly.Evaluate(PineValueInProcess.EmptyList);
            original.Should().Throw<InvalidOperationException>().WithMessage("expected");
            rewritten.Should().Throw<InvalidOperationException>().WithMessage("expected");
        }
        else
            optimizedAssembly.Evaluate(PineValueInProcess.EmptyList).Should().BeSameAs(PineValueInProcess.EmptyList);
    }

    [Fact]
    public void Pine_CSharp_untargeted_return_labels_are_removed_on_fallthrough()
    {
        var source =
            TestMethod(
                """
                if (source.LengthEqual(0)) return source;
                done: return PineValueInProcess.EmptyList;
                """);

        var optimized = PineCSharpSourceSimplification.Format(source, new HashSet<string>());
        optimized.Should().NotContain("done:");
        AssertEquivalent(source, optimized, PineValueInProcess.Create(PineValue.Blob([1])));
    }

    [Fact]
    public void Pine_CSharp_exit_inlining_preserves_work_before_a_return()
    {
        var source =
            TestMethod(
                """
                long number = 0;
                if (source.LengthEqual(0)) goto done;
                return PineValueInProcess.CreateInteger(number);
                done:
                {
                    Bump(ref number);
                    return PineValueInProcess.CreateInteger(number);
                }
                """);

        var optimized = PineCSharpSourceSimplification.Format(source, new HashSet<string> { "number" });
        optimized.Should().Contain("Bump(ref number)");
        AssertEquivalent(source, optimized, PineValueInProcess.EmptyList);
        AssertEquivalent(source, optimized, PineValueInProcess.Create(PineValue.Blob([1])));
    }

    [Fact]
    public void Pine_CSharp_common_subexpressions_share_numeric_and_memory_derivations()
    {
        var source =
            TestMethod(
                """
                PineValueInProcess first = source.GetElementAt(1);
                PineValueInProcess second = source.GetElementAt(1);
                BigInteger? firstNumber = first.AsInteger();
                BigInteger? secondNumber = first.AsInteger();
                BlobMemory bytes = ((PineValue.BlobValue)source.GetElementAt(0).Evaluate()).Bytes;
                BlobMemory sameBytes = ((PineValue.BlobValue)source.GetElementAt(0).Evaluate()).Bytes;
                long length = bytes.Length;
                long sameLength = bytes.Length;
                BigInteger offset = new BigInteger(length) * 4;
                BigInteger sameOffset = new BigInteger(length) * 4;
                return PineValueInProcess.CreateList(new PineValueInProcess[]
                {
                    first, second, PineValueInProcess.CreateBool(firstNumber == secondNumber),
                    PineValueInProcess.CreateInteger(offset + sameOffset + sameBytes.Length + sameLength)
                });
                """);

        var optimized = PineCSharpSourceSimplification.Format(source, new HashSet<string>());
        var tree = SyntaxFactory.ParseCompilationUnit(optimized);

        tree.DescendantNodes().OfType<InvocationExpressionSyntax>().Count(
            call =>
            call.Expression is MemberAccessExpressionSyntax { Name.Identifier.ValueText: "AsInteger" }).Should().Be(1);

        tree.DescendantNodes().OfType<CastExpressionSyntax>().Should().ContainSingle();

        tree.DescendantNodes().OfType<ObjectCreationExpressionSyntax>().Count(
            creation =>
            creation.Type.ToString() == "BigInteger").Should().Be(1);

        var input =
            PineValueInProcess.Create(
                PineValue.List(
                    [PineValue.Blob([17, 31, 47, 63, 79, 95, 111, 127]), IntegerEncoding.EncodeSignedInteger(20001)]));

        AssertEquivalent(source, optimized, input);
    }

    [Theory]
    [InlineData("""
        BigInteger number = 0;
        BigInteger before = number + 1;
        if (source.LengthEqual(0)) { number = 2; }
        BigInteger after = number + 1;
        return PineValueInProcess.CreateInteger(before + after);
        """)]
    [InlineData("""
        BigInteger number = 0;
        BigInteger total = 0;
        while (number < 3)
        {
            BigInteger before = number + 1;
            number += 1;
            BigInteger after = number + 1;
            total += before + after;
        }
        return PineValueInProcess.CreateInteger(total);
        """)]
    [InlineData("""
        long number = 1;
        long before = number + 1;
        Bump(ref number);
        long after = number + 1;
        return PineValueInProcess.CreateInteger(before + after);
        """)]
    [InlineData("""
        long number = 1;
        long before = number + 1;
        number++;
        long after = number + 1;
        return PineValueInProcess.CreateInteger(before + after);
        """)]
    [InlineData("""
        BigInteger number = 1;
        BigInteger increment = 2;
        number += increment;
        BigInteger after = number + increment;
        return PineValueInProcess.CreateInteger(after);
        """)]
    [InlineData("""
        long number = 1;
        long before = number + 1;
        long other = 0;
        (number, other) = (4, 2);
        long after = number + 1;
        return PineValueInProcess.CreateInteger(before + after + other);
        """)]
    [InlineData("""
        BigInteger number = 0;
        BigInteger before = number + 1;
        goto branch;
        skipped: { number = 2; goto end; }
        branch: { if (source.LengthEqual(0)) goto skipped; }
        end: { BigInteger after = number + 1; return PineValueInProcess.CreateInteger(before + after); }
        """)]
    [InlineData("""
        BigInteger wide = source.GetLength();
        long narrow = source.GetLength();
        return PineValueInProcess.CreateInteger(wide + narrow);
        """)]
    [InlineData("""
        long narrow = long.MaxValue;
        BigInteger wide = narrow;
        return PineValueInProcess.CreateInteger(wide + wide);
        """)]
    [InlineData("""
        long before = source.GetLength();
        source = source.GetElementAt(0);
        if (source.LengthEqual(0)) return PineValueInProcess.CreateInteger(before);
        return PineValueInProcess.EmptyList;
        """)]
    public void Pine_CSharp_common_subexpressions_respect_mutation_control_flow_and_representation_types(string body)
    {
        var source = TestMethod(body);
        var optimized = PineCSharpSourceSimplification.Format(source, new HashSet<string> { "number", "total" });
        AssertEquivalent(source, optimized, PineValueInProcess.EmptyList);
        AssertEquivalent(source, optimized, PineValueInProcess.Create(PineValue.Blob([1, 2])));
        AssertEquivalent(source, optimized, PineValueInProcess.Create(PineValue.List([PineValue.EmptyList])));
    }

    private static string TestMethod(string body) =>
        """
        using System.Numerics;
        using PineValue = Pine.Core.PineLanguage.PineValue;
        using PineValueInProcess = Pine.Core.Internal.PineValueInProcess;
        using BlobMemory = System.ReadOnlyMemory<byte>;
        public static class CompiledPine
        {
            private static void Bump(ref long number) { number++; }
            public static PineValueInProcess Run(PineValueInProcess source)
            {
        """ + body + "\n}\n}";

    private static void AssertEquivalent(string source, string optimized, PineValueInProcess input)
    {
        var compilation =
            new PineCSharpCompilation(
                source,
                "",
                new(EnableCancellation: false),
                [],
                new(false, false, false),
                "Run");

        using var originalAssembly = Load(compilation);
        using var optimizedAssembly = Load(compilation with { MethodsSource = optimized });

        optimizedAssembly.Evaluate(input)!.Evaluate().Should().Be(
            originalAssembly.Evaluate(input)!.Evaluate(),
            "optimized source must preserve the original result:\n" + optimized);
    }

    [Fact]
    public void Pine_CSharp_native_loop_has_constant_wrapping_allocations_and_preserves_zero_iteration_identity()
    {
        var compilation = Compile("self-tail");
        compilation.MethodsSource.Should().Contain("while (true)").And.NotContain("goto ");
        using var assembly = Load(compilation);

        var worker =
            assembly.CreateWorkerDelegate<Func<PineValueInProcess, PineValueInProcess, PineValueInProcess>>(
                compilation.Methods.Last());

        var count = PineValueInProcess.CreateInteger(100000);
        var accumulator = PineValueInProcess.CreateInteger(10);
        worker(count, accumulator).AsInteger().Should().Be(new BigInteger(100010));
        var before = GC.GetAllocatedBytesForCurrentThread();
        var result = worker(count, accumulator);
        var allocated = GC.GetAllocatedBytesForCurrentThread() - before;
        result.AsInteger().Should().Be(new BigInteger(100010));
        allocated.Should().BeLessThan(4096, "iteration state must not construct 200,000 Pine wrappers");
        var original = PineValueInProcess.Create(PineValue.Blob([4, 0, 1]));
        worker(PineValueInProcess.CreateInteger(0), original).Should().BeSameAs(original);
        worker(PineValueInProcess.CreateInteger(-1), original).Should().BeSameAs(original);

        worker(PineValueInProcess.CreateInteger(2), PineValueInProcess.EmptyList).Should().BeSameAs(
            PineValueInProcess.EmptyList);
    }

    [Fact]
    public void Pine_CSharp_numeric_views_preserve_overflow_invalid_results_and_original_encodings()
    {
        BigInteger[] values = [long.MinValue, long.MaxValue, -1, 0, 1, 2, BigInteger.One << 80];

        foreach (var x in values)
        {
            foreach (var y in values)
            {
                var left = PineCSharpValue.FromInteger(x);
                var right = PineCSharpValue.FromInteger(y);
                PineCSharpValue.Add(left, right).AsInteger().Should().Be(x + y);
                PineCSharpValue.Multiply(left, right).AsInteger().Should().Be(x * y);
                PineCSharpValue.LessThan(left, right).Should().Be(x < y);
                PineCSharpValue.LessThanOrEqual(left, right).Should().Be(x <= y);
            }
        }

        var original = PineValueInProcess.Create(PineValue.Blob([4, 0, 1]));
        var view = PineCSharpValue.FromValue(original);
        view.AsInteger().Should().Be(BigInteger.One);
        view.ToPineValue().Should().BeSameAs(original);

        PineCSharpValue.Add(view, PineCSharpValue.FromInt64(0)).ToPineValue().Evaluate()
            .Should().Be(IntegerEncoding.EncodeSignedInteger(1));

        var invalid = PineCSharpValue.FromValue(PineValueInProcess.EmptyList);
        var unevaluable = PineCSharpValue.FromValue(PineValueInProcess.CreateList([null!]));

        PineCSharpValue.MultiplyAdd(invalid, unevaluable, 2).ToPineValue().Should().BeSameAs(
            PineValueInProcess.EmptyList);

        PineCSharpValue.Add(invalid, unevaluable).ToPineValue().Should().BeSameAs(PineValueInProcess.EmptyList);
    }

    [Fact]
    public void Pine_CSharp_literal_parents_share_inprocess_children_and_materialized_roots()
    {
        var compilation = Compile("literal-reuse");
        using var assembly = Load(compilation);
        var type = assembly.Assembly.GetType(compilation.Options.LiteralsTypeName)!;

        var values =
            type.GetProperties(BindingFlags.Public | BindingFlags.Static)
            .Select(property => (PineValueInProcess)property.GetValue(null)!).ToArray();

        var parent = values.Single(value => value.IsList());

        foreach (var child in values.Where(value => !value.IsList()))
        {
            new[] { parent.GetElementAt(0), parent.GetElementAt(1) }.Should().Contain(
                value => ReferenceEquals(value, child));
        }

        assembly.Evaluate(PineValueInProcess.EmptyList).Should().BeSameAs(parent);
        compilation.MethodsSource.Should().NotContain("var result_0");
    }

    [Fact]
    public void Pine_CSharp_worker_delegates_validate_owner_and_signature_and_accept_tokens()
    {
        var compilation = Compile("spread", "without-cancellation");
        using var assembly = Load(compilation);

        var worker =
            assembly.CreateWorkerDelegate<Func<PineValueInProcess, PineValueInProcess,
            (PineValueInProcess item0, PineValueInProcess item1)>>(compilation.Methods.Single());

        var a = PineValueInProcess.CreateInteger(2);
        var b = PineValueInProcess.CreateInteger(3);
        var result = worker(a, b);
        result.item0.Should().BeSameAs(a);
        result.item1.Should().BeSameAs(b);
        Action wrongSignature = () => assembly.CreateWorkerDelegate<Action>(compilation.Methods.Single());
        wrongSignature.Should().Throw<ArgumentException>();
        Action wrongOwner = () => assembly.CreateWorkerDelegate<Action>(Compile("identity").Methods.Single());
        wrongOwner.Should().Throw<ArgumentException>();
        var checkedCompilation = Compile("arithmetic", "with-cancellation");
        using var checkedAssembly = Load(checkedCompilation);

        var checkedWorker =
            checkedAssembly.CreateWorkerDelegate<
            Func<PineValueInProcess, PineValueInProcess, CancellationToken, PineValueInProcess>>(checkedCompilation.Methods.Single());

        using var cancellation = new CancellationTokenSource();
        cancellation.Cancel();
        Action run = () => checkedWorker(a, b, cancellation.Token);
        run.Should().Throw<OperationCanceledException>();
    }

    [Fact]
    public void Pine_CSharp_no_return_plans_remove_dead_callees_and_keep_debug_source_context()
    {
        var compilation = Compile("failing-unused-argument", "generic");
        compilation.Methods.Should().ContainSingle();
        compilation.MethodsSource.Should().NotContain("value_0_2");
        using var assembly = Load(compilation);
        Action run = () => assembly.Evaluate(PineValueInProcess.EmptyList);

        run.Should().Throw<Core.CodeAnalysis.ParseExpressionException>().Which.StackTrace.Should().Contain(
            "Methods.g.cs");
    }

    [Fact]
    public void Pine_CSharp_slice_offset_view_reuses_counts_and_preserves_clamping_and_overflow()
    {
        var offset = new PineCSharpSliceOffset();
        offset.Get(PineCSharpValue.FromInt64(2), 4).Should().Be(8);
        offset.Get(PineCSharpValue.FromInt64(2), 4).Should().Be(8);
        offset.Get(PineCSharpValue.FromInt64(3), 4).Should().Be(12);
        offset.Get(PineCSharpValue.FromInt64(-2), 4).Should().Be(0);
        offset.Get(PineCSharpValue.FromValue(PineValueInProcess.EmptyList), 4).Should().BeNull();
        Action overflow = () => offset.Get(PineCSharpValue.FromInteger(BigInteger.One << 80), 4);
        overflow.Should().Throw<OverflowException>();
        offset.Get(PineCSharpValue.FromInt64(3), 4).Should().Be(12);
    }

    [Fact]
    public void Pine_CSharp_fused_concatenation_matches_right_association_for_blobs_lists_and_mixed_values()
    {
        PineValueInProcess[] choices =
            [
                PineValueInProcess.EmptyList,
                PineValueInProcess.Create(PineValue.EmptyBlob),
                PineValueInProcess.Create(PineValue.Blob([1, 2])),
                PineValueInProcess.CreateList([PineValueInProcess.CreateInteger(1)])
            ];

        foreach (var a in choices)
        {
            foreach (var b in choices)
            {
                foreach (var c in choices)
                {
                    var expected = PineValueInProcess.ConcatBinary(a, PineValueInProcess.ConcatBinary(b, c)).Evaluate();
                    PineValueInProcess.ConcatManyRight([a, b, c]).Evaluate().Should().Be(expected);
                }
            }
        }

        var item = PineValueInProcess.CreateInteger(2);
        var list = PineValueInProcess.CreateList([PineValueInProcess.CreateInteger(3)]);

        PineValueInProcess.PrependItem(item, list).Evaluate()
            .Should().Be(PineValueInProcess.ConcatBinary(PineValueInProcess.CreateList([item]), list).Evaluate());
    }

    private static PineCSharpCompilation Compile(string name, string variant = "without-cancellation")
    {
        var scenario = PineCSharpScenarios.Load(name);
        var configuration = scenario.Variant(variant);

        return
            PineCSharpCompiler.Compile(scenario.Entry, configuration.Specializations, configuration.Options)
            .Extract(error => throw new Exception(error.Describe()));
    }

    private static PineCSharpAssembly Load(PineCSharpCompilation compilation) =>
        PineCSharpAssembly.Load(compilation).Extract(errors => throw new Exception(string.Join("\n", errors)));

    [Fact]
    public void Pine_CSharp_prepend_preserves_linear_growth_for_deferred_large_lists()
    {
        var original =
            PineValueInProcess.Create(
                PineValue.List(
                    Enumerable.Range(0, 100)
                    .Select(value => IntegerEncoding.EncodeSignedInteger(value)).ToArray<PineValue>()));

        var item = PineValueInProcess.CreateInteger(1);
        Measure(50);
        var first = Measure(1000);
        var second = Measure(2000);

        second.Should().BeLessThan(
            first * 3 + 262144,
            "opaque large tails must retain lazy concatenation rather than copying each prefix");

        long Measure(int count)
        {
            var value = original;
            var before = GC.GetAllocatedBytesForCurrentThread();

            for (var index = 0; index < count; index++)
                value = PineValueInProcess.PrependItem(item, value);

            var allocated = GC.GetAllocatedBytesForCurrentThread() - before;
            value.GetLength().Should().Be(count + 100);
            return allocated;
        }
    }
}
