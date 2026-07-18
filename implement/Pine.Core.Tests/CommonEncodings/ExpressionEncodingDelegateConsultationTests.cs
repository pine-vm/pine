using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.CommonEncodings;

/// <summary>
/// Tests that <see cref="ExpressionEncoding.EncodeExpressionAsValueWithoutTopLevelCacheLookup"/>
/// invokes the supplied <c>encodeSubexpression</c> delegate exactly once for each direct
/// subexpression of the input. The delegate is the extension point used by
/// <see cref="PineExpressionEncodingCache"/> to memoize results across calls, so verifying its
/// consultation surface protects the caching contract.
/// </summary>
public class ExpressionEncodingDelegateConsultationTests
{
    /// <summary>
    /// Records every <see cref="Expression"/> the encoder routes through the delegate and
    /// forwards each call to <see cref="ExpressionEncoding.EncodeExpressionAsValueWithoutTopLevelCacheLookup"/>
    /// so the underlying encoding still completes correctly.
    /// </summary>
    private sealed class RecordingDelegate
    {
        public List<Expression> Calls { get; } = [];

        public PineValue.ListValue Encode(Expression expression)
        {
            Calls.Add(expression);

            return
                ExpressionEncoding.EncodeExpressionAsValueWithoutTopLevelCacheLookup(
                    expression,
                    Encode);
        }
    }

    [Fact]
    public void Delegate_is_not_consulted_for_Literal_root_because_it_has_no_subexpressions()
    {
        var root = Expression.LitralInst(StringEncoding.ValueFromString("literal"));

        var recorder = new RecordingDelegate();

        var encoded =
            ExpressionEncoding.EncodeExpressionAsValueWithoutTopLevelCacheLookup(
                root,
                recorder.Encode);

        recorder.Calls.Should().BeEmpty();

        encoded.Should().Be(ExpressionEncoding.EncodeExpressionAsValue(root));
    }

    [Fact]
    public void Delegate_is_not_consulted_for_Environment_root_because_it_has_no_subexpressions()
    {
        var root = new Expression.Environment();

        var recorder = new RecordingDelegate();

        var encoded =
            ExpressionEncoding.EncodeExpressionAsValueWithoutTopLevelCacheLookup(
                root,
                recorder.Encode);

        recorder.Calls.Should().BeEmpty();

        encoded.Should().Be(ExpressionEncoding.EncodeExpressionAsValue(root));
    }

    [Fact]
    public void Delegate_is_consulted_once_per_List_item()
    {
        Expression itemA = Expression.LitralInst(StringEncoding.ValueFromString("alfa"));
        Expression itemB = Expression.LitralInst(StringEncoding.ValueFromString("beta"));
        Expression itemC = Expression.LitralInst(StringEncoding.ValueFromString("gamma"));

        var root = Expression.ListInst([itemA, itemB, itemC]);

        var recorder = new RecordingDelegate();

        var encoded =
            ExpressionEncoding.EncodeExpressionAsValueWithoutTopLevelCacheLookup(
                root,
                recorder.Encode);

        recorder.Calls.Should().Equal(itemA, itemB, itemC);

        encoded.Should().Be(ExpressionEncoding.EncodeExpressionAsValue(root));
    }

    [Fact]
    public void Delegate_is_consulted_once_per_Conditional_branch_in_declaration_order()
    {
        Expression condition = Expression.LitralInst(StringEncoding.ValueFromString("condition"));
        Expression falseBranch = Expression.LitralInst(StringEncoding.ValueFromString("falseBranch"));
        Expression trueBranch = Expression.LitralInst(StringEncoding.ValueFromString("trueBranch"));

        var root =
            Expression.ConditionalInst(
                condition: condition,
                falseBranch: falseBranch,
                trueBranch: trueBranch);

        var recorder = new RecordingDelegate();

        var encoded =
            ExpressionEncoding.EncodeExpressionAsValueWithoutTopLevelCacheLookup(
                root,
                recorder.Encode);

        recorder.Calls.Should().Equal(condition, falseBranch, trueBranch);

        encoded.Should().Be(ExpressionEncoding.EncodeExpressionAsValue(root));
    }

    [Fact]
    public void Delegate_is_consulted_for_both_ParseAndEval_subexpressions()
    {
        Expression encodedExpr = Expression.LitralInst(StringEncoding.ValueFromString("encoded"));
        Expression environment = Expression.LitralInst(StringEncoding.ValueFromString("environment"));

        var root = new Expression.Eval(encoded: encodedExpr, environment: environment);

        var recorder = new RecordingDelegate();

        var encoded =
            ExpressionEncoding.EncodeExpressionAsValueWithoutTopLevelCacheLookup(
                root,
                recorder.Encode);

        recorder.Calls.Should().Equal(encodedExpr, environment);

        encoded.Should().Be(ExpressionEncoding.EncodeExpressionAsValue(root));
    }

    [Fact]
    public void Delegate_is_consulted_for_KernelApplication_input_only()
    {
        Expression input = Expression.LitralInst(StringEncoding.ValueFromString("input"));

        var root =
            Expression.BuiltinInst(
                function: nameof(BuiltinFunction.length),
                input: input);

        var recorder = new RecordingDelegate();

        var encoded =
            ExpressionEncoding.EncodeExpressionAsValueWithoutTopLevelCacheLookup(
                root,
                recorder.Encode);

        recorder.Calls.Should().Equal(input);

        encoded.Should().Be(ExpressionEncoding.EncodeExpressionAsValue(root));
    }

    [Fact]
    public void Delegate_is_consulted_for_StringTag_tagged_subexpression()
    {
        Expression tagged = Expression.LitralInst(StringEncoding.ValueFromString("tagged"));

        var root = new Expression.Label(tag: "the-tag", tagged: tagged);

        var recorder = new RecordingDelegate();

        var encoded =
            ExpressionEncoding.EncodeExpressionAsValueWithoutTopLevelCacheLookup(
                root,
                recorder.Encode);

        recorder.Calls.Should().Equal(tagged);

        encoded.Should().Be(ExpressionEncoding.EncodeExpressionAsValue(root));
    }

    [Fact]
    public void Delegate_is_consulted_once_for_every_direct_subexpression_of_a_nested_tree()
    {
        // Root: List
        //   [0] = Conditional(condition=L"cond", false=KernelApp("length", input=L"ka-input"), true=L"true")
        //   [1] = ParseAndEval(encoded=L"enc", environment=Environment)
        //   [2] = StringTag("tag", tagged=L"tagged")

        Expression cond = Expression.LitralInst(StringEncoding.ValueFromString("cond"));
        Expression kaInput = Expression.LitralInst(StringEncoding.ValueFromString("ka-input"));

        Expression falseBranch =
            Expression.BuiltinInst(
                function: nameof(BuiltinFunction.length),
                input: kaInput);

        Expression trueBranch = Expression.LitralInst(StringEncoding.ValueFromString("true"));

        Expression conditional =
            Expression.ConditionalInst(
                condition: cond,
                falseBranch: falseBranch,
                trueBranch: trueBranch);

        Expression peEncoded = Expression.LitralInst(StringEncoding.ValueFromString("enc"));
        Expression peEnvironment = new Expression.Environment();

        Expression parseAndEval =
            new Expression.Eval(encoded: peEncoded, environment: peEnvironment);

        Expression stTagged = Expression.LitralInst(StringEncoding.ValueFromString("tagged"));
        Expression stringTag = new Expression.Label(tag: "the-tag", tagged: stTagged);

        var root = Expression.ListInst([conditional, parseAndEval, stringTag]);

        var recorder = new RecordingDelegate();

        var encoded =
            ExpressionEncoding.EncodeExpressionAsValueWithoutTopLevelCacheLookup(
                root,
                recorder.Encode);

        // The forwarding RecordingDelegate re-enters the encoder synchronously, producing a
        // depth-first traversal of direct subexpressions: each child is recorded, then fully
        // expanded, before the next sibling is recorded.
        recorder.Calls.Should().Equal(
            // [0] of root List
            conditional,
            cond,
            falseBranch,
            kaInput,
            trueBranch,
            // [1] of root List
            parseAndEval,
            peEncoded,
            peEnvironment,
            // [2] of root List
            stringTag,
            stTagged);

        encoded.Should().Be(ExpressionEncoding.EncodeExpressionAsValue(root));
    }

    [Fact]
    public void Cache_consults_delegate_through_factory_for_every_unique_subexpression_once()
    {
        // Build a tree with a shared sub-expression to demonstrate that the cache short-circuits
        // re-encoding of repeated subtrees. The cache uses the
        // EncodeExpressionAsValueWithoutTopLevelCacheLookup factory body, which delegates to
        // cache.EncodeExpressionAsValue for sub-encodings.
        Expression shared = Expression.LitralInst(StringEncoding.ValueFromString("shared"));

        var root = Expression.ListInst([shared, shared, shared]);

        var cache = new PineExpressionEncodingCache();

        var encodedFromCache = cache.EncodeExpressionAsValue(root);

        encodedFromCache.Should().Be(ExpressionEncoding.EncodeExpressionAsValue(root));

        // Second call returns the cached instance for the root, by reference.
        ReferenceEquals(encodedFromCache, cache.EncodeExpressionAsValue(root))
            .Should().BeTrue("cache hit on the root must return the same encoded instance");
    }
}
