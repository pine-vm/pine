using AwesomeAssertions;
using Pine.Core.Addressing;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class PineVMExpressionEncodingCacheTests
{
    [Fact]
    public void Parsed_encodings_populate_existing_identities_without_materializing_views()
    {
        var cache = new PineVMExpressionEncodingCache();
        var expression = Expression.BuiltinInst("length", Expression.EnvironmentInstance);

        var identity =
            cache.GetIdentity(Core.Runtime.InternalExpression.ExpressionConversion.FromCanonicalCached(expression));

        var firstEncoding = ExpressionEncoding2026.EncodeExpressionAsValue(expression);
        var secondEncoding = new PineValue.ListValue(firstEncoding.Items.ToArray());
        cache.RegisterParsedEncoding(expression, firstEncoding);
        cache.RegisterParsedEncoding(expression, secondEncoding);
        cache.Count.Should().Be(1);
        cache.GetIdentity(identity.InternalExpression).Should().BeSameAs(identity);
        identity.IsCanonicalExpressionCreated.Should().BeFalse();
        identity.IsEncodedValueCreated.Should().BeFalse();
        identity.IsHashCreated.Should().BeFalse();
        cache.GetOrEncode(expression).Should().BeSameAs(firstEncoding);
        identity.IsCanonicalExpressionCreated.Should().BeFalse();
        identity.IsHashCreated.Should().BeFalse();

        identity.HashBase16.Should().Be(
            Convert.ToHexStringLower(PineValueHashFlat.ComputeHashForValue(firstEncoding).hashBytes.Span));
    }

    [Fact]
    public void Public_registration_rejects_legacy_and_mismatched_values_without_poisoning_the_cache()
    {
        var expression = Expression.EnvironmentInstance;
        var cache = new PineVMExpressionEncodingCache();

        foreach (var invalid in new[]
        {
            ExpressionEncoding2024.EncodeExpressionAsValue(expression),
            ExpressionEncoding2026.EncodeExpressionAsValue(Expression.EmptyList)
        })
        {
            Action register = () => cache.RegisterParsedEncoding(expression, invalid);
            register.Should().Throw<ArgumentException>();
            cache.Count.Should().Be(0);
        }

        cache.GetOrEncode(expression).Should().Be(ExpressionEncoding2026.EncodeExpressionAsValue(expression));
    }

    [Fact]
    public void Concurrent_demand_shares_canonical_views_without_changing_the_hash_domain()
    {
        var expression =
            new Core.Runtime.InternalExpression.Expression.BuiltinBinary(
                Core.Runtime.InternalExpression.BuiltinFunction.IntAdd,
                Core.Runtime.InternalExpression.Expression.EnvironmentInstance,
                new Core.Runtime.InternalExpression.Expression.Literal(IntegerEncoding.EncodeSignedInteger(3)));

        var cache = new PineVMExpressionEncodingCache();
        var identity = cache.GetIdentity(expression);
        var canonical = Core.Runtime.InternalExpression.ExpressionConversion.ToCanonical(expression);
        var expectedValue = ExpressionEncoding.EncodeExpressionAsValue(canonical);

        var expectedHash =
            Convert.ToHexStringLower(PineValueHashFlat.ComputeHashForValue(expectedValue).hashBytes.Span);

        identity.IsCanonicalExpressionCreated.Should().BeFalse();
        identity.IsEncodedValueCreated.Should().BeFalse();
        identity.IsHashCreated.Should().BeFalse();

        Parallel.For(
            0,
            32,
            _ =>
            {
                cache.RegisterParsedEncoding(expression, expectedValue);
                cache.GetIdentity(expression).Should().BeSameAs(identity);
                identity.HashBase16.Should().Be(expectedHash);
                identity.CanonicalExpression.Should().Be(canonical);
                identity.EncodedValue.Should().Be(expectedValue);
            });

        identity.EncodedValue.Should().BeSameAs(identity.EncodedValue);
        identity.CanonicalExpression.Should().BeSameAs(identity.CanonicalExpression);
    }
}
