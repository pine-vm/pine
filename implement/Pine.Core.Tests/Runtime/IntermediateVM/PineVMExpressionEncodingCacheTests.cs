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
    public void Repeated_registration_preserves_existing_encoding()
    {
        var cache = new PineVMExpressionEncodingCache();
        var expression = Expression.EnvironmentInstance;
        var firstEncoding = ExpressionEncoding2024.EncodeExpressionAsValue(expression);
        var secondEncoding = ExpressionEncoding2026.EncodeExpressionAsValue(expression);

        cache.RegisterParsedEncoding(expression, firstEncoding);
        cache.RegisterParsedEncoding(expression, secondEncoding);

        cache.Count.Should().Be(1);

        var identity =
            cache.GetIdentity(Core.Runtime.InternalExpression.ExpressionConversion.FromCanonicalCached(expression));

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
                cache.GetIdentity(expression).Should().BeSameAs(identity);
                identity.HashBase16.Should().Be(expectedHash);
                identity.CanonicalExpression.Should().Be(canonical);
                identity.EncodedValue.Should().Be(expectedValue);
            });

        identity.EncodedValue.Should().BeSameAs(identity.EncodedValue);
        identity.CanonicalExpression.Should().BeSameAs(identity.CanonicalExpression);
    }
}
