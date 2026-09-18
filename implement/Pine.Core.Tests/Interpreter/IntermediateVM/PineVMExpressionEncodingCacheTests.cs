using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

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
        cache.GetOrEncode(expression).Should().BeSameAs(firstEncoding);
    }
}
