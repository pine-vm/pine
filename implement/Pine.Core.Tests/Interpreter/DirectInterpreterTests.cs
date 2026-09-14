using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Interpreter;

public class DirectInterpreterTests
{
    [Fact]
    public void Evaluate_populates_eval_cache()
    {
        var targetExpression = Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(42));

        var expression =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(targetExpression)),
                Expression.EmptyList);

        var evalCache = new Dictionary<DirectInterpreter.EvalCacheEntryKey, PineValue>();

        var interpreter =
            DirectInterpreter.WithSharedEvalCache(new PineVMParseCache(), evalCache);

        interpreter.EvaluateExpressionDefault(expression, PineValue.EmptyList)
            .Should().Be(IntegerEncoding.EncodeSignedInteger(42));

        evalCache.Should().ContainSingle();

        var cacheKey = evalCache.Keys.Should().ContainSingle().Which;
        var replacementResult = IntegerEncoding.EncodeSignedInteger(99);
        evalCache[cacheKey] = replacementResult;

        interpreter.EvaluateExpressionDefault(expression, PineValue.EmptyList)
            .Should().Be(replacementResult);
    }
}
