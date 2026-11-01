using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Text.Json;
using Xunit;

using ProfiledVM = Pine.Core.Runtime.IntermediateVM.PineVM;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class DirectEvalTemplateMetricsTests
{
    [Fact]
    public void Direct_template_metrics_separate_parsing_recognition_and_application_reuse()
    {
        var leaf = Expression.LitralInst(PineValue.Blob([42, 43, 44]));
        var shared = Expression.ListInst([leaf, Expression.EnvironmentInstance]);
        var root = Expression.ListInst([shared, shared]);
        var encoded = ExpressionEncoding.EncodeExpressionAsValue(root);
        var cache = new PineVMParseCache();
        using var profile = new PineVMWorkProfile(referenceCapacity: 0);

        using (profile.Enter())
        {
            var parsed = cache.ParseExpression(encoded).IsOkOrNull()!;
            cache.ParseExpression(encoded).IsOkOrNull().Should().BeSameAs(parsed);
            profile.DirectTemplates.Recognize(parsed);
            var first = profile.DirectTemplates.Apply(parsed, PineValueInProcess.EmptyBlob, profile)!.Value;
            profile.DirectTemplates.Recognize(parsed);
            var second = profile.DirectTemplates.Apply(parsed, PineValueInProcess.EmptyBlob, profile)!.Value;
            first.value.Evaluate().Should().Be(second.value.Evaluate());

            profile.DirectTemplates.Recognize(
                new Expression.Eval(Expression.EnvironmentInstance, Expression.EnvironmentInstance));

            profile.DirectTemplates.Recognize(
                new Expression.Builtin(nameof(BuiltinFunction.length), Expression.EnvironmentInstance));

            // Other direct-evaluator users must not increase simple-template applications.
            ProfiledVM.EvalDirect(parsed, PineValueInProcess.EmptyBlob).Should().NotBeNull();
        }

        var json = JsonSerializer.SerializeToElement(profile.DirectTemplates.Snapshot());
        json.GetProperty("RecognitionProbes").GetInt64().Should().Be(4);
        json.GetProperty("RejectedEval").GetInt64().Should().Be(1);
        json.GetProperty("RejectedBuiltin").GetInt64().Should().Be(1);
        json.GetProperty("AcceptedShapes").GetInt64().Should().Be(2);
        json.GetProperty("ParseProbes").GetInt64().Should().Be(2);
        json.GetProperty("ParseHits").GetInt64().Should().Be(1);
        json.GetProperty("ParseMisses").GetInt64().Should().Be(1);
        json.GetProperty("ParseAddedEntries").GetInt64().Should().Be(cache.Count);
        json.GetProperty("DistinctAppliedTemplates").GetInt32().Should().Be(1);
        json.GetProperty("ApplicationAttempts").GetInt64().Should().Be(2);
        json.GetProperty("Applications").GetInt64().Should().Be(2);
        json.GetProperty("AppliedTemplateParseProbes").GetInt64().Should().Be(2);
        json.GetProperty("LogicalInstructions").GetInt64().Should().Be(14);
        json.GetProperty("PhysicalEntries").GetInt64().Should().Be(10);
        json.GetProperty("ChildArrays").GetInt64().Should().Be(4);
        json.GetProperty("InvocationLocalMemoHits").GetInt64().Should().Be(2);
        json.GetProperty("Templates")[0].GetProperty("Nodes").GetInt64().Should().Be(7);
    }

    [Fact]
    public void Direct_template_metrics_capture_cancelled_attempt_without_claiming_success()
    {
        var expression = Expression.ListInst([Expression.EnvironmentInstance]);
        using var profile = new PineVMWorkProfile(referenceCapacity: 0, directVisitBudget: 1);

        using (profile.Enter())
        {
            profile.DirectTemplates.Recognize(expression);
            Action apply = () => profile.DirectTemplates.Apply(expression, PineValueInProcess.EmptyBlob, profile);
            apply.Should().Throw<OperationCanceledException>();
        }

        var json = JsonSerializer.SerializeToElement(profile.DirectTemplates.Snapshot());
        json.GetProperty("ApplicationAttempts").GetInt64().Should().Be(1);
        json.GetProperty("Applications").GetInt64().Should().Be(0);
        json.GetProperty("PhysicalEntries").GetInt64().Should().Be(1);
        json.GetProperty("ChildArrays").GetInt64().Should().Be(1);
    }
}
