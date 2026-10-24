using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen.Pine;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using Pine.Core.Runtime.IntermediateVM;
using Pine.IntermediateVM;
using System;
using System.Linq;
using System.Threading.Tasks;
using Xunit;

using IntermediatePineVM = Pine.Core.Runtime.IntermediateVM.PineVM;

namespace Pine.IntegrationTests;

public class TemplatePlanCacheTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Setup_reuses_template_plans_independently_of_parse_and_compilation_caches(
        bool shareParseCache,
        bool shareCompilationCache)
    {
        var plans = new PineVMExpressionTemplatePlanCache();
        var parsed = new PineVMParseCache();
        var compiled = new ConcurrentExpressionCompilationCache();
        var template = BuildTemplate();

        var first = Evaluate(CreateVm(plans), template, 11);
        first.CachedWorkCounters.ExpressionTemplatePlanParseCount.Should().BeGreaterThan(0);
        var count = plans.Count;
        count.Should().BeGreaterThan(0);

        var second = Evaluate(CreateVm(plans), template, 13);
        second.CachedWorkCounters.ExpressionTemplatePlanParseCount.Should().Be(0);
        plans.Count.Should().Be(count);
        second.ReturnValue.Evaluate().Should().NotBe(first.ReturnValue.Evaluate());

        var separatePlans = new PineVMExpressionTemplatePlanCache();
        var separate = Evaluate(CreateVm(separatePlans), template, 13);
        separate.CachedWorkCounters.ExpressionTemplatePlanParseCount.Should().BeGreaterThan(0);
        separatePlans.Count.Should().Be(count);
        separate.ReturnValue.Evaluate().Should().Be(second.ReturnValue.Evaluate());
        separate.Counters.Should().Be(second.Counters);
        separate.CountersByOrigin.Total.Should().Be(separate.Counters);
        second.CachedWorkCounters.Should().Be(default(CachedWorkCounters));

        var defaultCache = Evaluate(CreateVm(null), template, 13);
        defaultCache.CachedWorkCounters.ExpressionTemplatePlanParseCount.Should().BeGreaterThan(0);
        defaultCache.ReturnValue.Evaluate().Should().Be(second.ReturnValue.Evaluate());

        IntermediatePineVM CreateVm(PineVMExpressionTemplatePlanCache? cache) =>
            SetupVM.Create(
                disableReductionInCompilation: true,
                parseCache: shareParseCache ? parsed : null,
                tryGetExpressionCompilation: shareCompilationCache ? compiled.TryGet : null,
                getOrAddExpressionCompilation: shareCompilationCache ? compiled.GetOrAdd : null,
                expressionTemplatePlanCache: cache);
    }

    [Fact]
    public async Task Concurrent_fresh_VMs_share_only_template_metadata_not_environments_or_counters()
    {
        var plans = new PineVMExpressionTemplatePlanCache();
        var template = BuildTemplate();

        var reports =
            await Task.WhenAll(
                Enumerable.Range(0, 16).Select(
                    index =>
                    Task.Run(
                        () =>
                        Evaluate(
                            SetupVM.Create(disableReductionInCompilation: true, expressionTemplatePlanCache: plans),
                            template,
                            index))));

        var count = plans.Count;
        count.Should().BeGreaterThan(0);

        // Materialize escaped results after their originating evaluations have all completed.
        for (var index = 0; index < reports.Length; ++index)
        {
            reports[index].ReturnValue.Evaluate().Should().Be(
                DirectInterpreter.WithoutEvalCaching(new PineVMParseCache())
                .EvaluateExpressionDefault(BuildExpression(index), template));
        }

        var warm = Evaluate(SetupVM.Create(expressionTemplatePlanCache: plans), template, 41);
        warm.CachedWorkCounters.ExpressionTemplatePlanParseCount.Should().Be(0);
        plans.Count.Should().Be(count);
    }

    [Fact]
    public void Shared_template_cache_reuses_negative_recognition_without_changing_results()
    {
        var plans = new PineVMExpressionTemplatePlanCache();

        var nonTemplate =
            ExpressionEncoding.EncodeExpressionAsValue(Expression.BuiltinInst("length", Expression.EnvironmentInstance));

        var expression =
            new Expression.Eval(Expression.EnvironmentInstance, Expression.LitralInst(PineValue.EmptyList));

        for (var iteration = 0; iteration < 2; ++iteration)
        {
            var report =
                SetupVM.Create(disableReductionInCompilation: true, expressionTemplatePlanCache: plans)
                .EvaluateExpressionOnCustomStack(
                    expression,
                    nonTemplate,
                    IntermediatePineVM.EvaluationConfig.Unbounded,
                    cancellationToken: default)
                .Extract(error => throw new InvalidOperationException(error.ToString()));

            report.ReturnValue.Evaluate().Should().Be(IntegerEncoding.EncodeSignedInteger(0));
            report.CachedWorkCounters.ExpressionTemplatePlanParseCount.Should().Be(iteration is 0 ? 1 : 0);
            report.CountersByOrigin.Total.Should().Be(report.Counters);
        }

        plans.Count.Should().Be(1);
    }

    private static PineValue BuildTemplate() =>
        FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
            Expression.EnvironmentInstance,
            parameterCount: 3)
        ?? throw new InvalidOperationException("Failed to build template.");

    private static Expression BuildExpression(int argument) =>
        new Expression.Eval(
            Expression.EnvironmentInstance,
            Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(argument)));

    private static EvaluationReport Evaluate(IntermediatePineVM vm, PineValue template, int argument)
    {
        var expression = BuildExpression(argument);

        return
            vm.EvaluateExpressionOnCustomStack(
                expression,
                template,
                IntermediatePineVM.EvaluationConfig.Unbounded,
                cancellationToken: default)
            .Extract(error => throw new InvalidOperationException(error.ToString()));
    }
}
