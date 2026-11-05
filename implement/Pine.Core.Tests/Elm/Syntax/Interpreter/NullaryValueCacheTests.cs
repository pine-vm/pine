using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.Functional;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

using Engine = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter;

namespace Pine.Core.Tests.Elm.Syntax.Interpreter;

public class NullaryValueCacheTests
{
    private static DeclQualifiedName Name(string name) => DeclQualifiedName.FromString(name);

    private static Engine.PreparedExpression Literal(int value) =>
        new Engine.PreparedExpression.ValueLiteral(PineValueInProcess.CreateInteger(value));

    private static Engine.PreparedExpression Identifier(string name) =>
        new Engine.PreparedExpression.Identifier(Name(name));

    private static Engine.PreparedExpression Sum() =>
        new Engine.PreparedExpression.Application(
            Identifier("Pine_builtin.int_add"),
            [new Engine.PreparedExpression.ListExpr([Literal(10), Literal(20)])]);

    private static Engine.Prepared Program(Engine.PreparedExpression constant) =>
        Program(
            ("Test.value", constant),
            ("Test.root",
            new Engine.PreparedExpression.ListExpr(
                [Identifier("Test.value"), Identifier("Test.value")])));

    private static Engine.Prepared Program(params (string Name, Engine.PreparedExpression Body)[] declarations) =>
        new(
            declarations.ToImmutableDictionary(
                declaration => Name(declaration.Name),
                declaration => (Engine.PreparedDeclaration)new Engine.PreparedDeclaration.FunctionDeclaration(
                    new Engine.PreparedFunctionStruct(
                        null,
                        new Engine.PreparedFunctionImplementation(Name(declaration.Name).DeclName, [], declaration.Body)))));

    private static (Result<ElmInterpretationError, PineValueInProcess> Result, FunctionStepCountingInvocationLogger Logger)
        Run(
        Engine.Prepared program,
        Engine.EvaluationConfig? config = null,
        Func<Engine.Application, Engine.ApplicationResolution>? resolver = null)
    {
        var logger = new FunctionStepCountingInvocationLogger();
        using var scope = Engine.BeginInstrumentationScope(logger);

        var result =
            Engine.Interpret(
                Name("Test.root"),
                [],
                resolver ?? Engine.BuildResolvers(program.Declarations),
                infixOperators: null,
                config ?? Engine.EvaluationConfig.Default);

        return (result, logger);
    }

    private static long AddCount(FunctionStepCountingInvocationLogger logger) =>
        logger.DirectApplicationCounts.GetValueOrDefault(Name("Pine_builtin.int_add"));

    [Fact]
    public void Nullary_scalar_body_runs_once_but_each_access_is_still_counted()
    {
        var (result, logger) = Run(Program(Sum()));

        result.Extract(error => throw new Exception(error.ToString())).Evaluate().Should().Be(
            PineValue.List(
                [
                PineValueInProcess.CreateInteger(30).Evaluate(),
                PineValueInProcess.CreateInteger(30).Evaluate()
                ]));

        AddCount(logger).Should().Be(1);
        logger.DirectApplicationCounts[Name("Test.value")].Should().Be(2);
    }

    [Fact]
    public void Nullary_cache_is_owned_by_each_evaluation_not_the_prepared_program_or_resolver()
    {
        var program = Program(Sum());
        var resolver = Engine.BuildResolvers(program.Declarations);
        AddCount(Run(program, resolver: resolver).Logger).Should().Be(1);
        AddCount(Run(program, resolver: resolver).Logger).Should().Be(1);
    }

    [Fact]
    public void Concurrent_evaluations_do_not_share_nullary_values_or_observer_state()
    {
        var program = Program(Sum());
        var resolver = Engine.BuildResolvers(program.Declarations);
        Parallel.For(0, 8, _ => AddCount(Run(program, resolver: resolver).Logger).Should().Be(1));
    }

    [Fact]
    public void Nested_scalar_observations_are_replayed_in_the_original_order()
    {
        var body = Sum();
        var aliasBody = Identifier("Test.value");

        var program =
            Program(
                ("Test.value", body),
                ("Test.alias", aliasBody),
                ("Test.root",
                new Engine.PreparedExpression.ListExpr(
                    [Identifier("Test.alias"), Identifier("Test.alias"), Identifier("Test.value")])));

        var resolver = Engine.BuildResolvers(program.Declarations);
        var observed = new List<string>();

        var config =
            Engine.EvaluationConfig.Default with
            {
                ExpressionObservers =
                new Dictionary<Engine.PreparedExpression, Action<PineValueInProcess>>(
                    ReferenceEqualityComparer.Instance)
                {
                    [body] = value => observed.Add("body:" + value.AsInteger()),
                    [aliasBody] = value => observed.Add("alias:" + value.AsInteger())
                }
            };

        var optimized = Run(program, config, resolver);
        var optimizedObservations = observed.ToArray();
        observed.Clear();
        var reference = Run(program, config, application => resolver(application));
        observed.Should().Equal(optimizedObservations);
        AddCount(optimized.Logger).Should().Be(1);
        AddCount(reference.Logger).Should().Be(3);
    }

    [Fact]
    public void Non_scalar_results_and_observations_are_not_reused()
    {
        var body = new Engine.PreparedExpression.ListExpr([Sum()]);
        var program = Program(body);
        var observations = 0;

        var config =
            Engine.EvaluationConfig.Default with
            {
                ExpressionObservers =
                new Dictionary<Engine.PreparedExpression, Action<PineValueInProcess>>
                {
                    [body] = _ => observations++
                }
            };

        AddCount(Run(program, config).Logger).Should().Be(2);
        observations.Should().Be(2);
    }

    [Fact]
    public void Scalar_result_with_non_scalar_intermediate_observation_is_not_reused()
    {
        var body = Sum();
        var arguments = ((Engine.PreparedExpression.Application)body).Arguments[0];
        var observations = 0;

        var config =
            Engine.EvaluationConfig.Default with
            {
                ExpressionObservers =
                new Dictionary<Engine.PreparedExpression, Action<PineValueInProcess>>
                {
                    [arguments] = _ => observations++
                }
            };

        AddCount(Run(Program(body), config).Logger).Should().Be(2);
        observations.Should().Be(2);
    }

    [Fact]
    public void Custom_resolvers_are_not_assumed_pure()
    {
        var program = Program(Identifier("External.next"));
        var calls = 0;

        var resolver =
            Engine.BuildResolvers(
                program.Declarations,
                new Dictionary<DeclQualifiedName, Func<IReadOnlyList<PineValueInProcess>, PineValueInProcess?>>
                {
                    [Name("External.next")] = _ => PineValueInProcess.CreateInteger(++calls)
                });

        var result = Run(program, resolver: resolver).Result.Extract(error => throw new Exception(error.ToString()));
        result.GetElementAt(0).AsInteger().Should().Be(new BigInteger(1));
        result.GetElementAt(1).AsInteger().Should().Be(new BigInteger(2));
        calls.Should().Be(2);
    }

    [Fact]
    public void Arbitrary_and_multicast_resolver_delegates_remain_uncached()
    {
        var program = Program(Sum());
        var resolver = Engine.BuildResolvers(program.Declarations);
        AddCount(Run(program, resolver: application => resolver(application)).Logger).Should().Be(2);

        Func<Engine.Application, Engine.ApplicationResolution> first =
            _ =>
            new Engine.ApplicationResolution.Resolved(PineValueInProcess.EmptyList);

        var multicast = first + resolver;
        AddCount(Run(program, resolver: multicast).Logger).Should().Be(2);
    }

    [Fact]
    public void Resolved_namespace_is_part_of_the_nullary_cache_identity()
    {
        var program =
            Program(
                ("One.value", Literal(1)),
                ("Two.value", Literal(2)),
                ("Test.root",
                new Engine.PreparedExpression.ListExpr(
                    [
                    Identifier("One.value"),
                    Identifier("Two.value"),
                    Identifier("One.value"),
                    Identifier("Two.value")
                    ])));

        var result = Run(program).Result.Extract(error => throw new Exception(error.ToString()));
        Enumerable.Range(0, 4).Select(index => result.GetElementAt(index).AsInteger()).Should().Equal(1, 2, 1, 2);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Cyclic_nullary_declarations_still_reach_the_instruction_quota(bool mutual)
    {
        var program =
            Program(
                ("Test.first", Identifier(mutual ? "Test.second" : "Test.first")),
                ("Test.second", Identifier("Test.first")),
                ("Test.root", Identifier("Test.first")));

        var config = new Engine.EvaluationConfig(InstructionCountLimit: 100, ContinuationDepthLimit: 16);
        var error = Run(program, config).Result.IsErrOrNull()!;
        error.QuotaExceeded!.QuotaKind.Should().Be(Engine.EvaluationQuotaKind.InstructionCount);
        error.QuotaExceeded.Observed.Should().Be(101);
        error.CallStack.Should().NotBeEmpty();
    }

    [Fact]
    public void Replayed_observers_can_cancel_evaluation()
    {
        var body = Sum();
        using var cancellation = new CancellationTokenSource();
        var observations = 0;

        var config =
            Engine.EvaluationConfig.Default with
            {
                CancellationToken = cancellation.Token,
                ExpressionObservers =
                new Dictionary<Engine.PreparedExpression, Action<PineValueInProcess>>
                {
                    [body] =
                    _ => 
                    {
                        if (++observations == 2)
                            cancellation.Cancel();
                    }
                }
            };

        Action run = () => Run(Program(body), config);
        run.Should().Throw<OperationCanceledException>();
        observations.Should().Be(2);
    }

    [Fact]
    public void Replayed_observer_errors_are_not_hidden_or_cached_as_success()
    {
        var body = Sum();
        var observations = 0;

        var config =
            Engine.EvaluationConfig.Default with
            {
                ExpressionObservers =
                new Dictionary<Engine.PreparedExpression, Action<PineValueInProcess>>
                {
                    [body] =
                    _ => 
                    {
                        if (++observations == 2)
                            throw new InvalidOperationException("observer failed");
                    }
                }
            };

        Action run = () => Run(Program(body), config);
        run.Should().Throw<InvalidOperationException>().WithMessage("observer failed");
    }

    [Fact]
    public void Debug_paths_are_not_memoized_even_when_their_result_is_scalar()
    {
        var body = new Engine.PreparedExpression.Application(Identifier("Debug.log"), [Literal(0), Sum()]);
        AddCount(Run(Program(body)).Logger).Should().Be(2);
    }

    [Fact]
    public void Nullary_closures_are_not_shared_between_accesses()
    {
        var body = new Engine.PreparedExpression.LambdaExpression([], Literal(5));
        var result = Run(Program(body)).Result.Extract(error => throw new Exception(error.ToString()));
        result.GetElementAt(0).Should().BeOfType<Engine.ElmClosureInProcess>();
        result.GetElementAt(0).Should().NotBeSameAs(result.GetElementAt(1));
    }

    [Fact]
    public void Large_scalar_results_are_not_retained()
    {
        var chunk =
            new Engine.PreparedExpression.ValueLiteral(PineValueInProcess.Create(PineValue.Blob(new byte[40_000])));

        var body =
            new Engine.PreparedExpression.Application(
                Identifier("Pine_builtin.concat"),
                [new Engine.PreparedExpression.ListExpr([chunk, chunk])]);

        var (result, logger) = Run(Program(body));
        result.Extract(error => throw new Exception(error.ToString())).GetElementAt(0).GetLength().Should().Be(80_000);
        logger.DirectApplicationCounts[Name("Pine_builtin.concat")].Should().Be(2);
    }

    [Fact]
    public void Entry_admission_is_bounded_without_changing_results()
    {
        const int count = 4100;
        var declarations = Enumerable.Range(0, count).Select(index => ("Test.value" + index, Sum())).ToList();

        declarations.Add(
            ("Test.root",
            new Engine.PreparedExpression.ListExpr(
                Enumerable.Range(0, count * 2).Select(index => Identifier("Test.value" + index % count)).ToArray())));

        var (result, logger) = Run(Program(declarations.ToArray()));
        var value = result.Extract(error => throw new Exception(error.ToString()));
        value.GetLength().Should().Be(count * 2);
        AddCount(logger).Should().BeGreaterThan(count).And.BeLessThan(count * 2);
    }

    [Fact]
    public void Aggregate_scalar_retention_is_bounded_without_changing_results()
    {
        const int count = 300;

        var chunk =
            new Engine.PreparedExpression.ValueLiteral(PineValueInProcess.Create(PineValue.Blob(new byte[32_000])));

        Engine.PreparedExpression body =
            new Engine.PreparedExpression.Application(
                Identifier("Pine_builtin.concat"),
                [new Engine.PreparedExpression.ListExpr([chunk, chunk])]);

        var declarations = Enumerable.Range(0, count).Select(index => ("Test.value" + index, body)).ToList();

        declarations.Add(
            ("Test.root",
            new Engine.PreparedExpression.ListExpr(
                Enumerable.Range(0, count * 2).Select(index => Identifier("Test.value" + index % count)).ToArray())));

        var (result, logger) = Run(Program(declarations.ToArray()));
        result.Extract(error => throw new Exception(error.ToString())).GetLength().Should().Be(count * 2);

        logger.DirectApplicationCounts[Name("Pine_builtin.concat")].Should().BeGreaterThan(count).And.BeLessThan(
            count * 2);
    }

    [Theory]
    [InlineData(10, true)]
    [InlineData(300, false)]
    public void Observation_capture_has_bounded_admission_without_dropping_callbacks(int depth, bool cached)
    {
        Engine.PreparedExpression body = Literal(30);

        for (var index = 0; index < depth; index++)
            body = new Engine.PreparedExpression.Negation(body);

        var program = Program(body);
        var resolver = Engine.BuildResolvers(program.Declarations);
        var observations = 0;

        var observers =
            new Dictionary<Engine.PreparedExpression, Action<PineValueInProcess>>(ReferenceEqualityComparer.Instance);

        foreach (var item in Engine.EnumerateExpressionPaths(body))
            observers.Add(item.Expression, _ => observations++);

        var config =
            Engine.EvaluationConfig.Default with
            {
                ExpressionObservers = observers
            };

        var optimized = Run(program, config, resolver);
        observations.Should().Be(2 * (depth + 1));
        observations = 0;
        var reference = Run(program, config, application => resolver(application));
        observations.Should().Be(2 * (depth + 1));

        if (cached)
        {
            optimized.Logger.Counters.InstructionLoopCount.Should().BeLessThan(
                reference.Logger.Counters.InstructionLoopCount);
        }
        else
        {
            optimized.Logger.Counters.InstructionLoopCount.Should().BeGreaterThanOrEqualTo(
                reference.Logger.Counters.InstructionLoopCount);
        }
    }
}
