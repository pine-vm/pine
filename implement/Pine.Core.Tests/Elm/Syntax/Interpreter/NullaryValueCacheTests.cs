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
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

using Engine = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter;
using AST = Pine.Core.Elm.Syntax.Abstract;

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
    public void Completed_values_are_shared_across_evaluations_of_the_same_program()
    {
        var program = Program(Sum());
        var resolver = Engine.BuildResolvers(program.Declarations);
        AddCount(Run(program, resolver: resolver).Logger).Should().Be(1);
        AddCount(Run(program, resolver: resolver).Logger).Should().Be(0);
    }

    [Fact]
    public void Concurrent_evaluations_share_only_completed_values()
    {
        var program = Program(Sum());
        var resolver = Engine.BuildResolvers(program.Declarations);

        Parallel.For(
            0,
            8,
            _ =>
            {
                var (result, logger) = Run(program, resolver: resolver);

                result.Extract(error => throw new Exception(error.ToString())).GetElementAt(0).AsInteger().Should().Be(
                    new BigInteger(30));

                AddCount(logger).Should().BeInRange(0, 1);
            });

        AddCount(Run(program, resolver: resolver).Logger).Should().Be(0);
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
    public void Compound_results_are_reused_and_observations_are_replayed()
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

        var (result, logger) = Run(program, config);
        AddCount(logger).Should().Be(1);
        observations.Should().Be(2);
        var value = result.Extract(error => throw new Exception(error.ToString()));
        value.GetElementAt(0).Should().BeSameAs(value.GetElementAt(1));

        AddCount(Run(program, config).Logger).Should().Be(0);
        observations.Should().Be(4);
    }

    [Fact]
    public void Compound_intermediate_observations_are_replayed()
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

        AddCount(Run(Program(body), config).Logger).Should().Be(1);
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
                        if (++observations is 2)
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
    public void Successful_declarations_using_default_Debug_facilities_are_memoized()
    {
        var body = new Engine.PreparedExpression.Application(Identifier("Debug.log"), [Literal(0), Sum()]);
        var program = Program(body);
        AddCount(Run(program).Logger).Should().Be(1);
        AddCount(Run(program).Logger).Should().Be(0);
    }

    [Fact]
    public void Debug_todo_failures_are_not_cached()
    {
        var program = Program(new Engine.PreparedExpression.Application(Identifier("Debug.todo"), [Literal(0)]));
        Run(program).Result.IsErrOrNull()!.Message.Should().Contain("Debug.todo");
        Run(program).Result.IsErrOrNull()!.Message.Should().Contain("Debug.todo");
    }

    [Fact]
    public void Nullary_closures_are_shared_between_accesses()
    {
        var body = new Engine.PreparedExpression.LambdaExpression([], Literal(5));
        var result = Run(Program(body)).Result.Extract(error => throw new Exception(error.ToString()));
        result.GetElementAt(0).Should().BeOfType<Engine.ElmClosureInProcess>();
        result.GetElementAt(0).Should().BeSameAs(result.GetElementAt(1));
    }

    [Fact]
    public void Large_values_are_retained_without_materializing_or_size_limits()
    {
        var chunk =
            new Engine.PreparedExpression.ValueLiteral(PineValueInProcess.Create(PineValue.Blob(new byte[40_000])));

        var body =
            new Engine.PreparedExpression.Application(
                Identifier("Pine_builtin.concat"),
                [new Engine.PreparedExpression.ListExpr([chunk, chunk])]);

        var (result, logger) = Run(Program(body));
        result.Extract(error => throw new Exception(error.ToString())).GetElementAt(0).GetLength().Should().Be(80_000);
        logger.DirectApplicationCounts[Name("Pine_builtin.concat")].Should().Be(1);
    }

    [Fact]
    public void All_invoked_nullary_declarations_are_admitted()
    {
        const int Count = 4100;
        var declarations = Enumerable.Range(0, Count).Select(index => ("Test.value" + index, Sum())).ToList();

        declarations.Add(
            ("Test.root",
            new Engine.PreparedExpression.ListExpr(
                [.. Enumerable.Range(0, Count * 2).Select(index => Identifier("Test.value" + index % Count))])));

        var (result, logger) = Run(Program([.. declarations]));
        var value = result.Extract(error => throw new Exception(error.ToString()));
        value.GetLength().Should().Be(Count * 2);
        AddCount(logger).Should().Be(Count);
    }

    [Fact]
    public void Aggregate_retention_does_not_evict_completed_declarations()
    {
        const int Count = 300;

        var chunk =
            new Engine.PreparedExpression.ValueLiteral(PineValueInProcess.Create(PineValue.Blob(new byte[32_000])));

        Engine.PreparedExpression body =
            new Engine.PreparedExpression.Application(
                Identifier("Pine_builtin.concat"),
                [new Engine.PreparedExpression.ListExpr([chunk, chunk])]);

        var declarations = Enumerable.Range(0, Count).Select(index => ("Test.value" + index, body)).ToList();

        declarations.Add(
            ("Test.root",
            new Engine.PreparedExpression.ListExpr(
                [.. Enumerable.Range(0, Count * 2).Select(index => Identifier("Test.value" + index % Count))])));

        var (result, logger) = Run(Program([.. declarations]));
        result.Extract(error => throw new Exception(error.ToString())).GetLength().Should().Be(Count * 2);

        logger.DirectApplicationCounts[Name("Pine_builtin.concat")].Should().Be(Count);
    }

    [Theory]
    [InlineData(10)]
    [InlineData(300)]
    public void Observation_capture_does_not_drop_callbacks_or_disable_value_reuse(int depth)
    {
        var body = Literal(30);

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

        optimized.Logger.Counters.InstructionLoopCount.Should().BeLessThan(
            reference.Logger.Counters.InstructionLoopCount);
    }

    [Fact]
    public void Unused_declarations_are_not_evaluated_and_default_prepared_entry_points_share_results()
    {
        var program =
            Program(
                ("Test.unused", Identifier("Missing.function")),
                ("Test.value", Sum()),
                ("Test.root", Identifier("Test.value")),
                ("Test.other", Identifier("Test.value")));

        var logger = new FunctionStepCountingInvocationLogger();
        using var scope = Engine.BeginInstrumentationScope(logger);

        var first =
            Engine.Interpret(Name("Test.root"), [], program).Extract(error => throw new Exception(error.ToString()));

        var second =
            Engine.Interpret(Name("Test.other"), [], program).Extract(error => throw new Exception(error.ToString()));

        second.Should().BeSameAs(first);
        AddCount(logger).Should().Be(1);
        logger.DirectApplicationCounts.Should().NotContainKey(Name("Test.unused"));
    }

    [Fact]
    public void Cache_can_be_disabled_without_reading_or_populating_default_values()
    {
        var program = Program(new Engine.PreparedExpression.ListExpr([Sum()]));
        var config = Engine.EvaluationConfig.Default with { CacheNullaryValues = false };
        AddCount(Run(program, config).Logger).Should().Be(2);
        AddCount(Run(program).Logger).Should().Be(1);
        AddCount(Run(program).Logger).Should().Be(0);
        AddCount(Run(program, config).Logger).Should().Be(2);
    }

    [Fact]
    public void Observer_sets_have_independent_caches_and_replay_only_their_own_callbacks()
    {
        var body = Sum();
        var program = Program(body);
        var firstObservations = 0;
        var secondObservations = 0;

        Engine.EvaluationConfig Config(Action<PineValueInProcess> observe) =>
            Engine.EvaluationConfig.Default with
            {
                ExpressionObservers =
                new Dictionary<Engine.PreparedExpression, Action<PineValueInProcess>>
                {
                    [body] = observe
                }
            };

        var first = Config(_ => firstObservations++);
        var second = Config(_ => secondObservations++);

        AddCount(Run(program).Logger).Should().Be(1);
        AddCount(Run(program, first).Logger).Should().Be(1);
        AddCount(Run(program, second).Logger).Should().Be(1);
        AddCount(Run(program, first).Logger).Should().Be(0);
        firstObservations.Should().Be(4);
        secondObservations.Should().Be(2);
    }

    [Fact]
    public void Different_program_snapshots_do_not_reuse_values_with_changed_dependencies()
    {
        var body = Identifier("Test.dependency");
        var first = Program(("Test.value", body), ("Test.dependency", Literal(1)), ("Test.root", body));

        var second =
            first with
            {
                Declarations =
                first.Declarations.ToImmutableDictionary().SetItem(
                    Name("Test.dependency"),
                    Program(("Test.dependency", Literal(2))).Declarations[Name("Test.dependency")])
            };

        Run(first).Result.Extract(error => throw new Exception(error.ToString())).AsInteger().Should().Be(
            new BigInteger(1));

        Run(second).Result.Extract(error => throw new Exception(error.ToString())).AsInteger().Should().Be(
            new BigInteger(2));
    }

    [Fact]
    public void Nullary_function_values_are_cached_before_overapplication_not_by_the_extra_argument()
    {
        var closure =
            new Engine.PreparedExpression.LetExpression(
                [
                    new Engine.PreparedLetDeclaration.LetFunction(
                        new(null, new("captured", [], Sum())))
                ],
                new Engine.PreparedExpression.LambdaExpression(
                    [new AST.Pattern.VarPattern("argument")],
                    new Engine.PreparedExpression.Application(
                        Identifier("Pine_builtin.int_add"),
                        [new Engine.PreparedExpression.ListExpr([Identifier("captured"), Identifier("argument")])])));

        var program = Program(("Test.value", closure));
        var logger = new FunctionStepCountingInvocationLogger();
        using var scope = Engine.BeginInstrumentationScope(logger);

        var first =
            Engine.Interpret(Name("Test.value"), [PineValueInProcess.CreateInteger(2)], program)
            .Extract(error => throw new Exception(error.ToString()));

        var second =
            Engine.Interpret(Name("Test.value"), [PineValueInProcess.CreateInteger(3)], program)
            .Extract(error => throw new Exception(error.ToString()));

        first.AsInteger().Should().Be(new BigInteger(32));
        second.AsInteger().Should().Be(new BigInteger(33));
        AddCount(logger).Should().Be(3);
    }

    [Fact]
    public void Failed_observations_do_not_publish_a_completed_value()
    {
        var body = Sum();
        var program = Program(body);
        var fail = true;

        var config =
            Engine.EvaluationConfig.Default with
            {
                ExpressionObservers =
                new Dictionary<Engine.PreparedExpression, Action<PineValueInProcess>>
                {
                    [body] =
                    _ =>
                    {
                        if (fail)
                            throw new InvalidOperationException("first observation failed");
                    }
                }
            };

        Action run = () => Run(program, config);
        run.Should().Throw<InvalidOperationException>().WithMessage("first observation failed");
        fail = false;
        AddCount(Run(program, config).Logger).Should().Be(1);
        AddCount(Run(program, config).Logger).Should().Be(0);
    }

    [Fact]
    public void Quota_failures_do_not_cache_unfinished_values()
    {
        var program = Program(new Engine.PreparedExpression.ListExpr([Sum(), Sum()]));
        var failed = Run(program, new(InstructionCountLimit: 5, ContinuationDepthLimit: 100));
        failed.Result.IsErrOrNull()!.QuotaExceeded.Should().NotBeNull();
        AddCount(Run(program).Logger).Should().Be(2);
        AddCount(Run(program).Logger).Should().Be(0);
    }

    [Fact]
    public void Cancellation_on_the_last_replayed_observation_prevents_a_root_cache_hit_from_returning_success()
    {
        var body = Sum();
        var program = Program(("Test.root", body));
        using var cancellation = new CancellationTokenSource();
        var cancel = false;

        var observers =
            new Dictionary<Engine.PreparedExpression, Action<PineValueInProcess>>
            {
                [body] =
                _ =>
                {
                    if (cancel)
                        cancellation.Cancel();
                }
            };

        var config = Engine.EvaluationConfig.Default with { ExpressionObservers = observers };
        AddCount(Run(program, config).Logger).Should().Be(1);
        cancel = true;
        Action run = () => Run(program, config with { CancellationToken = cancellation.Token });
        run.Should().Throw<OperationCanceledException>();
    }

    [Fact]
    public void Cancelled_observations_do_not_publish_unfinished_values()
    {
        var body = Sum();
        var program = Program(body);
        using var cancellation = new CancellationTokenSource();
        var cancel = true;

        var observers =
            new Dictionary<Engine.PreparedExpression, Action<PineValueInProcess>>
            {
                [body] =
                _ =>
                {
                    if (cancel)
                        cancellation.Cancel();
                }
            };

        var config = Engine.EvaluationConfig.Default with { ExpressionObservers = observers };
        Action run = () => Run(program, config with { CancellationToken = cancellation.Token });
        run.Should().Throw<OperationCanceledException>();
        cancel = false;
        AddCount(Run(program, config).Logger).Should().Be(1);
        AddCount(Run(program, config).Logger).Should().Be(0);
    }

    [Fact]
    public void Caller_supplied_operator_maps_do_not_share_results_with_different_operator_semantics()
    {
        var program =
            Program(
                ("Test.root",
                new Engine.PreparedExpression.OperatorApplication(
                    "custom",
                    Pine.Core.Elm.Syntax.Concrete.InfixDirection.Left,
                    Literal(10),
                    Literal(20))),
                ("Test.first",
                new Engine.PreparedExpression.LambdaExpression(
                    [new AST.Pattern.VarPattern("x"), new AST.Pattern.VarPattern("y")],
                    Literal(30))),
                ("Test.second",
                new Engine.PreparedExpression.LambdaExpression(
                    [new AST.Pattern.VarPattern("x"), new AST.Pattern.VarPattern("y")],
                    Literal(7))));

        var resolver = Engine.BuildResolvers(program.Declarations);
        var first = new Dictionary<string, DeclQualifiedName> { ["custom"] = Name("Test.first") };
        var second = new Dictionary<string, DeclQualifiedName> { ["custom"] = Name("Test.second") };

        var firstValue =
            Engine.Interpret(Name("Test.root"), [], resolver, first)
            .Extract(error => throw new Exception(error.ToString()));

        var secondValue =
            Engine.Interpret(Name("Test.root"), [], resolver, second)
            .Extract(error => throw new Exception(error.ToString()));

        firstValue.AsInteger().Should().Be(new BigInteger(30));
        secondValue.AsInteger().Should().Be(new BigInteger(7));
    }

    [Fact]
    public void Program_and_observer_cache_lifetimes_are_weak()
    {
        var (program, observers) = PopulateWeakCache();
        GC.Collect(2, GCCollectionMode.Forced, blocking: true, compacting: true);
        GC.WaitForPendingFinalizers();
        GC.Collect(2, GCCollectionMode.Forced, blocking: true, compacting: true);
        program.IsAlive.Should().BeFalse();
        observers.IsAlive.Should().BeFalse();
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static (WeakReference Program, WeakReference Observers) PopulateWeakCache()
    {
        var body = new Engine.PreparedExpression.ListExpr([Sum()]);
        var program = Program(body);
        var observers = new Dictionary<Engine.PreparedExpression, Action<PineValueInProcess>> { [body] = _ => { } };
        _ = Run(program, Engine.EvaluationConfig.Default with { ExpressionObservers = observers });
        return (new(program.Declarations), new(observers));
    }
}
