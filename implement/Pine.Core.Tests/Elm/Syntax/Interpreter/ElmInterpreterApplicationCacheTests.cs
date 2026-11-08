using AwesomeAssertions;
using Pine.Core.Caching;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.Functional;
using Pine.Core.Internal;
using Pine.Core.IO;
using Pine.Core.PineLanguage;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

using Input = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter.ApplicationCacheInput;
using EventKind = Pine.Core.Elm.Syntax.Interpreter.ElmSyntaxInterpreter.ApplicationCacheEventKind;

namespace Pine.Core.Tests.Elm.Syntax.Interpreter;

using Interpreter = ElmSyntaxInterpreter;

public class ElmInterpreterApplicationCacheTests
{
    private static readonly Lazy<ElmInterpreterEnvironment> s_library =
        new(() => ElmInterpreterSourcePreparation.Prepare(null).Extract(error => throw new Exception(error)));

    private sealed class LocalCache : ICache<Input, PineValue>
    {
        public Dictionary<Input, PineValue> Entries { get; } = [];

        public int Lookups { get; private set; }

        public int Writes { get; private set; }

        public bool TryGetValue(Input key, [MaybeNullWhen(false)] out PineValue value)
        {
            Lookups++;
            return Entries.TryGetValue(key, out value);
        }

        public void Set(Input key, PineValue value)
        {
            Writes++;
            Entries[key] = value;
        }
    }

    private static ElmInterpreterEnvironment Declare(
        ElmInterpreterEnvironment environment,
        params string[] declarations)
    {
        foreach (var declaration in declarations)
        {
            environment =
                environment.PrepareSubmission(declaration)
                .Extract(error => throw new Exception(error))
                .Should().BeOfType<ElmInterpreterSubmission.Declaration>().Subject.Environment;
        }

        return environment;
    }

    private static Interpreter.EvaluationConfig Config(
        ICache<Input, PineValue> cache,
        Func<DeclQualifiedName, bool>? policy = null,
        List<Interpreter.ApplicationCacheEvent>? events = null) =>
        new(100_000, 1_000)
        {
            ApplicationCache =
            new(cache, policy ?? (name => name.DeclName is "run"))
            {
                OnEvent = events is null ? null : events.Add
            }
        };

    private static PineValueInProcess Value(
        ElmInterpreterEnvironment environment, string expression, Interpreter.EvaluationConfig? config = null) =>
        environment.PrepareExpression(expression).Extract(error => throw new Exception(error))
        .Evaluate(config).Extract(error => throw new Exception(error.ToString()));

    private static string Render(PineValueInProcess value) => Interpreter.RenderAsElmExpression(value).expressionString;

    private static FileStoreCache<Input, PineValue> Persistent(IFileStore store) =>
        new(
            store,
            ImmutableList.Create("interpreter-applications", "v1"),
            input => input.ComputeHash(),
            Interpreter.ApplicationResultCodec);

    [Fact]
    public void Named_calls_reuse_successful_results_across_evaluations_and_initial_direct_calls()
    {
        var environment = Declare(s_library.Value, "run n = n + 1");
        var cache = new LocalCache();
        var events = new List<Interpreter.ApplicationCacheEvent>();
        var config = Config(cache, events: events);
        Render(Value(environment, "[ run 4, run 4 ]", config)).Should().Be("[ 5, 5 ]");
        Render(Value(environment, "run 4", config)).Should().Be("5");
        cache.Writes.Should().Be(1);
        events.Count(item => item.Kind is EventKind.Hit).Should().Be(2);

        var result =
            Interpreter.Interpret(
                environment.ValueBindings["run"],
                [PineValueInProcess.CreateInteger(4)],
                Interpreter.BuildResolvers(environment.Program.Declarations),
                null,
                config);

        Render(result.Extract(error => throw new Exception(error.ToString()))).Should().Be("5");
        cache.Lookups.Should().Be(4);
    }

    [Fact]
    public void Native_optimized_Elm_functions_lookup_before_resolving_and_alias_overapplication_is_safe()
    {
        var environment = Declare(s_library.Value, "run a b = Basics.add a b");
        var cache = new LocalCache();
        var config = Config(cache);
        Render(Value(environment, "[ run 3 7, run 3 7, run 3 9 ]", config)).Should().Be("[ 10, 10, 12 ]");
        cache.Lookups.Should().Be(3);
        cache.Writes.Should().Be(2);
        var alias = Declare(s_library.Value, "run = Basics.add");
        var aliasCache = new LocalCache();
        Render(Value(alias, "[ run 3 7, run 3 9 ]", Config(aliasCache))).Should().Be("[ 10, 12 ]");
        aliasCache.Writes.Should().Be(0);
    }

    [Fact]
    public void Function_policy_runs_before_modelling_or_storage_and_disables_implicit_nullary_memoization()
    {
        var environment = Declare(s_library.Value, "run = 42");
        var cache = new LocalCache();
        var selected = 0;
        var config = Config(cache, _ => { selected++; return false; });
        Render(Value(environment, "[ run, run ]", config)).Should().Be("[ 42, 42 ]");
        selected.Should().BeGreaterThanOrEqualTo(2);
        cache.Lookups.Should().Be(0);
        cache.Writes.Should().Be(0);
        Render(Value(environment, "[ run, run ]")).Should().Be("[ 42, 42 ]");
    }

    [Theory]
    [InlineData("apply x = x + 3", "apply", "10")]
    [InlineData("apply a b = a + b", "apply 3", "10")]
    [InlineData("apply a b = a + b", "(\\n -> apply 3 n)", "10")]
    [InlineData("apply a b = a + b", "let offset = 3 in \\n -> apply offset n", "10")]
    [InlineData("apply a b = a + b", "let loop n = if n < 1 then 3 else loop (n - 1) in \\n -> n + loop 2", "10")]
    public void Function_arguments_partial_arguments_and_recursive_captures_hit(
        string declaration,
        string function,
        string expected)
    {
        var environment = Declare(s_library.Value, declaration, "run f n = f n");
        var cache = new LocalCache();
        var events = new List<Interpreter.ApplicationCacheEvent>();
        var config = Config(cache, events: events);
        Render(Value(environment, $"run ({function}) 7", config)).Should().Be(expected);
        Render(Value(environment, $"run ({function}) 7", config)).Should().Be(expected);
        cache.Writes.Should().Be(1);
        events.Count(item => item.Kind is EventKind.Hit).Should().Be(1);
    }

    [Theory]
    [InlineData("run r = r.fn r.n", "run { fn = add 3, n = 7 }", "10")]
    [InlineData("run fs = List.map (\\f -> f 7) fs", "run [ add 1, add 2 ]", "[ 8, 9 ]")]
    [InlineData("run f = f { a = 9 }", "run .a", "9")]
    [InlineData("run f = f 7", "run Just", "Just 7")]
    public void Functions_nested_in_data_and_nonclosure_callables_are_modelled(
        string declaration,
        string expression,
        string expected)
    {
        var environment = Declare(s_library.Value, "add a b = a + b", declaration);
        var cache = new LocalCache();
        var config = Config(cache);
        Render(Value(environment, expression, config)).Should().Be(expected);
        Render(Value(environment, expression, config)).Should().Be(expected);
        cache.Writes.Should().Be(1);
        cache.Lookups.Should().Be(2);
    }

    [Fact]
    public void Partially_applied_record_constructors_are_supported_as_arguments()
    {
        var environment = Declare(s_library.Value, "type alias Pair = { a : Int, b : Int }", "run f = f 7");
        var cache = new LocalCache();
        var config = Config(cache);
        Render(Value(environment, "run (Pair 3)", config)).Should().Be("{ a = 3, b = 7 }");
        Render(Value(environment, "run (Pair 3)", config)).Should().Be("{ a = 3, b = 7 }");
        cache.Lookups.Should().Be(2);
        cache.Writes.Should().Be(1);
    }

    [Fact]
    public void Relevant_captures_and_collected_arguments_invalidate_but_unused_bindings_do_not()
    {
        var environment = Declare(s_library.Value, "add a b = a + b", "run f = f 7");
        var cache = new LocalCache();
        var config = Config(cache);
        Render(Value(environment, "run (let unused = 1 in add 3)", config)).Should().Be("10");
        Render(Value(environment, "run (let unused = 99 in add 3)", config)).Should().Be("10");
        cache.Writes.Should().Be(1);
        Render(Value(environment, "run (add 4)", config)).Should().Be("11");
        Render(Value(environment, "run (let offset = 3 in \\n -> offset + n)", config)).Should().Be("10");
        Render(Value(environment, "run (let offset = 4 in \\n -> offset + n)", config)).Should().Be("11");
        cache.Writes.Should().Be(4);
    }

    [Fact]
    public void Model_ignores_allocation_sharing_and_retains_function_binding_alias_relationships()
    {
        var environment =
            Declare(s_library.Value, "apply a b = a + b", "other a b = a + b", "run fs = List.map (\\f -> f 7) fs");

        var shared = Value(environment, "let f = apply 3 in [ f, f ]");
        var separate = Value(environment, "[ apply 3, apply 3 ]");
        var model = new Interpreter.ApplicationCacheModel(environment.Program);
        model.DescribeValue(shared).Should().Be(model.DescribeValue(separate));
        model.ComputeValueHash(shared).ToArray().Should().Equal(model.ComputeValueHash(separate).ToArray());
        var distinct = Value(environment, "[ apply 3, other 3 ]");
        model.DescribeValue(shared).Should().NotBe(model.DescribeValue(distinct));

        model.ComputeFunctionHash(environment.ValueBindings["apply"]).ToArray()
            .Should().Equal(model.ComputeFunctionHash(environment.ValueBindings["other"]).ToArray());
    }

    [Fact]
    public void Function_equality_observes_unused_captures_without_causing_false_cache_hits()
    {
        var environment = Declare(s_library.Value, "make unused = \\n -> n", "run f g = f == g");
        var cache = new LocalCache();
        var config = Config(cache);
        Render(Value(environment, "run (make 1) (make 1)", config)).Should().Be("True");
        Render(Value(environment, "run (make 1) (make 2)", config)).Should().Be("False");
        cache.Writes.Should().Be(2);
        Render(Value(environment, "run (make 1) (make 1)", config)).Should().Be("True");
        cache.Writes.Should().Be(2);
    }

    [Fact]
    public void Qualified_function_policy_is_consistent_between_named_and_closure_dispatch()
    {
        var environment = Declare(s_library.Value, "run n = n + 1");
        var cache = new LocalCache();
        var config = Config(cache, name => name == environment.ValueBindings["run"]);
        Render(Value(environment, "let f = run in f 3", config)).Should().Be("4");
        Render(Value(environment, "run 3", config)).Should().Be("4");
        cache.Lookups.Should().Be(2);
        cache.Writes.Should().Be(1);
    }

    [Fact]
    public void Public_application_address_normalizes_partial_and_fully_supplied_calls()
    {
        var environment = Declare(s_library.Value, "run a b = a + b");
        var model = new Interpreter.ApplicationCacheModel(environment.Program);
        var partial = Value(environment, "run 3");
        var folded = model.DescribeApplication(partial, [PineValueInProcess.CreateInteger(7)]);

        var direct =
            model.DescribeApplication(
                environment.ValueBindings["run"],
                [PineValueInProcess.CreateInteger(3), PineValueInProcess.CreateInteger(7)]);

        folded.Should().Be(direct);
        var cache = new LocalCache();
        Render(Value(environment, "let f = run 3 in f 7", Config(cache))).Should().Be("10");
        cache.Entries.Keys.Single().Should().Be(direct);
        direct.ComputeHash().ToArray().Should().Equal(folded.ComputeHash().ToArray());
    }

    [Fact]
    public void Executable_precomputed_field_values_are_part_of_the_key()
    {
        var name = DeclQualifiedName.Create(["Main"], "run");

        var body =
            new Interpreter.PreparedExpression.RecordAccess(
                Interpreter.PreparedExpression.Identifier.Create([], "r"),
                "a",
                Core.Encodings.StringEncoding.ValueFromString("a"));

        Interpreter.Prepared Program(Interpreter.PreparedExpression expression) =>
            new(
                new Dictionary<DeclQualifiedName, Interpreter.PreparedDeclaration>
                {
                    [name] =
                    new Interpreter.PreparedDeclaration.FunctionDeclaration(
                        new(null, new("run", [new Core.Elm.Syntax.Abstract.Pattern.VarPattern("r")], expression)))
                });

        var first = Program(body);
        var second = Program(body with { FieldNameValue = Core.Encodings.StringEncoding.ValueFromString("b") });
        var record = Value(s_library.Value, "{ a = 1, b = 2 }");
        var cache = new LocalCache();
        var config = Config(cache);

        Render(
            Interpreter.Interpret(name, [record], first, config).Extract(error => throw new Exception(error.ToString())))
            .Should().Be("1");

        Render(
            Interpreter.Interpret(name, [record], second, config).Extract(
                error => throw new Exception(error.ToString())))
            .Should().Be("2");

        cache.Writes.Should().Be(2);
    }

    [Fact]
    public void Dependency_edits_invalidate_and_shadowed_historical_functions_keep_their_keys()
    {
        var first = Declare(s_library.Value, "helper n = n + 1", "run n = helper n", "old = run");
        var changed = Declare(first, "helper n = n + 2", "run n = helper n", "unrelated n = 999");
        var cache = new LocalCache();
        var config = Config(cache);
        Render(Value(first, "run 4", config)).Should().Be("5");
        Render(Value(changed, "old 4", config)).Should().Be("5");
        cache.Writes.Should().Be(1);
        Render(Value(changed, "run 4", config)).Should().Be("6");
        cache.Writes.Should().Be(2);
        var before = new Interpreter.ApplicationCacheModel(first.Program).DescribeFunction(first.ValueBindings["run"]);

        var after =
            new Interpreter.ApplicationCacheModel(changed.Program).DescribeFunction(changed.ValueBindings["run"]);

        before.Should().NotBe(after);
    }

    [Fact]
    public void Mutually_recursive_declared_dependencies_are_closed_without_infinite_expansion()
    {
        var module =
            Core.Elm.Syntax.ElmSyntaxParser.ParseModuleText(
                "module Recursion exposing (run)\nhelper n = if n < 1 then 0 else other (n - 1)\n" +
                "other n = if n < 1 then 0 else helper (n - 1)\nrun n = helper n")
            .Extract(error => throw new Exception(error.ToString()));

        var environment =
            ElmInterpreterEnvironment.Create([.. s_library.Value.Modules.Values, module])
            .Extract(error => throw new Exception(error));

        var cache = new LocalCache();
        var config = Config(cache);
        Render(Value(environment, "Recursion.run 8", config)).Should().Be("0");
        Render(Value(environment, "Recursion.run 8", config)).Should().Be("0");
        cache.Lookups.Should().Be(2);
        cache.Writes.Should().Be(1);
    }

    private static async Task<ElmInterpreterEnvironment> PackageEnvironment(
        string name,
        string version,
        string module,
        bool higherOrder = false)
    {
        var world =
            new DeclarationUnitTests.World().Add(
                name,
                version,
                [$"module {module} exposing (helper)\nhelper x = 42"],
                [module]);

        var body = higherOrder ? $"run f x = f ({module}.helper x)" : $"run x = {module}.helper x";
        var build = await world.Prepare($"module Main exposing (run)\nimport {module}\n{body}");
        return ElmInterpreterSourcePreparation.PrepareResolved(build).Extract(error => throw new Exception(error));
    }

    [Fact]
    public async Task Package_names_versions_and_dependency_module_names_do_not_enter_keys()
    {
        var first = await PackageEnvironment("author/first", "1.0.0", "First");
        var second = await PackageEnvironment("someone/second", "1.1.0", "Second");
        var name = DeclQualifiedName.Create(["Main"], "run");

        var input =
            new Interpreter.ApplicationCacheModel(first.Program).DescribeApplication(
                name,
                [
                PineValueInProcess.CreateInteger(3)
                ]);

        var renamed =
            new Interpreter.ApplicationCacheModel(second.Program).DescribeApplication(
                name,
                [
                PineValueInProcess.CreateInteger(3)
                ]);

        input.Should().Be(renamed);
        input.ComputeHash().Length.Should().Be(32);
        input.ComputeHash().ToArray().Should().Equal(renamed.ComputeHash().ToArray());
        var cache = new LocalCache();
        Render(Value(first, "Main.run 3", Config(cache))).Should().Be("42");
        Render(Value(second, "Main.run 3", Config(cache))).Should().Be("42");
        cache.Lookups.Should().Be(2);
        cache.Writes.Should().Be(1);
    }

    [Fact]
    public void Closure_call_path_and_tail_result_coalescing_preserve_bounded_continuation_depth()
    {
        var environment =
            Declare(
                s_library.Value,
                "run n = if n < 1 then 42 else run (n - 1)");

        var cache = new LocalCache();
        var events = new List<Interpreter.ApplicationCacheEvent>();
        var config = Config(cache, events: events) with { ContinuationDepthLimit = 32 };
        Render(Value(environment, "let f = run in f 80", config)).Should().Be("42");
        cache.Entries.Count.Should().Be(81);
        Render(Value(environment, "let f = run in f 80", config)).Should().Be("42");
        events.Count(item => item.Kind is EventKind.Hit).Should().Be(1);
    }

    [Fact]
    public void Function_outputs_are_explicitly_excluded_and_overapplication_still_applies_leftovers()
    {
        var environment = Declare(s_library.Value, "run n = \\m -> n + m");
        var cache = new LocalCache();
        var events = new List<Interpreter.ApplicationCacheEvent>();
        var config = Config(cache, events: events);
        Render(Value(environment, "run 3 7", config)).Should().Be("10");
        cache.Writes.Should().Be(0);
        events.Should().Contain(item => item.Kind == EventKind.FunctionResultExcluded);

        var data = Declare(s_library.Value, "run n = n + 1");
        Render(Value(data, "run 3", config)).Should().Be("4");
        var invalid = data.PrepareExpression("run 3 7").Extract(error => throw new Exception(error)).Evaluate(config);
        invalid.IsErrOrNull().Should().NotBeNull();
    }

    [Fact]
    public void Observers_Debug_errors_quotas_and_cancellation_do_not_publish_incomplete_results()
    {
        var environment = Declare(s_library.Value, "run n = Debug.log \"value\" n");
        var cache = new LocalCache();
        var events = new List<Interpreter.ApplicationCacheEvent>();
        var config = Config(cache, events: events);
        Render(Value(environment, "run 3", config)).Should().Be("3");
        cache.Lookups.Should().Be(0);
        events.Should().Contain(item => item.Kind == EventKind.DebugExcluded);

        var higherOrder = Declare(s_library.Value, "run f n = f n");
        Render(Value(higherOrder, "run (Debug.log \"argument\") 3", config)).Should().Be("3");
        cache.Lookups.Should().Be(0);

        var failing = Declare(s_library.Value, "run n = Debug.todo \"failure\"");

        failing.PrepareExpression("run 3").Extract(error => throw new Exception(error)).Evaluate(config)
            .IsErrOrNull().Should().NotBeNull();

        var looping = Declare(s_library.Value, "run n = run n");

        looping.PrepareExpression("run 3").Extract(error => throw new Exception(error))
            .Evaluate(config with { InstructionCountLimit = 100 }).IsErrOrNull()!.QuotaExceeded.Should().NotBeNull();

        cache.Writes.Should().Be(0);
        using var cancellation = new CancellationTokenSource();
        cancellation.Cancel();
        Action cancelled = () => Value(looping, "run 3", config with { CancellationToken = cancellation.Token });
        cancelled.Should().Throw<OperationCanceledException>();

        var observed = Declare(s_library.Value, "run n = n + 1");
        var prepared = observed.PrepareExpression("run 3").Extract(error => throw new Exception(error));
        var notifications = 0;

        var observers =
            new Dictionary<Interpreter.PreparedExpression, Action<PineValueInProcess>>(
                ReferenceEqualityComparer.Instance)
            {
                [((Interpreter.PreparedDeclaration.FunctionDeclaration)prepared.Program.Declarations[prepared.EntryPoint])
                .Function.Declaration.Expression] =
                _ => notifications++
            };

        Render(
            prepared.Evaluate(config with { ExpressionObservers = observers })
            .Extract(error => throw new Exception(error.ToString()))).Should().Be("4");

        notifications.Should().Be(1);
        cache.Writes.Should().Be(0);
        events.Should().Contain(item => item.Kind == EventKind.ObserversExcluded);
    }

    [Fact]
    public async Task Thread_safe_storage_supports_concurrently_running_interpreter_instances()
    {
        var environment = Declare(s_library.Value, "run n = n + 1");
        var cache = new MemoryCache<Input, PineValue, int>(0);
        var config = Config(cache);

        await Task.WhenAll(
            Enumerable.Range(0, 16).Select(
                _ =>
                Task.Run(() => Render(Value(environment, "run 3", config)).Should().Be("4"))));

        cache.Count.Should().Be(1);
        Render(Value(environment, "run 3", config)).Should().Be("4");
    }

    [Fact]
    public void Layered_storage_restores_data_and_reports_corruption_instead_of_false_misses()
    {
        var environment = Declare(s_library.Value, "run n = { number = n, text = \"saved\" }");
        var store = new FileStoreFromConcurrentDictionary();
        var front = new LocalCache();
        var first = new LayeredCache<Input, PineValue>(front, Persistent(store));
        Render(Value(environment, "run 3", Config(first))).Should().Contain("saved");
        var newFront = new LocalCache();
        var events = new List<Interpreter.ApplicationCacheEvent>();
        var next = new LayeredCache<Input, PineValue>(newFront, Persistent(store));
        Render(Value(environment, "run 3", Config(next, events: events))).Should().Contain("saved");
        events.Should().Contain(item => item.Kind == EventKind.Hit);
        newFront.Entries.Count.Should().Be(1);
        var path = store.ListFiles().Single();
        var bytes = store.GetFileContent(path)!.Value.ToArray();
        bytes[^1] ^= 1;
        store.SetFileContent(path, bytes);
        Action read = () => Value(environment, "run 3", Config(Persistent(store)));
        read.Should().Throw<InvalidDataException>();
        Action decode = () => Interpreter.ApplicationResultCodec.Decode(new byte[] { 255 });
        decode.Should().Throw<InvalidDataException>();
        var encoded = Interpreter.ApplicationResultCodec.Encode(PineValue.EmptyList);
        Action trailing = () => Interpreter.ApplicationResultCodec.Decode(encoded.ToArray().Append((byte)0).ToArray());
        trailing.Should().Throw<InvalidDataException>();
    }

    [Fact]
    public void Custom_resolver_semantics_are_not_assumed_to_match_the_supplied_program()
    {
        var environment = Declare(s_library.Value, "run n = n");
        var trusted = Interpreter.BuildResolvers(environment.Program.Declarations);
        Func<Interpreter.Application, Interpreter.ApplicationResolution> custom = application => trusted(application);

        Action apply =
            () => Interpreter.Interpret(
                environment.ValueBindings["run"],
                [PineValueInProcess.CreateInteger(3)],
                custom,
                null,
                Config(new LocalCache()));

        apply.Should().Throw<ArgumentException>().WithMessage("*interpreter-owned*");
    }

    [Fact]
    public void Counter_enabled_entry_point_uses_the_same_application_cache_and_builtin_domain()
    {
        var environment = Declare(s_library.Value, "run n = n + 1");
        var cache = new LocalCache();
        var expression = environment.ValueBindings["run"].FullName + " 3";
        var config = Config(cache);

        Interpreter.ParseAndInterpretWithCounters(expression, environment.Program, config).Result
            .Extract(error => throw new Exception(error.ToString()));

        Interpreter.ParseAndInterpretWithCounters(expression, environment.Program, config).Result
            .Extract(error => throw new Exception(error.ToString()));

        cache.Lookups.Should().Be(2);
        cache.Writes.Should().Be(1);
    }

    [Fact]
    public async Task Cryptographic_addresses_and_persisted_results_are_reused_across_processes()
    {
        var directory =
            Path.GetFullPath(Path.Combine("artifacts", "interpreter-cache-tests", Guid.NewGuid().ToString("N")));

        Directory.CreateDirectory(directory);

        try
        {
            await Worker(directory, "first");
            await Worker(directory, "second");

            File.ReadAllText(Path.Combine(directory, "first.hash")).Should()
                .Be(File.ReadAllText(Path.Combine(directory, "second.hash")));
        }
        finally
        {
            Directory.Delete(directory, recursive: true);
        }
    }

    private static async Task Worker(string directory, string phase)
    {
        var start = new ProcessStartInfo("dotnet") { RedirectStandardOutput = true, RedirectStandardError = true };
        start.ArgumentList.Add(typeof(ElmInterpreterApplicationCacheTests).Assembly.Location);
        start.ArgumentList.Add("--filter-method");
        start.ArgumentList.Add("*Cross_process_cache_worker*");
        start.Environment["PINE_INTERPRETER_CACHE_TEST_DIRECTORY"] = directory;
        start.Environment["PINE_INTERPRETER_CACHE_TEST_PHASE"] = phase;
        using var process = Process.Start(start) ?? throw new Exception("Could not start cache test worker.");
        var stdout = process.StandardOutput.ReadToEndAsync();
        var stderr = process.StandardError.ReadToEndAsync();
        using var timeout = new CancellationTokenSource(TimeSpan.FromMinutes(2));

        try
        {
            await process.WaitForExitAsync(timeout.Token);
        }
        catch (OperationCanceledException)
        {
            process.Kill(entireProcessTree: true);
            throw;
        }

        process.ExitCode.Should().Be(0, (await stdout) + "\n" + (await stderr));
    }

    [Fact]
    public async Task Cross_process_cache_worker()
    {
        var directory = Environment.GetEnvironmentVariable("PINE_INTERPRETER_CACHE_TEST_DIRECTORY");

        if (directory is null)
            return;

        var phase = Environment.GetEnvironmentVariable("PINE_INTERPRETER_CACHE_TEST_PHASE")!;

        var environment =
            phase is "first"
            ?
            await PackageEnvironment("author/first", "1.0.0", "First", higherOrder: true)
            :
            await PackageEnvironment("someone/second", "1.1.0", "Second", higherOrder: true);

        var events = new List<Interpreter.ApplicationCacheEvent>();
        var cache = Persistent(new FileStoreFromSystemIOFile(directory));
        var helper = environment.Program.Declarations.Keys.Single(name => name.DeclName is "helper");
        PineValueInProcess[] arguments = [Value(environment, helper.FullName), PineValueInProcess.CreateInteger(3)];
        var name = DeclQualifiedName.Create(["Main"], "run");

        Render(
            Interpreter.Interpret(name, arguments, environment.Program, Config(cache, events: events))
            .Extract(error => throw new Exception(error.ToString()))).Should().Be("42");

        events.Count(item => item.Kind is EventKind.Hit).Should().Be(phase is "first" ? 0 : 1);
        events.Count(item => item.Kind is EventKind.Stored).Should().Be(phase is "first" ? 1 : 0);
        var input = new Interpreter.ApplicationCacheModel(environment.Program).DescribeApplication(name, arguments);
        events.Single(item => item.Kind is EventKind.Hit or EventKind.Stored).Input.Should().Be(input);
        File.WriteAllText(Path.Combine(directory, phase + ".hash"), Convert.ToHexString(input.ComputeHash().Span));
    }
}
