using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading;

namespace Pine.Core.Elm.Syntax.Interpreter;

public partial class ElmSyntaxInterpreter
{
    private sealed class ApplicationCacheExecution(
        ApplicationCaching settings, ApplicationCacheModel model, CancellationToken cancellationToken)
    {
        public sealed record Entry(DeclQualifiedName Name, ApplicationCacheInput Input);

        public Entry? FindNamed(Application application, out PineValueInProcess? cached)
        {
            cached = null;
            var declaration = model.ResolveDeclared(application);

            if (declaration is null || application.Arguments.Count != declaration.Function.Arguments.Count)
                return null;

            var name = declaration.ResolvedName ?? application.FunctionName;

            var function =
                new ElmClosureInProcess(
                    new ElmClosureInProcess.SourceRef.Declared(name, declaration.Function),
                    declaration.Function.Arguments.Count,
                    [],
                    LocalBindingEnvironment.Empty,
                    name);

            return Find(name, function, application.Arguments, out cached);
        }

        public Entry? Find(
            DeclQualifiedName name,
            ElmClosureInProcess function,
            IReadOnlyList<PineValueInProcess> arguments,
            out PineValueInProcess? cached)
        {
            cached = null;
            name = model.PolicyName(function, name);

            if (!settings.ShouldCache(name))
                return null;

            cancellationToken.ThrowIfCancellationRequested();
            var input = model.DescribeApplication(function.With([]), arguments, cancellationToken);

            if (input.ContainsDebug)
            {
                settings.OnEvent?.Invoke(new(ApplicationCacheEventKind.DebugExcluded, name, input));
                return null;
            }

            cancellationToken.ThrowIfCancellationRequested();
            var found = settings.Cache.TryGetValue(input, out var result);
            cancellationToken.ThrowIfCancellationRequested();
            settings.OnEvent?.Invoke(new(found ? ApplicationCacheEventKind.Hit : ApplicationCacheEventKind.Miss, name, input));

            if (found)
            {
                cached =
                    PineValueInProcess.Create(
                        result ??
                        throw new InvalidOperationException("Application cache returned a null result for a hit."));
            }

            return found ? null : new(name, input);
        }

        public void Complete(IReadOnlyList<Entry> entries, PineValueInProcess result)
        {
            cancellationToken.ThrowIfCancellationRequested();

            var kind =
                ContainsOpaque(result)
                ?
                ApplicationCacheEventKind.FunctionResultExcluded
                :
                ApplicationCacheEventKind.Stored;

            var data = kind is ApplicationCacheEventKind.Stored ? result.Evaluate() : null;

            foreach (var entry in entries)
            {
                cancellationToken.ThrowIfCancellationRequested();

                if (data is not null)
                    settings.Cache.Set(entry.Input, data);

                settings.OnEvent?.Invoke(new(kind, entry.Name, entry.Input));
            }
        }
    }

    private static readonly ConditionalWeakTable<DefaultApplicationResolver, ApplicationCacheModel> s_applicationCacheModels =
        new();

    private static ApplicationCacheExecution? CreateApplicationCacheExecution(
        Func<Application, ApplicationResolution> resolver, EvaluationConfig config)
    {
        if (config.ApplicationCache is not { } settings)
            return null;

        if (resolver.Target is not DefaultApplicationResolver owner ||
            resolver.GetInvocationList().Length is not 1)
        {
            throw new ArgumentException("Application caching requires interpreter-owned resolvers without custom overrides.");
        }

        if (config.ExpressionObservers is { Count: > 0 })
        {
            settings.OnEvent?.Invoke(new(ApplicationCacheEventKind.ObserversExcluded, null, null));
            return null;
        }

        return
            new(
                settings,
                s_applicationCacheModels.GetValue(
                    owner,
                    key => new ApplicationCacheModel(key.Program, key.EnableDefaultBuiltins)),
                config.CancellationToken);
    }

    private static void PushApplicationCacheResult(Stack<Kont> stack, ApplicationCacheExecution.Entry? entry)
    {
        if (entry is null)
            return;
        // Tail calls return the same value. Keep one continuation rather than one frame per
        // selected tail call, while retaining all keys whose successful result must be published.
        if (stack.TryPeek(out var top) && top.Tag is Kont.Kind.CacheApplicationResult)
            top.AsCacheApplicationResult.Entries.Add(entry);

        else
            stack.Push(new Kont.CacheApplicationResult([entry]));
    }
}
