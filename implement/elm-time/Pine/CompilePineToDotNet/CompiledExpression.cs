using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using static Pine.CompilePineToDotNet.CompileToCSharp;

namespace Pine.CompilePineToDotNet;

public record CompiledExpression(
    ExpressionSyntax Syntax,

    /*
     * true if the type of the expression is Result<string, PineValue>
     * false if the type of the expression is PineValue
     * */
    bool IsTypeResult,

    ImmutableDictionary<PineVM.Expression, LetBinding> LetBindings,
    CompiledExpressionDependencies Dependencies)
{
    public CompiledExpressionDependencies DependenciesIncludingLetBindings() =>
        CompiledExpressionDependencies.Union(
            EnumerateLetBindingsTransitive().Values.Select(binding => binding.Expression.Dependencies)
            .Prepend(Dependencies));

    public ImmutableDictionary<PineVM.Expression, LetBinding> EnumerateLetBindingsTransitive() =>
        Union([LetBindings, .. LetBindings.Values.Select(binding => binding.Expression.EnumerateLetBindingsTransitive())]);

    public static CompiledExpression WithTypePlainValue(ExpressionSyntax syntax) =>
        WithTypePlainValue(syntax, NoLetBindings, CompiledExpressionDependencies.Empty);

    public static CompiledExpression WithTypePlainValue(
        ExpressionSyntax syntax,
        ImmutableDictionary<PineVM.Expression, LetBinding> letBindings,
        CompiledExpressionDependencies dependencies) =>
        new(
            syntax,
            IsTypeResult: false,
            LetBindings: letBindings,
            Dependencies: dependencies);

    public static CompiledExpression WithTypeResult(ExpressionSyntax syntax) =>
        WithTypeResult(syntax, NoLetBindings, CompiledExpressionDependencies.Empty);

    public static CompiledExpression WithTypeResult(
        ExpressionSyntax syntax,
        ImmutableDictionary<PineVM.Expression, LetBinding> letBindings,
        CompiledExpressionDependencies dependencies) =>
        new(
            syntax,
            IsTypeResult: true,
            LetBindings: letBindings,
            Dependencies: dependencies);

    public static readonly ImmutableDictionary<PineVM.Expression, LetBinding> NoLetBindings =
        ImmutableDictionary<PineVM.Expression, LetBinding>.Empty;

    public CompiledExpression MergeBindings(IReadOnlyDictionary<PineVM.Expression, LetBinding> bindings) =>
        this
        with
        {
            LetBindings = LetBindings.SetItems(bindings)
        };

    public CompiledExpression MergeDependencies(CompiledExpressionDependencies dependencies) =>
        this
        with
        {
            Dependencies = Dependencies.Union(dependencies)
        };

    public CompiledExpression MapSyntax(Func<ExpressionSyntax, ExpressionSyntax> map) =>
        this
        with
        {
            Syntax = map(Syntax)
        };

    public CompiledExpression MapOrAndThen(
        ExpressionCompilationEnvironment environment,
        Func<ExpressionSyntax, CompiledExpression> continueWithPlainValue)
    {
        if (!IsTypeResult)
        {
            return
                continueWithPlainValue(Syntax)
                .MergeBindings(LetBindings)
                .MergeDependencies(Dependencies);
        }

        var syntaxName = GetNameForExpression(Syntax);

        var okIdentifier = SyntaxFactory.Identifier("ok_of_" + syntaxName);

        var combinedExpression =
            continueWithPlainValue(SyntaxFactory.IdentifierName(okIdentifier))
            .MergeBindings(LetBindings);

        var (combinedExpressionSyntax, combinedExpressionDependencies) =
            ExpressionBodyOrBlock(environment, combinedExpression);

        /*
         * 2023-10-15
         * 
        var mapErrorExpression =
            BuildMapErrorExpression(
                Syntax,
                map: errExpr =>
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.AddExpression,
                    SyntaxFactory.LiteralExpression(
                        SyntaxKind.StringLiteralExpression,
                        SyntaxFactory.Literal("Failed to evaluate expression " + syntaxName + ":")),
                    errExpr));
        */

        return
            WithTypeResult(
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        /*
                         * 2023-10-15
                        mapErrorExpression,
                        */
                        Syntax,
                        SyntaxFactory.IdentifierName(combinedExpression.IsTypeResult ? "AndThen" : "Map")))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Argument(
                                SyntaxFactory.SimpleLambdaExpression(SyntaxFactory.Parameter(okIdentifier))
                                .WithBody(combinedExpressionSyntax))))))
            .MergeDependencies(combinedExpressionDependencies)
            .MergeDependencies(Dependencies);
    }

    public static ExpressionSyntax BuildMapErrorExpression(
        ExpressionSyntax leftExpression,
        Func<ExpressionSyntax, ExpressionSyntax> map) =>
        SyntaxFactory.InvocationExpression(
            SyntaxFactory.MemberAccessExpression(
                SyntaxKind.SimpleMemberAccessExpression,
                leftExpression,
                SyntaxFactory.IdentifierName(nameof(Result<int, int>.MapError))))
        .WithArgumentList(
            SyntaxFactory.ArgumentList(
                SyntaxFactory.SingletonSeparatedList(
                    SyntaxFactory.Argument(
                        SyntaxFactory.SimpleLambdaExpression(
                            SyntaxFactory.Parameter(
                                SyntaxFactory.Identifier("err")))
                        .WithExpressionBody(map(SyntaxFactory.IdentifierName("err")))))));

    static (CSharpSyntaxNode syntax, CompiledExpressionDependencies dependencies) ExpressionBodyOrBlock(
        ExpressionCompilationEnvironment environment,
        CompiledExpression compiledExpression)
    {
        var letBindingsAvailableFromParentKeys =
            (environment.ParentEnvironment?.EnumerateSelfAndAncestorsLetBindingsTransitive().Keys ?? [])
            .ToImmutableHashSet();

        var letBindingsTransitive =
            compiledExpression.EnumerateLetBindingsTransitive();

        var variableDeclarations =
            VariableDeclarationsForLetBindings(
                letBindingsTransitive,
                usagesSyntaxes: [compiledExpression.Syntax],
                excludeBinding: letBindingsAvailableFromParentKeys.Contains);

        var letBindingsAggregateDependencies =
            CompiledExpressionDependencies.Union(
                variableDeclarations.Select(b => b.letBinding.Expression.DependenciesIncludingLetBindings()));

        var aggregateDependencies =
            compiledExpression.Dependencies.Union(letBindingsAggregateDependencies);

        var blockSyntax =
            variableDeclarations is []
            ?
            compiledExpression.Syntax
            :
            (CSharpSyntaxNode)
            SyntaxFactory.Block(
                (StatementSyntax[])
                ([.. variableDeclarations.Select(b => b.declarationSyntax),
                    SyntaxFactory.ReturnStatement(compiledExpression.Syntax)])
                );

        return (blockSyntax, aggregateDependencies);
    }

    public static IReadOnlyList<(LetBinding letBinding, LocalDeclarationStatementSyntax declarationSyntax)>
        VariableDeclarationsForLetBindings(
        IReadOnlyDictionary<PineVM.Expression, LetBinding> availableLetBindings,
        IReadOnlyCollection<ExpressionSyntax> usagesSyntaxes,
        Func<PineVM.Expression, bool>? excludeBinding)
    {
        var usedLetBindings =
            usagesSyntaxes
            .SelectMany(usageSyntax => EnumerateUsedLetBindingsTransitive(usageSyntax, availableLetBindings))
            .Where(b => excludeBinding is null || !excludeBinding(b.Key))
            .Distinct()
            .ToImmutableDictionary();

        var orderedBindingsExpressions =
            CSharpDeclarationOrder.OrderExpressionsByContainment(
                usedLetBindings
                .OrderBy(b => b.Value.DeclarationName)
                .Select(b => b.Key));

        var orderedBindings =
            orderedBindingsExpressions
            .Select(bindingExpression => usedLetBindings[bindingExpression])
            .ToImmutableArray();

        return
            orderedBindings
            .Select(letBinding =>
            (letBinding,
            SyntaxFactory.LocalDeclarationStatement(
                SyntaxFactory.VariableDeclaration(
                    SyntaxFactory.IdentifierName(
                        SyntaxFactory.Identifier(
                            SyntaxFactory.TriviaList(),
                            SyntaxKind.VarKeyword,
                            "var",
                            "var",
                            SyntaxFactory.TriviaList())))
                .WithVariables(
                    variables: SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.VariableDeclarator(
                            SyntaxFactory.Identifier(letBinding.DeclarationName))
                        .WithInitializer(
                            SyntaxFactory.EqualsValueClause(letBinding.Expression.AsCsWithTypeResult())))))))
            .ToImmutableArray();
    }

    public static IEnumerable<KeyValuePair<PineVM.Expression, LetBinding>> EnumerateUsedLetBindingsTransitive(
        ExpressionSyntax usageRoot,
        IReadOnlyDictionary<PineVM.Expression, LetBinding> availableBindings)
    {
        foreach (var identiferName in usageRoot.DescendantNodesAndSelf().OfType<IdentifierNameSyntax>())
        {
            var matchingBinding =
                availableBindings
                .Where(binding => binding.Value.DeclarationName == identiferName.Identifier.ValueText)
                .FirstOrDefault();

            if (matchingBinding.Key is null)
                continue;

            yield return matchingBinding;

            foreach (var binding in EnumerateUsedLetBindingsTransitive(matchingBinding.Value.Expression.Syntax, availableBindings))
                yield return binding;
        }
    }

    public CompiledExpression Map(
        ExpressionCompilationEnvironment environment,
        Func<ExpressionSyntax, ExpressionSyntax> map)
    {
        return MapOrAndThen(
            environment,
            inner => new CompiledExpression(
            map(inner),
            IsTypeResult: false,
            LetBindings: NoLetBindings,
            CompiledExpressionDependencies.Empty));
    }

    public ExpressionSyntax AsCsWithTypeResult()
    {
        if (IsTypeResult)
            return Syntax;

        return WrapExpressionInPineValueResultOk(Syntax);
    }

    public static CompiledExpression ListMapOrAndThen(
        ExpressionCompilationEnvironment environment,
        Func<IReadOnlyList<ExpressionSyntax>, CompiledExpression> combine,
        IReadOnlyList<CompiledExpression> compiledList)
    {
        CompiledExpression recursive(
            Func<IReadOnlyList<ExpressionSyntax>, CompiledExpression> combine,
            ImmutableList<CompiledExpression> compiledList,
            ImmutableList<ExpressionSyntax> syntaxesCs)
        {
            if (compiledList.IsEmpty)
                return combine(syntaxesCs);

            return
                compiledList.First().MapOrAndThen(
                    environment,
                    itemCs => recursive(
                        combine,
                        compiledList.RemoveAt(0),
                        syntaxesCs.Add(itemCs)));
        }

        return
            recursive(
                combine,
                [.. compiledList],
                []);
    }

    public static ImmutableDictionary<KeyT, ValueT> Union<KeyT, ValueT>(
        IEnumerable<IReadOnlyDictionary<KeyT, ValueT>> dictionaries)
        where KeyT : notnull
        =>
        dictionaries.Aggregate(
            seed: ImmutableDictionary<KeyT, ValueT>.Empty,
            func: (aggregate, next) => aggregate.SetItems(next));
}

public record LetBinding(
    string DeclarationName,
    CompiledExpression Expression);


public record CompiledExpressionDependencies(
    ImmutableHashSet<PineValue> Values,
    IImmutableSet<(string hash, PineVM.Expression expression)> Expressions,
    IImmutableDictionary<PineVM.Expression, CompiledExpressionId> ExpressionFunctions)
{
    public static readonly CompiledExpressionDependencies Empty = new(
        Values: [],
        Expressions: [],
        ExpressionFunctions:
        ImmutableDictionary<PineVM.Expression, CompiledExpressionId>.Empty);

    public static (T, CompiledExpressionDependencies) WithNoDependencies<T>(T other) => (other, Empty);

    public CompiledExpressionDependencies Union(CompiledExpressionDependencies other) =>
        new(Values: Values.Union(other.Values),
            Expressions: Expressions.Union(other.Expressions),
            ExpressionFunctions: CompiledExpression.Union([ExpressionFunctions, other.ExpressionFunctions]));

    public static CompiledExpressionDependencies Union(IEnumerable<CompiledExpressionDependencies> dependencies) =>
        dependencies.Aggregate(
            seed: Empty,
            func: (aggregate, next) => aggregate.Union(next));
}

public record CompiledExpressionId(
    PineValue ExpressionValue,
    string ExpressionHashBase16);
