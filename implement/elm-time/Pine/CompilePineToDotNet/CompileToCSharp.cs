using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Globalization;
using System.Linq;
using System.Numerics;
using System.Security.Cryptography;
using System.Text;

namespace Pine.CompilePineToDotNet;

public record SyntaxContainerConfig(
    string containerTypeName,
    string dictionaryMemberName);

public record CompileCSharpClassResult(
    SyntaxContainerConfig SyntaxContainerConfig,
    ClassDeclarationSyntax ClassDeclarationSyntax,
    IReadOnlyList<UsingDirectiveSyntax> UsingDirectives);

public record GenerateCSharpFileResult(
    SyntaxContainerConfig SyntaxContainerConfig,
    CompilationUnitSyntax CompilationUnitSyntax,
    string FileText);

public partial class CompileToCSharp
{
    static private readonly CompilerMutableCache compilerCache = new();

    public static GenerateCSharpFileResult GenerateCSharpFile(
        CompileCSharpClassResult compileCSharpClassResult,
        IReadOnlyList<MemberDeclarationSyntax>? additionalMembers = null)
    {
        var compilationUnitSyntax =
            SyntaxFactory.CompilationUnit()
                .WithUsings(new SyntaxList<UsingDirectiveSyntax>(compileCSharpClassResult.UsingDirectives))
                .WithMembers(
                    SyntaxFactory.List(
                        [.. (additionalMembers ?? []), compileCSharpClassResult.ClassDeclarationSyntax]));

        var formattedNode =
            FormatCSharpSyntaxRewriter.FormatSyntaxTree(compilationUnitSyntax.NormalizeWhitespace(eol: "\n"));

        return
            new GenerateCSharpFileResult(
                SyntaxContainerConfig: compileCSharpClassResult.SyntaxContainerConfig,
                formattedNode,
                FileText: formattedNode.ToFullString());
    }

    record CompiledExpressionFunction(
        CompiledExpressionId Identifier,
        BlockSyntax BlockSyntax,
        CompiledExpressionDependencies Dependencies);

    public static Result<string, CompileCSharpClassResult> CompileExpressionsToCSharpClass(
        IReadOnlyCollection<Expression> expressions,
        SyntaxContainerConfig containerConfig)
    {
        const string argumentEnvironmentName = "pine_environment";

        const string argumentEvalGenericName = "eval_generic";

        var parametersSyntaxes = new[]
        {
            SyntaxFactory.Parameter(SyntaxFactory.Identifier(argumentEvalGenericName))
                .WithType(EvalExprDelegateTypeSyntax),

            SyntaxFactory.Parameter(SyntaxFactory.Identifier(argumentEnvironmentName))
                .WithType(SyntaxFactory.IdentifierName("PineValue")),
        };

        var usingDirectivesTypes = new[]
        {
            typeof(PineValue),
            typeof(ImmutableArray),
            typeof(IReadOnlyDictionary<,>),
            typeof(Func<,>),
            typeof(Enumerable)
        };

        var usingDirectives =
            usingDirectivesTypes
            .Select(t => t.Namespace)
            .WhereNotNull()
            .Distinct()
            .Select(ns => SyntaxFactory.UsingDirective(SyntaxFactory.IdentifierName(ns)))
            .ToImmutableList();

        MemberDeclarationSyntax memberDeclarationSyntaxForExpression(
            string declarationName,
            BlockSyntax blockSyntax)
        {
            return
                SyntaxFactory.MethodDeclaration(
                        returnType:
                        SyntaxFactory.GenericName(
                                SyntaxFactory.Identifier("Result"))
                            .WithTypeArgumentList(
                                SyntaxFactory.TypeArgumentList(
                                    SyntaxFactory.SeparatedList<TypeSyntax>(
                                        new SyntaxNodeOrToken[]
                                        {
                                            SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.StringKeyword)),
                                            SyntaxFactory.Token(SyntaxKind.CommaToken),
                                            SyntaxFactory.IdentifierName("PineValue")
                                        }))),
                        SyntaxFactory.Identifier(declarationName))
                    .WithModifiers(
                        SyntaxFactory.TokenList(
                            SyntaxFactory.Token(SyntaxKind.StaticKeyword),
                            SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                    .WithParameterList(
                        SyntaxFactory.ParameterList(
                            SyntaxFactory.SeparatedList(parametersSyntaxes)))
                    .WithBody(blockSyntax);
        }

        static Result<string, IReadOnlyDictionary<Expression, CompiledExpressionFunction>> CompileExpressionFunctions(
            IReadOnlyCollection<Expression> expressions)
        {
            var dictionary = new Dictionary<Expression, CompiledExpressionFunction>();

            var queue = new Queue<Expression>(expressions);

            while (queue.Any())
            {
                var expression = queue.Dequeue();

                if (dictionary.ContainsKey(expression))
                    continue;

                var derivedId =
                CompiledExpressionId(expression)
                .Extract(err => throw new Exception(err));

                var result =
                    compilerCache.CompileToCSharpFunctionBlockSyntax(
                        expression,
                        new FunctionCompilationEnvironment(
                            ArgumentEnvironmentName: argumentEnvironmentName,
                            ArgumentEvalGenericName: argumentEvalGenericName))
                        .MapError(err => "Failed to compile expression " + derivedId.ExpressionHashBase16[..10] + ": " + err)
                        .Map(ok =>
                            new CompiledExpressionFunction(
                                derivedId,
                                ok.blockSyntax,
                                ok.dependencies));

                if (result is Result<string, CompiledExpressionFunction>.Ok ok)
                {
                    dictionary.Add(expression, ok.Value);

                    foreach (var item in ok.Value.Dependencies.ExpressionFunctions)
                        queue.Enqueue(item.Key);
                }
                else
                {
                    return
                        result
                        .MapError(err => "Failed to compile expression " + expression + ": " + err)
                        .Map(_ => (IReadOnlyDictionary<Expression, CompiledExpressionFunction>)
                        ImmutableDictionary<Expression, CompiledExpressionFunction>.Empty);
                }
            }

            return
                Result<string, IReadOnlyDictionary<Expression, CompiledExpressionFunction>>.ok(dictionary);
        }

        return
            CompileExpressionFunctions(expressions)
            .Map(compiledExpressions => compiledExpressions.OrderBy(ce => ce.Value.Identifier.ExpressionHashBase16))
            .AndThen(compiledExpressions =>
            {
                var aggregateDependencies =
                    CompiledExpressionDependencies.Union(compiledExpressions.Select(er => er.Value.Dependencies));

                var aggregateValueDependencies =
                    aggregateDependencies.Values
                    .Union(compiledExpressions.Select(forExprFunction => forExprFunction.Value.Identifier.ExpressionValue))
                    .Union(aggregateDependencies.Expressions.SelectMany(e => EnumerateAllLiterals(e.expression)));

                var usedValues = new HashSet<PineValue>();

                void registerValueUsagesRecursive(PineValue pineValue)
                {
                    if (usedValues.Contains(pineValue))
                        return;

                    usedValues.Add(pineValue);

                    if (pineValue is not PineValue.ListValue list)
                        return;

                    foreach (var i in list.Elements)
                        registerValueUsagesRecursive(i);
                }

                foreach (var item in aggregateValueDependencies)
                {
                    registerValueUsagesRecursive(item);
                }

                var valuesToDeclare =
                CSharpDeclarationOrder.OrderValuesForDeclaration(aggregateValueDependencies.Concat(usedValues).Distinct())
                .ToImmutableList();

                ExpressionSyntax? specialSyntaxForPineValue(PineValue pineValue) =>
                    valuesToDeclare.Contains(pineValue)
                        ? SyntaxFactory.IdentifierName(DeclarationNameForValue(pineValue))
                        : null;

                (string memberName, TypeSyntax typeSyntax, ExpressionSyntax memberDeclaration)
                    memberDeclarationForValue(PineValue pineValue)
                {
                    var valueExpression = CompileToCSharpLiteralExpression(pineValue, specialSyntaxForPineValue);

                    var memberName = DeclarationNameForValue(pineValue);

                    return
                        (memberName,
                            SyntaxFactory.IdentifierName("PineValue"),
                            valueExpression);
                }

                (string memberName, TypeSyntax typeSyntax, ExpressionSyntax memberDeclaration)
                    memberDeclarationForExpression((string hash, Expression expression) hashAndExpr)
                {
                    var expressionExpression =
                        EncodePineExpressionAsCSharpExpression(hashAndExpr.expression, specialSyntaxForPineValue)
                            .Extract(err => throw new Exception("Failed to encode expression: " + err));

                    var memberName = MemberNameForExpression(hashAndExpr.hash[..10]);

                    return
                        (memberName,
                            SyntaxFactory.QualifiedName(
                                SyntaxFactory.QualifiedName(
                                    SyntaxFactory.IdentifierName("Pine"),
                                    SyntaxFactory.IdentifierName("PineVM")),
                                SyntaxFactory.IdentifierName("Expression")),
                            expressionExpression);
                }

                var valuesStaticMembers =
                    valuesToDeclare
                    .Select(memberDeclarationForValue)
                    .ToImmutableList();

                var expressionStaticMembers =
                    aggregateDependencies.Expressions
                        .Select(memberDeclarationForExpression)
                        .DistinctBy(member => member.memberName)
                        .ToImmutableList();

                var dictionaryKeyTypeSyntax =
                CompileTypeSyntax.TypeSyntaxFromType(
                    typeof(PineValue),
                    usingDirectives);

                var dictionaryValueTypeSyntax =
                CompileTypeSyntax.TypeSyntaxFromType(
                    typeof(Func<PineVM.PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>),
                    usingDirectives);

                var dictionaryMemberType =
                CompileTypeSyntax.TypeSyntaxFromType(
                    typeof(IReadOnlyDictionary<PineValue, Func<PineVM.PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>),
                    usingDirectives);

                var dictionaryEntries =
                    compiledExpressions
                    .Select(compiledExpression =>
                    {
                        var declarationName = MemberNameForCompiledExpressionFunction(compiledExpression.Value.Identifier);

                        return
                            (SyntaxFactory.IdentifierName(DeclarationNameForValue(compiledExpression.Value.Identifier.ExpressionValue)),
                            SyntaxFactory.IdentifierName(declarationName));
                    })
                    .ToImmutableList();

                var dictionaryExpression =
                CompileDictionarySyntax.ImmutableDictionaryExpressionSyntax(
                    keyTypeSyntax: dictionaryKeyTypeSyntax,
                    valueTypeSyntax: dictionaryValueTypeSyntax,
                    dictionaryEntries: dictionaryEntries);

                var dictionaryMemberDeclaration =
                    SyntaxFactory.MethodDeclaration(
                        dictionaryMemberType,
                        identifier: containerConfig.dictionaryMemberName)
                        .WithModifiers(
                            SyntaxFactory.TokenList(
                                SyntaxFactory.Token(SyntaxKind.StaticKeyword),
                                SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                        .WithExpressionBody(
                            SyntaxFactory.ArrowExpressionClause(dictionaryExpression))
                        .WithSemicolonToken(
                            SyntaxFactory.Token(SyntaxKind.SemicolonToken));

                var staticReadonlyFieldMembers = new[]
                {
                    (memberName: "value_true",
                    typeSyntax:
                    (TypeSyntax)SyntaxFactory.IdentifierName("PineValue"),
                    (ExpressionSyntax)SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            SyntaxFactory.MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                SyntaxFactory.IdentifierName("Pine"),
                                SyntaxFactory.IdentifierName("PineVM")),
                            SyntaxFactory.IdentifierName("PineVM")),
                        SyntaxFactory.IdentifierName("TrueValue"))),

                    ("value_false",
                    SyntaxFactory.IdentifierName("PineValue"),
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            SyntaxFactory.MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                SyntaxFactory.IdentifierName("Pine"),
                                SyntaxFactory.IdentifierName("PineVM")),
                            SyntaxFactory.IdentifierName("PineVM")),
                        SyntaxFactory.IdentifierName("FalseValue"))),
                }
                .Concat(valuesStaticMembers)
                .Concat(expressionStaticMembers)
                .ToImmutableList();

                var staticFieldsDeclarations =
                    staticReadonlyFieldMembers
                    .Select(member =>
                    SyntaxFactory.FieldDeclaration(
                        SyntaxFactory.VariableDeclaration(member.typeSyntax)
                        .WithVariables(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.VariableDeclarator(SyntaxFactory.Identifier(member.memberName))
                                .WithInitializer(SyntaxFactory.EqualsValueClause(member.Item3)))))
                    .WithModifiers(
                        SyntaxFactory.TokenList(
                            SyntaxFactory.Token(SyntaxKind.StaticKeyword),
                            SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                            SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword)))
                    )
                    .ToImmutableList();

                var compiledExpressionsMemberDeclarations =
                compiledExpressions
                .Select(compiledExpression =>
                memberDeclarationSyntaxForExpression(
                    declarationName: MemberNameForCompiledExpressionFunction(compiledExpression.Value.Identifier),
                    blockSyntax: compiledExpression.Value.BlockSyntax))
                .ToImmutableList();

                return Result<string, CompileCSharpClassResult>.ok(
                    new CompileCSharpClassResult(
                        SyntaxContainerConfig: containerConfig,
                        ClassDeclarationSyntax:
                        SyntaxFactory.ClassDeclaration(containerConfig.containerTypeName)
                            .WithMembers(
                                SyntaxFactory.List(
                                    [dictionaryMemberDeclaration
                                    ,
                                        .. compiledExpressionsMemberDeclarations
                                    ,
                                        .. staticFieldsDeclarations])),
                        UsingDirectives: usingDirectives));
            });
    }

    private static QualifiedNameSyntax EvalExprDelegateTypeSyntax =>
        SyntaxFactory.QualifiedName(
            PineVmClassQualifiedNameSyntax,
            SyntaxFactory.IdentifierName(nameof(PineVM.PineVM.EvalExprDelegate)));

    private static QualifiedNameSyntax PineVmClassQualifiedNameSyntax =>
        SyntaxFactory.QualifiedName(
            SyntaxFactory.QualifiedName(
                SyntaxFactory.IdentifierName("Pine"),
                SyntaxFactory.IdentifierName("PineVM")),
            SyntaxFactory.IdentifierName("PineVM"));

    public static Result<string, (BlockSyntax blockSyntax, CompiledExpressionDependencies dependencies)>
        CompileToCSharpFunctionBlockSyntax(
            Expression expression,
            FunctionCompilationEnvironment environment) =>
        CompileToCSharpExpression(
            expression,
            new ExpressionCompilationEnvironment(
                FunctionEnvironment: environment,
                LetBindings: ImmutableDictionary<Expression, LetBinding>.Empty,
                ParentEnvironment: null),
            createLetBindingsForCse: true)
            .Map(exprWithDependencies =>
            {
                var availableLetBindings =
                exprWithDependencies.EnumerateLetBindingsTransitive();

                var returnExpression = exprWithDependencies.AsCsWithTypeResult();

                var variableDeclarations =
                CompiledExpression.VariableDeclarationsForLetBindings(
                    availableLetBindings,
                    usagesSyntaxes: [returnExpression],
                    excludeBinding: null);

                var combinedDependencies =
                    CompiledExpressionDependencies.Union(
                        variableDeclarations
                        .Select(b => b.letBinding.Expression.DependenciesIncludingLetBindings())
                        .Prepend(exprWithDependencies.DependenciesIncludingLetBindings()));

                return
                (SyntaxFactory.Block(
                    (StatementSyntax[])
                    ([.. variableDeclarations.Select(b => b.declarationSyntax),
                        SyntaxFactory.ReturnStatement(returnExpression)])),
                        combinedDependencies);
            });

    public static ExpressionSyntax WrapExpressionInPineValueResultOk(ExpressionSyntax expression) =>
        SyntaxFactory.InvocationExpression(
            SyntaxFactory.MemberAccessExpression(
                SyntaxKind.SimpleMemberAccessExpression,
                SyntaxFactory.GenericName(
                        SyntaxFactory.Identifier(nameof(Result<int, int>)))
                    .WithTypeArgumentList(
                        SyntaxFactory.TypeArgumentList(
                            SyntaxFactory.SeparatedList<TypeSyntax>(
                                (IEnumerable<SyntaxNodeOrToken>)
                                [
                                    SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.StringKeyword)),
                                    SyntaxFactory.Token(SyntaxKind.CommaToken),
                                    SyntaxFactory.IdentifierName(nameof(PineValue))
                                ]))),
                SyntaxFactory.IdentifierName(nameof(Result<int, int>.ok))))
        .WithArgumentList(
            SyntaxFactory.ArgumentList(
                SyntaxFactory.SingletonSeparatedList(SyntaxFactory.Argument(expression))));

    public static Result<string, CompiledExpression> CompileToCSharpExpression(
        Expression expression,
        ExpressionCompilationEnvironment parentEnvironment,
        bool createLetBindingsForCse)
    {
        var letBindingsAvailableFromParent =
            parentEnvironment.EnumerateSelfAndAncestorsLetBindingsTransitive();

        if (letBindingsAvailableFromParent.TryGetValue(expression, out var letBinding))
        {
            return
                Result<string, CompiledExpression>.ok(
                    CompiledExpression.WithTypeResult(
                        SyntaxFactory.IdentifierName(letBinding.DeclarationName))
                    .MergeDependencies(letBinding.Expression.Dependencies));
        }

        var letBindingsAvailableFromParentKeys =
            letBindingsAvailableFromParent.Keys.ToImmutableHashSet();

        ExpressionCompilationEnvironment DescendantEnvironmentFromNewLetBindings(
            IReadOnlyDictionary<Expression, LetBinding> newLetBindings) =>
            parentEnvironment
            with
            {
                LetBindings = newLetBindings,
                ParentEnvironment = parentEnvironment
            };

        var newLetBindingsExpressionsForCse =
            createLetBindingsForCse
            ?
            CollectForCommonSubexpressionElimination(
                expression,
                skipSubexpression: letBindingsAvailableFromParentKeys.Contains)
            :
            [];

        var newLetBindingsExpressions = newLetBindingsExpressionsForCse;

        var newLetBindings =
            CSharpDeclarationOrder.OrderExpressionsByContainment(newLetBindingsExpressions)
            .Aggregate(
                seed: CompiledExpression.NoLetBindings,
                func:
                (dict, subexpression) =>
                {
                    return
                    CompileToCSharpExpression(
                        subexpression,
                        DescendantEnvironmentFromNewLetBindings(dict),
                        createLetBindingsForCse: true)
                    .Unpack(
                        fromErr: _ => dict,
                        fromOk: compileOk =>
                        {
                            var subexpressionValue =
                            PineVM.PineVM.EncodeExpressionAsValue(subexpression)
                            .Extract(err => throw new Exception(err));

                            var expressionHash = CommonConversion.StringBase16(compilerCache.ComputeHash(subexpressionValue));

                            var declarationName = "bind_" + expressionHash[..10];

                            return dict.SetItem(
                                subexpression,
                                new LetBinding(
                                    declarationName,
                                    compileOk));
                        });
                });

        var descendantEnvironment = DescendantEnvironmentFromNewLetBindings(newLetBindings);

        return
            CompileToCSharpExpressionWithoutCSE(
                expression,
                descendantEnvironment)
            .Map(beforeNewBindings => beforeNewBindings.MergeBindings(newLetBindings));
    }

    public static ImmutableHashSet<Expression> CollectForCommonSubexpressionElimination(
        Expression expression,
        Func<Expression, bool> skipSubexpression)
    {
        var subexpressionUsages =
            CountExpressionUsage(
                expression,
                skipDescending: skipSubexpression);

        var commonSubexpressionsIncludingDescendants =
            subexpressionUsages
            .Where(kvp => 0 < kvp.Value.Unconditional && (1 < kvp.Value.Unconditional + kvp.Value.Conditional))
            .Select(kvp => kvp.Key)
            .Where(IncludeForCommonSubexpressionElimination)
            .Where(c => !skipSubexpression(c))
            .ToImmutableHashSet();

        var commonSubexpressions =
            commonSubexpressionsIncludingDescendants.Intersect(
                CountExpressionUsage(
                    expression,
                    skipDescending:
                    se => skipSubexpression(se) || commonSubexpressionsIncludingDescendants.Contains(se)).Keys);

        return commonSubexpressions;
    }

    public static bool IncludeForCommonSubexpressionElimination(Expression expression) =>
        expression switch
        {
            Expression.DecodeAndEvaluateExpression => true,
            Expression.KernelApplicationExpression => true,
            Expression.ConditionalExpression => true,
            Expression.StringTagExpression => true,
            _ => false
        };

    private static Result<string, CompiledExpression> CompileToCSharpExpressionWithoutCSE(
        Expression expression,
        ExpressionCompilationEnvironment environment)
    {
        return
            expression switch
            {
                Expression.EnvironmentExpression =>
                Result<string, CompiledExpression>.ok(
                    CompiledExpression.WithTypePlainValue(
                        SyntaxFactory.IdentifierName(environment.FunctionEnvironment.ArgumentEnvironmentName))),

                Expression.ListExpression listExpr =>
                CompileToCSharpExpression(listExpr, environment),

                Expression.LiteralExpression literalExpr =>
                CompileToCSharpExpression(literalExpr),

                Expression.ConditionalExpression conditional =>
                CompileToCSharpExpression(conditional, environment),

                Expression.KernelApplicationExpression kernelApp =>
                CompileToCSharpExpression(kernelApp, environment),

                Expression.DecodeAndEvaluateExpression decodeAndEval =>
                CompileToCSharpExpression(decodeAndEval, environment),

                Expression.StringTagExpression stringTagExpr =>
                CompileToCSharpExpression(stringTagExpr, environment),

                _ =>
                Result<string, CompiledExpression>.err(
                    "Unsupported syntax kind: " + expression.GetType().FullName)
            };
    }

    public static Result<string, CompiledExpression> CompileToCSharpExpression(
        Expression.ListExpression listExpression,
        ExpressionCompilationEnvironment environment)
    {
        if (!listExpression.List.Any())
            return Result<string, CompiledExpression>.ok(
                CompiledExpression.WithTypePlainValue(PineCSharpSyntaxFactory.PineValueEmptyListSyntax));

        return
            listExpression.List.Select((itemExpression, itemIndex) =>
            CompileToCSharpExpression(
                itemExpression,
                environment,
                createLetBindingsForCse: false)
            .MapError(err => "Failed to translate list item " + itemIndex + ": " + err))
            .ListCombine()
            .Map(compiledItems =>
            {
                var aggregateSyntax =
                CompiledExpression.ListMapOrAndThen(
                    environment,
                    combine:
                    csharpItems =>
                    CompiledExpression.WithTypePlainValue(
                        SyntaxFactory.InvocationExpression(
                            SyntaxFactory.MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                SyntaxFactory.IdentifierName("PineValue"),
                                SyntaxFactory.IdentifierName("List")))
                        .WithArgumentList(
                            SyntaxFactory.ArgumentList(
                                SyntaxFactory.SingletonSeparatedList(
                                    SyntaxFactory.Argument(
                                        SyntaxFactory.CollectionExpression(
                                            SyntaxFactory.SeparatedList<CollectionElementSyntax>(
                                                csharpItems.Select(SyntaxFactory.ExpressionElement)))))))),
                    compiledItems);

                return aggregateSyntax;
            });
    }

    public static Result<string, CompiledExpression> CompileToCSharpExpression(
        Expression.KernelApplicationExpression kernelApplicationExpression,
        ExpressionCompilationEnvironment environment)
    {
        if (!KernelFunctionsInfo.Value.TryGetValue(kernelApplicationExpression.functionName,
              out var kernelFunctionInfo))
        {
            return
                Result<string, CompiledExpression>.err(
                    "Kernel function name " + kernelApplicationExpression.functionName + " does not match any of the " +
                    KernelFunctionsInfo.Value.Count + " known names: " +
                    string.Join(", ", KernelFunctionsInfo.Value.Keys));
        }

        return
            CompileKernelFunctionApplicationToCSharpExpression(
                kernelFunctionInfo,
                kernelApplicationExpression.argument,
                environment);
    }

    private static Result<string, CompiledExpression> CompileKernelFunctionApplicationToCSharpExpression(
        KernelFunctionInfo kernelFunctionInfo,
        Expression kernelApplicationArgumentExpression,
        ExpressionCompilationEnvironment environment)
    {
        var staticallyKnownArgumentsList =
            ParseKernelApplicationArgumentAsList(kernelApplicationArgumentExpression, environment)
            ?.Unpack(fromErr: err =>
            {
                Console.WriteLine("Failed to parse argument list: " + err);
                return null;
            },
            fromOk: ok => ok);

        static InvocationExpressionSyntax wrapInvocationInWithDefault(InvocationExpressionSyntax invocationExpressionSyntax)
        {
            return
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        invocationExpressionSyntax,
                        SyntaxFactory.IdentifierName("WithDefault")))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Argument(
                                SyntaxFactory.MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    SyntaxFactory.IdentifierName("PineValue"),
                                    SyntaxFactory.IdentifierName("EmptyList"))))));
        }

        if (kernelFunctionInfo.TryInline?.Invoke(kernelApplicationArgumentExpression, environment) is { } inlineNotNull)
        {
            return inlineNotNull;
        }

        if (staticallyKnownArgumentsList is not null)
        {
            foreach (var specializedImpl in kernelFunctionInfo.SpecializedImplementations)
            {
                if (specializedImpl.ParameterTypes.Count == staticallyKnownArgumentsList.Count)
                {
                    var argumentsResults =
                        specializedImpl.ParameterTypes
                            .Select((parameterType, parameterIndex) =>
                            {
                                if (!staticallyKnownArgumentsList[parameterIndex].ArgumentSyntaxFromParameterType
                                        .TryGetValue(parameterType, out var param))
                                    return Result<string, CompiledExpression>.err(
                                        "No transformation found for parameter type " + parameterType);

                                return Result<string, CompiledExpression>.ok(param);
                            });

                    if (argumentsResults.ListCombine() is
                        Result<string, IReadOnlyList<CompiledExpression>>.Ok specializedOk)
                    {
                        var aggregateDependencies =
                            CompiledExpressionDependencies.Union(specializedOk.Value.Select(p => p.Dependencies));

                        var aggregateLetBindings =
                            CompiledExpression.Union(specializedOk.Value.Select(c => c.LetBindings));

                        var expressionReturningPineValue =
                            CompiledExpression.ListMapOrAndThen(
                                environment,
                                argumentsCs =>
                                {
                                    var plainInvocationSyntax = specializedImpl.CompileInvocation(argumentsCs);

                                    return
                                    CompiledExpression.WithTypePlainValue(
                                        specializedImpl.ReturnType.IsInstanceOfResult ?
                                        wrapInvocationInWithDefault(plainInvocationSyntax)
                                        :
                                        plainInvocationSyntax,
                                        aggregateLetBindings,
                                        aggregateDependencies);
                                },
                                specializedOk.Value);

                        return
                            Result<string, CompiledExpression>.ok(expressionReturningPineValue);
                    }
                }
            }
        }

        return
            CompileToCSharpExpression(
                kernelApplicationArgumentExpression,
                environment,
                createLetBindingsForCse: false)
            .Map(compiledArgument =>
            compiledArgument.Map(environment, argumentCs =>
            wrapInvocationInWithDefault(kernelFunctionInfo.CompileGenericInvocation(argumentCs)))
            .MergeBindings(compiledArgument.LetBindings));
    }

    public static Result<string, CompiledExpression> CompileToCSharpExpression(
        Expression.ConditionalExpression conditionalExpression,
        ExpressionCompilationEnvironment environment)
    {
        return
            CompileToCSharpExpression(
                conditionalExpression.condition,
                environment,
                createLetBindingsForCse: false)
            .MapError(err => "Failed to compile condition: " + err)
            .AndThen(compiledCondition =>
            CompileToCSharpExpression(
                conditionalExpression.ifTrue,
                environment,
                createLetBindingsForCse: true)
            .MapError(err => "Failed to compile branch if true: " + err)
            .AndThen(compiledIfTrue =>
            CompileToCSharpExpression(
                conditionalExpression.ifFalse,
                environment,
                createLetBindingsForCse: true)
            .MapError(err => "Failed to compile branch if false: " + err)
            .Map(compiledIfFalse =>
            {
                CompiledExpression continueWithConditionCs(ExpressionSyntax conditionCs)
                {
                    if (!(compiledIfTrue.IsTypeResult || compiledIfFalse.IsTypeResult))
                    {
                        return
                        CompiledExpression.WithTypePlainValue(SyntaxFactory.ConditionalExpression(
                            conditionCs,
                            compiledIfTrue.Syntax,
                            compiledIfFalse.Syntax));
                    }

                    return
                    CompiledExpression.WithTypeResult(
                        SyntaxFactory.ConditionalExpression(
                            conditionCs,
                            compiledIfTrue.AsCsWithTypeResult(),
                            compiledIfFalse.AsCsWithTypeResult()));
                }

                var aggregateLetBindings =
                CompiledExpression.Union(
                    [
                        compiledCondition.LetBindings,
                        compiledIfTrue.LetBindings,
                        compiledIfFalse.LetBindings
                    ]);

                var
                combinedExpr =
                compiledCondition
                .MapOrAndThen(
                    environment,
                    conditionCs =>
                    continueWithConditionCs(
                        SyntaxFactory.BinaryExpression(
                            SyntaxKind.EqualsExpression,
                            SyntaxFactory.IdentifierName("value_true"),
                            conditionCs))
                    .MergeBindings(aggregateLetBindings)
                    .MergeDependencies(
                        compiledCondition.Dependencies
                        .Union(compiledIfTrue.Dependencies)
                        .Union(compiledIfFalse.Dependencies)));

                return combinedExpr;
            }
            )));
    }

    public static Result<string, CompiledExpression> CompileToCSharpExpression(
        Expression.DecodeAndEvaluateExpression decodeAndEvaluateExpression,
        ExpressionCompilationEnvironment environment)
    {
        var decodeAndEvaluateExpressionValue =
            PineVM.PineVM.EncodeExpressionAsValue(decodeAndEvaluateExpression)
            .Extract(err => throw new Exception(err));

        var decodeAndEvaluateExpressionHash =
            CommonConversion.StringBase16(compilerCache.ComputeHash(decodeAndEvaluateExpressionValue));

        CompiledExpression continueWithGenericCase()
        {
            var decodeAndEvaluateExpressionExpressionId =
                CompiledExpressionId(decodeAndEvaluateExpression.expression)
                .Extract(err => throw new Exception(err));

            var decodeAndEvaluateExpressionEnvironmentId =
                CompiledExpressionId(decodeAndEvaluateExpression.environment)
                .Extract(err => throw new Exception(err));

            var envResultExpr =
                InvocationExpressionForCurrentEnvironment(
                    environment.FunctionEnvironment,
                    decodeAndEvaluateExpressionEnvironmentId);

            var exprResultExpr =
                InvocationExpressionForCurrentEnvironment(
                    environment.FunctionEnvironment,
                    decodeAndEvaluateExpressionExpressionId);

            return
                envResultExpr
                .MapOrAndThen(
                    environment,
                    envResultExprCs =>
                    exprResultExpr
                    .MapOrAndThen(
                        environment,
                        exprResultExprCs =>
                        {
                            var decodeAndEvalLiteralExpr =
                            NewConstructorOfExpressionVariant(
                                nameof(Expression.DecodeAndEvaluateExpression),
                                NewConstructorOfExpressionVariant(
                                    nameof(Expression.LiteralExpression),
                                    exprResultExprCs),
                                NewConstructorOfExpressionVariant(
                                    nameof(Expression.LiteralExpression),
                                    envResultExprCs));

                            var invocationExpression =
                            SyntaxFactory.InvocationExpression(
                                SyntaxFactory.IdentifierName(environment.FunctionEnvironment.ArgumentEvalGenericName))
                            .WithArgumentList(
                                SyntaxFactory.ArgumentList(
                                    SyntaxFactory.SeparatedList<ArgumentSyntax>(
                                        new SyntaxNodeOrToken[]
                                        {
                                            SyntaxFactory.Argument(decodeAndEvalLiteralExpr),
                                            SyntaxFactory.Token(SyntaxKind.CommaToken),
                                            SyntaxFactory.Argument(PineCSharpSyntaxFactory.PineValueEmptyListSyntax)
                                        })));

                            return
                            CompiledExpression.WithTypeResult(invocationExpression)
                                .MergeDependencies(
                                envResultExpr.Dependencies.Union(exprResultExpr.Dependencies)
                                .Union(
                                    CompiledExpressionDependencies.Empty
                                    with
                                    {
                                        ExpressionFunctions =
                                            ImmutableDictionary<Expression, CompiledExpressionId>.Empty
                                            .SetItem(decodeAndEvaluateExpression.expression, decodeAndEvaluateExpressionExpressionId)
                                            .SetItem(decodeAndEvaluateExpression.environment, decodeAndEvaluateExpressionEnvironmentId)
                                    }));
                        }));
        }

        if (Expression.IsIndependent(decodeAndEvaluateExpression.expression))
        {
            return
                TryEvaluateExpressionIndependent(decodeAndEvaluateExpression.expression)
                .MapError(err => "Failed evaluate inner as independent expression: " + err)
                .AndThen(innerExpressionValue =>
                compilerCache.DecodeExpressionFromValue(innerExpressionValue)
                .MapError(err => "Failed to decode inner expression: " + err)
                .AndThen(innerExpression =>
                {
                    var innerExpressionId = CompiledExpressionId(innerExpressionValue);

                    return
                    CompileToCSharpExpression(
                        decodeAndEvaluateExpression.environment,
                        environment,
                        createLetBindingsForCse: false)
                    .Map(compiledArgumentExpression =>
                    {
                        return
                        compiledArgumentExpression.MapOrAndThen(
                            environment,
                            argumentExprPlainValue =>
                        {
                            return
                            InvocationExpressionForCompiledExpressionFunction(
                                environment.FunctionEnvironment,
                                innerExpressionId,
                                argumentExprPlainValue)
                            .MergeDependencies(
                                compiledArgumentExpression.Dependencies.Union(
                                    CompiledExpressionDependencies.Empty
                                    with
                                    {
                                        ExpressionFunctions =
                                        ImmutableDictionary<Expression, CompiledExpressionId>.Empty
                                        .SetItem(innerExpression, innerExpressionId),
                                    }));
                        });
                    });
                }));
        }

        return
            Result<string, CompiledExpression>.ok(continueWithGenericCase());
    }

    public static CompiledExpression InvocationExpressionForCurrentEnvironment(
        FunctionCompilationEnvironment environment,
        CompiledExpressionId invokedFunction) =>
        InvocationExpressionForCompiledExpressionFunction(
            environment,
            invokedFunction,
            SyntaxFactory.IdentifierName(environment.ArgumentEnvironmentName));

    public static CompiledExpression InvocationExpressionForCompiledExpressionFunction(
        FunctionCompilationEnvironment environment,
        CompiledExpressionId invokedFunction,
        ExpressionSyntax environmentExpressionSyntax) =>
        CompiledExpression.WithTypeResult(
            SyntaxFactory.InvocationExpression(
                SyntaxFactory.IdentifierName(
                    MemberNameForCompiledExpressionFunction(invokedFunction)))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList<ArgumentSyntax>(
                        new SyntaxNodeOrToken[]
                        {
                            SyntaxFactory.Argument(
                                SyntaxFactory.IdentifierName(environment.ArgumentEvalGenericName)),
                            SyntaxFactory.Token(SyntaxKind.CommaToken),
                            SyntaxFactory.Argument(environmentExpressionSyntax)
                        }))));

    public static Result<string, CompiledExpressionId> CompiledExpressionId(Expression expression) =>
        PineVM.PineVM.EncodeExpressionAsValue(expression)
        .Map(CompiledExpressionId);

    public static CompiledExpressionId CompiledExpressionId(PineValue expressionValue)
    {
        var expressionHash = CommonConversion.StringBase16(compilerCache.ComputeHash(expressionValue));

        return
            new CompiledExpressionId(
                ExpressionValue: expressionValue,
                ExpressionHashBase16: expressionHash);
    }

    static string MemberNameForExpression(string expressionValueHash) =>
        "expression_" + expressionValueHash[..10];

    static string MemberNameForCompiledExpressionFunction(CompiledExpressionId derivedId) =>
        "expr_function_" + derivedId.ExpressionHashBase16[..10];

    public static Result<string, Expression> TransformPineExpressionWithOptionalReplacement(
        Func<Expression, Result<string, Maybe<Expression>>> findReplacement,
        Expression expression)
    {
        return
            findReplacement(expression)
            .MapError(err => "Failed to find replacement: " + err)
            .AndThen(maybeReplacement =>
            maybeReplacement
            .Map(r => Result<string, Expression>.ok(r))
            .WithDefaultBuilder(() =>
            {
                return expression switch
                {
                    Expression.LiteralExpression literal =>
                    Result<string, Expression>.ok(literal),

                    Expression.EnvironmentExpression =>
                    Result<string, Expression>.ok(expression),

                    Expression.ListExpression list =>
                    list.List.Select(e => TransformPineExpressionWithOptionalReplacement(findReplacement, e))
                    .ListCombine()
                    .Map(elements => (Expression)new Expression.ListExpression([.. elements])),

                    Expression.ConditionalExpression conditional =>
                    TransformPineExpressionWithOptionalReplacement(
                        findReplacement,
                        conditional.condition)
                    .AndThen(transformedCondition =>
                    TransformPineExpressionWithOptionalReplacement(
                        findReplacement,
                        conditional.ifTrue)
                    .AndThen(transformedIfTrue =>
                    TransformPineExpressionWithOptionalReplacement(
                        findReplacement,
                        conditional.ifFalse)
                    .Map(transformedIfFalse =>
                    (Expression)new Expression.ConditionalExpression(
                        transformedCondition,
                        transformedIfTrue,
                        transformedIfFalse)))),

                    Expression.KernelApplicationExpression kernelAppl =>
                    TransformPineExpressionWithOptionalReplacement(findReplacement, kernelAppl.argument)
                    .MapError(err => "Failed to transform kernel application argument: " + err)
                    .Map(transformedArgument => (Expression)new Expression.KernelApplicationExpression(
                        functionName: kernelAppl.functionName,
                        argument: transformedArgument,
                        function: null)),

                    Expression.StringTagExpression stringTag =>
                    TransformPineExpressionWithOptionalReplacement(findReplacement, stringTag.tagged)
                    .Map(transformedTagged => (Expression)new Expression.StringTagExpression(tag: stringTag.tag, tagged: transformedTagged)),

                    _ =>
                    Result<string, Expression>.err("Unsupported expression type: " + expression.GetType().FullName)
                };
            }));
    }

    public static Result<string, PineValue> TryEvaluateExpressionIndependent(Expression expression) =>
        expression switch
        {
            Expression.EnvironmentExpression =>
            Result<string, PineValue>.err("Expression depends on environment"),

            Expression.LiteralExpression literal =>
            Result<string, PineValue>.ok(literal.Value),

            Expression.ListExpression list =>
            list.List.Select(TryEvaluateExpressionIndependent)
            .ListCombine()
            .Map(PineValue.List),

            Expression.KernelApplicationExpression kernelApplication =>
            TryEvaluateExpressionIndependent(kernelApplication.argument)
            .MapError(err => "Failed to evaluate kernel application argument independent: " + err)
            .AndThen(argument => kernelApplication.function(argument)),

            Expression.DecodeAndEvaluateExpression decodeAndEvaluateExpression =>
            TryEvaluateExpressionIndependent(decodeAndEvaluateExpression)
            .Map(ok =>
            {
                Console.WriteLine("Successfully evaluated DecodeAndEvaluateExpression independent 🙃");

                return ok;
            }),

            Expression.StringTagExpression stringTag =>
            TryEvaluateExpressionIndependent(stringTag.tagged),

            _ =>
            Result<string, PineValue>.err("Unsupported expression type: " + expression.GetType().FullName)
        };

    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression.DecodeAndEvaluateExpression decodeAndEvaluateExpression)
    {
        if (TryEvaluateExpressionIndependent(decodeAndEvaluateExpression.environment) is Result<string, PineValue>.Ok envOk)
        {
            return
                new PineVM.PineVM().EvaluateExpression(decodeAndEvaluateExpression, PineValue.EmptyList)
                .MapError(err => "Got independent environment, but failed to evaluated: " + err);
        }

        return
            TryEvaluateExpressionIndependent(decodeAndEvaluateExpression.expression)
            .MapError(err => "Expression is not independent: " + err)
            .AndThen(compilerCache.DecodeExpressionFromValue)
            .AndThen(innerExpr => TryEvaluateExpressionIndependent(innerExpr)
            .MapError(err => "Inner expression is not independent: " + err));
    }

    public static Result<string, CompiledExpression> CompileToCSharpExpression(
        Expression.LiteralExpression literalExpression)
    {
        return
            Result<string, CompiledExpression>.ok(
                CompiledExpression.WithTypePlainValue(SyntaxFactory.IdentifierName(DeclarationNameForValue(literalExpression.Value)))
                .MergeDependencies(
                    CompiledExpressionDependencies.Empty with { Values = [literalExpression.Value] }));
    }

    public static string DeclarationNameForValue(PineValue pineValue) =>
        "value_" + CommonConversion.StringBase16(compilerCache.ComputeHash(pineValue))[..10];

    public static Result<string, CompiledExpression> CompileToCSharpExpression(
        Expression.StringTagExpression stringTagExpression,
        ExpressionCompilationEnvironment environment)
    {
        Console.WriteLine("Compiling string tag: " + stringTagExpression.tag);

        return
            CompileToCSharpExpression(
                stringTagExpression.tagged,
                environment,
                createLetBindingsForCse: false)
            .Map(compiledExpr =>
            compiledExpr.MapSyntax(s => s.InsertTriviaBefore(
                SyntaxFactory.Comment("/*\n" + stringTagExpression.tag + "\n*/"),
                SyntaxFactory.TriviaList())));
    }

    public static ExpressionSyntax CompileToCSharpLiteralExpression(
        PineValue pineValue,
        Func<PineValue, ExpressionSyntax?> overrideDefaultExpression)
    {
        ExpressionSyntax continueCompile(PineValue pineValue) =>
            overrideDefaultExpression(pineValue) ??
            CompileToCSharpLiteralExpression(pineValue, overrideDefaultExpression);

        if (pineValue == PineValue.EmptyList)
            return PineCSharpSyntaxFactory.PineValueEmptyListSyntax;

        if (PineValueAsInteger.SignedIntegerFromValue(pineValue) is Result<string, BigInteger>.Ok okInteger &&
            PineValueAsInteger.ValueFromSignedInteger(okInteger.Value) == pineValue)
        {
            if (okInteger.Value < long.MaxValue)
            {
                return
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName(nameof(PineValueAsInteger)),
                        SyntaxFactory.IdentifierName(nameof(PineValueAsInteger.ValueFromSignedInteger))))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Argument(
                                PineCSharpSyntaxFactory.ExpressionSyntaxForIntegerLiteral((long)okInteger.Value)))));
            }
        }

        if (PineValueAsString.StringFromValue(pineValue) is Result<string, string>.Ok okString)
        {
            return
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName(nameof(PineValueAsString)),
                        SyntaxFactory.IdentifierName(nameof(PineValueAsString.ValueFromString))))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Argument(
                                SyntaxFactory.LiteralExpression(
                                    SyntaxKind.StringLiteralExpression,
                                    SyntaxFactory.Literal(okString.Value))))));
        }

        ExpressionSyntax defaultRepresentationOfBlob(ReadOnlyMemory<byte> blob)
        {
            var bytesIntegers =
                blob
                .ToArray()
                .Select(b => PineCSharpSyntaxFactory.ExpressionSyntaxForIntegerLiteral(b));

            return
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName("PineValue"),
                        SyntaxFactory.IdentifierName("Blob")))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Argument(
                                SyntaxFactory.CastExpression(
                                    SyntaxFactory.ArrayType(
                                        SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.ByteKeyword)))
                                    .WithRankSpecifiers(
                                        SyntaxFactory.SingletonList(
                                            SyntaxFactory.ArrayRankSpecifier(
                                                SyntaxFactory.SingletonSeparatedList<ExpressionSyntax>(
                                                    SyntaxFactory.OmittedArraySizeExpression())))),
                                    SyntaxFactory.CollectionExpression(
                                        SyntaxFactory.SeparatedList<CollectionElementSyntax>(
                                            bytesIntegers.Select(SyntaxFactory.ExpressionElement))))
                                ))));
        }

        ExpressionSyntax defaultRepresentationOfList(IReadOnlyList<PineValue> list)
        {
            var elementsSyntaxes =
                list.Select(continueCompile).ToImmutableList();

            return
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName("PineValue"),
                        SyntaxFactory.IdentifierName("List")))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Argument(
                                SyntaxFactory.CollectionExpression(
                                    SyntaxFactory.SeparatedList<CollectionElementSyntax>(
                                        elementsSyntaxes
                                        .Select(SyntaxFactory.ExpressionElement)))
                                ))));
        }

        return pineValue switch
        {
            PineValue.BlobValue blobValue =>
            defaultRepresentationOfBlob(blobValue.Bytes),

            PineValue.ListValue listValue =>
            defaultRepresentationOfList(listValue.Elements),

            _ =>
            throw new Exception("Unknown value type: " + pineValue.GetType().FullName)
        };
    }

    private static IEnumerable<PineValue> EnumerateAllLiterals(Expression expression) =>
        expression switch
        {
            Expression.LiteralExpression literal =>
            [literal.Value],

            Expression.EnvironmentExpression =>
            [],

            Expression.ListExpression list =>
            list.List.SelectMany(EnumerateAllLiterals),

            Expression.KernelApplicationExpression kernelApplicationExpression =>
            EnumerateAllLiterals(kernelApplicationExpression.argument),

            Expression.ConditionalExpression conditionalExpression =>
            [.. EnumerateAllLiterals(conditionalExpression.condition)
            ,
                .. EnumerateAllLiterals(conditionalExpression.ifTrue)
            ,
                .. EnumerateAllLiterals(conditionalExpression.ifFalse)],

            Expression.DecodeAndEvaluateExpression decodeAndEvaluateExpression =>
            [.. EnumerateAllLiterals(decodeAndEvaluateExpression.expression)
            ,
                .. EnumerateAllLiterals(decodeAndEvaluateExpression.environment)],

            Expression.StringTagExpression stringTagExpression =>
            EnumerateAllLiterals(stringTagExpression.tagged),

            _ => throw new NotImplementedException("Expression type not implemented: " + expression.GetType().FullName)
        };

    public static string GetNameForExpression(ExpressionSyntax syntax)
    {
        var serialized = syntax.ToString();

        var utf8 = Encoding.UTF8.GetBytes(serialized);

        var hash = SHA256.HashData(utf8);

        return CommonConversion.StringBase16(hash)[..10];
    }

    public record ExpressionUsageCount(
        int Unconditional,
        int Conditional);

    public static IReadOnlyDictionary<Expression, ExpressionUsageCount> CountExpressionUsage(
        Expression expression,
        Func<Expression, bool> skipDescending)
    {
        var dictionary = new Dictionary<Expression, ExpressionUsageCount>();

        void Traverse(Expression expr, bool isConditional)
        {
            if (dictionary.TryGetValue(expr, out ExpressionUsageCount? currentCount))
            {
                dictionary[expr] = new ExpressionUsageCount(
                    isConditional ? currentCount.Unconditional : currentCount.Unconditional + 1,
                    isConditional ? currentCount.Conditional + 1 : currentCount.Conditional
                );
            }
            else
            {
                dictionary[expr] = new ExpressionUsageCount(isConditional ? 0 : 1, isConditional ? 1 : 0);
            }

            if (skipDescending(expr))
                return;

            switch (expr)
            {
                case Expression.LiteralExpression _:
                    // Leaf node, no further traversal needed
                    break;
                case Expression.ListExpression listExpr:
                    foreach (var subExpr in listExpr.List)
                        Traverse(subExpr, isConditional);
                    break;
                case Expression.DecodeAndEvaluateExpression decodeEvalExpr:
                    Traverse(decodeEvalExpr.expression, isConditional);
                    Traverse(decodeEvalExpr.environment, isConditional);
                    break;
                case Expression.KernelApplicationExpression kernelAppExpr:
                    Traverse(kernelAppExpr.argument, isConditional);
                    break;
                case Expression.ConditionalExpression conditionalExpr:
                    // For ConditionalExpression, traverse its branches as conditional

                    Traverse(conditionalExpr.condition, isConditional);
                    Traverse(conditionalExpr.ifTrue, true);
                    Traverse(conditionalExpr.ifFalse, true);
                    break;
                case Expression.EnvironmentExpression _:
                    // Leaf node, no further traversal needed
                    break;
                case Expression.StringTagExpression stringTagExpr:
                    Traverse(stringTagExpr.tagged, isConditional);
                    break;
                case Expression.DelegatingExpression _:
                    // DelegatingExpression might not need traversal depending on its delegate's behavior
                    // Adjust this part if necessary
                    break;
                default:
                    throw new NotImplementedException();
            }
        }

        Traverse(expression, false);
        return dictionary.ToImmutableDictionary();
    }
}
