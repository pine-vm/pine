using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Pine.Core.PopularEncodings;
using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Security.Cryptography;
using System.Text;

namespace Pine.CompilePineToDotNet;

using CoreSyntaxFactory = Core.DotNet.PineCSharpSyntaxFactory;

public record SyntaxContainerConfig(
    string ContainerTypeName,
    string DictionaryMemberName);

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
    static private readonly CompilerMutableCache s_compilerCache = new();

    static private readonly PineVMParseCache s_parseCache = new();

    public static GenerateCSharpFileResult GenerateCSharpFile(
        CompileCSharpClassResult compileCSharpClassResult,
        IReadOnlyList<MemberDeclarationSyntax>? additionalMembers = null)
    {
        var compilationUnitSyntax =
            SyntaxFactory.CompilationUnit()
                .WithUsings([.. compileCSharpClassResult.UsingDirectives])
                .WithMembers(
                    SyntaxFactory.List(
                        [.. additionalMembers ?? [], compileCSharpClassResult.ClassDeclarationSyntax]));

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
        CompiledExpressionDependencies Dependencies,
        FunctionCompilationEnv FunctionCompilationEnv);

    public static Result<string, CompileCSharpClassResult> CompileExpressionsToCSharpClass(
        IReadOnlyCollection<ExpressionUsageAnalysis> expressions,
        SyntaxContainerConfig containerConfig)
    {
        const string argumentEnvironmentName = "pine_environment";

        const string argumentEvalGenericName = "eval_generic";

        var usingDirectivesTypes = new[]
        {
            typeof(PineValue),
            typeof(ImmutableArray),
            typeof(IReadOnlyDictionary<,>),
            typeof(Func<,>),
            typeof(Enumerable),
            typeof(GenericEvalException),
            typeof(ParseExpressionException),
        };

        var usingDirectives =
            usingDirectivesTypes
            .Select(t => t.Namespace)
            .WhereNotNull()
            .Distinct()
            .Select(ns => SyntaxFactory.UsingDirective(SyntaxFactory.IdentifierName(ns)))
            .ToImmutableList();

        var declarationSyntaxContext =
            new DeclarationSyntaxContext(usingDirectives);

        MethodDeclarationSyntax memberDeclarationSyntaxForExpression(
            string declarationName,
            BlockSyntax blockSyntax,
            ExprFunctionCompilationInterface functionInterface)
        {
            return
                SyntaxFactory.MethodDeclaration(
                    returnType: SyntaxFactory.IdentifierName("PineValue"),
                    SyntaxFactory.Identifier(declarationName))
                .WithModifiers(
                    SyntaxFactory.TokenList(
                        SyntaxFactory.Token(SyntaxKind.StaticKeyword),
                        SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                .WithParameterList(
                    SyntaxFactory.ParameterList(
                        SyntaxFactory.SeparatedList(ComposeParameterList(functionInterface))))
                .WithBody(blockSyntax);
        }

        Result<string, IReadOnlyDictionary<ExpressionUsageAnalysis, CompiledExpressionFunction>> CompileExpressionFunctions(
            IReadOnlyCollection<ExpressionUsageAnalysis> expressionsUsagesBeforeOrdering)
        {
            var expressionsUsages =
                expressionsUsagesBeforeOrdering
                .OrderBy(eu => eu.CompiledExpressionId.ExpressionHashBase16)
                .ThenBy(eu => eu.EnvId?.HashBase16)
                .ToImmutableArray();

            var dictionary = new Dictionary<ExpressionUsageAnalysis, CompiledExpressionFunction>();

            var expressions =
                expressionsUsages
                .Select(eu => eu.Expression)
                .Distinct()
                .ToImmutableArray();

            var functionInterfaceBase =
                new ExprFunctionCompilationInterface(
                    EnvItemsParamNames: [([], argumentEnvironmentName)],
                    ArgumentEvalGenericName: argumentEvalGenericName);

            var compilationUnitAvailableExpr =
                expressions
                .Select(expr =>
                {
                    var availableSpecialized =
                    expressionsUsages
                    .SelectMany(exprUsage =>
                    exprUsage.Expression == expr && exprUsage.EnvId is { } envId ? (IReadOnlyList<PineValueClass>)[envId] : [])
                    .Distinct()
                    .Select(envConstraint =>
                    {
                        var envConstraintFunctionInterface =
                        BuildFunctionSpecializedCompilationInterface(functionInterfaceBase, expr, envConstraint);

                        return new KeyValuePair<PineValueClass, ExprFunctionCompilationInterface>(
                            envConstraint,
                            envConstraintFunctionInterface);
                    })
                    .ToImmutableDictionary();

                    var genericReprInterface =
                    availableSpecialized.Count is 0
                    ?
                    /*
                     * Since we currently use the generic function representations for entries in the global dictionary,
                     * we need to keep their interfaces (types) the same.
                     * */
                    // BuildFunctionSpecializedCompilationInterface(functionInterfaceBase, expr, envConstraint: null)
                    functionInterfaceBase
                    :
                    /*
                     * If we will emit any specialized representations for this expression, this implies we will branch
                     * statements to transition from the general case to the specialized cases.
                     * The current implementation to build these branch statements depends on the complete environment
                     * being available as function parameter, therefore do not use a specialized parameter list in this case.
                     * */
                    functionInterfaceBase;

                    return
                    new KeyValuePair<Expression, CompilationUnitEnvExprEntry>(
                        expr,
                        new CompilationUnitEnvExprEntry(
                            GenericReprInterface: genericReprInterface,
                            AvailableSpecialized: availableSpecialized));
                })
                .ToImmutableDictionary();

            var compilationUnitEnv =
                new CompilationUnitEnv(
                    AvailableExpr: compilationUnitAvailableExpr,
                    DefaultInterface: functionInterfaceBase);

            var queue = new Queue<Expression>(expressions);

            while (queue.Count is not 0)
            {
                var expression = queue.Dequeue();

                var onlyExprDerivedId =
                    CompiledExpressionId(expression)
                    .Extract(err => throw new Exception(err));

                var expressionUsages =
                    expressionsUsages
                    .Where(eu => eu.Expression == expression)
                    // Ensure we also have an entry for the general case, not just the constrained environments.
                    .Append(new ExpressionUsageAnalysis(expression, null))
                    .Distinct()
                    .ToImmutableArray();

                var supportedConstrainedEnvironments =
                    expressionsUsages
                    .Where(eu => eu.Expression == expression)
                    .Select(eu => eu.EnvId)
                    .WhereNotNull()
                    .ToImmutableArray();

                foreach (var expressionUsage in expressionUsages)
                {
                    if (dictionary.ContainsKey(expressionUsage))
                        continue;

                    var functionInterface =
                        compilationUnitEnv.GetInterfaceForExprUsage(
                            expression,
                            envConstraint: expressionUsage.EnvId)
                        ?? throw new Exception("Missing function interface for " + onlyExprDerivedId.ExpressionHashBase16[..10]);

                    var functionEnv =
                        new FunctionCompilationEnv(
                            SelfInterface: functionInterface,
                            CompilationUnit: compilationUnitEnv,
                            declarationSyntaxContext);

                    var result =
                        s_compilerCache.CompileToCSharpFunctionBlockSyntax(
                            expressionUsage,
                            branchesConstrainedEnvIds: supportedConstrainedEnvironments,
                            functionEnv)
                            .MapError(err =>
                            "Failed to compile expression " +
                            expressionUsage.CompiledExpressionId.ExpressionHashBase16[..10] + ": " + err)
                            .Map(ok =>
                                new CompiledExpressionFunction(
                                    expressionUsage.CompiledExpressionId,
                                    ok.blockSyntax,
                                    ok.dependencies,
                                    functionEnv));

                    if (result is Result<string, CompiledExpressionFunction>.Ok ok)
                    {
                        dictionary.Add(expressionUsage, ok.Value);

                        foreach (var item in ok.Value.Dependencies.ExpressionFunctions)
                            queue.Enqueue(item.Key);
                    }
                    else
                    {
                        return
                            result
                            .MapError(err =>
                            "Failed to compile expression " + expressionUsage.CompiledExpressionId.ExpressionHashBase16[..10] +
                            ": " + err)
                            .Map(_ => (IReadOnlyDictionary<ExpressionUsageAnalysis, CompiledExpressionFunction>)
                            ImmutableDictionary<ExpressionUsageAnalysis, CompiledExpressionFunction>.Empty);
                    }
                }
            }

            return
                Result<string, IReadOnlyDictionary<ExpressionUsageAnalysis, CompiledExpressionFunction>>.ok(dictionary);
        }

        return
            CompileExpressionFunctions(expressions)
            .AndThen(compiledExpressionsBeforeOrdering =>
            {
                var compiledExpressions =
                compiledExpressionsBeforeOrdering
                .OrderBy(ce => ce.Key.CompiledExpressionId.ExpressionHashBase16).ToImmutableArray();

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

                    for (var i = 0; i < list.Items.Length; i++)
                        registerValueUsagesRecursive(list.Items.Span[i]);
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

                ((string memberName, TypeSyntax typeSyntax, ExpressionSyntax memberDeclaration) commonProps, ValueSyntaxKind syntaxKind)
                    memberDeclarationForValue(PineValue pineValue)
                {
                    var valueExpression =
                    CoreSyntaxFactory.CompileToCSharpLiteralExpression(
                        pineValue,
                        specialSyntaxForPineValue,
                        declarationSyntaxContext: declarationSyntaxContext);

                    var memberName = DeclarationNameForValue(pineValue);

                    return
                        ((memberName,
                            SyntaxFactory.IdentifierName("PineValue"),
                            valueExpression.exprSyntax),
                            valueExpression.syntaxKind);
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
                    .Select(valueToInclude => (valueToInclude, decl: memberDeclarationForValue(valueToInclude)))
                    .OrderBy(valueAndMember => valueAndMember.decl.syntaxKind, CSharpDeclarationOrder.ValueSyntaxKindDeclarationOrder.Instance)
                    .ThenBy(valueAndMember => valueAndMember.valueToInclude, CSharpDeclarationOrder.ValueDeclarationOrder.Instance)
                    .Select(valueAndMember => valueAndMember.decl.commonProps)
                    .ToImmutableList();

                var expressionStaticMembers =
                    aggregateDependencies.Expressions
                    .Select(memberDeclarationForExpression)
                    .DistinctBy(member => member.memberName)
                    .OrderBy(member => member.memberName)
                    .ToImmutableList();

                var dictionaryKeyTypeSyntax =
                CompileTypeSyntax.TypeSyntaxFromType(
                    typeof(PineValue),
                    usingDirectives);

                var dictionaryValueTypeSyntax =
                CompileTypeSyntax.TypeSyntaxFromType(
                    typeof(Func<EvalExprDelegate, PineValue, PineValue>),
                    usingDirectives);

                var dictionaryMemberType =
                CompileTypeSyntax.TypeSyntaxFromType(
                    typeof(IReadOnlyDictionary<PineValue, Func<EvalExprDelegate, PineValue, PineValue>>),
                    usingDirectives);

                var dictionaryEntries =
                    compiledExpressions
                    /*
                     * Dictionary entries only for the general case, not for the constrained environments.
                     * */
                    .DistinctBy(ce => ce.Key.Expression)
                    .Select(compiledExpression =>
                    {
                        var declarationName =
                        MemberNameForCompiledExpressionFunction(
                            compiledExpression.Value.Identifier,
                            envConstraint: null);

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
                        identifier: containerConfig.DictionaryMemberName)
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
                            SyntaxFactory.IdentifierName("PineVMValues")),
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
                            SyntaxFactory.IdentifierName("PineVMValues")),
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
                    declarationName: MemberNameForCompiledExpressionFunction(
                        compiledExpression.Value.Identifier,
                        compiledExpression.Key.EnvId),
                    blockSyntax: compiledExpression.Value.BlockSyntax,
                    functionInterface: compiledExpression.Value.FunctionCompilationEnv.SelfInterface))
                .OrderBy(member => member.Identifier.ValueText)
                .ToImmutableList();

                return Result<string, CompileCSharpClassResult>.ok(
                    new CompileCSharpClassResult(
                        SyntaxContainerConfig: containerConfig,
                        ClassDeclarationSyntax:
                        SyntaxFactory.ClassDeclaration(containerConfig.ContainerTypeName)
                            .WithMembers(
                                SyntaxFactory.List(
                                    [dictionaryMemberDeclaration
                                    ,
                                        .. compiledExpressionsMemberDeclarations.Cast<MemberDeclarationSyntax>()
                                        , PineCSharpSyntaxFactory.ValueFromPathInValueDeclaration
                                        , PineCSharpSyntaxFactory.IsBlobDeclaration
                                        , PineCSharpSyntaxFactory.IsListDeclaration
                                    ,
                                        .. staticFieldsDeclarations])),
                        UsingDirectives: usingDirectives));
            });
    }

    public static Result<string, (BlockSyntax blockSyntax, CompiledExpressionDependencies dependencies)>
        CompileToCSharpFunctionBlockSyntax(
            Expression expression,
            PineValueClass? constrainedEnvId,
            IReadOnlyList<PineValueClass> branchesEnvIds,
            FunctionCompilationEnv compilationEnv)
    {
        if (constrainedEnvId is { })
        {
            return
                CompileToCSharpGeneralFunctionBlockSyntax(
                    expression,
                    branchesEnvIds: [],
                    compilationEnv,
                    envConstraint: constrainedEnvId);
        }


        return
            CompileToCSharpGeneralFunctionBlockSyntax(
                expression,
                branchesEnvIds: branchesEnvIds,
                compilationEnv,
                envConstraint: constrainedEnvId);
    }


    public static Result<string, (BlockSyntax blockSyntax, CompiledExpressionDependencies dependencies)>
        CompileToCSharpGeneralFunctionBlockSyntax(
            Expression expression,
            IReadOnlyList<PineValueClass> branchesEnvIds,
            FunctionCompilationEnv compilationEnv,
            PineValueClass? envConstraint) =>
        CompileToCSharpExpression(
            ReducePineExpression.SearchForExpressionReductionRecursive(
                maxDepth: 5,
                expression,
                s_parseCache),
            new ExpressionCompilationEnvironment(
                FunctionEnvironment: compilationEnv,
                LetBindings: ImmutableDictionary<Expression, LetBinding>.Empty,
                ParentEnvironment: null,
                EnvConstraint: envConstraint),
            createLetBindingsForCse: true)
            .Map(exprWithDependencies =>
            {
                var availableLetBindings =
                exprWithDependencies.EnumerateLetBindingsTransitive();

                var returnExpression = exprWithDependencies.AsCsWithTypeGenericValue();

                var variableDeclarations =
                CompiledExpression.VariableDeclarationsForLetBindings(
                    availableLetBindings,
                    usagesSyntaxes: [returnExpression],
                    excludeBinding: null);

                var generalExprFuncName =
                CompiledExpressionId(expression)
                .Extract(err => throw new Exception(err));

                var branchesForSpecializedRepr =
                branchesEnvIds
                // Order by number of items, to prioritize the more specialized branches.
                .OrderByDescending(envId => envId.ParsedItems.Count)
                .Select(envId =>
                {
                    return
                        PineCSharpSyntaxFactory.BranchForEnvId(
                        expression,
                        envId,
                        compilationEnv: compilationEnv,
                        prependStatments: []);
                })
                    .ToImmutableList();

                var valueDepsForBranchStatements =
                CompiledExpressionDependencies.Empty
                with
                {
                    Values =
                    [.. branchesEnvIds.SelectMany(envId => envId.ParsedItems.Select(item => item.Value))]
                };

                var combinedDependencies =
                    CompiledExpressionDependencies.Union(
                        [..variableDeclarations
                        .Select(b => b.letBinding.Expression.DependenciesIncludingLetBindings()),
                        exprWithDependencies.DependenciesIncludingLetBindings(),
                        valueDepsForBranchStatements]);

                return
                (SyntaxFactory.Block(
                    (StatementSyntax[])
                    [..branchesForSpecializedRepr
                    , ..variableDeclarations.Select(b => b.declarationSyntax),
                        SyntaxFactory.ReturnStatement(returnExpression)]),
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
                    new CompiledExpression(
                        Syntax: SyntaxFactory.IdentifierName(letBinding.DeclarationName),
                        LetBindings: CompiledExpression.NoLetBindings,
                        Dependencies: letBinding.Expression.Dependencies));
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

        bool skipSubexpression(Expression subexpression)
        {
            if (letBindingsAvailableFromParentKeys.Contains(subexpression))
                return true;

            return
                ExprFunctionCompilationInterface.TryResolveExpressionInFunction(
                    parentEnvironment.FunctionEnvironment.SelfInterface,
                    subexpression,
                    envConstraint: parentEnvironment.EnvConstraint)
                is not null;
        }

        var newLetBindingsExpressionsForCse =
            createLetBindingsForCse
            ?
            CollectForCommonSubexpressionElimination(
                expression,
                skipSubexpression: skipSubexpression)
            :
            [];

        var newLetBindingsExpressions =
            newLetBindingsExpressionsForCse
            .Where(subexpr =>
            /*
             * 2024-03-01: Disable CSE for expressions that contain further calls.
             * The observation was that CSE for these sometimes caused infinite recursion and stack overflow.
             * The reason for the infinite recursion is that with the current implementation, the bindings
             * can end up too in too outer scope, when they should be contained in a branch that is not taken
             * when the recursion reaches the base case.
             * */
            !Expression.EnumerateSelfAndDescendants(subexpr).Any(sec => sec is Expression.ParseAndEval))
            .ToImmutableArray();

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
                            ExpressionEncoding.EncodeExpressionAsValue(subexpression);

                            var expressionHash = Convert.ToHexStringLower(s_compilerCache.ComputeHash(subexpressionValue).Span);

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
            Expression.ParseAndEval => true,
            Expression.KernelApplication => true,
            Expression.Conditional => true,
            Expression.StringTag => true,
            _ => false
        };

    private static Result<string, CompiledExpression> CompileToCSharpExpressionWithoutCSE(
        Expression expression,
        ExpressionCompilationEnvironment environment)
    {
        if (expression is Expression.Literal literalExpr)
        {
            return
                CompileToCSharpExpression(
                    literalExpr,
                    environment.FunctionEnvironment.DeclarationSyntaxContext);
        }

        if (ExprFunctionCompilationInterface.TryResolveExpressionInFunction(
            environment.FunctionEnvironment.SelfInterface,
            expression,
            envConstraint: environment.EnvConstraint)
            is { } specialResolution)
        {
            return
                specialResolution switch
                {
                    ExprResolvedInFunction.ExprResolvedToLiteral resolvedAsLiteral =>
                    CompileToCSharpExpression(
                        Expression.LiteralInstance(resolvedAsLiteral.Value),
                        environment.FunctionEnvironment.DeclarationSyntaxContext),

                    ExprResolvedInFunction.ExprResolvedToFunctionParam resolvedToParam =>
                    Result<string, CompiledExpression>.ok(
                        CompiledExpression.WithTypeGenericValue(
                            CoreSyntaxFactory.BuildCSharpExpressionToGetItemFromPathOrEmptyList(
                                compositionExpr: SyntaxFactory.IdentifierName(resolvedToParam.ParameterName),
                                path: resolvedToParam.PathFromParam,
                                declarationSyntaxContext: DeclarationSyntaxContext.None))),

                    _ =>
                    Result<string, CompiledExpression>.err(
                        "Unsupported special resolution: " + specialResolution.GetType().FullName)
                };
        }

        return
            expression switch
            {
                Expression.Environment =>
                Result<string, CompiledExpression>.err("Did not find parameter matching environment reference"),

                Expression.List listExpr =>
                CompileToCSharpExpression(listExpr, environment),

                Expression.Conditional conditional =>
                CompileToCSharpExpression(conditional, environment),

                Expression.KernelApplication kernelApp =>
                CompileToCSharpExpression(kernelApp, environment),

                Expression.ParseAndEval parseAndEval =>
                CompileToCSharpExpression(parseAndEval, environment),

                Expression.StringTag stringTagExpr =>
                CompileToCSharpExpression(stringTagExpr, environment),

                _ =>
                Result<string, CompiledExpression>.err(
                    "Unsupported syntax kind: " + expression.GetType().FullName)
            };
    }

    public static Result<string, CompiledExpression> CompileToCSharpExpression(
        Expression.List listExpression,
        ExpressionCompilationEnvironment environment)
    {
        if (!listExpression.Items.Any())
            return Result<string, CompiledExpression>.ok(
                CompiledExpression.WithTypeGenericValue(
                    CoreSyntaxFactory.PineValueEmptyListSyntax(
                        environment.FunctionEnvironment.DeclarationSyntaxContext)));

        return
            listExpression.Items.Select((itemExpression, itemIndex) =>
            CompileToCSharpExpression(
                itemExpression,
                environment,
                createLetBindingsForCse: false)
            .MapError(err => "Failed to translate list item " + itemIndex + ": " + err))
            .ListCombine()
            .Map(compiledItems =>
            {
                var aggregateSyntax =
                CompiledExpression.ListMap(
                    environment,
                    combine:
                    csharpItems =>
                    CompiledExpression.WithTypeGenericValue(
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
        Expression.KernelApplication kernelApplicationExpression,
        ExpressionCompilationEnvironment environment)
    {
        if (!PineKernelFunctions.KernelFunctionsInfo.Value.TryGetValue(kernelApplicationExpression.Function,
              out var kernelFunctionInfo))
        {
            return
                Result<string, CompiledExpression>.err(
                    "Kernel function name " + kernelApplicationExpression.Function + " does not match any of the " +
                    PineKernelFunctions.KernelFunctionsInfo.Value.Count + " known names: " +
                    string.Join(", ", PineKernelFunctions.KernelFunctionsInfo.Value.Keys));
        }

        return
            CompileKernelFunctionApplicationToCSharpExpression(
                kernelFunctionInfo,
                kernelApplicationExpression.Input,
                environment);
    }

    private static Result<string, CompiledExpression> CompileKernelFunctionApplicationToCSharpExpression(
        PineKernelFunctions.KernelFunctionInfo kernelFunctionInfo,
        Expression kernelApplicationArgumentExpression,
        ExpressionCompilationEnvironment environment)
    {
        var staticallyKnownArgumentsList =
            ParseKernelApplicationInputAsList(kernelApplicationArgumentExpression, environment)
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
                            CompiledExpression.ListMap(
                                environment,
                                argumentsCs =>
                                {
                                    var plainInvocationSyntax =
                                    specializedImpl.CompileInvocation(
                                        argumentsCs,
                                        environment.FunctionEnvironment.DeclarationSyntaxContext);

                                    return
                                    CompiledExpression.WithTypeGenericValue(
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
            compiledArgument
            .Map(environment, argumentCs =>
            kernelFunctionInfo.CompileGenericInvocation(
                argumentCs,
                environment.FunctionEnvironment.DeclarationSyntaxContext))
            .MergeBindings(compiledArgument.LetBindings));
    }

    public static Result<string, CompiledExpression> CompileToCSharpExpression(
        Expression.Conditional conditionalExpression,
        ExpressionCompilationEnvironment environment)
    {
        return
            CompileToCSharpExpression(
                conditionalExpression.Condition,
                environment,
                createLetBindingsForCse: false)
            .MapError(err => "Failed to compile condition: " + err)
            .AndThen(compiledCondition =>
            CompileToCSharpExpression(
                conditionalExpression.TrueBranch,
                environment,
                createLetBindingsForCse: true)
            .MapError(err => "Failed to compile branch if true: " + err)
            .AndThen(compiledIfTrue =>
            CompileToCSharpExpression(
                conditionalExpression.FalseBranch,
                environment,
                createLetBindingsForCse: true)
            .MapError(err => "Failed to compile branch if false: " + err)
            .Map(compiledIfFalse =>
            {
                CompiledExpression continueWithConditionCs(ExpressionSyntax conditionCs)
                {
                    return
                    CompiledExpression.WithTypeGenericValue(SyntaxFactory.ConditionalExpression(
                        conditionCs,
                        compiledIfTrue.Syntax,
                        compiledIfFalse.Syntax));
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
        Expression.ParseAndEval parseAndEvalExpr,
        ExpressionCompilationEnvironment environment)
    {
        var parseAndEvalExprValue =
            ExpressionEncoding.EncodeExpressionAsValue(parseAndEvalExpr);

        var parseAndEvalExprHash =
            Convert.ToHexStringLower(s_compilerCache.ComputeHash(parseAndEvalExprValue).Span);

        var childPathEnvMap =
            PineVM.CodeAnalysis.BuildPathMapFromChildToParentEnv(parseAndEvalExpr.Environment);

        /*
         * 
         * 2024-02-17: Switch to older implementation of generic case from 2023,
         * to fix bug that caused generation of invalid C# code.
         * 
        CompiledExpression continueWithGenericCase()
        {
            var parseAndEvalExprExpressionId =
                CompiledExpressionId(parseAndEvalExpr.expression)
                .Extract(err => throw new Exception(err));

            var parseAndEvalExprEnvironmentId =
                CompiledExpressionId(parseAndEvalExpr.environment)
                .Extract(err => throw new Exception(err));

            var envResultExpr =
                InvocationExpressionForCurrentEnvironment(
                    environment.FunctionEnvironment,
                    parseAndEvalExprEnvironmentId);

            var exprResultExpr =
                InvocationExpressionForCurrentEnvironment(
                    environment.FunctionEnvironment,
                    parseAndEvalExprExpressionId);

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
                            var parseAndEvalLiteralExpr =
                            NewConstructorOfExpressionVariant(
                                nameof(Expression.ParseAndEvalExpression),
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
                                            SyntaxFactory.Argument(parseAndEvalLiteralExpr),
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
                                            .SetItem(parseAndEvalExpr.expression, parseAndEvalExprExpressionId)
                                            .SetItem(parseAndEvalExpr.environment, parseAndEvalExprEnvironmentId)
                                    }));
                        }));
        }
        */

        Result<string, CompiledExpression> continueWithGenericCase()
        {
            return
                CompileToCSharpExpression(
                    parseAndEvalExpr.Environment,
                    environment,
                    createLetBindingsForCse: false)
                .AndThen(envCompiledExpr =>
                CompileToCSharpExpression(
                    parseAndEvalExpr.Encoded,
                    environment,
                    createLetBindingsForCse: false)
                .Map(exprCompiledExpr =>
                {
                    var invocationExpression =
                        PineCSharpSyntaxFactory.GenericInvocationThrowingRuntimeExceptionOnError(
                            environment.FunctionEnvironment,
                            NewConstructorOfExpressionVariant(
                                nameof(Expression.ParseAndEval),
                                NewConstructorOfExpressionVariant(
                                    nameof(Expression.Literal),
                                    exprCompiledExpr.Syntax),
                                NewConstructorOfExpressionVariantWithoutArguments(
                                    nameof(Expression.Environment))),
                            environmentExpr: envCompiledExpr.Syntax);

                    return
                        CompiledExpression.WithTypeGenericValue(invocationExpression)
                        .MergeDependencies(envCompiledExpr.Dependencies)
                        .MergeBindings(envCompiledExpr.LetBindings)
                        .MergeDependencies(exprCompiledExpr.Dependencies)
                        .MergeBindings(exprCompiledExpr.LetBindings);
                }));
        }

        Result<string, CompiledExpression> ContinueForKnownExprValue(PineValue innerExpressionValue)
        {
            var innerExprValueId = CompiledExpressionId(innerExpressionValue);

            return
                s_parseCache.ParseExpression(innerExpressionValue)
                .Unpack(
                    fromErr: err =>
                    {
                        var messageTitle =
                        "Failed to parse expression from value " +
                        innerExprValueId.ExpressionHashBase16[..8] +
                        ": " + err +
                        " - expressionValue is " + Core.Interpreter.IntermediateVM.PineVM.DescribeValueForErrorMessage(innerExpressionValue);

                        var throwSyntax = CoreSyntaxFactory.ThrowParseExpressionException(
                            SyntaxFactory.LiteralExpression(
                                SyntaxKind.StringLiteralExpression,
                                SyntaxFactory.Literal(messageTitle)),
                            declarationSyntaxContext: environment.FunctionEnvironment.DeclarationSyntaxContext);

                        return
                        (Result<string, CompiledExpression>)
                        CompiledExpression.WithTypeGenericValue(throwSyntax);
                    },
                    fromOk:
                    innerExpression =>
                    {
                        var innerExpressionId = CompiledExpressionId(innerExpressionValue);

                        if (environment.EnvConstraint is { } envConstraint)
                        {
                            bool ChildEnvContstraintItemSatisfied(KeyValuePair<IReadOnlyList<int>, PineValue> envItem)
                            {
                                var mappedChildPath = childPathEnvMap(envItem.Key);

                                var mappedChildValue =
                                    mappedChildPath switch
                                    {
                                        ExprMappedToParentEnv.PathInParentEnv mappedToParentPath =>
                                        envConstraint.TryGetValue(mappedToParentPath.Path),

                                        ExprMappedToParentEnv.LiteralInParentEnv mappedLiteralInParentEnv =>
                                        mappedLiteralInParentEnv.Value,

                                        null =>
                                        null,

                                        _ =>
                                        throw new Exception("Unexpected path map result: " + mappedChildPath)
                                    };

                                return mappedChildValue == envItem.Value;
                            }

                            if (environment.FunctionEnvironment.CompilationUnit.AvailableExpr.TryGetValue(
                                innerExpression,
                                out var exprEntry))
                            {
                                var specializedMostConstrainedFirst =
                                    exprEntry.AvailableSpecialized
                                    .OrderByDescending(kv => kv.Key.ParsedItems.Count);

                                foreach (var specializedEnvConstraint in specializedMostConstrainedFirst)
                                {
                                    if (specializedEnvConstraint.Key.ParsedItems.All(ChildEnvContstraintItemSatisfied))
                                    {
                                        return
                                        InvocationExpressionForCompiledExpressionFunction(
                                            environment,
                                            invokedExpr: innerExpression,
                                            envConstraint: specializedEnvConstraint.Key,
                                            parseAndEvalEnvExpr: parseAndEvalExpr.Environment);
                                    }
                                }
                            }
                        }

                        return
                            InvocationExpressionForCompiledExpressionFunction(
                                environment,
                                invokedExpr: innerExpression,
                                envConstraint: null,
                                parseAndEvalExpr.Environment)
                            .Map(compiledInvocation =>
                            compiledInvocation
                            .MergeDependencies(
                                CompiledExpressionDependencies.Empty
                                with
                                {
                                    ExpressionFunctions =
                                    ImmutableDictionary<Expression, CompiledExpressionId>.Empty
                                    .SetItem(innerExpression, innerExpressionId),
                                }));
                    });
        }

        if (!parseAndEvalExpr.Encoded.ReferencesEnvironment)
        {
            return
                ReducePineExpression.TryEvaluateExpressionIndependent(parseAndEvalExpr.Encoded, s_parseCache)
                .MapError(err => "Failed evaluate inner as independent expression: " + err)
                .AndThen(ContinueForKnownExprValue);
        }

        var exprMappedToParent =
            ExprFunctionCompilationInterface.TryResolveExpressionInFunction(
                environment.FunctionEnvironment.SelfInterface,
                parseAndEvalExpr.Encoded,
                environment.EnvConstraint);

        if (exprMappedToParent is ExprResolvedInFunction.ExprResolvedToLiteral literal)
        {
            return ContinueForKnownExprValue(literal.Value);
        }

        return continueWithGenericCase();
    }

    public static Result<string, CompiledExpression> InvocationExpressionForCompiledExpressionFunction(
        ExpressionCompilationEnvironment currentEnv,
        Expression invokedExpr,
        PineValueClass? envConstraint,
        Expression parseAndEvalEnvExpr)
    {
        var invokedExprFunctionId =
            CompiledExpressionId(invokedExpr)
            .Extract(err => throw new Exception(err));

        var invokedExprFunctionInterface =
            currentEnv.FunctionEnvironment.CompilationUnit.GetInterfaceForExprUsage(
                invokedExpr,
                envConstraint: envConstraint)
            ?? throw new Exception("Missing function interface for " + invokedExprFunctionId.ExpressionHashBase16[..10]);

        return
        ComposeArgumentList(
            parentEnv: currentEnv,
            invocationEnvExpr: parseAndEvalEnvExpr,
            invokedExprInterface: invokedExprFunctionInterface)
        .Map(argListAndDeps =>
            CompiledExpression.WithTypeGenericValue(
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.IdentifierName(
                        MemberNameForCompiledExpressionFunction(
                            invokedExprFunctionId,
                            envConstraint: envConstraint)))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList(argListAndDeps.argumentList))))
            .MergeDependencies(argListAndDeps.argumentsDeps)
            .MergeBindings(argListAndDeps.argumentsBindings));
    }

    public static IReadOnlyList<ParameterSyntax> ComposeParameterList(
        ExprFunctionCompilationInterface functionInterface)
    {
        var evalGenericDelegateParam =
            SyntaxFactory.Parameter(SyntaxFactory.Identifier(functionInterface.ArgumentEvalGenericName))
            .WithType(PineCSharpSyntaxFactory.EvalExprDelegateTypeSyntax);

        var envItemsParameters =
            functionInterface.EnvItemsParamNames
            .Select(pathAndParamName =>
            SyntaxFactory.Parameter(SyntaxFactory.Identifier(pathAndParamName.paramName))
            .WithType(SyntaxFactory.IdentifierName("PineValue")));

        return
        [
            evalGenericDelegateParam,
            ..envItemsParameters
        ];
    }

    public static Result<string, (IReadOnlyList<ArgumentSyntax> argumentList, CompiledExpressionDependencies argumentsDeps, ImmutableDictionary<Expression, LetBinding> argumentsBindings)>
        ComposeArgumentList(
        ExpressionCompilationEnvironment parentEnv,
        Expression invocationEnvExpr,
        ExprFunctionCompilationInterface invokedExprInterface)
    {
        var envItemsArgExprs =
            invokedExprInterface.ComposeArgumentsExpressionsForInvocation(invocationEnvExpr, parseCache: s_parseCache);

        var compiledItemsArgs =
            envItemsArgExprs
            .Select(envItemArgExpr =>
            CompileToCSharpExpression(
                envItemArgExpr,
                parentEnv,
                createLetBindingsForCse: false)
                .MapError(err => "Failed to compile argument: " + err))
            .ListCombine();

        return
            compiledItemsArgs
            .Map(compiledArgumentsExpressions =>
            {
                var dependencies =
                CompiledExpressionDependencies.Union(compiledArgumentsExpressions.Select(c => c.Dependencies));

                var bindings =
                compiledArgumentsExpressions
                .SelectMany(c => c.LetBindings)
                .ToImmutableArray();

                return
                    ((IReadOnlyList<ArgumentSyntax>)
                    [
                        SyntaxFactory.Argument(SyntaxFactory.IdentifierName(
                            parentEnv.FunctionEnvironment.SelfInterface.ArgumentEvalGenericName)),
                        ..
                        compiledArgumentsExpressions.Select(compiledArgumentExpression =>
                        SyntaxFactory.Argument(compiledArgumentExpression.Syntax))
                    ],
                    dependencies,
                    bindings.ToImmutableDictionary());
            });
    }

    public static ExprFunctionCompilationInterface BuildFunctionSpecializedCompilationInterface(
        ExprFunctionCompilationInterface baseEnv,
        Expression compiledExpr,
        PineValueClass? envConstraint)
    {
        var invokedExprEnvItemsPaths =
            ExprFunctionCompilationInterface.CompileEnvItemsPathsForExprFunction(compiledExpr, envConstraint);

        var envParams =
            ExprFunctionCompilationInterface.EnvItemsParamNamesFromPaths(
                invokedExprEnvItemsPaths,
                commonPrefix: "func_env");

        return
            baseEnv
            with
            {
                EnvItemsParamNames = envParams
            };
    }

    public static Result<string, CompiledExpressionId> CompiledExpressionId(Expression expression) =>
        CompiledExpressionId(ExpressionEncoding.EncodeExpressionAsValue(expression));

    public static CompiledExpressionId CompiledExpressionId(PineValue expressionValue)
    {
        var expressionHash = Convert.ToHexStringLower(s_compilerCache.ComputeHash(expressionValue).Span);

        return
            new CompiledExpressionId(
                ExpressionValue: expressionValue,
                ExpressionHashBase16: expressionHash);
    }

    static string MemberNameForExpression(string expressionValueHash) =>
        "expression_" + expressionValueHash[..10];

    public static string MemberNameForCompiledExpressionFunction(
        CompiledExpressionId derivedId,
        PineValueClass? envConstraint) =>
        "expr_function_" + derivedId.ExpressionHashBase16[..10] +
        (envConstraint is null ? null : "_env_" + MemberNameForConstrainedEnv(envConstraint));

    static string MemberNameForConstrainedEnv(PineValueClass constrainedEnv) =>
        constrainedEnv.HashBase16[..8];

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
                    Expression.Literal literal =>
                    Result<string, Expression>.ok(literal),

                    Expression.Environment =>
                    Result<string, Expression>.ok(expression),

                    Expression.List list =>
                    list.Items.Select(e => TransformPineExpressionWithOptionalReplacement(findReplacement, e))
                    .ListCombine()
                    .Map(elements => (Expression)Expression.ListInstance([.. elements])),

                    Expression.Conditional conditional =>
                    TransformPineExpressionWithOptionalReplacement(
                        findReplacement,
                        conditional.Condition)
                    .AndThen(transformedCondition =>
                    TransformPineExpressionWithOptionalReplacement(
                        findReplacement,
                        conditional.TrueBranch)
                    .AndThen(transformedIfTrue =>
                    TransformPineExpressionWithOptionalReplacement(
                        findReplacement,
                        conditional.FalseBranch)
                    .Map(transformedIfFalse =>
                    (Expression)Expression.ConditionalInstance(
                        transformedCondition,
                        falseBranch: transformedIfFalse,
                        trueBranch: transformedIfTrue)))),

                    Expression.KernelApplication kernelAppl =>
                    TransformPineExpressionWithOptionalReplacement(findReplacement, kernelAppl.Input)
                    .MapError(err => "Failed to transform kernel application argument: " + err)
                    .Map(transformedArgument => (Expression)Expression.KernelApplicationInstance(
                        function: kernelAppl.Function,
                        input: transformedArgument)),

                    Expression.StringTag stringTag =>
                    TransformPineExpressionWithOptionalReplacement(findReplacement, stringTag.Tagged)
                    .Map(transformedTagged => (Expression)new Expression.StringTag(tag: stringTag.Tag, tagged: transformedTagged)),

                    _ =>
                    Result<string, Expression>.err("Unsupported expression type: " + expression.GetType().FullName)
                };
            }));
    }

    public static Result<string, CompiledExpression> CompileToCSharpExpression(
        Expression.Literal literalExpression,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        if (literalExpression.Value == PineValue.EmptyList)
            return Result<string, CompiledExpression>.ok(
                CompiledExpression.WithTypeGenericValue(
                    CoreSyntaxFactory.PineValueEmptyListSyntax(declarationSyntaxContext)));

        return
            Result<string, CompiledExpression>.ok(
                CompiledExpression.WithTypeGenericValue(SyntaxFactory.IdentifierName(DeclarationNameForValue(literalExpression.Value)))
                .MergeDependencies(
                    CompiledExpressionDependencies.Empty with { Values = [literalExpression.Value] }));
    }

    public static string DeclarationNameForValue(PineValue pineValue) =>
        "value_" + Convert.ToHexStringLower(s_compilerCache.ComputeHash(pineValue).Span)[..10];

    public static Result<string, CompiledExpression> CompileToCSharpExpression(
        Expression.StringTag stringTagExpression,
        ExpressionCompilationEnvironment environment)
    {
        Console.WriteLine("Compiling string tag: " + stringTagExpression.Tag);

        return
            CompileToCSharpExpression(
                stringTagExpression.Tagged,
                environment,
                createLetBindingsForCse: false)
            .Map(compiledExpr =>
            compiledExpr.MapSyntax(s => s.InsertTriviaBefore(
                SyntaxFactory.Comment("/*\n" + stringTagExpression.Tag + "\n*/"),
                SyntaxFactory.TriviaList())));
    }

    private static IEnumerable<PineValue> EnumerateAllLiterals(Expression expression) =>
        expression switch
        {
            Expression.Literal literal =>
            [literal.Value],

            Expression.Environment =>
            [],

            Expression.List list =>
            list.Items.SelectMany(EnumerateAllLiterals),

            Expression.KernelApplication kernelApplicationExpression =>
            EnumerateAllLiterals(kernelApplicationExpression.Input),

            Expression.Conditional conditionalExpression =>
            [.. EnumerateAllLiterals(conditionalExpression.Condition)
            ,
                .. EnumerateAllLiterals(conditionalExpression.TrueBranch)
            ,
                .. EnumerateAllLiterals(conditionalExpression.FalseBranch)],

            Expression.ParseAndEval parseAndEvalExpr =>
            [.. EnumerateAllLiterals(parseAndEvalExpr.Encoded)
            ,
                .. EnumerateAllLiterals(parseAndEvalExpr.Environment)],

            Expression.StringTag stringTagExpression =>
            EnumerateAllLiterals(stringTagExpression.Tagged),

            _ => throw new NotImplementedException("Expression type not implemented: " + expression.GetType().FullName)
        };

    public static string GetNameForExpression(ExpressionSyntax syntax)
    {
        var serialized = syntax.ToString();

        var utf8 = Encoding.UTF8.GetBytes(serialized);

        var hash = SHA256.HashData(utf8);

        return Convert.ToHexStringLower(hash)[..10];
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
            if (dictionary.TryGetValue(expr, out var currentCount))
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
                case Expression.Literal:
                    // Leaf node, no further traversal needed
                    break;
                case Expression.List listExpr:
                    foreach (var subExpr in listExpr.Items)
                        Traverse(subExpr, isConditional);
                    break;
                case Expression.ParseAndEval parseAndEvalExpr:
                    Traverse(parseAndEvalExpr.Encoded, isConditional);
                    Traverse(parseAndEvalExpr.Environment, isConditional);
                    break;
                case Expression.KernelApplication kernelAppExpr:
                    Traverse(kernelAppExpr.Input, isConditional);
                    break;
                case Expression.Conditional conditionalExpr:
                    // For ConditionalExpression, traverse its branches as conditional

                    Traverse(conditionalExpr.Condition, isConditional);
                    Traverse(conditionalExpr.TrueBranch, true);
                    Traverse(conditionalExpr.FalseBranch, true);
                    break;
                case Expression.Environment:
                    // Leaf node, no further traversal needed
                    break;
                case Expression.StringTag stringTagExpr:
                    Traverse(stringTagExpr.Tagged, isConditional);
                    break;
                default:
                    throw new NotImplementedException();
            }
        }

        Traverse(expression, false);
        return dictionary.ToImmutableDictionary();
    }
}
