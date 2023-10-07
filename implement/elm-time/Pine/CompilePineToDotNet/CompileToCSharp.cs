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
    ClassDeclarationSyntax ClassDeclarationSyntax,
    IReadOnlyList<UsingDirectiveSyntax> UsingDirectives);

public record GenerateCSharpFileResult(
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
                    SyntaxFactory.List<MemberDeclarationSyntax>(
                        [.. (additionalMembers ?? []), compileCSharpClassResult.ClassDeclarationSyntax]));

        var formattedNode =
            FormatCSharpSyntaxRewriter.FormatSyntaxTree(compilationUnitSyntax.NormalizeWhitespace(eol: "\n"));

        return
            new GenerateCSharpFileResult(
                formattedNode,
                FileText: formattedNode.ToFullString());
    }

    public static Result<string, CompileCSharpClassResult> CompileExpressionsToCSharpClass(
        IReadOnlyList<Expression> expressions,
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

        MemberDeclarationSyntax memberDeclarationSyntaxForExpression(
            string functionName,
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
                        SyntaxFactory.Identifier(functionName))
                    .WithModifiers(
                        SyntaxFactory.TokenList(
                            SyntaxFactory.Token(SyntaxKind.StaticKeyword),
                            SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                    .WithParameterList(
                        SyntaxFactory.ParameterList(
                            SyntaxFactory.SeparatedList(parametersSyntaxes)))
                    .WithBody(blockSyntax);
        }

        return
            expressions
                .Select(expression =>
                    {
                        var expressionValue = PineVM.PineVM.EncodeExpressionAsValue(expression).Extract(err => throw new Exception(err));

                        var expressionHash = CommonConversion.StringBase16(compilerCache.ComputeHash(expressionValue))[..10];

                        var functionName = MemberNameForCompiledExpressionFunction(expressionHash);

                        return
                            CompileToCSharpFunctionBlockSyntax(
                                    expression,
                                    new EnvironmentConfig(
                                        ArgumentEnvironmentName: argumentEnvironmentName,
                                        ArgumentEvalGenericName: argumentEvalGenericName))
                                .MapError(err => "Failed to compile expression " + expressionHash[..10] + ": " + err)
                                .Map(ok =>
                                    (expression,
                                        expressionValue,
                                        expressionHash,
                                        ok.dependencies,
                                        functionName,
                                        memberDeclarationSyntax:
                                        memberDeclarationSyntaxForExpression(functionName, ok.blockSyntax)));
                    }
                ).ListCombine()
                .Map(compiledExpressions => compiledExpressions.OrderBy(ce => ce.expressionHash))
                .AndThen(compiledExpressions =>
                {
                    var aggregateDependencies =
                        DependenciesFromCompilation.Union(compiledExpressions.Select(er => er.dependencies));

                    var aggregateValueDependencies =
                        aggregateDependencies.Values.Union(
                                compiledExpressions.Select(forExprFunction => forExprFunction.expressionValue))
                            .Union(
                                aggregateDependencies.Expressions.SelectMany(e => EnumerateAllLiterals(e.expression)));

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
                        memberDeclarationForValue(
                            PineValue pineValue)
                    {
                        var valueExpression = CompileToCSharpLiteralExpression(pineValue, specialSyntaxForPineValue);

                        var memberName = DeclarationNameForValue(pineValue);

                        return
                            (memberName,
                                SyntaxFactory.IdentifierName("PineValue"),
                                valueExpression);
                    }

                    (string memberName, TypeSyntax typeSyntax, ExpressionSyntax memberDeclaration)
                        memberDeclarationForExpression(
                            (string hash, Expression expression) hashAndExpr)
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

                    var usingsTypes = new[]
                    {
                        typeof(PineValue),
                        typeof(ImmutableArray),
                        typeof(IReadOnlyDictionary<,>),
                        typeof(Func<,>)
                    };

                    var usings =
                        usingsTypes.Select(t => t.Namespace)
                            .WhereNotNull()
                            .Distinct()
                            .Select(ns => SyntaxFactory.UsingDirective(SyntaxFactory.IdentifierName(ns)))
                            .ToImmutableList();

                    var valuesStaticMembers =
                        valuesToDeclare
                            .Select(memberDeclarationForValue)
                            .ToImmutableList();

                    var expressionStaticMembers =
                        aggregateDependencies.Expressions
                            .Select(memberDeclarationForExpression)
                            .DistinctBy(member => member.memberName)
                            .ToImmutableList();

                    var emptyDictionaryExpression =
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            SyntaxFactory.GenericName(
                                    SyntaxFactory.Identifier("ImmutableDictionary"))
                                .WithTypeArgumentList(
                                    SyntaxFactory.TypeArgumentList(
                                        SyntaxFactory.SeparatedList<TypeSyntax>(
                                            new SyntaxNodeOrToken[]
                                            {
                                                SyntaxFactory.IdentifierName("PineValue"),
                                                SyntaxFactory.Token(SyntaxKind.CommaToken),
                                                SyntaxFactory.GenericName(
                                                        SyntaxFactory.Identifier("Func"))
                                                    .WithTypeArgumentList(
                                                        SyntaxFactory.TypeArgumentList(
                                                            SyntaxFactory.SeparatedList<TypeSyntax>(
                                                                new SyntaxNodeOrToken[]
                                                                {
                                                                    EvalExprDelegateTypeSyntax,
                                                                    SyntaxFactory.Token(SyntaxKind.CommaToken),
                                                                    SyntaxFactory.IdentifierName("PineValue"),
                                                                    SyntaxFactory.Token(SyntaxKind.CommaToken),
                                                                    SyntaxFactory.GenericName(
                                                                            SyntaxFactory.Identifier("Result"))
                                                                        .WithTypeArgumentList(
                                                                            SyntaxFactory.TypeArgumentList(
                                                                                SyntaxFactory.SeparatedList<TypeSyntax>(
                                                                                    new SyntaxNodeOrToken[]
                                                                                    {
                                                                                        SyntaxFactory.PredefinedType(
                                                                                            SyntaxFactory.Token(
                                                                                                SyntaxKind
                                                                                                    .StringKeyword)),
                                                                                        SyntaxFactory.Token(SyntaxKind
                                                                                            .CommaToken),
                                                                                        SyntaxFactory.IdentifierName(
                                                                                            "PineValue")
                                                                                    })))
                                                                })))
                                            }))),
                            SyntaxFactory.IdentifierName("Empty"));

                    var dictionaryExpression =
                        compiledExpressions
                            .Aggregate(
                                seed: (ExpressionSyntax)emptyDictionaryExpression,
                                func: (dictionaryExpression, fromCompiledExpression) =>
                                    SyntaxFactory.InvocationExpression(
                                            SyntaxFactory.MemberAccessExpression(
                                                SyntaxKind.SimpleMemberAccessExpression,
                                                dictionaryExpression,
                                                SyntaxFactory.IdentifierName("SetItem")))
                                        .WithArgumentList(
                                            SyntaxFactory.ArgumentList(
                                                SyntaxFactory.SeparatedList<ArgumentSyntax>(
                                                    new SyntaxNodeOrToken[]
                                                    {
                                                        SyntaxFactory.Argument(SyntaxFactory.IdentifierName(
                                                            DeclarationNameForValue(fromCompiledExpression
                                                                .expressionValue))),
                                                        SyntaxFactory.Token(SyntaxKind.CommaToken),
                                                        SyntaxFactory.Argument(
                                                            SyntaxFactory.IdentifierName(fromCompiledExpression.functionName))
                                                    }))));

                    var dictionaryMemberDeclaration =
                        SyntaxFactory.MethodDeclaration(
                                SyntaxFactory.GenericName(
                                        SyntaxFactory.Identifier("IReadOnlyDictionary"))
                                    .WithTypeArgumentList(
                                        SyntaxFactory.TypeArgumentList(
                                            SyntaxFactory.SeparatedList<TypeSyntax>(
                                                new SyntaxNodeOrToken[]
                                                {
                                                    SyntaxFactory.IdentifierName("PineValue"),
                                                    SyntaxFactory.Token(SyntaxKind.CommaToken),
                                                    SyntaxFactory.GenericName(
                                                            SyntaxFactory.Identifier("Func"))
                                                        .WithTypeArgumentList(
                                                            SyntaxFactory.TypeArgumentList(
                                                                SyntaxFactory.SeparatedList<TypeSyntax>(
                                                                    new SyntaxNodeOrToken[]
                                                                    {
                                                                        EvalExprDelegateTypeSyntax,
                                                                        SyntaxFactory.Token(SyntaxKind.CommaToken),
                                                                        SyntaxFactory.IdentifierName("PineValue"),
                                                                        SyntaxFactory.Token(SyntaxKind.CommaToken),
                                                                        SyntaxFactory.GenericName(
                                                                                SyntaxFactory.Identifier("Result"))
                                                                            .WithTypeArgumentList(
                                                                                SyntaxFactory.TypeArgumentList(
                                                                                    SyntaxFactory
                                                                                        .SeparatedList<TypeSyntax>(
                                                                                            new SyntaxNodeOrToken[]
                                                                                            {
                                                                                                SyntaxFactory
                                                                                                    .PredefinedType(
                                                                                                        SyntaxFactory
                                                                                                            .Token(
                                                                                                                SyntaxKind
                                                                                                                    .StringKeyword)),
                                                                                                SyntaxFactory.Token(
                                                                                                    SyntaxKind
                                                                                                        .CommaToken),
                                                                                                SyntaxFactory
                                                                                                    .IdentifierName(
                                                                                                        "PineValue")
                                                                                            })))
                                                                    })))
                                                }))),
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
                                typeSyntax: (TypeSyntax)SyntaxFactory.IdentifierName("PineValue"),
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
                                                    SyntaxFactory
                                                        .VariableDeclarator(SyntaxFactory.Identifier(member.memberName))
                                                        .WithInitializer(
                                                            SyntaxFactory.EqualsValueClause(member.Item3)))))
                                    .WithModifiers(
                                        SyntaxFactory.TokenList(
                                            SyntaxFactory.Token(SyntaxKind.StaticKeyword),
                                            SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                                            SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword)))
                            )
                            .ToImmutableList();

                    return Result<string, CompileCSharpClassResult>.ok(
                        new CompileCSharpClassResult(
                            ClassDeclarationSyntax:
                            SyntaxFactory.ClassDeclaration(containerConfig.containerTypeName)
                                .WithMembers(
                                    SyntaxFactory.List<MemberDeclarationSyntax>(
                                        [dictionaryMemberDeclaration
                                        ,
                                            .. compiledExpressions.Select(f => f.memberDeclarationSyntax)
                                        ,
                                            .. staticFieldsDeclarations])),
                            UsingDirectives: usings));
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

    public record EnvironmentConfig(
        string ArgumentEnvironmentName,
        string ArgumentEvalGenericName);

    public record DependenciesFromCompilation(
        ImmutableHashSet<PineValue> Values,
        IImmutableSet<(string hash, Expression expression)> Expressions)
    {
        public static readonly DependenciesFromCompilation Empty = new(
            Values: [],
            Expressions: ImmutableHashSet<(string, Expression)>.Empty);

        public static (T, DependenciesFromCompilation) WithNoDependencies<T>(T other) => (other, Empty);

        public DependenciesFromCompilation Union(DependenciesFromCompilation other) =>
            new(Values: Values.Union(other.Values),
                Expressions: Expressions.Union(other.Expressions));

        public static DependenciesFromCompilation Union(IEnumerable<DependenciesFromCompilation> dependencies) =>
            dependencies.Aggregate(
                seed: Empty,
                func: (aggregate, next) => aggregate.Union(next));
    }

    public static Result<string, (BlockSyntax blockSyntax, DependenciesFromCompilation dependencies)>
        CompileToCSharpFunctionBlockSyntax(
            Expression expression,
            EnvironmentConfig environment) =>
        CompileToCSharpExpression(expression, environment)
            .Map(exprWithDependencies =>
            {
                var returnExpression = exprWithDependencies.expression.AsCsWithTypeResult();

                return
                    (SyntaxFactory.Block(SyntaxFactory.ReturnStatement(returnExpression)), exprWithDependencies.dependencies);
            });

    public static ExpressionSyntax WrapExpressionInPineValueResultOk(ExpressionSyntax expression) =>
        SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    SyntaxFactory.GenericName(
                            SyntaxFactory.Identifier("Result"))
                        .WithTypeArgumentList(
                            SyntaxFactory.TypeArgumentList(
                                SyntaxFactory.SeparatedList<TypeSyntax>(
                                    new SyntaxNodeOrToken[]
                                    {
                                        SyntaxFactory.PredefinedType(
                                            SyntaxFactory.Token(SyntaxKind.StringKeyword)),
                                        SyntaxFactory.Token(SyntaxKind.CommaToken),
                                        SyntaxFactory.IdentifierName("PineValue")
                                    }))),
                    SyntaxFactory.IdentifierName("ok")))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SingletonSeparatedList(SyntaxFactory.Argument(expression))));

    public static Result<string, (CompiledExpression expression, DependenciesFromCompilation dependencies)> CompileToCSharpExpression(
        Expression expression,
        EnvironmentConfig environment)
    {
        return
            expression switch
            {
                Expression.EnvironmentExpression =>
                Result<string, (CompiledExpression, DependenciesFromCompilation)>.ok(
                    DependenciesFromCompilation.WithNoDependencies(
                        CompiledExpression.WithTypePlainValue(SyntaxFactory.IdentifierName(environment.ArgumentEnvironmentName)))),

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
                Result<string, (CompiledExpression, DependenciesFromCompilation)>.err(
                    "Unsupported syntax kind: " + expression.GetType().FullName)
            };
    }

    public static Result<string, (CompiledExpression, DependenciesFromCompilation)> CompileToCSharpExpression(
        Expression.ListExpression listExpression,
        EnvironmentConfig environment)
    {
        if (!listExpression.List.Any())
            return Result<string, (CompiledExpression, DependenciesFromCompilation)>.ok(
                (CompiledExpression.WithTypePlainValue(pineValueEmptyListSyntax),
                    DependenciesFromCompilation.Empty));

        return
            listExpression.List.Select((elementExpression, elementIndex) =>
            CompileToCSharpExpression(elementExpression, environment)
            .MapError(err => "Failed to translate list element " + elementIndex + ": " + err))
            .ListCombine()
            .Map(compiledElements =>
            {
                var aggregateSyntax =
                CompiledExpression.ListMapOrAndThen(
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
                    compiledElements.Select(ce => ce.expression).ToImmutableList());

                var aggregateDeps = DependenciesFromCompilation.Union(compiledElements.Select(e => e.dependencies));

                return (aggregateSyntax, aggregateDeps);
            });
    }

    public static Result<string, (CompiledExpression, DependenciesFromCompilation)> CompileToCSharpExpression(
        Expression.KernelApplicationExpression kernelApplicationExpression,
        EnvironmentConfig environment)
    {
        if (!KernelFunctionsInfo.Value.TryGetValue(kernelApplicationExpression.functionName,
              out var kernelFunctionInfo))
        {
            return
                Result<string, (CompiledExpression, DependenciesFromCompilation)>.err(
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

    private static Result<string, (CompiledExpression, DependenciesFromCompilation)> CompileKernelFunctionApplicationToCSharpExpression(
        KernelFunctionInfo kernelFunctionInfo,
        Expression kernelApplicationArgumentExpression,
        EnvironmentConfig environment)
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
                                    return Result<string, (CompiledExpression, DependenciesFromCompilation)>.err(
                                        "No transformation found for parameter type " + parameterType);

                                return Result<string, (CompiledExpression, DependenciesFromCompilation)>.ok(param);
                            });

                    if (argumentsResults.ListCombine() is
                        Result<string, IReadOnlyList<(CompiledExpression, DependenciesFromCompilation)>>.Ok specializedOk)
                    {
                        var expressionReturningPineValue =
                            CompiledExpression.ListMapOrAndThen(
                                argumentsCs =>
                                {
                                    var plainInvocationSyntax = specializedImpl.CompileInvocation(argumentsCs);

                                    return
                                    CompiledExpression.WithTypePlainValue(
                                        specializedImpl.ReturnType.IsInstanceOfResult ?
                                        wrapInvocationInWithDefault(plainInvocationSyntax)
                                        :
                                        plainInvocationSyntax);
                                },
                                specializedOk.Value.Select(p => p.Item1).ToImmutableList());

                        return
                            Result<string, (CompiledExpression, DependenciesFromCompilation)>.ok(
                                (expressionReturningPineValue,
                                DependenciesFromCompilation.Union(specializedOk.Value.Select(p => p.Item2))));
                    }
                }
            }
        }

        return
            CompileToCSharpExpression(
                kernelApplicationArgumentExpression, environment)
            .Map(compiledArgument =>
            (compiledArgument.expression.Map(argumentCs =>
            wrapInvocationInWithDefault(kernelFunctionInfo.CompileGenericInvocation(argumentCs))),
            compiledArgument.dependencies));
    }

    public static Result<string, (CompiledExpression, DependenciesFromCompilation)> CompileToCSharpExpression(
        Expression.ConditionalExpression conditionalExpression,
        EnvironmentConfig environment)
    {
        return
            CompileToCSharpExpression(
                conditionalExpression.condition,
                environment)
            .MapError(err => "Failed to compile condition: " + err)
            .AndThen(compiledCondition =>
            CompileToCSharpExpression(
                conditionalExpression.ifTrue,
                environment)
            .MapError(err => "Failed to compile branch if true: " + err)
            .AndThen(compiledIfTrue =>
            CompileToCSharpExpression(
                conditionalExpression.ifFalse,
                environment)
            .MapError(err => "Failed to compile branch if false: " + err)
            .Map(compiledIfFalse =>
            {
                CompiledExpression continueWithConditionCs(ExpressionSyntax conditionCs)
                {
                    if (!(compiledIfTrue.expression.IsTypeResult || compiledIfFalse.expression.IsTypeResult))
                    {
                        return
                        CompiledExpression.WithTypePlainValue(SyntaxFactory.ConditionalExpression(
                            conditionCs,
                            compiledIfTrue.expression.Syntax,
                            compiledIfFalse.expression.Syntax));
                    }

                    return
                    CompiledExpression.WithTypeResult(
                        SyntaxFactory.ConditionalExpression(
                            conditionCs,
                            compiledIfTrue.expression.AsCsWithTypeResult(),
                            compiledIfFalse.expression.AsCsWithTypeResult()));
                }

                var
                combinedExpr =
                compiledCondition.expression
                .MapOrAndThen(conditionCs =>
                continueWithConditionCs(
                    SyntaxFactory.BinaryExpression(SyntaxKind.EqualsExpression, SyntaxFactory.IdentifierName("value_true"), conditionCs)));

                return
                (combinedExpr,
                compiledCondition.dependencies.Union(compiledIfTrue.dependencies).Union(compiledIfFalse.dependencies));
            }
            )));
    }

    public static Result<string, (CompiledExpression, DependenciesFromCompilation)> CompileToCSharpExpression(
        Expression.DecodeAndEvaluateExpression decodeAndEvaluateExpression,
        EnvironmentConfig environment)
    {
        var decodeAndEvaluateExpressionValue =
            PineVM.PineVM.EncodeExpressionAsValue(decodeAndEvaluateExpression)
            .Extract(err => throw new Exception(err));

        var decodeAndEvaluateExpressionHash =
            CommonConversion.StringBase16(compilerCache.ComputeHash(decodeAndEvaluateExpressionValue));

        (CompiledExpression, DependenciesFromCompilation) continueWithGenericCase()
        {
            var invocationExpression =
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.IdentifierName(environment.ArgumentEvalGenericName))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList<ArgumentSyntax>(
                            new SyntaxNodeOrToken[]
                            {
                                SyntaxFactory.Argument(
                                    SyntaxFactory.IdentifierName(
                                        MemberNameForExpression(decodeAndEvaluateExpressionHash))),
                                SyntaxFactory.Token(SyntaxKind.CommaToken),
                                SyntaxFactory.Argument(
                                    SyntaxFactory.IdentifierName(environment.ArgumentEnvironmentName))
                            })));

            return
                (CompiledExpression.WithTypeResult(invocationExpression),
                DependenciesFromCompilation.Empty
                with
                {
                    Expressions = ImmutableHashSet.Create((decodeAndEvaluateExpressionHash, (Expression)decodeAndEvaluateExpression)),
                });
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
                    var innerExpressionValueHash =
                        CommonConversion.StringBase16(compilerCache.ComputeHash(innerExpressionValue));

                    return
                    CompileToCSharpExpression(decodeAndEvaluateExpression.environment, environment)
                    .Map(compiledArgumentExpression =>
                    {
                        var invocationExpression =
                        compiledArgumentExpression.expression.MapOrAndThen(argumentExprPlainValue =>
                        {
                            return
                            CompiledExpression.WithTypeResult(
                                SyntaxFactory.InvocationExpression(
                                    SyntaxFactory.IdentifierName(
                                        MemberNameForCompiledExpressionFunction(innerExpressionValueHash)))
                                .WithArgumentList(
                                    SyntaxFactory.ArgumentList(
                                        SyntaxFactory.SeparatedList<ArgumentSyntax>(
                                            new SyntaxNodeOrToken[]
                                            {
                                                SyntaxFactory.Argument(
                                                    SyntaxFactory.IdentifierName(environment.ArgumentEvalGenericName)),
                                                SyntaxFactory.Token(SyntaxKind.CommaToken),
                                                SyntaxFactory.Argument(argumentExprPlainValue)
                                            }))));
                        });

                        return
                            (invocationExpression,
                            compiledArgumentExpression.dependencies.Union(
                            DependenciesFromCompilation.Empty
                            with
                            {
                                Expressions = ImmutableHashSet.Create((innerExpressionValueHash, innerExpression)),
                            }));
                    });
                }));
        }

        return
            Result<string, (CompiledExpression, DependenciesFromCompilation)>.ok(
                continueWithGenericCase());
    }

    static string MemberNameForExpression(string expressionValueHash) =>
        "expression_" + expressionValueHash[..10];

    static string MemberNameForCompiledExpressionFunction(string expressionValueHash) =>
        "expr_function_" + expressionValueHash[..10];

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

    public static Result<string, (CompiledExpression, DependenciesFromCompilation)> CompileToCSharpExpression(
        Expression.LiteralExpression literalExpression)
    {
        return
            Result<string, (CompiledExpression, DependenciesFromCompilation)>.ok(
                (CompiledExpression.WithTypePlainValue(SyntaxFactory.IdentifierName(DeclarationNameForValue(literalExpression.Value))),
                DependenciesFromCompilation.Empty with { Values = [literalExpression.Value] }));
    }

    public static string DeclarationNameForValue(PineValue pineValue) =>
        "value_" + CommonConversion.StringBase16(compilerCache.ComputeHash(pineValue))[..10];

    public static Result<string, (CompiledExpression expressionSyntax, DependenciesFromCompilation dependencies)> CompileToCSharpExpression(
        Expression.StringTagExpression stringTagExpression,
        EnvironmentConfig environment)
    {
        Console.WriteLine("Compiling string tag: " + stringTagExpression.tag);

        return
            CompileToCSharpExpression(
                stringTagExpression.tagged,
                environment)
            .Map(compiledExpr =>
            (compiledExpr.expression.MapSyntax(s => s.InsertTriviaBefore(
                SyntaxFactory.Comment("/*\n" + stringTagExpression.tag + "\n*/"),
                SyntaxFactory.TriviaList())),
                compiledExpr.dependencies));
    }

    public static ExpressionSyntax CompileToCSharpLiteralExpression(
        PineValue pineValue,
        Func<PineValue, ExpressionSyntax?> overrideDefaultExpression)
    {
        ExpressionSyntax continueCompile(PineValue pineValue) =>
            overrideDefaultExpression(pineValue) ??
            CompileToCSharpLiteralExpression(pineValue, overrideDefaultExpression);

        if (pineValue == PineValue.EmptyList)
            return pineValueEmptyListSyntax;

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
                                ExpressionSyntaxForIntegerLiteral((long)okInteger.Value)))));
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
                .Select(b => ExpressionSyntaxForIntegerLiteral(b));

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

    public static LiteralExpressionSyntax ExpressionSyntaxForIntegerLiteral(long integer) =>
        SyntaxFactory.LiteralExpression(
            SyntaxKind.NumericLiteralExpression,
            SyntaxTokenForIntegerLiteral(integer));

    public static SyntaxToken SyntaxTokenForIntegerLiteral(long integer) =>
        SyntaxFactory.Literal(
            integer.ToString("N0", IntegerLiteralNumberFormatInfo),
            integer);

    static readonly NumberFormatInfo IntegerLiteralNumberFormatInfo = new()
    {
        NumberGroupSeparator = "_",
        NumberGroupSizes = [3]
    };

    private static readonly ExpressionSyntax pineValueEmptyListSyntax =
        SyntaxFactory.MemberAccessExpression(
            SyntaxKind.SimpleMemberAccessExpression,
            SyntaxFactory.IdentifierName("PineValue"),
            SyntaxFactory.IdentifierName("EmptyList"));

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
}
