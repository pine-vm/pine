using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Reflection;
using System.Security.Cryptography;
using System.Text;

namespace Pine.PineVM;

public class PineCompileToDotNet
{
    public record SyntaxContainerConfig(
        string containerTypeName,
        string dictionaryMemberName);

    public record CompileCSharpClassResult(
        ClassDeclarationSyntax ClassDeclarationSyntax,
        IReadOnlyList<UsingDirectiveSyntax> UsingDirectives);

    public record GenerateCSharpFileResult(
        CompilationUnitSyntax CompilationUnitSyntax,
        string FileText);

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

        var formattedNode = new FormatCSharpSyntaxRewriter().Visit(compilationUnitSyntax.NormalizeWhitespace(eol: "\n"));

        return
            new GenerateCSharpFileResult(
                (CompilationUnitSyntax)formattedNode, FileText: formattedNode.ToFullString());
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
                .WithType(evalExprDelegateTypeSyntax),

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
                        var expressionValue = PineVM.EncodeExpressionAsValue(expression).Extract(err => throw new Exception(err));

                        var expressionHash = CommonConversion.StringBase16(PineValueHashTree.ComputeHash(expressionValue))[..10];

                        var functionName = "expr_function_" + expressionHash[..10];

                        return
                            CompileToCSharpFunctionBlockSyntax(
                                    expression,
                                    new EnvironmentConfig(
                                        argumentEnvironmentName: argumentEnvironmentName,
                                        argumentEvalGenericName: argumentEvalGenericName))
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
                        OrderValuesByContainment(aggregateValueDependencies.Concat(usedValues).Distinct())
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

                        var memberName = "expression_" + hashAndExpr.hash[..10];

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
                                                                    evalExprDelegateTypeSyntax,
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
                                                                        evalExprDelegateTypeSyntax,
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

    private static QualifiedNameSyntax evalExprDelegateTypeSyntax =>
        SyntaxFactory.QualifiedName(
            pineVmClassQualifiedNameSyntax,
            SyntaxFactory.IdentifierName(nameof(PineVM.EvalExprDelegate)));

    private static QualifiedNameSyntax pineVmClassQualifiedNameSyntax =>
        SyntaxFactory.QualifiedName(
            SyntaxFactory.QualifiedName(
                SyntaxFactory.IdentifierName("Pine"),
                SyntaxFactory.IdentifierName("PineVM")),
            SyntaxFactory.IdentifierName("PineVM"));

    public record CompileToAssemblyResult(
        byte[] Assembly,
        Func<Result<string, IReadOnlyDictionary<PineValue, Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>>>
        BuildCompiledExpressionsDictionary);

    public static Result<string, CompileToAssemblyResult> CompileToAssembly(
        SyntaxContainerConfig syntaxContainerConfig,
        CompileCSharpClassResult compileCSharpClassResult)
    {
        var syntaxText = GenerateCSharpFile(compileCSharpClassResult, additionalMembers: null).FileText;

        var syntaxTree = CSharpSyntaxTree.ParseText(syntaxText);

        var compilation = CSharpCompilation.Create("assembly-name")
            .WithOptions(new CSharpCompilationOptions(
                OutputKind.DynamicallyLinkedLibrary,
                optimizationLevel: OptimizationLevel.Release)
            .WithAssemblyIdentityComparer(DesktopAssemblyIdentityComparer.Default))
            .WithReferences(MetadataReferences.Value)
            .AddSyntaxTrees(syntaxTree);

        using var codeStream = new MemoryStream();

        var compilationResult = compilation.Emit(codeStream);

        var compilationErrors =
            compilationResult.Diagnostics
            .Where(d => d.Severity == DiagnosticSeverity.Error)
            .ToImmutableList();

        if (!compilationResult.Success && 0 < compilationErrors.Count)
        {
            return Result<string, CompileToAssemblyResult>.err(
                "Compilation failed with " + compilationErrors.Count + " errors:\n" +
                string.Join("\n", compilationErrors.Select(d => d.ToString())));
        }

        var assembly = codeStream.ToArray();

        Result<string, IReadOnlyDictionary<PineValue, Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>> buildDictionary()
        {
            var loadedAssembly = Assembly.Load(assembly);

            var compiledType = loadedAssembly.GetType(syntaxContainerConfig.containerTypeName);

            if (compiledType is null)
                return Result<string, IReadOnlyDictionary<PineValue, Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>>.err(
                    "Did not find type " + syntaxContainerConfig.containerTypeName + " in assembly " + loadedAssembly.FullName);

            var dictionaryMember =
                compiledType.GetMethod(syntaxContainerConfig.dictionaryMemberName, BindingFlags.Public | BindingFlags.Static);

            if (dictionaryMember is null)
                return Result<string, IReadOnlyDictionary<PineValue, Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>>.err(
                    "Did not find method " + syntaxContainerConfig.dictionaryMemberName + " in type " + compiledType.FullName);

            return
                Result<string, IReadOnlyDictionary<PineValue, Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>>.ok(
                    (IReadOnlyDictionary<PineValue, Func<PineVM.EvalExprDelegate, PineValue, Result<string, PineValue>>>)
                    dictionaryMember.Invoke(null, null));
        }

        return Result<string, CompileToAssemblyResult>.ok(
            new CompileToAssemblyResult(
                Assembly: assembly,
                BuildCompiledExpressionsDictionary: buildDictionary));
    }

    private static readonly Lazy<IImmutableList<MetadataReference>> MetadataReferences =
        new(() => ListMetadataReferences().ToImmutableList());

    private static IEnumerable<MetadataReference> ListMetadataReferences()
    {
        var types = new[]
        {
            typeof(object),
            typeof(Func<>),
            typeof(IImmutableList<>),
            typeof(PineVM)
        };

        var typesAssembliesLocations =
            types
            .Select(t => t.Assembly)
            .Concat(AppDomain.CurrentDomain.GetAssemblies())
            .Select(a => a.Location)
            .Where(loc => 0 < loc?.Length)
            .Distinct()
            .ToImmutableList();

        foreach (var assemblyLocation in typesAssembliesLocations)
        {
            yield return MetadataReference.CreateFromFile(assemblyLocation);
        }
    }

    public record CompiledExpression(
        ExpressionSyntax Syntax,

        /*
         * true if the type of the expression is Result<string, PineValue>
         * false if the type of the expression is PineValue
         * */
        bool IsTypeResult)
    {
        public static CompiledExpression WithTypePlainValue(ExpressionSyntax syntax) =>
            new(syntax, IsTypeResult: false);

        public static CompiledExpression WithTypeResult(ExpressionSyntax syntax) =>
            new(syntax, IsTypeResult: true);

        public CompiledExpression MapSyntax(Func<ExpressionSyntax, ExpressionSyntax> map) =>
            this
            with
            {
                Syntax = map(Syntax)
            };

        public CompiledExpression MapOrAndThen(Func<ExpressionSyntax, CompiledExpression> continueWithPlainValue)
        {
            if (!IsTypeResult)
                return continueWithPlainValue(Syntax);

            var syntaxName = GetNameForExpression(Syntax);

            var okIdentifier = SyntaxFactory.Identifier("ok_of_" + syntaxName);

            var combinedExpression = continueWithPlainValue(SyntaxFactory.IdentifierName(okIdentifier));

            var mapErrorExpression =
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        Syntax,
                        SyntaxFactory.IdentifierName("MapError")))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Argument(
                                SyntaxFactory.SimpleLambdaExpression(
                                        SyntaxFactory.Parameter(
                                            SyntaxFactory.Identifier("err")))
                                    .WithExpressionBody(
                                        SyntaxFactory.BinaryExpression(
                                            SyntaxKind.AddExpression,
                                            SyntaxFactory.LiteralExpression(
                                                SyntaxKind.StringLiteralExpression,
                                                SyntaxFactory.Literal(
                                                    "Failed to evaluate expression " + syntaxName + ":")),
                                            SyntaxFactory.IdentifierName("err")))))));

            return
                WithTypeResult(
                    SyntaxFactory.InvocationExpression(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            mapErrorExpression,
                            SyntaxFactory.IdentifierName(combinedExpression.IsTypeResult ? "AndThen" : "Map")))
                    .WithArgumentList(
                        SyntaxFactory.ArgumentList(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.Argument(
                                    SyntaxFactory.SimpleLambdaExpression(
                                            SyntaxFactory.Parameter(okIdentifier))
                                    .WithExpressionBody(combinedExpression.Syntax))))));
        }

        public CompiledExpression Map(Func<ExpressionSyntax, ExpressionSyntax> map)
        {
            return MapOrAndThen(inner => new CompiledExpression(map(inner), IsTypeResult: false));
        }

        public ExpressionSyntax AsCsWithTypeResult()
        {
            if (IsTypeResult)
                return Syntax;

            return WrapExpressionInPineValueResultOk(Syntax);
        }

        public static CompiledExpression ListMapOrAndThen(
            Func<IReadOnlyList<ExpressionSyntax>, CompiledExpression> combine,
            IReadOnlyList<CompiledExpression> compiledList)
        {
            static CompiledExpression recursive(
                Func<IReadOnlyList<ExpressionSyntax>, CompiledExpression> combine,
                ImmutableList<CompiledExpression> compiledList,
                ImmutableList<ExpressionSyntax> syntaxesCs)
            {
                if (compiledList.IsEmpty)
                    return combine(syntaxesCs);

                return
                    compiledList.First().MapOrAndThen(
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
    }

    public record EnvironmentConfig(
        string argumentEnvironmentName,
        string argumentEvalGenericName);

    public record DependenciesFromCompilation(
        ImmutableHashSet<PineValue> Values,
        IImmutableSet<(string hash, Expression expression)> Expressions)
    {
        public static readonly DependenciesFromCompilation Empty = new(
            Values: ImmutableHashSet<PineValue>.Empty,
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
                        CompiledExpression.WithTypePlainValue(SyntaxFactory.IdentifierName(environment.argumentEnvironmentName)))),

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
                                        SyntaxFactory.ImplicitArrayCreationExpression(
                                            SyntaxFactory.InitializerExpression(
                                                SyntaxKind.ArrayInitializerExpression,
                                                SyntaxFactory.SeparatedList(csharpItems)))))))),
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
                if (specializedImpl.parameterTypes.Count == staticallyKnownArgumentsList.Count)
                {
                    var argumentsResults =
                        specializedImpl.parameterTypes
                            .Select((parameterType, parameterIndex) =>
                            {
                                if (!staticallyKnownArgumentsList[parameterIndex].argumentSyntaxFromParameterType
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
                                        specializedImpl.returnType.isInstanceOfResult ?
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

    private static Result<string, IReadOnlyList<ParsedKernelApplicationArgumentExpression>>? ParseKernelApplicationArgumentAsList(
        Expression kernelApplicationArgumentExpression,
        EnvironmentConfig environment)
    {
        Result<string, IReadOnlyList<ParsedKernelApplicationArgumentExpression>> continueWithList(IEnumerable<Expression> list)
        {
            return
                list
                    .Select(e => ParseKernelApplicationArgument(e, environment))
                    .ListCombine();
        }

        return
            kernelApplicationArgumentExpression switch
            {
                Expression.ListExpression listExpressionArgument =>
                    continueWithList(listExpressionArgument.List),

                Expression.LiteralExpression literalExpressionArgument =>
                    literalExpressionArgument.Value switch
                    {
                        PineValue.ListValue literalList =>
                            continueWithList(
                                literalList.Elements.Select(elementValue => new Expression.LiteralExpression(elementValue))),

                        _ => null
                    },

                _ => null
            };
    }

    private static Result<string, ParsedKernelApplicationArgumentExpression> ParseKernelApplicationArgument(
        Expression argumentExpression,
        EnvironmentConfig environment)
    {
        var dictionary = new Dictionary<KernelFunctionParameterType, (CompiledExpression, DependenciesFromCompilation)>();

        if (argumentExpression is Expression.LiteralExpression literal)
        {
            if (PineValueAsInteger.SignedIntegerFromValue(literal.Value) is Result<string, BigInteger>.Ok okInteger &&
                PineValueAsInteger.ValueFromSignedInteger(okInteger.Value) == literal.Value)
            {
                dictionary[KernelFunctionParameterType.Integer] =
                    (CompiledExpression.WithTypePlainValue(
                        SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal((long)okInteger.Value))),
                            DependenciesFromCompilation.Empty);
            }
        }

        return
            CompileToCSharpExpression(argumentExpression, environment)
                .Map(csharpExpression =>
                    new ParsedKernelApplicationArgumentExpression(
                        argumentSyntaxFromParameterType:
                        ImmutableDictionary<KernelFunctionParameterType, (CompiledExpression, DependenciesFromCompilation)>.Empty
                            .SetItem(KernelFunctionParameterType.Generic, (csharpExpression.expression, csharpExpression.dependencies))
                            .SetItems(dictionary)));
    }

    private record ParsedKernelApplicationArgumentExpression(
        IReadOnlyDictionary<KernelFunctionParameterType, (CompiledExpression, DependenciesFromCompilation)> argumentSyntaxFromParameterType);

    private record KernelFunctionInfo(
        Func<ExpressionSyntax, InvocationExpressionSyntax> CompileGenericInvocation,
        IReadOnlyList<KernelFunctionSpecializedInfo> SpecializedImplementations);

    private record KernelFunctionSpecializedInfo(
        IReadOnlyList<KernelFunctionParameterType> parameterTypes,
        KernelFunctionSpecializedInfoReturnType returnType,
        Func<IReadOnlyList<ExpressionSyntax>, InvocationExpressionSyntax> CompileInvocation);

    private record KernelFunctionSpecializedInfoReturnType(
        bool isInstanceOfResult);

    private enum KernelFunctionParameterType
    {
        Generic = 1,
        Integer = 10,
    }

    private static readonly Lazy<IReadOnlyDictionary<string, KernelFunctionInfo>> KernelFunctionsInfo =
        new(ReadKernelFunctionsInfoViaReflection);

    private static IReadOnlyDictionary<string, KernelFunctionInfo> ReadKernelFunctionsInfoViaReflection()
    {
        var kernelFunctionContainerType = typeof(KernelFunction);
        var methodsInfos = kernelFunctionContainerType.GetMethods(BindingFlags.Static | BindingFlags.Public);

        static KernelFunctionParameterType parseKernelFunctionParameterType(Type parameterType)
        {
            if (parameterType == typeof(BigInteger))
                return KernelFunctionParameterType.Integer;

            if (parameterType == typeof(PineValue))
                return KernelFunctionParameterType.Generic;

            throw new Exception("Unknown parameter type: " + parameterType.FullName);
        }

        static Result<string, KernelFunctionSpecializedInfoReturnType> parseKernelFunctionReturnType(Type returnType)
        {
            if (returnType == typeof(PineValue))
                return Result<string, KernelFunctionSpecializedInfoReturnType>.ok(
                    new KernelFunctionSpecializedInfoReturnType(isInstanceOfResult: false));

            if (returnType == typeof(Result<string, PineValue>))
                return Result<string, KernelFunctionSpecializedInfoReturnType>.ok(
                    new KernelFunctionSpecializedInfoReturnType(isInstanceOfResult: true));

            return Result<string, KernelFunctionSpecializedInfoReturnType>.err("Not a supported type");
        }

        KernelFunctionInfo ReadKernelFunctionInfo(MethodInfo genericMethodInfo)
        {
            InvocationExpressionSyntax compileInvocationForArgumentList(ArgumentListSyntax argumentListSyntax)
            {
                return
                    SyntaxFactory.InvocationExpression(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            SyntaxFactory.MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                SyntaxFactory.QualifiedName(SyntaxFactory.IdentifierName("Pine"),
                                    SyntaxFactory.IdentifierName("PineVM")),
                                SyntaxFactory.IdentifierName(kernelFunctionContainerType.Name)),
                            SyntaxFactory.IdentifierName(genericMethodInfo.Name)),
                        argumentListSyntax);
            }

            InvocationExpressionSyntax compileGenericInvocation(ExpressionSyntax argumentExpression) =>
                compileInvocationForArgumentList(SyntaxFactory.ArgumentList(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Argument(argumentExpression))));

            var specializedImplementations =
                methodsInfos
                    .Where(candidateMethod =>
                        candidateMethod.Name == genericMethodInfo.Name &&
                        candidateMethod != genericMethodInfo &&
                        candidateMethod.DeclaringType == kernelFunctionContainerType)
                    .SelectWhere(methodInfo =>
                    parseKernelFunctionReturnType(methodInfo.ReturnType).ToMaybe().Map(returnType => (methodInfo, returnType)))
                    .Select(specializedMethodInfoAndReturnType =>
                    {
                        var parameterTypes =
                            specializedMethodInfoAndReturnType.methodInfo
                                .GetParameters().Select(pi => parseKernelFunctionParameterType(pi.ParameterType))
                                .ToImmutableList();

                        return
                            new KernelFunctionSpecializedInfo(
                                parameterTypes: parameterTypes,
                                returnType: specializedMethodInfoAndReturnType.returnType,
                                CompileInvocation: argumentsExpressions =>
                                    compileInvocationForArgumentList(SyntaxFactory.ArgumentList(
                                        SyntaxFactory.SeparatedList(
                                            argumentsExpressions.Select(SyntaxFactory.Argument)))));
                    })
                    .ToImmutableList();

            return
                new KernelFunctionInfo(
                    CompileGenericInvocation: compileGenericInvocation,
                    SpecializedImplementations: specializedImplementations);
        }

        return
            methodsInfos
            .Where(methodInfo =>
            methodInfo.ReturnType == typeof(Result<string, PineValue>) &&
            methodInfo.GetParameters().Length == 1 && methodInfo.GetParameters()[0].ParameterType == typeof(PineValue))
            .ToImmutableDictionary(m => m.Name, ReadKernelFunctionInfo);
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
            PineVM.EncodeExpressionAsValue(decodeAndEvaluateExpression)
                .Extract(err => throw new Exception(err));

        var decodeAndEvaluateExpressionHash =
            CommonConversion.StringBase16(PineValueHashTree.ComputeHash(decodeAndEvaluateExpressionValue));

        var invocationExpression =
            SyntaxFactory.InvocationExpression(
                SyntaxFactory.IdentifierName(environment.argumentEvalGenericName))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList<ArgumentSyntax>(
                        new SyntaxNodeOrToken[]
                        {
                            SyntaxFactory.Argument(
                                SyntaxFactory.IdentifierName("expression_" + decodeAndEvaluateExpressionHash[..10])),
                            SyntaxFactory.Token(SyntaxKind.CommaToken),
                            SyntaxFactory.Argument(
                                SyntaxFactory.IdentifierName(environment.argumentEnvironmentName))
                        })));

        return
            Result<string, (CompiledExpression, DependenciesFromCompilation)>.ok(
                (CompiledExpression.WithTypeResult(invocationExpression),
                    DependenciesFromCompilation.Empty
                    with
                    {
                        Expressions = ImmutableHashSet.Create((decodeAndEvaluateExpressionHash, (Expression)decodeAndEvaluateExpression)),
                    }));
    }

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
                new PineVM().EvaluateExpression(decodeAndEvaluateExpression, PineValue.EmptyList)
                .MapError(err => "Got independent environment, but failed to evaluated: " + err);
        }

        return
            TryEvaluateExpressionIndependent(decodeAndEvaluateExpression.expression)
            .MapError(err => "Expression is not independent: " + err)
            .AndThen(PineVM.DecodeExpressionFromValueDefault)
            .AndThen(innerExpr => TryEvaluateExpressionIndependent(innerExpr)
            .MapError(err => "Inner expression is not independent: " + err));
    }

    public static Result<string, (CompiledExpression, DependenciesFromCompilation)> CompileToCSharpExpression(
        Expression.LiteralExpression literalExpression)
    {
        return
            Result<string, (CompiledExpression, DependenciesFromCompilation)>.ok(
                (CompiledExpression.WithTypePlainValue(SyntaxFactory.IdentifierName(DeclarationNameForValue(literalExpression.Value))),
                DependenciesFromCompilation.Empty with { Values = ImmutableHashSet.Create(literalExpression.Value) }));
    }

    public static string DeclarationNameForValue(PineValue pineValue) =>
        "value_" + CommonConversion.StringBase16(PineValueHashTree.ComputeHash(pineValue))[..10];

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
                                SyntaxFactory.LiteralExpression(
                                    SyntaxKind.NumericLiteralExpression,
                                    SyntaxFactory.Literal((long)okInteger.Value))))));
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
                .Select(b => SyntaxFactory.LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    SyntaxFactory.Literal(b)));

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
                                SyntaxFactory.ArrayCreationExpression(
                                    SyntaxFactory.ArrayType(
                                        SyntaxFactory.PredefinedType(
                                            SyntaxFactory.Token(SyntaxKind.ByteKeyword)))
                                    .WithRankSpecifiers(
                                        SyntaxFactory.SingletonList(
                                            SyntaxFactory.ArrayRankSpecifier(
                                                SyntaxFactory.SingletonSeparatedList<ExpressionSyntax>(
                                                    SyntaxFactory.OmittedArraySizeExpression())))))
                                .WithInitializer(
                                    SyntaxFactory.InitializerExpression(
                                        SyntaxKind.ArrayInitializerExpression,
                                        SyntaxFactory.SeparatedList<ExpressionSyntax>(bytesIntegers)))))));
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
                                SyntaxFactory.ArrayCreationExpression(
                                    SyntaxFactory.ArrayType(SyntaxFactory.IdentifierName("PineValue"))
                                    .WithRankSpecifiers(
                                        SyntaxFactory.SingletonList(
                                            SyntaxFactory.ArrayRankSpecifier(
                                                SyntaxFactory.SingletonSeparatedList<ExpressionSyntax>(
                                                    SyntaxFactory.OmittedArraySizeExpression())))))
                                .WithInitializer(
                                    SyntaxFactory.InitializerExpression(
                                        SyntaxKind.ArrayInitializerExpression,
                                        SyntaxFactory.SeparatedList(elementsSyntaxes)))))));
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

    private static Result<string, ExpressionSyntax> EncodePineExpressionAsCSharpExpression(
        Expression expression,
        Func<PineValue, ExpressionSyntax?> overrideDefaultExpressionForValue)
    {
        var continueEncode = new Func<Expression, Result<string, ExpressionSyntax>>(
            descendant => EncodePineExpressionAsCSharpExpression(descendant, overrideDefaultExpressionForValue));

        return expression switch
        {
            Expression.LiteralExpression literal =>
            Result<string, ExpressionSyntax>.ok(
                NewConstructorOfExpressionVariant(
                    nameof(Expression.LiteralExpression),
                    CompileToCSharpLiteralExpression(literal.Value, overrideDefaultExpressionForValue))),

            Expression.EnvironmentExpression =>
            Result<string, ExpressionSyntax>.ok(
                NewConstructorOfExpressionVariant(
                    nameof(Expression.EnvironmentExpression))),

            Expression.ListExpression list =>
            list.List.Select(continueEncode)
            .ListCombine()
            .MapError(err => "Failed to encode list expression element: " + err)
            .Map(elementsSyntaxes =>
            NewConstructorOfExpressionVariant(
                nameof(Expression.ListExpression),
                SyntaxFactory.InvocationExpression(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            SyntaxFactory.IdentifierName("ImmutableArray"),
                            SyntaxFactory.GenericName(
                                    SyntaxFactory.Identifier("Create"))
                                .WithTypeArgumentList(
                                    SyntaxFactory.TypeArgumentList(
                                        SyntaxFactory.SingletonSeparatedList<TypeSyntax>(
                                            SyntaxFactory.QualifiedName(
                                                SyntaxFactory.QualifiedName(
                                                    SyntaxFactory.IdentifierName("Pine"),
                                                    SyntaxFactory.IdentifierName("PineVM")),
                                                SyntaxFactory.IdentifierName("Expression")))))))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList(
                            elementsSyntaxes.Select(SyntaxFactory.Argument)))))),

            Expression.ConditionalExpression conditionalExpression =>
                continueEncode(conditionalExpression.condition)
                    .MapError(err => "Failed to encode condition: " + err)
                    .AndThen(encodedCondition =>
                        continueEncode(conditionalExpression.ifTrue)
                            .MapError(err => "Failed to encode branch if true: " + err)
                            .AndThen(encodedIfTrue =>
                                continueEncode(conditionalExpression.ifFalse)
                                    .MapError(err => "Failed to encode branch if false: " + err)
                                    .Map(encodedIfFalse =>
                                        NewConstructorOfExpressionVariant(
                                            nameof(Expression.ConditionalExpression),
                                            encodedCondition,
                                            encodedIfTrue,
                                            encodedIfFalse)))),

            Expression.KernelApplicationExpression kernelApplicationExpr =>
            continueEncode(kernelApplicationExpr.argument)
            .MapError(err => "Failed to encode argument of kernel application: " + err)
            .Map(encodedArgument =>
            (ExpressionSyntax)SyntaxFactory.InvocationExpression(
                SyntaxFactory.QualifiedName(
                    pineVmClassQualifiedNameSyntax,
                    SyntaxFactory.IdentifierName(nameof(PineVM.DecodeKernelApplicationExpressionThrowOnUnknownName))))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList(
                    [
                        SyntaxFactory.Argument(
                            SyntaxFactory.LiteralExpression(
                                SyntaxKind.StringLiteralExpression,
                                SyntaxFactory.Literal(kernelApplicationExpr.functionName))),
                        SyntaxFactory.Argument(encodedArgument)
                    ])))),

            Expression.DecodeAndEvaluateExpression decodeAndEvaluate =>
            continueEncode(decodeAndEvaluate.expression)
            .MapError(err => "Failed to encode expression of decode and evaluate: " + err)
            .AndThen(encodedExpression =>
            continueEncode(decodeAndEvaluate.environment)
            .MapError(err => "Failed to encode environment of decode and evaluate: " + err)
            .Map(encodedEnvironment =>
            NewConstructorOfExpressionVariant(
                nameof(Expression.DecodeAndEvaluateExpression),
                encodedExpression,
                encodedEnvironment))),

            _ =>
            Result<string, ExpressionSyntax>.err("Expression type not implemented: " + expression.GetType().FullName)
        };
    }

    private static ExpressionSyntax NewConstructorOfExpressionVariant(
        string expressionVariantTypeName,
        params ExpressionSyntax[] argumentsExpressions)
    {
        return
            SyntaxFactory.ObjectCreationExpression(
                SyntaxFactory.QualifiedName(
                    SyntaxFactory.QualifiedName(
                        SyntaxFactory.QualifiedName(
                            SyntaxFactory.IdentifierName("Pine"),
                                SyntaxFactory.IdentifierName("PineVM")),
                        SyntaxFactory.IdentifierName("Expression")),
                    SyntaxFactory.IdentifierName(expressionVariantTypeName)))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList(argumentsExpressions.Select(SyntaxFactory.Argument))));
    }

    public static IEnumerable<PineValue> OrderValuesByContainment(IEnumerable<PineValue> pineValues)
    {
        var blobs = pineValues.OfType<PineValue.BlobValue>();

        var rootLists = pineValues.OfType<PineValue.ListValue>().Distinct();

        var descendantLists =
            EnumerateDescendantListsBreadthFirst(rootLists)
            .ToImmutableList();

        var orderedLists =
            rootLists
            .Concat(descendantLists)
            .Reverse()
            .Distinct()
            .Intersect(rootLists)
            .ToImmutableList();

        return blobs.Cast<PineValue>().Concat(orderedLists);
    }

    private static IEnumerable<PineValue.ListValue> EnumerateDescendantListsBreadthFirst(IEnumerable<PineValue.ListValue> roots)
    {
        var queue = new Queue<PineValue.ListValue>(roots);

        while (queue.Any())
        {
            foreach (var item in queue.Dequeue().Elements.OfType<PineValue.ListValue>())
            {
                yield return item;

                if (!queue.Contains(item))
                    queue.Enqueue(item);
            }
        }
    }
}
