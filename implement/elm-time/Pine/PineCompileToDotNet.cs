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

namespace Pine;

public class PineCompileToDotNet
{
    public record SyntaxContainerConfig(
        string containerTypeName,
        string dictionaryMemberName);

    public record CompileCSharpClassResult(
        ClassDeclarationSyntax ClassDeclarationSyntax,
        IReadOnlyList<UsingDirectiveSyntax> Usings);

    static public Result<string, CompileCSharpClassResult> CompileExpressionsToCSharpFile(
        IReadOnlyList<PineVM.Expression> expressions,
        SyntaxContainerConfig containerConfig,
        int? limitNumber)
    {
        var argumentEnvironmentName = "pine_environment";

        var expressionsResults =
            expressions
            .ToImmutableDictionary(
                keySelector: expression => expression,
                elementSelector: expression =>
                CompileToCSharpFunctionBlockSyntax(
                    expression,
                    new EnvironmentConfig(argumentEnvironmentName: argumentEnvironmentName)));

        var parametersSyntaxes = new ParameterSyntax[]
        {
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
                                new SyntaxNodeOrToken[]{
                                    SyntaxFactory.PredefinedType(
                                        SyntaxFactory.Token(SyntaxKind.StringKeyword)),
                                    SyntaxFactory.Token(SyntaxKind.CommaToken),
                                    SyntaxFactory.IdentifierName("PineValue")}))),
                    SyntaxFactory.Identifier(functionName))
                .WithModifiers(
                    SyntaxFactory.TokenList(
                        new[]{
                            SyntaxFactory.Token(SyntaxKind.StaticKeyword),
                            SyntaxFactory.Token(SyntaxKind.PublicKeyword)}))
                .WithParameterList(
                    SyntaxFactory.ParameterList(
                        SyntaxFactory.SeparatedList(parametersSyntaxes)))
                .WithBody(blockSyntax);
        }

        static (PineValue expressionValue, string functionName) functionNameForExpression(PineVM.Expression expression)
        {
            var asValue = PineVM.EncodeExpressionAsValue(expression).Extract(err => throw new Exception(err));

            return
                (asValue,
                "expr_function_" + CommonConversion.StringBase16(Composition.GetHash(asValue))[..10]);
        }

        var expressionsMethodDeclarations =
            expressionsResults
            .SelectMany(expressionAndResult =>
            expressionAndResult.Value
            .Map(ok =>
            {
                var functionNameAndValue = functionNameForExpression(expressionAndResult.Key);

                return
                (functionNameAndValue.expressionValue,
                functionNameAndValue.functionName,
                memberDeclarationSyntax: memberDeclarationSyntaxForExpression(functionNameAndValue.functionName, ok.blockSyntax));
            })
            .Map(ImmutableList.Create)
            .WithDefault(ImmutableList<(PineValue, string, MemberDeclarationSyntax)>.Empty))
            .Take(limitNumber ?? int.MaxValue)
            .ToImmutableList();

        var aggregateDependencies =
            expressionsResults
            .Select(er => er.Value.Map(ok => ok.dependencies).WithDefault(DependenciesFromCompilation.Empty))
            .Aggregate(
                seed: DependenciesFromCompilation.Empty,
                (acc, next) => acc.Union(next));

        var valueUsages = new Dictionary<PineValue, int>();

        void incrementValueUsageCounter(PineValue pineValue)
        {
            if (!valueUsages.TryGetValue(pineValue, out var counter))
                counter = 0;

            valueUsages[pineValue] = ++counter;
        }

        void countUsagesInValueRecursive(PineValue pineValue)
        {
            incrementValueUsageCounter(pineValue);

            if (pineValue is PineValue.ListValue list)
            {
                foreach (var i in list.Elements)
                    countUsagesInValueRecursive(i);
            }
        }

        foreach (var item in aggregateDependencies.Values)
        {
            countUsagesInValueRecursive(item);
        }

        var valuesToDeclare =
            OrderValuesByContainment(
                aggregateDependencies.Values
                .Concat(valueUsages.Where(stats => 2 < stats.Value).Select(stats => stats.Key))
                .Concat(expressionsMethodDeclarations.Select(forExprFunction => forExprFunction.expressionValue))
                .Distinct())
            .ToImmutableList();

        ExpressionSyntax? specialSyntaxForPineValue(PineValue pineValue)
        {
            if (valuesToDeclare.Contains(pineValue))
                return SyntaxFactory.IdentifierName(DeclarationNameForValue(pineValue));

            return null;
        }

        (string memberName, TypeSyntax typeSyntax, ExpressionSyntax memberDeclaration) memberDeclarationForValue(
            PineValue pineValue)
        {
            var valueExpression = CompileToCSharpLiteralExpression(pineValue, specialSyntaxForPineValue);

            var memberName = DeclarationNameForValue(pineValue);

            return
            (memberName,
            SyntaxFactory.IdentifierName("PineValue"),
            valueExpression);
        }

        (string memberName, TypeSyntax typeSyntax, ExpressionSyntax memberDeclaration) memberDeclarationForExpression(
            PineVM.Expression expression)
        {
            var expressionExpression =
                EncodePineExpressionAsCSharpExpression(expression, specialSyntaxForPineValue)
                .Extract(err => throw new Exception("Failed to encode expression: " + err));

            var memberName = "expression_" + GetNameForExpression(expressionExpression);

            return
            (memberName,
            SyntaxFactory.QualifiedName(
                SyntaxFactory.IdentifierName("PineVM"),
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
                            new SyntaxNodeOrToken[]{
                                SyntaxFactory.IdentifierName("PineValue"),
                                SyntaxFactory.Token(SyntaxKind.CommaToken),
                                SyntaxFactory.GenericName(
                                    SyntaxFactory.Identifier("Func"))
                                .WithTypeArgumentList(
                                    SyntaxFactory.TypeArgumentList(
                                        SyntaxFactory.SeparatedList<TypeSyntax>(
                                            new SyntaxNodeOrToken[]{
                                                SyntaxFactory.IdentifierName("PineValue"),
                                                SyntaxFactory.Token(SyntaxKind.CommaToken),
                                                SyntaxFactory.GenericName(
                                                    SyntaxFactory.Identifier("Result"))
                                                .WithTypeArgumentList(
                                                    SyntaxFactory.TypeArgumentList(
                                                        SyntaxFactory.SeparatedList<TypeSyntax>(
                                                            new SyntaxNodeOrToken[]{
                                                                SyntaxFactory.PredefinedType(
                                                                    SyntaxFactory.Token(SyntaxKind.StringKeyword)),
                                                                SyntaxFactory.Token(SyntaxKind.CommaToken),
                                                                SyntaxFactory.IdentifierName("PineValue")})))})))}))),
                SyntaxFactory.IdentifierName("Empty"));

        var dictionaryExpression =
            expressionsMethodDeclarations
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
                                    new SyntaxNodeOrToken[]{
                                        SyntaxFactory.Argument(SyntaxFactory.IdentifierName(
                                            DeclarationNameForValue(fromCompiledExpression.expressionValue))),
                                        SyntaxFactory.Token(SyntaxKind.CommaToken),
                                        SyntaxFactory.Argument(
                                            SyntaxFactory.IdentifierName(fromCompiledExpression.functionName))}))));

        var dictionaryMemberDeclaration =
            SyntaxFactory.MethodDeclaration(
            SyntaxFactory.GenericName(
                SyntaxFactory.Identifier("IReadOnlyDictionary"))
            .WithTypeArgumentList(
                SyntaxFactory.TypeArgumentList(
                    SyntaxFactory.SeparatedList<TypeSyntax>(
                        new SyntaxNodeOrToken[]{
                            SyntaxFactory.IdentifierName("PineValue"),
                            SyntaxFactory.Token(SyntaxKind.CommaToken),
                            SyntaxFactory.GenericName(
                                SyntaxFactory.Identifier("Func"))
                            .WithTypeArgumentList(
                                SyntaxFactory.TypeArgumentList(
                                    SyntaxFactory.SeparatedList<TypeSyntax>(
                                        new SyntaxNodeOrToken[]{
                                            SyntaxFactory.IdentifierName("PineValue"),
                                            SyntaxFactory.Token(SyntaxKind.CommaToken),
                                            SyntaxFactory.GenericName(
                                                SyntaxFactory.Identifier("Result"))
                                            .WithTypeArgumentList(
                                                SyntaxFactory.TypeArgumentList(
                                                    SyntaxFactory.SeparatedList<TypeSyntax>(
                                                        new SyntaxNodeOrToken[]{
                                                            SyntaxFactory.PredefinedType(
                                                                SyntaxFactory.Token(SyntaxKind.StringKeyword)),
                                                            SyntaxFactory.Token(SyntaxKind.CommaToken),
                                                            SyntaxFactory.IdentifierName("PineValue")})))})))}))),
            identifier: containerConfig.dictionaryMemberName)
        .WithModifiers(
            SyntaxFactory.TokenList(
                new[]{
                    SyntaxFactory.Token(SyntaxKind.StaticKeyword),
                    SyntaxFactory.Token(SyntaxKind.PublicKeyword)}))
        .WithExpressionBody(
            SyntaxFactory.ArrowExpressionClause(dictionaryExpression))
            .WithSemicolonToken(
                SyntaxFactory.Token(SyntaxKind.SemicolonToken));

        var staticReadonlyFieldMembers = new[]
        {
            (memberName:"value_true",
            typeSyntax:(TypeSyntax)SyntaxFactory.IdentifierName("PineValue"),
            (ExpressionSyntax)SyntaxFactory.MemberAccessExpression(
                SyntaxKind.SimpleMemberAccessExpression,
                SyntaxFactory.IdentifierName("PineVM"),
                SyntaxFactory.IdentifierName("TrueValue"))),

            ("value_false",
            SyntaxFactory.IdentifierName("PineValue"),
            SyntaxFactory.MemberAccessExpression(
                SyntaxKind.SimpleMemberAccessExpression,
                SyntaxFactory.IdentifierName("PineVM"),
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
                    .WithInitializer(
                        SyntaxFactory.EqualsValueClause(member.Item3)))))
        .WithModifiers(
            SyntaxFactory.TokenList(
                new[]{
                    SyntaxFactory.Token(SyntaxKind.StaticKeyword),
                    SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                    SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword)}))
        )
        .ToImmutableList();

        return Result<string, CompileCSharpClassResult>.ok(
            new CompileCSharpClassResult(
                ClassDeclarationSyntax:
                SyntaxFactory.ClassDeclaration(containerConfig.containerTypeName)
                .WithMembers(
                    SyntaxFactory.List(
                        new MemberDeclarationSyntax[] { dictionaryMemberDeclaration }
                        .Concat(expressionsMethodDeclarations.Select(f => f.memberDeclarationSyntax))
                        .Concat(staticFieldsDeclarations).ToArray()))
                .NormalizeWhitespace(eol: "\n"),
                Usings: usings));
    }

    public record CompileToAssemblyResult(
        byte[] Assembly,
        Func<IReadOnlyDictionary<PineValue, Func<PineValue, Result<string, PineValue>>>> BuildCompiledExpressionsDictionary);

    static public Result<string, CompileToAssemblyResult> CompileToAssembly(
        SyntaxContainerConfig syntaxContainerConfig,
        CompilationUnitSyntax compilationUnitSyntax)
    {
        var syntaxTree = CSharpSyntaxTree.Create(compilationUnitSyntax);

        var compilation = CSharpCompilation.Create("GeneratedContainer.cs")
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

        static bool CanIgnoreErrorMessage(Diagnostic diagnostic)
        {
            if (diagnostic.ToString().Contains("error CS0246: The type or namespace name 'System.Collections.Immutable' could not be found"))
            {
                // It is unclear why the compiler creates this error message, despite the references containing the assembly.
                return true;
            }

            return false;
        }

        var compilationErrorsAccountingForCompilerProblem =
            compilationErrors.Where(d => !CanIgnoreErrorMessage(d))
            .ToImmutableList();

        if (!compilationResult.Success && 0 < compilationErrorsAccountingForCompilerProblem.Count)
        {
            return Result<string, CompileToAssemblyResult>.err(
                "Compilation failed with " + compilationErrorsAccountingForCompilerProblem.Count + " errors:\n" +
                string.Join("\n", compilationErrorsAccountingForCompilerProblem.Select(d => d.ToString())));
        }

        var assembly = codeStream.ToArray();

        IReadOnlyDictionary<PineValue, Func<PineValue, Result<string, PineValue>>> buildDictionary()
        {
            var loadedAssembly = Assembly.Load(assembly);

            var compiledType = loadedAssembly.GetType(syntaxContainerConfig.containerTypeName);

            var dictionaryMember = compiledType.GetField(syntaxContainerConfig.containerTypeName, BindingFlags.Public | BindingFlags.Static);

            return (IReadOnlyDictionary<PineValue, Func<PineValue, Result<string, PineValue>>>)dictionaryMember.GetValue(null);
        }

        return Result<string, CompileToAssemblyResult>.ok(
            new CompileToAssemblyResult(
                Assembly: assembly,
                BuildCompiledExpressionsDictionary: buildDictionary));
    }

    static readonly Lazy<IImmutableList<MetadataReference>> MetadataReferences =
        new(() => ListMetadataReferences().ToImmutableList());

    static IEnumerable<MetadataReference> ListMetadataReferences()
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

    public record EnvironmentConfig(string argumentEnvironmentName);

    public record DependenciesFromCompilation(
        ImmutableHashSet<PineValue> Values,
        IImmutableSet<PineVM.Expression> Expressions)
    {
        static readonly public DependenciesFromCompilation Empty = new(
            Values: ImmutableHashSet<PineValue>.Empty,
            Expressions: ImmutableHashSet<PineVM.Expression>.Empty);

        static public (T, DependenciesFromCompilation) WithNoDependencies<T>(T other) => (other, Empty);

        public DependenciesFromCompilation Union(DependenciesFromCompilation other) =>
            new(Values: Values.Union(other.Values),
                Expressions: Expressions.Union(other.Expressions));
    }

    static public Result<string, (BlockSyntax blockSyntax, DependenciesFromCompilation dependencies)> CompileToCSharpFunctionBlockSyntax(
        PineVM.Expression expression,
        EnvironmentConfig environment) =>
            CompileToCSharpExpression(expression, environment)
            .Map(exprAndDeps => (SyntaxFactory.Block(SyntaxFactory.ReturnStatement(
                WrapExpressionInPineValueResultOk(exprAndDeps.expression))), exprAndDeps.dependencies));

    static public ExpressionSyntax WrapExpressionInPineValueResultOk(ExpressionSyntax expression) =>
        SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    SyntaxFactory.GenericName(
                        SyntaxFactory.Identifier("Result"))
                    .WithTypeArgumentList(
                        SyntaxFactory.TypeArgumentList(
                            SyntaxFactory.SeparatedList<TypeSyntax>(
                                new SyntaxNodeOrToken[]{
                                    SyntaxFactory.PredefinedType(
                                        SyntaxFactory.Token(SyntaxKind.StringKeyword)),
                                    SyntaxFactory.Token(SyntaxKind.CommaToken),
                                    SyntaxFactory.IdentifierName("PineValue")}))),
                    SyntaxFactory.IdentifierName("ok")))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SingletonSeparatedList(SyntaxFactory.Argument(expression))));

    static public Result<string, (ExpressionSyntax expression, DependenciesFromCompilation dependencies)> CompileToCSharpExpression(
        PineVM.Expression expression,
        EnvironmentConfig environment)
    {
        return
            expression switch
            {
                PineVM.Expression.EnvironmentExpression =>
                Result<string, (ExpressionSyntax, DependenciesFromCompilation)>.ok(
                    DependenciesFromCompilation.WithNoDependencies(
                        SyntaxFactory.IdentifierName(environment.argumentEnvironmentName))),

                PineVM.Expression.ListExpression listExpr =>
                CompileToCSharpExpression(listExpr, environment),

                PineVM.Expression.LiteralExpression literalExpr =>
                CompileToCSharpExpression(literalExpr),

                PineVM.Expression.ConditionalExpression conditional =>
                CompileToCSharpExpression(conditional, environment),

                PineVM.Expression.KernelApplicationExpression kernelApp =>
                CompileToCSharpExpression(kernelApp, environment),

                PineVM.Expression.DecodeAndEvaluateExpression decodeAndEval =>
                CompileToCSharpExpression(decodeAndEval, environment),

                PineVM.Expression.StringTagExpression stringTagExpr =>
                CompileToCSharpExpression(stringTagExpr, environment),

                _ =>
                Result<string, (ExpressionSyntax, DependenciesFromCompilation)>.err(
                    "Unsupported syntax kind: " + expression.GetType().FullName)
            };
    }

    static public Result<string, (ExpressionSyntax, DependenciesFromCompilation)> CompileToCSharpExpression(
        PineVM.Expression.ListExpression listExpression,
        EnvironmentConfig environment)
    {
        return
            listExpression.List.Select((elementExpression, elementIndex) =>
            CompileToCSharpExpression(elementExpression, environment)
            .MapError(err => "Failed to translate list element " + elementIndex + ": " + err))
            .ListCombine()
            .Map(compiledElements =>
            ((ExpressionSyntax)
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
                                    SyntaxFactory.SeparatedList(
                                        compiledElements.Select(ce => ce.expression)))))))),
                                        compiledElements.Aggregate(
                                            seed: DependenciesFromCompilation.Empty,
                                            func: (aggregate, next) => aggregate.Union(next.dependencies))));
    }

    static public Result<string, (ExpressionSyntax, DependenciesFromCompilation)> CompileToCSharpExpression(
        PineVM.Expression.KernelApplicationExpression kernelApplicationExpression,
        EnvironmentConfig environment)
    {
        if (!KernelFunctionInfo.Value.TryGetValue(kernelApplicationExpression.functionName, out var syntaxBuilder))
            Result<string, ExpressionSyntax>.err("Kernel function name " + kernelApplicationExpression.functionName + " does not match any of the "
                + KernelFunctionInfo.Value.Count + " known names: " + string.Join(", ", KernelFunctionInfo.Value.Keys));

        return
            CompileToCSharpExpression(
                kernelApplicationExpression.argument,
                environment)
            .Map(compiledArgument =>
            ((ExpressionSyntax)
            SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    syntaxBuilder(compiledArgument.expression),
                    SyntaxFactory.IdentifierName("WithDefault")))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Argument(
                            SyntaxFactory.MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                SyntaxFactory.IdentifierName("PineValue"),
                                SyntaxFactory.IdentifierName("EmptyList")))))),
             compiledArgument.dependencies));
    }

    static readonly Lazy<IReadOnlyDictionary<string, Func<ExpressionSyntax, InvocationExpressionSyntax>>> KernelFunctionInfo =
        new(ReadKernelMethodInfoViaReflection);

    static IReadOnlyDictionary<string, Func<ExpressionSyntax, InvocationExpressionSyntax>> ReadKernelMethodInfoViaReflection()
    {
        var kernelFunctionContainerType = typeof(PineVM.KernelFunction);
        var methodsInfos = kernelFunctionContainerType.GetMethods(BindingFlags.Static | BindingFlags.Public);

        return
            methodsInfos
            .Where(m =>
            m.ReturnType == typeof(Result<string, PineValue>) &&
            m.GetParameters().Length == 1 && m.GetParameters()[0].ParameterType == typeof(PineValue))
            .ToImmutableDictionary(m => m.Name, m => new Func<ExpressionSyntax, InvocationExpressionSyntax>(argumentExpression =>
            SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName("PineVM"),
                        SyntaxFactory.IdentifierName(kernelFunctionContainerType.Name)),
                    SyntaxFactory.IdentifierName(m.Name)),
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.Argument(argumentExpression)))
                .WithOpenParenToken(
                    SyntaxFactory.Token(
                        SyntaxFactory.TriviaList(),
                        SyntaxKind.OpenParenToken,
                        SyntaxFactory.TriviaList(
                            SyntaxFactory.LineFeed))))));
    }

    static public Result<string, (ExpressionSyntax, DependenciesFromCompilation)> CompileToCSharpExpression(
        PineVM.Expression.ConditionalExpression conditionalExpression,
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
            ((ExpressionSyntax)SyntaxFactory.ConditionalExpression(
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.EqualsExpression,
                    SyntaxFactory.IdentifierName("value_true"),
                    compiledCondition.expression),
                compiledIfTrue.expression.InsertTriviaBefore(SyntaxFactory.LineFeed, SyntaxFactory.TriviaList()),
                compiledIfFalse.expression.InsertTriviaBefore(SyntaxFactory.LineFeed, SyntaxFactory.TriviaList())),
                compiledCondition.dependencies.Union(compiledIfTrue.dependencies).Union(compiledIfFalse.dependencies)))));
    }

    static public Result<string, (ExpressionSyntax, DependenciesFromCompilation)> CompileToCSharpExpression(
        PineVM.Expression.DecodeAndEvaluateExpression decodeAndEvaluateExpression,
        EnvironmentConfig environment)
    {
        return
            TryEvaluateExpressionIndependent(decodeAndEvaluateExpression.expression)
            .MapError(err => "Expression is not independent: " + err)
            .AndThen(PineVM.DecodeExpressionFromValueDefault)
            .AndThen(innerExpression =>
            {
                return
                Result<string, (ExpressionSyntax, DependenciesFromCompilation)>.err(
                    "Transforming inner expression not implemented.");
            });
    }

    static public Result<string, PineVM.Expression> TransformPineExpressionWithOptionalReplacement(
        Func<PineVM.Expression, Result<string, Maybe<PineVM.Expression>>> findReplacement,
        PineVM.Expression expression)
    {
        return
            findReplacement(expression)
            .MapError(err => "Failed to find replacement: " + err)
            .AndThen(maybeReplacement =>
            maybeReplacement
            .Map(r => Result<string, PineVM.Expression>.ok(r))
            .WithDefaultBuilder(() =>
            {
                return expression switch
                {
                    PineVM.Expression.LiteralExpression literal =>
                    Result<string, PineVM.Expression>.ok(literal),

                    PineVM.Expression.EnvironmentExpression =>
                    Result<string, PineVM.Expression>.ok(expression),

                    PineVM.Expression.ListExpression list =>
                    list.List.Select(e => TransformPineExpressionWithOptionalReplacement(findReplacement, e))
                    .ListCombine()
                    .Map(elements => (PineVM.Expression)new PineVM.Expression.ListExpression(elements.ToImmutableArray())),

                    PineVM.Expression.ConditionalExpression conditional =>
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
                    (PineVM.Expression)new PineVM.Expression.ConditionalExpression(
                        transformedCondition,
                        transformedIfTrue,
                        transformedIfFalse)))),

                    PineVM.Expression.KernelApplicationExpression kernelAppl =>
                    TransformPineExpressionWithOptionalReplacement(findReplacement, kernelAppl.argument)
                    .MapError(err => "Failed to transform kernel application argument: " + err)
                    .Map(transformedArgument => (PineVM.Expression)new PineVM.Expression.KernelApplicationExpression(
                        functionName: kernelAppl.functionName,
                        argument: transformedArgument,
                        function: null)),

                    PineVM.Expression.StringTagExpression stringTag =>
                    TransformPineExpressionWithOptionalReplacement(findReplacement, stringTag.tagged)
                    .Map(transformedTagged => (PineVM.Expression)new PineVM.Expression.StringTagExpression(tag: stringTag.tag, tagged: transformedTagged)),

                    _ =>
                    Result<string, PineVM.Expression>.err("Unsupported expression type: " + expression.GetType().FullName)
                };
            }));
    }

    static public Result<string, PineValue> TryEvaluateExpressionIndependent(PineVM.Expression expression) =>
        expression switch
        {
            PineVM.Expression.LiteralExpression literal =>
            Result<string, PineValue>.ok(literal.Value),

            PineVM.Expression.ListExpression list =>
            list.List.Select(TryEvaluateExpressionIndependent)
            .ListCombine()
            .Map(PineValue.List),

            PineVM.Expression.KernelApplicationExpression kernelApplication =>
            TryEvaluateExpressionIndependent(kernelApplication.argument)
            .MapError(err => "Failed to evaluate kernel application argument independent: " + err)
            .AndThen(argument => kernelApplication.function(argument)),

            PineVM.Expression.DecodeAndEvaluateExpression decodeAndEvaluateExpression =>
            TryEvaluateExpressionIndependent(decodeAndEvaluateExpression)
            .Map(ok =>
            {
                Console.WriteLine("Succesfully evaluated DecodeAndEvaluateExpression independent 🙃");

                return ok;
            }),

            PineVM.Expression.StringTagExpression stringTag =>
            TryEvaluateExpressionIndependent(stringTag.tagged),

            _ =>
            Result<string, PineValue>.err("Unsupported expression type: " + expression.GetType().FullName)
        };

    static public Result<string, PineValue> TryEvaluateExpressionIndependent(
        PineVM.Expression.DecodeAndEvaluateExpression decodeAndEvaluateExpression)
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

    static public Result<string, (ExpressionSyntax, DependenciesFromCompilation)> CompileToCSharpExpression(
        PineVM.Expression.LiteralExpression literalExpression)
    {
        return
            Result<string, (ExpressionSyntax, DependenciesFromCompilation)>.ok(
                (SyntaxFactory.IdentifierName(DeclarationNameForValue(literalExpression.Value)),
                DependenciesFromCompilation.Empty with { Values = ImmutableHashSet.Create(literalExpression.Value) }));
    }

    static public string DeclarationNameForValue(PineValue pineValue) =>
        "value_" + CommonConversion.StringBase16(Composition.GetHash(pineValue))[..10];

    static public Result<string, (ExpressionSyntax expressionSyntax, DependenciesFromCompilation dependencies)> CompileToCSharpExpression(
        PineVM.Expression.StringTagExpression stringTagExpression,
        EnvironmentConfig environment)
    {
        Console.WriteLine("Compiling string tag: " + stringTagExpression.tag);

        return
            CompileToCSharpExpression(
                stringTagExpression.tagged,
                environment)
            .Map(compiledExpr =>
            (compiledExpr.expression.InsertTriviaBefore(
                SyntaxFactory.Comment("/*\n" + stringTagExpression.tag + "\n*/"),
                SyntaxFactory.TriviaList()),
                compiledExpr.dependencies));
    }

    static public ExpressionSyntax CompileToCSharpLiteralExpression(
        PineValue pineValue,
        Func<PineValue, ExpressionSyntax?> overrideDefaultExpression)
    {
        ExpressionSyntax continueCompile(PineValue pineValue) =>
            overrideDefaultExpression(pineValue) ??
            CompileToCSharpLiteralExpression(pineValue, overrideDefaultExpression);

        if (pineValue == PineValue.EmptyList)
        {
            return
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    SyntaxFactory.IdentifierName("PineValue"),
                    SyntaxFactory.IdentifierName("EmptyList"));
        }

        if (Composition.SignedIntegerFromComponent(pineValue) is Result<string, BigInteger>.Ok okInteger &&
            Composition.ComponentFromSignedInteger(okInteger.Value) == pineValue)
        {
            if (okInteger.Value < long.MaxValue)
            {
                return
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName("Composition"),
                        SyntaxFactory.IdentifierName("ComponentFromSignedInteger")))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Argument(
                                SyntaxFactory.LiteralExpression(
                                    SyntaxKind.NumericLiteralExpression,
                                    SyntaxFactory.Literal((long)okInteger.Value))))));
            }
        }

        if (Composition.StringFromComponent(pineValue) is Result<string, string>.Ok okString)
        {
            return
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName("Composition"),
                        SyntaxFactory.IdentifierName("ComponentFromString")))
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

    static public string GetNameForExpression(ExpressionSyntax syntax)
    {
        var serialized = syntax.ToString();

        var utf8 = Encoding.UTF8.GetBytes(serialized);

        var hash = SHA256.HashData(utf8);

        return CommonConversion.StringBase16(hash)[..10];
    }

    static Result<string, ExpressionSyntax> EncodePineExpressionAsCSharpExpression(
        PineVM.Expression expression,
        Func<PineValue, ExpressionSyntax?> overrideDefaultExpressionForValue)
    {
        var continueEncode = new Func<PineVM.Expression, Result<string, ExpressionSyntax>>(
            descendant => EncodePineExpressionAsCSharpExpression(descendant, overrideDefaultExpressionForValue));

        static ExpressionSyntax continueWithNewConstructorOfExpressionVariant(
            string expressionVariantTypeName,
            params ExpressionSyntax[] argumentsExpressions)
        {
            return
                SyntaxFactory.ObjectCreationExpression(
                    SyntaxFactory.QualifiedName(
                        SyntaxFactory.QualifiedName(
                            SyntaxFactory.IdentifierName("PineVM"),
                            SyntaxFactory.IdentifierName("Expression")),
                        SyntaxFactory.IdentifierName(expressionVariantTypeName)))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList(argumentsExpressions.Select(SyntaxFactory.Argument))));
        }

        return expression switch
        {
            PineVM.Expression.LiteralExpression literal =>
            Result<string, ExpressionSyntax>.ok(
                continueWithNewConstructorOfExpressionVariant(
                    nameof(PineVM.Expression.LiteralExpression),
                    CompileToCSharpLiteralExpression(literal.Value, overrideDefaultExpressionForValue))),

            PineVM.Expression.EnvironmentExpression =>
            Result<string, ExpressionSyntax>.ok(
                continueWithNewConstructorOfExpressionVariant(
                    nameof(PineVM.Expression.EnvironmentExpression))),

            PineVM.Expression.ListExpression list =>
            list.List.Select(continueEncode)
            .ListCombine()
            .MapError(err => "Failed to encode list expression element: " + err)
            .Map(elementsSyntaxes =>
            continueWithNewConstructorOfExpressionVariant(
                nameof(PineVM.Expression.ListExpression),
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName("ImmutableArray"),
                        SyntaxFactory.IdentifierName("Create")))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList(
                            elementsSyntaxes.Select(SyntaxFactory.Argument)))))),

            PineVM.Expression.KernelApplicationExpression kernelApplicationExpr =>
            continueEncode(kernelApplicationExpr.argument)
            .MapError(err => "Failed to encode argument of kernel application: " + err)
            .Map(encodedArgument =>
            (ExpressionSyntax)SyntaxFactory.InvocationExpression(
                SyntaxFactory.QualifiedName(
                    SyntaxFactory.IdentifierName("PineVM"),
                    SyntaxFactory.IdentifierName("DecodeKernelApplicationExpressionThrowOnUnknownName")))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList(
                    new[]
                    {
                        SyntaxFactory.Argument(
                            SyntaxFactory.LiteralExpression(
                                SyntaxKind.StringLiteralExpression,
                                SyntaxFactory.Literal(kernelApplicationExpr.functionName))),
                        SyntaxFactory.Argument(encodedArgument)
                    })))),

            PineVM.Expression.DecodeAndEvaluateExpression decodeAndEvaluate =>
            continueEncode(decodeAndEvaluate.expression)
            .MapError(err => "Failed to encode expression of decode and evaluate: " + err)
            .AndThen(encodedExpression =>
            continueEncode(decodeAndEvaluate.environment)
            .MapError(err => "Failed to encode environment of decode and evaluate: " + err)
            .Map(encodedEnvironment =>
            continueWithNewConstructorOfExpressionVariant(
                nameof(PineVM.Expression.DecodeAndEvaluateExpression),
                encodedExpression,
                encodedEnvironment))),

            _ =>
            Result<string, ExpressionSyntax>.err("Expression type not implemented: " + expression.GetType().FullName)
        };
    }

    static public IEnumerable<PineValue> OrderValuesByContainment(IEnumerable<PineValue> pineValues)
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

    static IEnumerable<PineValue.ListValue> EnumerateDescendantListsBreadthFirst(IEnumerable<PineValue.ListValue> roots)
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
