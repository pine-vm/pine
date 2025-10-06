using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Pine.Core.Internal;
using Pine.Core.PineVM;
using Pine.Core.PopularEncodings;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Pine.PineVM;

public record StaticProgramCSharpClass(
    ClassDeclarationSyntax ClassDeclarationSyntax)
{
    public static StaticProgramCSharpClass FromDeclarations(
        DeclQualifiedName className,
        IReadOnlyDictionary<string, (StaticFunctionInterface interf, StaticExpression<DeclQualifiedName> body)> declarations,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        IReadOnlyList<MethodDeclarationSyntax> functions =
            [.. declarations
            .OrderBy(kv => kv.Key)
            .Select(kv =>
            RenderFunctionToMethod(
                selfFunctionName: className.ContainedDeclName(kv.Key),
                functionInterface: kv.Value.interf,
                body: kv.Value.body,
                availableFunctions,
                availableValueDecls,
                declarationSyntaxContext))];

        var classSyntax =
            SyntaxFactory.ClassDeclaration(className.DeclName)
                .WithMembers(
                    SyntaxFactory.List<MemberDeclarationSyntax>([.. functions]))
                .WithModifiers(
                     SyntaxFactory.TokenList(
                         SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                         SyntaxFactory.Token(SyntaxKind.StaticKeyword)));

        return new StaticProgramCSharpClass(classSyntax);
    }

    public string RenderToString()
    {
        return RenderToString(ClassDeclarationSyntax);
    }

    public static string RenderToString(
        ClassDeclarationSyntax classDeclarationSyntax)
    {
        var syntaxTree =
            StaticProgramCSharpExtension.BuildCompilationUnitSyntax(
                classDeclarationSyntax,
                declarationSyntaxContext: DeclarationSyntaxContext.None,
                namespacePrefix: []);

        return syntaxTree.ToString();
    }

    public static MethodDeclarationSyntax RenderFunctionToMethod(
        DeclQualifiedName selfFunctionName,
        StaticFunctionInterface functionInterface,
        StaticExpression<DeclQualifiedName> body,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        try
        {
            var statementSyntax =
                CompileToCSharpFunction(
                    functionBody: body,
                    selfFunctionName: selfFunctionName,
                    functionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext);

            return
                MemberDeclarationSyntaxForExpression(
                    declarationName: selfFunctionName.DeclName,
                    statementSyntax: statementSyntax,
                    functionInterface: functionInterface,
                    declarationSyntaxContext);
        }
        catch (System.Exception ex)
        {
            throw new System.Exception(
                "Error compiling function '" + selfFunctionName.FullName + "': " + ex.Message,
                ex);
        }
    }


    static MethodDeclarationSyntax MemberDeclarationSyntaxForExpression(
        string declarationName,
        StatementSyntax statementSyntax,
        StaticFunctionInterface functionInterface,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        var blockSyntax =
            (statementSyntax as BlockSyntax)
            ??
            SyntaxFactory.Block(statementSyntax);

        return
            SyntaxFactory.MethodDeclaration(
                returnType:
                CompileTypeSyntax.TypeSyntaxFromType(typeof(PineValue), declarationSyntaxContext),
                SyntaxFactory.Identifier(declarationName))
            .WithModifiers(
                SyntaxFactory.TokenList(
                    SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                    SyntaxFactory.Token(SyntaxKind.StaticKeyword)))
            .WithParameterList(
                SyntaxFactory.ParameterList(
                    SyntaxFactory.SeparatedList(
                        ComposeParameterList(functionInterface, declarationSyntaxContext))))
            .WithBody(blockSyntax);
    }

    public static StatementSyntax CompileToCSharpFunction(
        StaticExpression<DeclQualifiedName> functionBody,
        DeclQualifiedName selfFunctionName,
        StaticFunctionInterface selfFunctionInterface,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        var hasTailRecursiveCalls =
            ContainsFunctionApplicationAsTailCall(
                functionBody,
                selfFunctionName);

        // Build initial blocked names (parameters)
        var initialBlocked =
            selfFunctionInterface.ParamsPaths
            .Select(RenderParamRef)
            .ToImmutableHashSet();

        if (hasTailRecursiveCalls)
        {
            // TODO: Expand to cover cases where we have a common reused derivation from params that does not vary between calls.

            // Map all params to locals that we can update in place.

            var paramToLocalMap =
                selfFunctionInterface.ParamsPaths
                .ToImmutableDictionary(
                    path => path,
                    path => "local_" + RenderParamRef(path),
                    keyComparer: IntPathEqualityComparer.Instance);

            ExpressionSyntax? SelfFunctionInterfaceDelegate(IReadOnlyList<int> path)
            {
                if (paramToLocalMap.TryGetValue(path, out var localName))
                {
                    return SyntaxFactory.IdentifierName(localName);
                }

                return null;
            }

            IReadOnlyList<StatementSyntax> StatementsFromResult(
                StaticExpression<DeclQualifiedName> expr,
                ImmutableDictionary<StaticExpression<DeclQualifiedName>, string> alreadyDeclared)
            {
                // Check if the result is a tail call to self.

                if (expr is StaticExpression<DeclQualifiedName>.FunctionApplication funcApp &&
                    funcApp.FunctionName == selfFunctionName)
                {
                    /*
                     * Parameters for the new instance can depend on parameters of the current instance,
                     * so we cannot always directly overwrite the locals representing parameters.
                     * To avoid overwriting too early, we store the new values in temporary locals first,
                     * then assign them to the parameter locals, then continue the loop.
                     * */

                    var tempDeclarations = new List<LocalDeclarationStatementSyntax>();

                    var assignments = new List<StatementSyntax>();

                    foreach (var paramPath in selfFunctionInterface.ParamsPaths)
                    {
                        if (!paramToLocalMap.TryGetValue(paramPath, out var localName))
                        {
                            throw new System.Exception("Internal error: Missing local for param path.");
                        }

                        var argumentExpr =
                            ExpressionsForFunctionArgument(
                                paramPath,
                                funcApp.Arguments,
                                SelfFunctionInterfaceDelegate,
                                availableFunctions,
                                availableValueDecls,
                                declarationSyntaxContext,
                                alreadyDeclared)
                            .AsGenericValue(declarationSyntaxContext);

                        // Do not emit redundant assignments if the argument is the same as the current local value.

                        if (argumentExpr is IdentifierNameSyntax idName &&
                            idName.Identifier.Text == localName)
                        {
                            continue;
                        }

                        var tempLocalName = localName + "_temp";

                        tempDeclarations.Add(
                            SyntaxFactory.LocalDeclarationStatement(
                                SyntaxFactory.VariableDeclaration(
                                    CompileTypeSyntax.TypeSyntaxFromType(typeof(PineValue), declarationSyntaxContext))
                                .WithVariables(
                                    SyntaxFactory.SingletonSeparatedList(
                                        SyntaxFactory.VariableDeclarator(
                                            SyntaxFactory.Identifier(tempLocalName))
                                        .WithInitializer(
                                            SyntaxFactory.EqualsValueClause(argumentExpr))))));

                        assignments.Add(
                            SyntaxFactory.ExpressionStatement(
                                SyntaxFactory.AssignmentExpression(
                                    SyntaxKind.SimpleAssignmentExpression,
                                    SyntaxFactory.IdentifierName(localName),
                                    SyntaxFactory.IdentifierName(tempLocalName))));
                    }

                    // Contain the local declarations in their own scope to avoid name clashes

                    var tempDeclarationsBlock =
                        SyntaxFactory.Block(
                            (SyntaxList<StatementSyntax>)
                            [
                            .. tempDeclarations,
                            .. assignments
                            ]);

                    return
                        [
                        tempDeclarationsBlock,
                        SyntaxFactory.ContinueStatement()
                        ];
                }

                var resultExpression =
                    EnumerateExpressions(
                        expr,
                        SelfFunctionInterfaceDelegate,
                        availableFunctions,
                        availableValueDecls,
                        declarationSyntaxContext,
                        alreadyDeclared)
                    .AsGenericValue(declarationSyntaxContext);

                return [ResultThrowOrReturn(resultExpression)];
            }

            var paramDeclarations =
                selfFunctionInterface.ParamsPaths
                .Select(paramPath =>
                    SyntaxFactory.LocalDeclarationStatement(
                SyntaxFactory.VariableDeclaration(
                    CompileTypeSyntax.TypeSyntaxFromType(typeof(PineValue), declarationSyntaxContext))
                .WithVariables(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.VariableDeclarator(
                            SyntaxFactory.Identifier(paramToLocalMap[paramPath]))
                        .WithInitializer(
                            SyntaxFactory.EqualsValueClause(
                                SyntaxFactory.IdentifierName(RenderParamRef(paramPath))))))))
                .ToImmutableArray();

            // Extend blocked with our param locals as well
            var blockedWithParamLocals =
                initialBlocked
                .Union(paramToLocalMap.Values);

            var loopBodyCompiled =
                CompileToCSharpStatement(
                    functionBody,
                    SelfFunctionInterfaceDelegate,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    statementsFromResult: StatementsFromResult,
                    alreadyDeclared: ImmutableDictionary<StaticExpression<DeclQualifiedName>, string>.Empty,
                    blockedDeclarations: blockedWithParamLocals);

            // Create while(true) loop
            var whileLoop =
                SyntaxFactory.WhileStatement(
                    SyntaxFactory.LiteralExpression(SyntaxKind.TrueLiteralExpression),
                    loopBodyCompiled.Statement);

            IReadOnlyList<StatementSyntax> allStatements =
                [
                .. paramDeclarations,
                whileLoop
                ];

            return SyntaxFactory.Block(allStatements);
        }
        else
        {
            ExpressionSyntax? SelfFunctionInterfaceDelegate(IReadOnlyList<int> path)
            {
                if (selfFunctionInterface.ParamsPaths
                    .Select((p, i) => (path: p, index: i))
                    .FirstOrDefault(pi => IntPathEqualityComparer.Instance.Equals(pi.path, path)).path is { } matchPath)
                {
                    var paramRef = RenderParamRef(matchPath);

                    return SyntaxFactory.IdentifierName(paramRef);
                }

                return null;
            }

            IReadOnlyList<StatementSyntax> StatementsFromResult(
                StaticExpression<DeclQualifiedName> expr,
                ImmutableDictionary<StaticExpression<DeclQualifiedName>, string> alreadyDeclared)
            {
                var resultExpression =
                    EnumerateExpressions(
                        expr,
                        SelfFunctionInterfaceDelegate,
                        availableFunctions,
                        availableValueDecls,
                        declarationSyntaxContext,
                        alreadyDeclared)
                    .AsGenericValue(declarationSyntaxContext);

                return [ResultThrowOrReturn(resultExpression)];
            }

            var compiled =
                CompileToCSharpStatement(
                    functionBody,
                    SelfFunctionInterfaceDelegate,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    statementsFromResult: StatementsFromResult,
                    alreadyDeclared: ImmutableDictionary<StaticExpression<DeclQualifiedName>, string>.Empty,
                    blockedDeclarations: initialBlocked);

            return compiled.Statement;
        }
    }

    public readonly record struct CompiledStatement(
        StatementSyntax Statement,
        ImmutableHashSet<string> DeclaredLocals);

    public static CompiledStatement CompileToCSharpStatement(
        StaticExpression<DeclQualifiedName> expression,
        System.Func<IReadOnlyList<int>, ExpressionSyntax?> selfFunctionInterface,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls,
        DeclarationSyntaxContext declarationSyntaxContext,
        System.Func<StaticExpression<DeclQualifiedName>, ImmutableDictionary<StaticExpression<DeclQualifiedName>, string>, IReadOnlyList<StatementSyntax>> statementsFromResult,
        ImmutableDictionary<StaticExpression<DeclQualifiedName>, string> alreadyDeclared,
        IReadOnlySet<string> blockedDeclarations)
    {
        bool IgnoreSubexpressionCollectingForCSE(
            StaticExpression<DeclQualifiedName> expr)
        {
            if (expr is StaticExpression<DeclQualifiedName>.Literal)
            {
                return true;
            }

            if (alreadyDeclared.ContainsKey(expr))
            {
                return true;
            }

            if (StaticExpressionExtension.TryParseAsPathToExpression(
                expr,
                StaticExpression<DeclQualifiedName>.EnvironmentInstance) is { } pathToEnv)
            {
                // TODO: Precise condition (will change when we lift the 2-level limit on parameters).

                if (pathToEnv.Count <= 2 &&
                   selfFunctionInterface(pathToEnv) is { })
                {
                    // Don't CSE short environment references (they get replaced with parameters).

                    return true;
                }

                return false;
            }

            return false;
        }

        var subexprDecls =
            CollectSubexpressionsToSeparate(
                expression,
                ignoreExpr: IgnoreSubexpressionCollectingForCSE)
            .OrderBy(expr => expr.SubexpressionCount)
            .ToImmutableArray();

        var mutatedDeclared = alreadyDeclared;

        var newDeclaredStatements = new List<LocalDeclarationStatementSyntax>();
        var newlyDeclaredLocals = ImmutableHashSet.CreateBuilder<string>();

        // Helper to find a unique local name that does not collide with any blocked/local names so far
        string FindFreeLocalName()
        {
            for (var i = 0; ; i++)
            {
                var candidate = "local_" + i.ToString().PadLeft(3, '0');
                if (!blockedDeclarations.Contains(candidate) &&
                    !newlyDeclaredLocals.Contains(candidate))
                {
                    return candidate;
                }
            }
        }

        for (var i = 0; i < subexprDecls.Length; i++)
        {
            var subexpr = subexprDecls[i];

            var localName = FindFreeLocalName();

            newlyDeclaredLocals.Add(localName);

            var statement =
                SyntaxFactory.LocalDeclarationStatement(
                SyntaxFactory.VariableDeclaration(
                    CompileTypeSyntax.TypeSyntaxFromType(typeof(PineValue), declarationSyntaxContext))
                .WithVariables(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.VariableDeclarator(
                            SyntaxFactory.Identifier(localName))
                        .WithInitializer(
                            SyntaxFactory.EqualsValueClause(
                                CompileToCSharpExpression(
                                    subexpr,
                                    selfFunctionInterface,
                                    availableFunctions,
                                    availableValueDecls,
                                    declarationSyntaxContext,
                                    mutatedDeclared)
                                .AsGenericValue(declarationSyntaxContext))))));

            newDeclaredStatements.Add(statement);

            mutatedDeclared =
                mutatedDeclared.SetItem(subexpr, localName);
        }

        var newAlreadyDeclared = mutatedDeclared;

        if (expression is StaticExpression<DeclQualifiedName>.Conditional conditionalExpr)
        {
            var conditionExpr =
                BuildConditionExpression(
                    conditionalExpr.Condition,
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    newAlreadyDeclared);

            // Compile true branch first, using current blocked + newly declared names
            var blockedAfterPrefix = blockedDeclarations.Union(newlyDeclaredLocals).ToImmutableHashSet();

            var trueBranchCompiled =
                CompileToCSharpStatement(
                    conditionalExpr.TrueBranch,
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    statementsFromResult,
                    newAlreadyDeclared,
                    blockedAfterPrefix);

            // If the 'if' block ends with a return/throw/continue, inline false branch into outer scope
            if (BranchEndsWithExitOrLoop(trueBranchCompiled.Statement))
            {
                var falseBranchCompiled =
                    CompileToCSharpStatement(
                        conditionalExpr.FalseBranch,
                        selfFunctionInterface,
                        availableFunctions,
                        availableValueDecls,
                        declarationSyntaxContext,
                        statementsFromResult,
                        newAlreadyDeclared,
                        blockedAfterPrefix.Union(trueBranchCompiled.DeclaredLocals));

                var ifStatementNoElse =
                    SyntaxFactory.IfStatement(
                        condition: conditionExpr,
                        statement: trueBranchCompiled.Statement,
                        @else: null);

                IReadOnlyList<StatementSyntax> allStatementsNoElse =
                    [
                        .. newDeclaredStatements,
                        ifStatementNoElse,
                        .. ExtractStatements(falseBranchCompiled.Statement)
                    ];

                var unionLocals = newlyDeclaredLocals.ToImmutable().Union(trueBranchCompiled.DeclaredLocals).Union(falseBranchCompiled.DeclaredLocals);

                return new CompiledStatement(SyntaxFactory.Block(allStatementsNoElse), unionLocals);
            }

            // Else branch compiled normally; still pass union to keep names globally unique
            var falseBranchCompiledNormal =
                CompileToCSharpStatement(
                    conditionalExpr.FalseBranch,
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    statementsFromResult,
                    newAlreadyDeclared,
                    blockedAfterPrefix.Union(trueBranchCompiled.DeclaredLocals));

            var ifStatement =
                SyntaxFactory.IfStatement(
                    condition: conditionExpr,
                    statement: trueBranchCompiled.Statement,
                    @else: SyntaxFactory.ElseClause(falseBranchCompiledNormal.Statement));

            IReadOnlyList<StatementSyntax> allStatements =
                [
                .. newDeclaredStatements,
                ifStatement,
                ];

            var unionLocalsNormal = newlyDeclaredLocals.ToImmutable().Union(trueBranchCompiled.DeclaredLocals).Union(falseBranchCompiledNormal.DeclaredLocals);

            return new CompiledStatement(SyntaxFactory.Block(allStatements), unionLocalsNormal);
        }

        {
            IReadOnlyList<StatementSyntax> allStatements =
                [
                .. newDeclaredStatements,
                .. statementsFromResult(expression, newAlreadyDeclared)
                ];

            return new CompiledStatement(SyntaxFactory.Block(allStatements), newlyDeclaredLocals.ToImmutable());
        }
    }

    private static bool BranchEndsWithExitOrLoop(StatementSyntax statement)
    {
        if (statement is BlockSyntax block)
        {
            if (block.Statements.Count is 0)
            {
                return false;
            }

            var last = block.Statements.Last();

            return StatementIsExitOrLoop(last);
        }

        return StatementIsExitOrLoop(statement);
    }

    private static bool StatementIsExitOrLoop(StatementSyntax statementSyntax) =>
        statementSyntax is ReturnStatementSyntax or ThrowStatementSyntax or ContinueStatementSyntax;

    private static SyntaxList<StatementSyntax> ExtractStatements(StatementSyntax statement)
    {
        if (statement is BlockSyntax block)
        {
            return block.Statements;
        }

        return [statement];
    }

    public static CompiledCSharpExpression CompileToCSharpExpression(
        StaticExpression<DeclQualifiedName> expression,
        System.Func<IReadOnlyList<int>, ExpressionSyntax?> selfFunctionInterface,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls,
        DeclarationSyntaxContext declarationSyntaxContext,
        ImmutableDictionary<StaticExpression<DeclQualifiedName>, string> alreadyDeclared)
    {
        return
            EnumerateExpressions(
                expression,
                selfFunctionInterface,
                availableFunctions,
                availableValueDecls,
                declarationSyntaxContext,
                alreadyDeclared)
            .FirstOrDefault()!;
    }

    public static IEnumerable<CompiledCSharpExpression> EnumerateExpressions(
        StaticExpression<DeclQualifiedName> expression,
        System.Func<IReadOnlyList<int>, ExpressionSyntax?> selfFunctionInterface,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls,
        DeclarationSyntaxContext declarationSyntaxContext,
        ImmutableDictionary<StaticExpression<DeclQualifiedName>, string> alreadyDeclared)
    {
        if (alreadyDeclared.TryGetValue(expression, out var existingVarName))
        {
            return
                [CompiledCSharpExpression.Generic(
                    SyntaxFactory.IdentifierName(existingVarName))];
        }

        (ExpressionSyntax, IReadOnlyList<int>)? FindNearestParameterForPathInEnv(
            IReadOnlyList<int> pathInEnv)
        {
            // Try each possible prefix of pathInEnv to find a parameter

            for (var len = pathInEnv.Count; len >= 1; len--)
            {
                var prefix = pathInEnv.Take(len).ToImmutableArray();

                if (selfFunctionInterface(prefix) is { } paramRef)
                {
                    var remainingPath = pathInEnv.Skip(len).ToImmutableArray();

                    return (paramRef, remainingPath);
                }
            }

            return null;
        }

        var pathsFromParametersAndLocals = new List<(ExpressionSyntax paramRef, IReadOnlyList<int> remainingPath)>();

        if (StaticExpressionExtension.TryParseAsPathToExpression(
            expression,
            StaticExpression<DeclQualifiedName>.EnvironmentInstance) is { } pathInEnv)
        {
            // Try for each possible prefix of pathToEnv to find a parameter

            if (FindNearestParameterForPathInEnv(pathInEnv) is { } paramAndRemainder)
            {
                var (paramRef, remainingPath) = paramAndRemainder;

                if (remainingPath.Count is 0)
                {
                    return [CompiledCSharpExpression.Generic(paramRef)];
                }

                // Then cover remainder of path with PineValueExtension.ValueFromPathOrEmptyList

                var fromPathGeneric =
                    PineCSharpSyntaxFactory.BuildCSharpExpressionToGetItemFromPathOrEmptyList(
                        paramRef,
                        remainingPath,
                        declarationSyntaxContext);

                pathsFromParametersAndLocals.Add((paramRef, remainingPath));
            }
        }

        {
            (IReadOnlyList<int> pathInSubexpr, StaticExpression<DeclQualifiedName> subexpr)? lastSubExpr = null;

            foreach (var current in StaticExpressionExtension.InterpretAsPathReversed(expression))
            {
                lastSubExpr = current;

                if (alreadyDeclared.TryGetValue(current.subexpr, out var varName))
                {
                    pathsFromParametersAndLocals.Add((SyntaxFactory.IdentifierName(varName), current.pathInSubexpr));
                }
            }

            if (lastSubExpr.HasValue)
            {
                var referencesEnvironment =
                    StaticExpressionExtension.TryParseAsPathToExpression(
                        lastSubExpr.Value.subexpr,
                        StaticExpression<DeclQualifiedName>.EnvironmentInstance) is { };

                if (!referencesEnvironment)
                {
                    var subExprCompiled =
                        CompileToCSharpExpression(
                            lastSubExpr.Value.subexpr,
                            selfFunctionInterface,
                            availableFunctions,
                            availableValueDecls,
                            declarationSyntaxContext,
                            alreadyDeclared);

                    pathsFromParametersAndLocals.Add(
                        (subExprCompiled.AsGenericValue(declarationSyntaxContext), lastSubExpr.Value.pathInSubexpr));
                }
            }
        }

        if (pathsFromParametersAndLocals.Count is not 0)
        {
            var shortest =
                pathsFromParametersAndLocals
                .OrderBy(p => p.remainingPath.Count)
                .First();

            var fromPathGeneric =
                PineCSharpSyntaxFactory.BuildCSharpExpressionToGetItemFromPathOrEmptyList(
                    shortest.paramRef,
                    shortest.remainingPath,
                    declarationSyntaxContext);

            return [CompiledCSharpExpression.Generic(fromPathGeneric)];
        }

        if (expression is StaticExpression<DeclQualifiedName>.Literal literal)
        {
            return
                EnumerateExpressionsForLiteral(
                    literal,
                    availableValueDecls,
                    declarationSyntaxContext);
        }

        if (expression is StaticExpression<DeclQualifiedName>.List list)
        {
            var itemExprs =
                list.Items
                .Select(item =>
                CompileToCSharpExpression(
                    item,
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    alreadyDeclared))
                .ToImmutableArray();

            var collectionExprs =
                SyntaxFactory.CollectionExpression(
                    SyntaxFactory.SeparatedList<CollectionElementSyntax>(
                        itemExprs
                        .Select(itemExpr => SyntaxFactory.ExpressionElement(itemExpr.AsGenericValue(declarationSyntaxContext)))));

            // Invoke PineValue.List( ... )

            var genericCSharpExpr =
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        CompileTypeSyntax.TypeSyntaxFromType(
                            typeof(PineValue),
                            declarationSyntaxContext),
                        SyntaxFactory.IdentifierName(nameof(PineValue.List))))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Argument(collectionExprs))));

            return [CompiledCSharpExpression.Generic(genericCSharpExpr)];
        }

        if (expression is StaticExpression<DeclQualifiedName>.Conditional conditional)
        {
            return
                EnumerateExpressionsForConditional(
                    conditional,
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    alreadyDeclared);
        }

        if (expression is StaticExpression<DeclQualifiedName>.KernelApplication kernelApp)
        {
            return
                EnumerateExpressionsForKernelApp(
                    kernelApp,
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    alreadyDeclared);
        }

        if (expression is StaticExpression<DeclQualifiedName>.FunctionApplication funcApp)
        {
            if (!availableFunctions.TryGetValue(funcApp.FunctionName, out var funcInterface))
            {
                throw new System.Exception(
                    "Function application references unknown function '" + funcApp.FunctionName + "'.");
            }

            var arguments =
                funcInterface.ParamsPaths
                .Select(argumentPath =>
                ExpressionsForFunctionArgument(
                    argumentPath,
                    funcApp.Arguments,
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    alreadyDeclared))
                .ToImmutableArray();

            var genericCSharpExpr =
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.ParseName(funcApp.FunctionName.FullName))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList(
                            [
                            .. arguments.Select(argExpr => SyntaxFactory.Argument(argExpr.AsGenericValue(declarationSyntaxContext)))
                            ])));

            return [CompiledCSharpExpression.Generic(genericCSharpExpr)];
        }

        if (expression is StaticExpression<DeclQualifiedName>.CrashingParseAndEval parseAndEval)
        {
            var renderedEncodedExpr =
                CompileToCSharpExpression(
                    parseAndEval.Encoded,
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    alreadyDeclared);

            var genericCSharpExpr =
                PineCSharpSyntaxFactory.ThrowParseExpressionException(
                    SyntaxFactory.LiteralExpression(
                        SyntaxKind.StringLiteralExpression,
                        SyntaxFactory.Literal("TODO: Include details from encoded and env subexpressions")),
                    declarationSyntaxContext);

            return [CompiledCSharpExpression.Generic(genericCSharpExpr)];
        }

        if (expression is StaticExpression<DeclQualifiedName>.Environment)
        {
            throw new System.NotImplementedException(
                "Encountered Environment which was not resolved to a parameter.");
        }

        throw new System.NotImplementedException(
            "C# code generation for expression type " + expression.GetType() + " is not implemented.");
    }


    public static IEnumerable<CompiledCSharpExpression> EnumerateExpressionsForLiteral(
        StaticExpression<DeclQualifiedName>.Literal literal,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        ExpressionSyntax? OverrideValueLiteralExpression(PineValue pineValue)
        {
            if (availableValueDecls.TryGetValue(pineValue, out var declName))
            {
                return SyntaxFactory.ParseName(declName.FullName);
            }

            return null;
        }

        if (literal.Value == PineKernelValues.TrueValue)
        {
            yield return
                CompiledCSharpExpression.Boolean(
                    PineCSharpSyntaxFactory.ExpressionSyntaxForBooleanLiteral(true));
        }

        if (literal.Value == PineKernelValues.FalseValue)
        {
            yield return
                CompiledCSharpExpression.Boolean(
                    PineCSharpSyntaxFactory.ExpressionSyntaxForBooleanLiteral(false));
        }

        if (IntegerEncoding.ParseSignedIntegerStrict(literal.Value).IsOkOrNullable() is { } integer &&
            long.MinValue < integer && integer < long.MaxValue &&
            IntegerEncoding.EncodeSignedInteger(integer) == literal.Value)
        {
            yield return
                CompiledCSharpExpression.Integer(
                    PineCSharpSyntaxFactory.ExpressionSyntaxForIntegerLiteral((long)integer));
        }

        if (OverrideValueLiteralExpression(literal.Value) is { } overriddenExpr)
        {
            yield return CompiledCSharpExpression.Generic(overriddenExpr);

            yield break;
        }

        var toLiteral =
            PineCSharpSyntaxFactory.CompileToCSharpLiteralExpression(
                literal.Value,
                overrideDefaultExpression: OverrideValueLiteralExpression,
                declarationSyntaxContext);

        yield return CompiledCSharpExpression.Generic(toLiteral.exprSyntax);
    }


    public static IEnumerable<CompiledCSharpExpression> EnumerateExpressionsForConditional(
        StaticExpression<DeclQualifiedName>.Conditional conditional,
        System.Func<IReadOnlyList<int>, ExpressionSyntax?> selfFunctionInterface,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls,
        DeclarationSyntaxContext declarationSyntaxContext,
        ImmutableDictionary<StaticExpression<DeclQualifiedName>, string> alreadyDeclared)
    {
        var conditionExpr =
            BuildConditionExpression(
                conditional.Condition,
                selfFunctionInterface,
                availableFunctions,
                availableValueDecls,
                declarationSyntaxContext,
                alreadyDeclared);

        var trueBranchExprs =
            EnumerateExpressions(
                conditional.TrueBranch,
                selfFunctionInterface,
                availableFunctions,
                availableValueDecls,
                declarationSyntaxContext,
                alreadyDeclared)
            .ToImmutableArray();

        var falseBranchExprs =
            EnumerateExpressions(
                conditional.FalseBranch,
                selfFunctionInterface,
                availableFunctions,
                availableValueDecls,
                declarationSyntaxContext,
                alreadyDeclared)
            .ToImmutableArray();

        // Try to find matching types in branches

        var trueBranchExprAsBoolean =
            trueBranchExprs
            .FirstOrDefault(e => e.Type == CompiledCSharpExpression.ValueType.Boolean);

        var falseBranchExprAsBoolean =
            falseBranchExprs
            .FirstOrDefault(e => e.Type == CompiledCSharpExpression.ValueType.Boolean);

        if (trueBranchExprAsBoolean is not null &&
            falseBranchExprAsBoolean is not null)
        {
            // Both branches can be boolean, so produce a boolean result.

            var booleanCSharpExpr =
                SyntaxFactory.ConditionalExpression(
                    condition: conditionExpr,
                    whenTrue:
                    CompiledCSharpExpression.EnsureIsParenthesizedForComposition(
                        trueBranchExprAsBoolean.ExpressionSyntax),
                    whenFalse:
                    CompiledCSharpExpression.EnsureIsParenthesizedForComposition(
                        falseBranchExprAsBoolean.ExpressionSyntax));

            yield return CompiledCSharpExpression.Boolean(booleanCSharpExpr);
        }

        var trueBranchExprAsInteger =
            trueBranchExprs
            .FirstOrDefault(e => e.Type == CompiledCSharpExpression.ValueType.Integer);

        var falseBranchExprAsInteger =
            falseBranchExprs
            .FirstOrDefault(e => e.Type == CompiledCSharpExpression.ValueType.Integer);

        if (trueBranchExprAsInteger is not null &&
            falseBranchExprAsInteger is not null)
        {
            // Both branches are integer, so produce an integer result.

            var integerCSharpExpr =
                SyntaxFactory.ConditionalExpression(
                    condition: conditionExpr,
                    whenTrue:
                    CompiledCSharpExpression.EnsureIsParenthesizedForComposition(
                        trueBranchExprAsInteger.ExpressionSyntax),
                    whenFalse:
                    CompiledCSharpExpression.EnsureIsParenthesizedForComposition(
                        falseBranchExprAsInteger.ExpressionSyntax));

            yield return CompiledCSharpExpression.Integer(integerCSharpExpr);
        }

        // Fallback: Produce generic result.

        var trueBranchExpr =
            trueBranchExprs
            .FirstOrDefault(e => e.Type == CompiledCSharpExpression.ValueType.Generic)
            ?? trueBranchExprs.First();

        var falseBranchExpr =
            falseBranchExprs
            .FirstOrDefault(e => e.Type == CompiledCSharpExpression.ValueType.Generic)
            ?? falseBranchExprs.First();

        var genericCSharpExpr =
            SyntaxFactory.ConditionalExpression(
                condition: conditionExpr,
                whenTrue:
                CompiledCSharpExpression.EnsureIsParenthesizedForComposition(
                    trueBranchExpr.AsGenericValue(declarationSyntaxContext)),
                whenFalse:
                CompiledCSharpExpression.EnsureIsParenthesizedForComposition(
                    falseBranchExpr.AsGenericValue(declarationSyntaxContext)));

        yield return CompiledCSharpExpression.Generic(genericCSharpExpr);
    }

    public static ExpressionSyntax BuildConditionExpression(
        StaticExpression<DeclQualifiedName> condition,
        System.Func<IReadOnlyList<int>, ExpressionSyntax?> selfFunctionInterface,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls,
        DeclarationSyntaxContext declarationSyntaxContext,
        ImmutableDictionary<StaticExpression<DeclQualifiedName>, string> alreadyDeclared)
    {
        var conditionAsAndChain = ParseAsAndChain(condition);

        var andChainConjunctsCompiled =
            conditionAsAndChain
            .Select(expr =>
                CompileToCSharpExpression(
                    expr,
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    alreadyDeclared))
            .ToImmutableArray();

        if (andChainConjunctsCompiled.Length is 0)
        {
            return PineCSharpSyntaxFactory.ExpressionSyntaxForBooleanLiteral(true);
        }

        if (andChainConjunctsCompiled.Length is 1)
        {
            return andChainConjunctsCompiled[0].AsBooleanValue(declarationSyntaxContext);
        }

        // Combine conjuncts with '&&'

        return
            andChainConjunctsCompiled
            .Select(e => e.AsBooleanValue(declarationSyntaxContext))
            .Aggregate((left, right) =>
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.LogicalAndExpression,
                    left: CompiledCSharpExpression.EnsureIsParenthesizedForComposition(left),
                    right: CompiledCSharpExpression.EnsureIsParenthesizedForComposition(right)));
    }

    public static IReadOnlyList<StaticExpression<DeclQualifiedName>> ParseAsAndChain(
        StaticExpression<DeclQualifiedName> expression)
    {
        if (expression is StaticExpression<DeclQualifiedName>.Conditional conditionalExpr)
        {
            if (conditionalExpr.FalseBranch is StaticExpression<DeclQualifiedName>.Literal falseLiteral &&
                falseLiteral.Value == PineKernelValues.FalseValue)
            {
                var rightChain = ParseAsAndChain(conditionalExpr.TrueBranch);

                return [conditionalExpr.Condition, .. rightChain];
            }
        }

        return [expression];
    }

    public static IEnumerable<CompiledCSharpExpression> EnumerateExpressionsForKernelApp(
        StaticExpression<DeclQualifiedName>.KernelApplication kernelApp,
        System.Func<IReadOnlyList<int>, ExpressionSyntax?> selfFunctionInterface,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls,
        DeclarationSyntaxContext declarationSyntaxContext,
        ImmutableDictionary<StaticExpression<DeclQualifiedName>, string> alreadyDeclared)
    {
        if (kernelApp.Function is nameof(KernelFunction.equal))
        {
            // Special case: Use '==' operator for equality.

            if (kernelApp.Input is StaticExpression<DeclQualifiedName>.List listInput)
            {
                if (listInput.Items.Count is 2)
                {
                    var leftExprs =
                        EnumerateExpressions(
                            listInput.Items[0],
                            selfFunctionInterface,
                            availableFunctions,
                            availableValueDecls,
                            declarationSyntaxContext,
                            alreadyDeclared)
                        .ToImmutableArray();

                    var rightExprs =
                        EnumerateExpressions(
                            listInput.Items[1],
                            selfFunctionInterface,
                            availableFunctions,
                            availableValueDecls,
                            declarationSyntaxContext,
                            alreadyDeclared)
                        .ToImmutableArray();

                    var leftAsBoolean =
                        leftExprs
                        .FirstOrDefault(e => e.Type == CompiledCSharpExpression.ValueType.Boolean);

                    var rightAsBoolean =
                        rightExprs
                        .FirstOrDefault(e => e.Type == CompiledCSharpExpression.ValueType.Boolean);

                    if (leftAsBoolean is not null &&
                        rightAsBoolean is not null)
                    {
                        var booleanCSharpExpr =
                            SyntaxFactory.BinaryExpression(
                                SyntaxKind.EqualsExpression,
                                CompiledCSharpExpression.EnsureIsParenthesizedForComposition(
                                    leftAsBoolean.ExpressionSyntax),
                                CompiledCSharpExpression.EnsureIsParenthesizedForComposition(
                                    rightAsBoolean.ExpressionSyntax));

                        yield return CompiledCSharpExpression.Boolean(booleanCSharpExpr);
                    }

                    var leftAsInteger =
                        leftExprs
                        .FirstOrDefault(e => e.Type == CompiledCSharpExpression.ValueType.Integer);

                    var rightAsInteger =
                        rightExprs
                        .FirstOrDefault(e => e.Type == CompiledCSharpExpression.ValueType.Integer);

                    if (leftAsInteger is not null &&
                        rightAsInteger is not null)
                    {
                        var booleanCSharpExpr =
                            SyntaxFactory.BinaryExpression(
                                SyntaxKind.EqualsExpression,
                                CompiledCSharpExpression.EnsureIsParenthesizedForComposition(
                                    leftAsInteger.ExpressionSyntax),
                                CompiledCSharpExpression.EnsureIsParenthesizedForComposition(
                                    rightAsInteger.ExpressionSyntax));

                        yield return CompiledCSharpExpression.Boolean(booleanCSharpExpr);
                    }

                    // Fallback: Compare as generic values.
                    var leftExpr =
                        leftExprs
                        .FirstOrDefault(e => e.Type == CompiledCSharpExpression.ValueType.Generic)
                        ?? leftExprs.First();

                    var rightExpr =
                        rightExprs
                        .FirstOrDefault(e => e.Type == CompiledCSharpExpression.ValueType.Generic)
                        ?? rightExprs.First();

                    {
                        var booleanCSharpExpr =
                            SyntaxFactory.BinaryExpression(
                                SyntaxKind.EqualsExpression,
                                CompiledCSharpExpression.EnsureIsParenthesizedForComposition(
                                    leftExpr.AsGenericValue(declarationSyntaxContext)),
                                CompiledCSharpExpression.EnsureIsParenthesizedForComposition(
                                    rightExpr.AsGenericValue(declarationSyntaxContext)));

                        yield return CompiledCSharpExpression.Boolean(booleanCSharpExpr);
                    }
                }
            }
        }

        var resultsFromFusion =
            TryCompileKernelFusion(
                kernelApp,
                selfFunctionInterface,
                availableFunctions,
                availableValueDecls,
                declarationSyntaxContext,
                alreadyDeclared);

        foreach (var fromFusion in resultsFromFusion)
        {
            yield return fromFusion;
        }

        ExpressionSyntax? TryRenderArgument(
            StaticExpression<DeclQualifiedName> argumentExpr,
            PineKernelFunctions.KernelFunctionParameterType paramType)
        {
            switch (paramType)
            {
                case PineKernelFunctions.KernelFunctionParameterType.Generic:
                    return
                        CompileToCSharpExpression(
                            argumentExpr,
                            selfFunctionInterface,
                            availableFunctions,
                            availableValueDecls,
                            declarationSyntaxContext,
                            alreadyDeclared)
                        .AsGenericValue(declarationSyntaxContext);

                case PineKernelFunctions.KernelFunctionParameterType.Integer:
                    {
                        if (argumentExpr is StaticExpression<DeclQualifiedName>.Literal literal &&
                            KernelFunction.SignedIntegerFromValueRelaxed(literal.Value) is { } integer &&
                            long.MinValue < integer && integer < long.MaxValue)
                        {
                            return
                                PineCSharpSyntaxFactory.ExpressionSyntaxForIntegerLiteral((long)integer);
                        }

                        return null;
                    }

                default:
                    return null;
            }
        }

        if (kernelApp.Function is nameof(KernelFunction.length))
        {
            if (PineKernelFunctions.SpecializedInterfacesFromKernelFunctionName(kernelApp.Function) is { } specializedInterfaces)
            {
                var matches =
                    EnumerateMatchingSpecializedInterface(
                    specializedInterfaces,
                    [kernelApp.Input],
                    isCommutative: false,
                    TryRenderArgument,
                    declarationSyntaxContext);

                foreach (var match in matches)
                {
                    yield return match;
                }
            }
        }
        else if (kernelApp.Input is StaticExpression<DeclQualifiedName>.List argumentsList)
        {
            if (PineKernelFunctions.SpecializedInterfacesFromKernelFunctionName(kernelApp.Function) is { } specializedInterfaces)
            {
                var isCommutative =
                    kernelApp.Function
                    switch
                    {
                        nameof(KernelFunction.int_add) => true,
                        nameof(KernelFunction.int_mul) => true,

                        nameof(KernelFunction.bit_and) => true,
                        nameof(KernelFunction.bit_or) => true,
                        nameof(KernelFunction.bit_xor) => true,

                        _ => false,
                    };

                var matches =
                    EnumerateMatchingSpecializedInterface(
                    specializedInterfaces,
                    argumentsList.Items,
                    isCommutative,
                    TryRenderArgument,
                    declarationSyntaxContext);

                foreach (var match in matches)
                {
                    yield return match;
                }
            }
        }

        var inputExpr =
            CompileToCSharpExpression(
                kernelApp.Input,
                selfFunctionInterface,
                availableFunctions,
                availableValueDecls,
                declarationSyntaxContext,
                alreadyDeclared);

        if (PineKernelFunctions.CompileKernelFunctionGenericInvocation(
            kernelApp.Function,
            inputExpr.AsGenericValue(declarationSyntaxContext),
            declarationSyntaxContext)
            is { } specializedInvocation)
        {
            yield return CompiledCSharpExpression.Generic(specializedInvocation);
        }

        // Generic case: Invoke KernelFunction.ApplyKernelFunctionGeneric

        var genericCSharpExpr =
            SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    CompileTypeSyntax.TypeSyntaxFromType(
                        typeof(KernelFunction),
                        declarationSyntaxContext),
                    SyntaxFactory.IdentifierName(nameof(KernelFunction.ApplyKernelFunctionGeneric))))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList(
                        [
                        SyntaxFactory.Argument(
                            SyntaxFactory.LiteralExpression(
                                SyntaxKind.StringLiteralExpression,
                                SyntaxFactory.Literal(kernelApp.Function))),
                        SyntaxFactory.Argument(inputExpr.AsGenericValue(declarationSyntaxContext))
                        ])));

        yield return CompiledCSharpExpression.Generic(genericCSharpExpr);
    }

    private static IEnumerable<CompiledCSharpExpression> TryCompileKernelFusion(
        StaticExpression<DeclQualifiedName>.KernelApplication kernelApp,
        System.Func<IReadOnlyList<int>, ExpressionSyntax?> selfFunctionInterface,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls,
        DeclarationSyntaxContext declarationSyntaxContext,
        ImmutableDictionary<StaticExpression<DeclQualifiedName>, string> alreadyDeclared)
    {

        TypeSyntax TypeSyntaxFromType(System.Type type)
        {
            return CompileTypeSyntax.TypeSyntaxFromType(type, declarationSyntaxContext);
        }

        // Variant: Fuse take(skip(seq)) where take count is a compile-time integer
        if (kernelApp.Function is nameof(KernelFunction.take) &&
            kernelApp.Input is StaticExpression<DeclQualifiedName>.List takeArgsList &&
            takeArgsList.Items.Count is 2 &&
            takeArgsList.Items[1] is StaticExpression<DeclQualifiedName>.KernelApplication skipApp &&
            skipApp.Function is nameof(KernelFunction.skip) &&
            skipApp.Input is StaticExpression<DeclQualifiedName>.List skipArgsList &&
            skipArgsList.Items.Count is 2)
        {
            if (takeArgsList.Items[0] is StaticExpression<DeclQualifiedName>.Literal takeCountLiteral &&
                KernelFunction.SignedIntegerFromValueRelaxed(takeCountLiteral.Value) is { } takeCountBI &&
                takeCountBI >= int.MinValue && takeCountBI <= int.MaxValue)
            {
                var argumentExpr =
                    CompileToCSharpExpression(
                        skipArgsList.Items[1],
                        selfFunctionInterface,
                        availableFunctions,
                        availableValueDecls,
                        declarationSyntaxContext,
                        alreadyDeclared);

                var skipCountExpr =
                    CompileToCSharpExpression(
                        skipArgsList.Items[0],
                        selfFunctionInterface,
                        availableFunctions,
                        availableValueDecls,
                        declarationSyntaxContext,
                        alreadyDeclared);

                // Build fully-qualified invocation: Pine.Core.Internal.KernelFunctionFused.SkipAndTake(argument: ..., skipCountValue: ..., takeCount: ...)
                var genericCSharpExpr =
                    SyntaxFactory.InvocationExpression(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            TypeSyntaxFromType(typeof(KernelFunctionFused)),
                            SyntaxFactory.IdentifierName(nameof(KernelFunctionFused.SkipAndTake))))
                    .WithArgumentList(
                        SyntaxFactory.ArgumentList(
                            SyntaxFactory.SeparatedList<ArgumentSyntax>(
                                new SyntaxNodeOrToken[]
                                {
                                    SyntaxFactory.Argument(PineCSharpSyntaxFactory.ExpressionSyntaxForIntegerLiteral((int)takeCountBI))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("takeCount"))),

                                    SyntaxFactory.Token(SyntaxKind.CommaToken),

                                    SyntaxFactory.Argument(skipCountExpr.AsGenericValue(declarationSyntaxContext))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("skipCountValue"))),

                                    SyntaxFactory.Token(SyntaxKind.CommaToken),

                                    SyntaxFactory.Argument(argumentExpr.AsGenericValue(declarationSyntaxContext))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("argument"))),
                                })));

                yield return CompiledCSharpExpression.Generic(genericCSharpExpr);
            }
        }

        // Variant: Fuse skip(take(seq)) where both counts are generic PineValue
        /*
         * public static PineValue TakeAndSkip(
                PineValue skipCountValue,
                PineValue takeCountValue,
                PineValue argument)
         * */

        if (kernelApp.Function is nameof(KernelFunction.skip) &&
            kernelApp.Input is StaticExpression<DeclQualifiedName>.List skipArgsList2 &&
            skipArgsList2.Items.Count is 2 &&
            skipArgsList2.Items[1] is StaticExpression<DeclQualifiedName>.KernelApplication takeApp2 &&
            takeApp2.Function is nameof(KernelFunction.take) &&
            takeApp2.Input is StaticExpression<DeclQualifiedName>.List takeArgsList2 &&
            takeArgsList2.Items.Count is 2)
        {
            var argumentExpr =
                CompileToCSharpExpression(
                    takeArgsList2.Items[1],
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    alreadyDeclared);

            var takeCountExpr =
                CompileToCSharpExpression(
                    takeArgsList2.Items[0],
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    alreadyDeclared);

            var skipCountExpr =
                CompileToCSharpExpression(
                    skipArgsList2.Items[0],
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    alreadyDeclared);

            // Build fully-qualified invocation: Pine.Core.Internal.KernelFunctionFused.TakeAndSkip(skipCountValue: ..., takeCountValue: ..., argument: ...)
            var genericCSharpExpr =
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        TypeSyntaxFromType(typeof(KernelFunctionFused)),
                        SyntaxFactory.IdentifierName(nameof(KernelFunctionFused.TakeAndSkip))))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList<ArgumentSyntax>(
                            new SyntaxNodeOrToken[]
                            {
                                    SyntaxFactory.Argument(skipCountExpr.AsGenericValue(declarationSyntaxContext))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("skipCountValue"))),
                                    SyntaxFactory.Token(SyntaxKind.CommaToken),
                                    SyntaxFactory.Argument(takeCountExpr.AsGenericValue(declarationSyntaxContext))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("takeCountValue"))),
                                    SyntaxFactory.Token(SyntaxKind.CommaToken),
                                    SyntaxFactory.Argument(argumentExpr.AsGenericValue(declarationSyntaxContext))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("argument"))),
                            })));

            yield return CompiledCSharpExpression.Generic(genericCSharpExpr);
        }

        // Variant: Fuse reverse(take(n, reverse(seq))) => KernelFunctionFused.TakeLast(n, seq)
        if (kernelApp.Function is nameof(KernelFunction.reverse))
        {
            var outerReverseInput = kernelApp.Input;

            if (outerReverseInput is StaticExpression<DeclQualifiedName>.KernelApplication takeApp &&
                takeApp.Function is nameof(KernelFunction.take) &&
                takeApp.Input is StaticExpression<DeclQualifiedName>.List takeArgs &&
                takeArgs.Items.Count is 2 &&
                takeArgs.Items[0] is StaticExpression<DeclQualifiedName>.Literal takeLastCountLiteral &&
                KernelFunction.SignedIntegerFromValueRelaxed(takeLastCountLiteral.Value) is { } takeLastCountBI &&
                takeLastCountBI >= int.MinValue && takeLastCountBI <= int.MaxValue &&
                takeArgs.Items[1] is StaticExpression<DeclQualifiedName>.KernelApplication innerReverseApp &&
                innerReverseApp.Function is nameof(KernelFunction.reverse))
            {
                // seq expression is the input to the inner reverse
                var seqExpr =
                    CompileToCSharpExpression(
                        innerReverseApp.Input,
                        selfFunctionInterface,
                        availableFunctions,
                        availableValueDecls,
                        declarationSyntaxContext,
                        alreadyDeclared);

                // Build fully-qualified invocation: Pine.Core.Internal.KernelFunctionFused.TakeLast(takeCount: n, value: seq)
                var genericCSharpExpr =
                    SyntaxFactory.InvocationExpression(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            TypeSyntaxFromType(typeof(KernelFunctionFused)),
                            SyntaxFactory.IdentifierName(nameof(KernelFunctionFused.TakeLast))))
                    .WithArgumentList(
                        SyntaxFactory.ArgumentList(
                            SyntaxFactory.SeparatedList<ArgumentSyntax>(
                                new SyntaxNodeOrToken[]
                                {
                                    SyntaxFactory.Argument(PineCSharpSyntaxFactory.ExpressionSyntaxForIntegerLiteral((int)takeLastCountBI))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("takeCount"))),

                                    SyntaxFactory.Token(SyntaxKind.CommaToken),

                                    SyntaxFactory.Argument(seqExpr.AsGenericValue(declarationSyntaxContext))
                                        .WithNameColon(SyntaxFactory.NameColon(SyntaxFactory.IdentifierName("value")))
                                })));

                yield return CompiledCSharpExpression.Generic(genericCSharpExpr);
            }
        }
    }

    public static IEnumerable<CompiledCSharpExpression> ExpressionsForFunctionArgument(
        IReadOnlyList<int> paramPath,
        StaticExpression<DeclQualifiedName> argumentExpr,
        System.Func<IReadOnlyList<int>, ExpressionSyntax?> selfFunctionInterface,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls,
        DeclarationSyntaxContext declarationSyntaxContext,
        ImmutableDictionary<StaticExpression<DeclQualifiedName>, string> alreadyDeclared)
    {
        if (StaticExpressionExtension.GetSubexpressionAtPath(argumentExpr, paramPath) is { } subexpr)
        {
            var renderedExpr =
                EnumerateExpressions(
                    subexpr.subexpr,
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    alreadyDeclared: alreadyDeclared);

            if (subexpr.pathRemaining.Count is 0)
            {
                return renderedExpr;
            }

            var withRemainingPathExpr =
                PineCSharpSyntaxFactory.BuildCSharpExpressionToGetItemFromPathOrEmptyList(
                    renderedExpr.AsGenericValue(declarationSyntaxContext),
                    subexpr.pathRemaining,
                    declarationSyntaxContext);

            return
                [CompiledCSharpExpression.Generic(withRemainingPathExpr)];
        }

        throw new System.NotImplementedException(
            "Failed to find subexpression at path [" + string.Join(',', paramPath) + "] in argument expression.");
    }

    private static IEnumerable<CompiledCSharpExpression> EnumerateMatchingSpecializedInterface(
        IReadOnlyList<PineKernelFunctions.KernelFunctionSpecializedInfo> specializedInterfaces,
        IReadOnlyList<StaticExpression<DeclQualifiedName>> argumentsList,
        bool isCommutative,
        System.Func<StaticExpression<DeclQualifiedName>, PineKernelFunctions.KernelFunctionParameterType, ExpressionSyntax?> tryRenderArgument,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        IEnumerable<IReadOnlyList<StaticExpression<DeclQualifiedName>>> EnumerateArgumentsPermutations()
        {
            yield return argumentsList;

            if (isCommutative && argumentsList.Count > 1)
            {
                foreach (var permutation in GetPermutations(argumentsList))
                {
                    // Skip the original order, we already tried it
                    if (permutation.SequenceEqual(argumentsList))
                        continue;

                    yield return permutation;
                }
            }
        }

        foreach (var specializedInterface in specializedInterfaces)
        {
            foreach (var argumentOrder in EnumerateArgumentsPermutations())
            {
                if (TryMatchSpecializedInterfaceWithArgumentsOrder(
                    specializedInterface,
                    argumentOrder,
                    tryRenderArgument,
                    declarationSyntaxContext) is { } matchedExpr)
                {
                    yield return matchedExpr;
                }
            }
        }
    }

    private static CompiledCSharpExpression? TryMatchSpecializedInterfaceWithArgumentsOrder(
        PineKernelFunctions.KernelFunctionSpecializedInfo specializedInterface,
        IReadOnlyList<StaticExpression<DeclQualifiedName>> arguments,
        System.Func<StaticExpression<DeclQualifiedName>, PineKernelFunctions.KernelFunctionParameterType, ExpressionSyntax?> tryRenderArgument,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        if (specializedInterface.ParameterTypes.Count != arguments.Count)
            return null;

        var argumentExprs =
            new List<ExpressionSyntax>(capacity: specializedInterface.ParameterTypes.Count);

        for (var paramIndex = 0; paramIndex < specializedInterface.ParameterTypes.Count; paramIndex++)
        {
            var paramType = specializedInterface.ParameterTypes[paramIndex];

            // Can we render the argument to the required parameter type?

            var argumentExpr = tryRenderArgument(arguments[paramIndex], paramType);

            if (argumentExpr is null)
            {
                return null;
            }

            argumentExprs.Add(argumentExpr);
        }

        var csharpExpr =
            specializedInterface.CompileInvocation(
                argumentExprs,
                declarationSyntaxContext);

        return
            specializedInterface.ReturnType switch
            {
                PineKernelFunctions.KernelFunctionSpecializedReturnType.Boolean =>
                CompiledCSharpExpression.Boolean(csharpExpr),

                PineKernelFunctions.KernelFunctionSpecializedReturnType.Generic =>
                CompiledCSharpExpression.Generic(csharpExpr),

                PineKernelFunctions.KernelFunctionSpecializedReturnType.Integer =>
                CompiledCSharpExpression.Integer(csharpExpr),

                _ =>
                throw new System.NotImplementedException(
                    "Unknown specialized return type " + specializedInterface.ReturnType)
            };
    }

    private static IEnumerable<IReadOnlyList<T>> GetPermutations<T>(IReadOnlyList<T> items)
    {
        if (items.Count <= 1)
        {
            yield return items;
            yield break;
        }

        for (var i = 0; i < items.Count; i++)
        {
            var item = items[i];
            var remainingItems = items.Where((_, index) => index != i).ToList();

            foreach (var permutation in GetPermutations(remainingItems))
            {
                yield return new List<T> { item }.Concat(permutation).ToList();
            }
        }
    }

    public static IEnumerable<StaticExpression<FuncId>> CollectSubexpressionsToSeparate<FuncId>(
        StaticExpression<FuncId> expression,
        System.Func<StaticExpression<FuncId>, bool> ignoreExpr)
    {
        IReadOnlyList<StaticExpression<FuncId>> currentRoots = [expression];

        while (true)
        {
            var collectedThisRound =
                CollectSubexpressionsToSeparateStep(currentRoots, ignoreExpr)
                .ToImmutableArray();

            if (collectedThisRound.Length is 0)
            {
                return currentRoots.Except([expression]);
            }

            currentRoots = [.. currentRoots, .. collectedThisRound];
        }
    }

    public static IEnumerable<StaticExpression<FuncId>> CollectSubexpressionsToSeparateStep<FuncId>(
        IReadOnlyList<StaticExpression<FuncId>> rootExpressions,
        System.Func<StaticExpression<FuncId>, bool> ignoreExpr)
    {
        /*
         * Primary reason to separate a subexpression into a declaration is CSE (prevent repeated evaluation).
         * 
         * For any subexpression that occurs at least once unconditional and a second time, emit to CSE.
         * */

        var seenOnceUnconditional = new HashSet<StaticExpression<FuncId>>();

        var seenOnceConditional = new HashSet<StaticExpression<FuncId>>();

        var collected = new HashSet<StaticExpression<FuncId>>();

        var queue = new Queue<(StaticExpression<FuncId> expr, bool conditional)>(capacity: rootExpressions.Count);

        foreach (var rootExpr in rootExpressions)
        {
            queue.Enqueue((rootExpr, false));
        }

        void EnqueueIfNoRoot(StaticExpression<FuncId> expr, bool conditional)
        {
            if (rootExpressions.Contains(expr))
            {
                return;
            }

            queue.Enqueue((expr, conditional));
        }

        while (queue.Count > 0)
        {
            var current = queue.Dequeue();

            if (ignoreExpr(current.expr))
            {
                continue;
            }

            if (collected.Contains(current.expr))
            {
                continue;
            }

            if (seenOnceUnconditional.Contains(current.expr) ||
                (seenOnceConditional.Contains(current.expr) && !current.conditional))
            {
                yield return current.expr;

                collected.Add(current.expr);

                continue;
            }

            if (current.conditional)
            {
                seenOnceConditional.Add(current.expr);
            }
            else
            {
                seenOnceUnconditional.Add(current.expr);
            }

            if (current.expr is StaticExpression<FuncId>.Conditional conditional)
            {
                EnqueueIfNoRoot(conditional.Condition, current.conditional);

                EnqueueIfNoRoot(conditional.TrueBranch, true);
                EnqueueIfNoRoot(conditional.FalseBranch, true);

                continue;
            }

            if (current.expr is StaticExpression<FuncId>.Literal)
            {
                continue;
            }

            if (current.expr is StaticExpression<FuncId>.Environment)
            {
                continue;
            }

            if (current.expr is StaticExpression<FuncId>.List list)
            {
                foreach (var item in list.Items)
                {
                    EnqueueIfNoRoot(item, current.conditional);
                }

                continue;
            }

            if (current.expr is StaticExpression<FuncId>.KernelApplication kernelApp)
            {
                EnqueueIfNoRoot(kernelApp.Input, current.conditional);

                continue;
            }

            if (current.expr is StaticExpression<FuncId>.FunctionApplication funcApp)
            {
                EnqueueIfNoRoot(funcApp.Arguments, current.conditional);

                continue;
            }

            if (current.expr is StaticExpression<FuncId>.CrashingParseAndEval parseAndEval)
            {
                EnqueueIfNoRoot(parseAndEval.Encoded, current.conditional);
                EnqueueIfNoRoot(parseAndEval.EnvironmentExpr, current.conditional);

                continue;
            }

            throw new System.NotImplementedException(
                "CSE collection for expression type " + current.expr.GetType() + " is not implemented.");
        }
    }

    public static IReadOnlyList<ParameterSyntax> ComposeParameterList(
        StaticFunctionInterface functionInterface,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        var envItemsParameters =
            functionInterface.ParamsPaths
            .Select(paramPath =>
            SyntaxFactory.Parameter(SyntaxFactory.Identifier(RenderParamRef(paramPath)))
            .WithType(
                CompileTypeSyntax.TypeSyntaxFromType(
                    typeof(PineValue),
                    declarationSyntaxContext)));

        return [.. envItemsParameters];
    }

    private static StatementSyntax ResultThrowOrReturn(ExpressionSyntax expr) =>
        expr is ThrowExpressionSyntax throwExpr
        ?
        SyntaxFactory.ThrowStatement(throwExpr.Expression)
        :
        SyntaxFactory.ReturnStatement(expr);

    private static string RenderParamRef(IReadOnlyList<int> path)
    {
        return "param" + string.Concat(path.Select(i => "_" + i));
    }

    private static bool ContainsFunctionApplicationAsTailCall(
        StaticExpression<DeclQualifiedName> expression,
        DeclQualifiedName functionName)
    {
        if (expression is StaticExpression<DeclQualifiedName>.FunctionApplication funcApp)
        {
            return funcApp.FunctionName == functionName;
        }

        if (expression is StaticExpression<DeclQualifiedName>.Conditional conditional)
        {
            return
                ContainsFunctionApplicationAsTailCall(conditional.TrueBranch, functionName) ||
                ContainsFunctionApplicationAsTailCall(conditional.FalseBranch, functionName);
        }

        return false;
    }
}
