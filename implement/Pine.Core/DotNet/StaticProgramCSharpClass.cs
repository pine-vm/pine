using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Pine.Core.Internal;
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
                    var assignments = new List<StatementSyntax>();

                    foreach (var paramPath in selfFunctionInterface.ParamsPaths)
                    {
                        if (!paramToLocalMap.TryGetValue(paramPath, out var localName))
                        {
                            throw new System.Exception("Internal error: Missing local for param path.");
                        }

                        var argumentExpr =
                            ExpressionForFunctionParam(
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

                        assignments.Add(
                            SyntaxFactory.ExpressionStatement(
                                SyntaxFactory.AssignmentExpression(
                                    SyntaxKind.SimpleAssignmentExpression,
                                    SyntaxFactory.IdentifierName(localName),
                                    argumentExpr)));
                    }

                    return
                        [
                        .. assignments,
                        SyntaxFactory.ContinueStatement()
                        ];
                }

                var resultExpression =
                    CompileToCSharpExpression(
                        expr,
                        SelfFunctionInterfaceDelegate,
                        availableFunctions,
                        availableValueDecls,
                        declarationSyntaxContext,
                        alreadyDeclared)
                    .AsGenericValue(declarationSyntaxContext);

                return [ResultThrowOrReturn(resultExpression)];
            }

            var loopBodyStatement =
                CompileToCSharpStatement(
                    functionBody,
                    SelfFunctionInterfaceDelegate,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    statementsFromResult: StatementsFromResult,
                    alreadyDeclared: ImmutableDictionary<StaticExpression<DeclQualifiedName>, string>.Empty);

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

            // Create while(true) loop
            var whileLoop =
                SyntaxFactory.WhileStatement(
                    SyntaxFactory.LiteralExpression(SyntaxKind.TrueLiteralExpression),
                    loopBodyStatement);

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
                    CompileToCSharpExpression(
                        expr,
                        SelfFunctionInterfaceDelegate,
                        availableFunctions,
                        availableValueDecls,
                        declarationSyntaxContext,
                        alreadyDeclared)
                    .AsGenericValue(declarationSyntaxContext);

                return [ResultThrowOrReturn(resultExpression)];
            }

            return
                CompileToCSharpStatement(
                    functionBody,
                    SelfFunctionInterfaceDelegate,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    statementsFromResult: StatementsFromResult,
                    alreadyDeclared: ImmutableDictionary<StaticExpression<DeclQualifiedName>, string>.Empty);
        }
    }

    public static StatementSyntax CompileToCSharpStatement(
        StaticExpression<DeclQualifiedName> expression,
        System.Func<IReadOnlyList<int>, ExpressionSyntax?> selfFunctionInterface,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls,
        DeclarationSyntaxContext declarationSyntaxContext,
        System.Func<StaticExpression<DeclQualifiedName>, ImmutableDictionary<StaticExpression<DeclQualifiedName>, string>, IReadOnlyList<StatementSyntax>> statementsFromResult,
        ImmutableDictionary<StaticExpression<DeclQualifiedName>, string> alreadyDeclared)
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
                StaticExpression<DeclQualifiedName>.EnvironmentInstance) is not null)
            {
                // Don't CSE environment references (they get replaced with parameters).
                return true;
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

        for (var i = 0; i < subexprDecls.Length; i++)
        {
            var subexpr = subexprDecls[i];

            var localName = "local_" + mutatedDeclared.Count.ToString().PadLeft(3, '0');

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
                CompileToCSharpExpression(
                    conditionalExpr.Condition,
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    newAlreadyDeclared);

            var trueBranchStatement =
                CompileToCSharpStatement(
                    conditionalExpr.TrueBranch,
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    statementsFromResult,
                    newAlreadyDeclared);

            var falseBranchStatement =
                CompileToCSharpStatement(
                    conditionalExpr.FalseBranch,
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    statementsFromResult,
                    newAlreadyDeclared);

            // If the 'if' block ends with a return/throw, the 'else' is redundant. Emit without 'else' and inline the false branch.
            if (BranchEndsWithExitOrLoop(trueBranchStatement))
            {
                var ifStatementNoElse =
                    SyntaxFactory.IfStatement(
                        condition: conditionExpr.AsBooleanValue(declarationSyntaxContext),
                        statement: trueBranchStatement,
                        @else: null);

                IReadOnlyList<StatementSyntax> allStatementsNoElse =
                    [
                        .. newDeclaredStatements,
                        ifStatementNoElse,
                        .. ExtractStatements(falseBranchStatement)
                    ];

                return SyntaxFactory.Block(allStatementsNoElse);
            }

            var ifStatement =
                SyntaxFactory.IfStatement(
                    condition: conditionExpr.AsBooleanValue(declarationSyntaxContext),
                    statement: trueBranchStatement,
                    @else: SyntaxFactory.ElseClause(falseBranchStatement));

            IReadOnlyList<StatementSyntax> allStatements =
                [
                .. newDeclaredStatements,
                ifStatement,
                ];

            return SyntaxFactory.Block(allStatements);
        }

        {
            IReadOnlyList<StatementSyntax> allStatements =
                [
                .. newDeclaredStatements,
                .. statementsFromResult(expression, newAlreadyDeclared)
                ];

            return SyntaxFactory.Block(allStatements);
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

    private static IReadOnlyList<StatementSyntax> ExtractStatements(StatementSyntax statement)
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
        if (alreadyDeclared.TryGetValue(expression, out var existingVarName))
        {
            return
                CompiledCSharpExpression.Generic(
                    SyntaxFactory.IdentifierName(existingVarName));
        }

        ExpressionSyntax? OverrideValueLiteralExpression(PineValue pineValue)
        {
            if (availableValueDecls.TryGetValue(pineValue, out var declName))
            {
                return SyntaxFactory.ParseName(declName.FullName);
            }

            return null;
        }

        if (StaticExpressionExtension.TryParseAsPathToExpression(
            expression,
            StaticExpression<DeclQualifiedName>.EnvironmentInstance) is { } pathToEnv)
        {
            if (selfFunctionInterface(pathToEnv) is { } match)
            {
                return CompiledCSharpExpression.Generic(match);
            }
        }

        if (expression is StaticExpression<DeclQualifiedName>.Literal literal)
        {
            if (OverrideValueLiteralExpression(literal.Value) is { } overriddenExpr)
            {
                return CompiledCSharpExpression.Generic(overriddenExpr);
            }

            var toLiteral =
                PineCSharpSyntaxFactory.CompileToCSharpLiteralExpression(
                    literal.Value,
                    overrideDefaultExpression: OverrideValueLiteralExpression,
                    declarationSyntaxContext);

            return CompiledCSharpExpression.Generic(toLiteral.exprSyntax);
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

            return CompiledCSharpExpression.Generic(genericCSharpExpr);
        }

        if (expression is StaticExpression<DeclQualifiedName>.Conditional conditional)
        {
            var conditionExpr =
                CompileToCSharpExpression(
                    conditional.Condition,
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    alreadyDeclared);

            var trueBranchExpr =
                CompileToCSharpExpression(
                    conditional.TrueBranch,
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    alreadyDeclared);

            var falseBranchExpr =
                CompileToCSharpExpression(
                    conditional.FalseBranch,
                    selfFunctionInterface,
                    availableFunctions,
                    availableValueDecls,
                    declarationSyntaxContext,
                    alreadyDeclared);

            if (trueBranchExpr.Type == CompiledCSharpExpression.ValueType.Boolean &&
                falseBranchExpr.Type == CompiledCSharpExpression.ValueType.Boolean)
            {
                // Both branches are boolean, so produce a boolean result.

                var booleanCSharpExpr =
                    SyntaxFactory.ConditionalExpression(
                        condition: conditionExpr.AsBooleanValue(declarationSyntaxContext),
                        whenTrue: trueBranchExpr.AsBooleanValue(declarationSyntaxContext),
                        whenFalse: falseBranchExpr.AsBooleanValue(declarationSyntaxContext));

                return CompiledCSharpExpression.Boolean(booleanCSharpExpr);
            }

            var genericCSharpExpr =
                SyntaxFactory.ConditionalExpression(
                    condition: conditionExpr.AsBooleanValue(declarationSyntaxContext),
                    whenTrue: trueBranchExpr.AsGenericValue(declarationSyntaxContext),
                    whenFalse: falseBranchExpr.AsGenericValue(declarationSyntaxContext));

            return CompiledCSharpExpression.Generic(genericCSharpExpr);
        }

        if (expression is StaticExpression<DeclQualifiedName>.KernelApplication kernelApp)
        {
            return
                CompileKernelAppToCSharpExpression(
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
                ExpressionForFunctionParam(
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

            return CompiledCSharpExpression.Generic(genericCSharpExpr);
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

            return CompiledCSharpExpression.Generic(genericCSharpExpr);
        }

        if (expression is StaticExpression<DeclQualifiedName>.Environment)
        {
            throw new System.NotImplementedException(
                "Encountered Environment which was not resolved to a parameter.");
        }

        throw new System.NotImplementedException(
            "C# code generation for expression type " + expression.GetType() + " is not implemented.");
    }

    public static CompiledCSharpExpression CompileKernelAppToCSharpExpression(
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
                    var leftExpr =
                        CompileToCSharpExpression(
                            listInput.Items[0],
                            selfFunctionInterface,
                            availableFunctions,
                            availableValueDecls,
                            declarationSyntaxContext,
                            alreadyDeclared);

                    var rightExpr =
                        CompileToCSharpExpression(
                            listInput.Items[1],
                            selfFunctionInterface,
                            availableFunctions,
                            availableValueDecls,
                            declarationSyntaxContext,
                            alreadyDeclared);

                    if (leftExpr.Type == CompiledCSharpExpression.ValueType.Boolean &&
                        rightExpr.Type == CompiledCSharpExpression.ValueType.Boolean)
                    {
                        var booleanCSharpExpr =
                            SyntaxFactory.BinaryExpression(
                                SyntaxKind.EqualsExpression,
                                leftExpr.AsBooleanValue(declarationSyntaxContext),
                                rightExpr.AsBooleanValue(declarationSyntaxContext));

                        return CompiledCSharpExpression.Boolean(booleanCSharpExpr);
                    }

                    {
                        var booleanCSharpExpr =
                            SyntaxFactory.BinaryExpression(
                                SyntaxKind.EqualsExpression,
                                EnsureIsParenthesizedForComposition(
                                    leftExpr.AsGenericValue(declarationSyntaxContext)),
                                EnsureIsParenthesizedForComposition(
                                    rightExpr.AsGenericValue(declarationSyntaxContext)));

                        return CompiledCSharpExpression.Boolean(booleanCSharpExpr);
                    }
                }
            }
        }

        if (TryCompileKernelFusion(
            kernelApp,
            selfFunctionInterface,
            availableFunctions,
            availableValueDecls,
            declarationSyntaxContext,
            alreadyDeclared) is { } fusedExpression)
        {
            return fusedExpression;
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

        if (kernelApp.Input is StaticExpression<DeclQualifiedName>.List argumentsList &&
            PineKernelFunctions.SpecializedInterfacesFromKernelFunctionName(kernelApp.Function) is { } specializedInterfaces)
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

            if (TryMatchSpecializedInterface(
                specializedInterfaces,
                argumentsList,
                isCommutative,
                TryRenderArgument,
                declarationSyntaxContext) is { } matchedExpression)
            {
                return matchedExpression;
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
            return CompiledCSharpExpression.Generic(specializedInvocation);
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

        return CompiledCSharpExpression.Generic(genericCSharpExpr);
    }

    private static CompiledCSharpExpression? TryCompileKernelFusion(
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

        // Variant 1: Fuse take(skip(seq)) where take count is a compile-time integer
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

                return CompiledCSharpExpression.Generic(genericCSharpExpr);
            }
        }

        // Variant 2: Fuse reverse(take(n, reverse(seq))) => KernelFunctionFused.TakeLast(n, seq)
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

                return CompiledCSharpExpression.Generic(genericCSharpExpr);
            }
        }

        return null;
    }

    public static CompiledCSharpExpression ExpressionForFunctionParam(
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
                CompileToCSharpExpression(
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

            throw new System.NotImplementedException(
                "Path remaining after extracting subexpression at path [" +
                string.Join(',', paramPath) + "]: [" + string.Join(',', subexpr.pathRemaining) + "]");
        }

        throw new System.NotImplementedException(
            "Failed to find subexpression at path [" + string.Join(',', paramPath) + "] in argument expression.");
    }

    private static CompiledCSharpExpression? TryMatchSpecializedInterface(
        IReadOnlyList<PineKernelFunctions.KernelFunctionSpecializedInfo> specializedInterfaces,
        StaticExpression<DeclQualifiedName>.List argumentsList,
        bool isCommutative,
        System.Func<StaticExpression<DeclQualifiedName>, PineKernelFunctions.KernelFunctionParameterType, ExpressionSyntax?> tryRenderArgument,
        DeclarationSyntaxContext declarationSyntaxContext)
    {
        IEnumerable<IReadOnlyList<StaticExpression<DeclQualifiedName>>> EnumerateArgumentsPermutations()
        {
            yield return argumentsList.Items;

            if (isCommutative && argumentsList.Items.Count > 1)
            {
                foreach (var permutation in GetPermutations(argumentsList.Items))
                {
                    // Skip the original order, we already tried it
                    if (permutation.SequenceEqual(argumentsList.Items))
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
                    return matchedExpr;
                }
            }
        }

        return null;
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
        /*
         * Primary reason to separate a subexpression into a declaration is CSE (prevent repeated evaluation).
         * 
         * For any subexpression that occurs at least once unconditional and a second time, emit to CSE.
         * */

        var seenOnceUnconditional = new HashSet<StaticExpression<FuncId>>();

        var collected = new HashSet<StaticExpression<FuncId>>();

        var queue = new Queue<(StaticExpression<FuncId> expr, bool conditional)>([(expression, false)]);

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

            if (seenOnceUnconditional.Contains(current.expr))
            {
                yield return current.expr;

                collected.Add(current.expr);

                continue;
            }

            if (!current.conditional)
            {
                seenOnceUnconditional.Add(current.expr);
            }

            if (current.expr is StaticExpression<FuncId>.Conditional conditional)
            {
                queue.Enqueue((conditional.Condition, current.conditional));

                queue.Enqueue((conditional.TrueBranch, true));
                queue.Enqueue((conditional.FalseBranch, true));

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
                    queue.Enqueue((item, current.conditional));
                }

                continue;
            }

            if (current.expr is StaticExpression<FuncId>.KernelApplication kernelApp)
            {
                queue.Enqueue((kernelApp.Input, current.conditional));

                continue;
            }

            if (current.expr is StaticExpression<FuncId>.FunctionApplication funcApp)
            {
                queue.Enqueue((funcApp.Arguments, current.conditional));

                continue;
            }

            if (current.expr is StaticExpression<FuncId>.CrashingParseAndEval parseAndEval)
            {
                queue.Enqueue((parseAndEval.Encoded, current.conditional));
                queue.Enqueue((parseAndEval.EnvironmentExpr, current.conditional));

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

    public static ExpressionSyntax EnsureIsParenthesizedForComposition(
        ExpressionSyntax expression)
    {
        if (ExpressionNeedsParensForComposition(expression))
        {
            return SyntaxFactory.ParenthesizedExpression(expression);
        }

        return expression;
    }

    public static bool ExpressionNeedsParensForComposition(
        ExpressionSyntax expression)
    {
        if (expression is IdentifierNameSyntax)
            return false;

        if (expression is QualifiedNameSyntax)
            return false;

        if (expression is MemberAccessExpressionSyntax memberAccess)
            return false;

        if (expression is LiteralExpressionSyntax)
            return false;

        if (expression is InvocationExpressionSyntax)
            return false;

        return true;
    }
}
