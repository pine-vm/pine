using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.CompilePineToDotNet;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Pine.PineVM;

public record StaticProgramCSharpClass(
    ClassDeclarationSyntax ClassDeclarationSyntax)
{
    public static StaticProgramCSharpClass FromDeclarations(
        string className,
        IReadOnlyDictionary<string, (Expression origExpr, StaticFunctionInterface interf, StaticExpression<DeclQualifiedName> body, PineValueClass constraint)> declarations,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls)
    {
        IReadOnlyList<MethodDeclarationSyntax> functions =
            [.. declarations
            .Select(kv =>
            RenderFunctionToMethod(
                functionName: kv.Key,
                functionInterface: kv.Value.interf,
                body: kv.Value.body,
                availableFunctions,
                availableValueDecls))];

        var classSyntax =
            SyntaxFactory.ClassDeclaration(className)
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
        var generateTextResult =
            CompileToCSharp.GenerateCSharpFile(
                compileCSharpClassResult: new CompileCSharpClassResult(
                    new SyntaxContainerConfig(
                        ContainerTypeName: classDeclarationSyntax.Identifier.Text,
                        DictionaryMemberName: ""),
                    ClassDeclarationSyntax: classDeclarationSyntax,
                    UsingDirectives: []));

        return generateTextResult.FileText;
    }

    public static MethodDeclarationSyntax RenderFunctionToMethod(
        string functionName,
        StaticFunctionInterface functionInterface,
        StaticExpression<DeclQualifiedName> body,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls)
    {
        var statementSyntax =
            CompileToCSharpStatement(
                expression: body,
                functionInterface,
                availableFunctions,
                availableValueDecls);

        return
            MemberDeclarationSyntaxForExpression(
                declarationName: functionName,
                statementSyntax: statementSyntax,
                functionInterface: functionInterface);
    }


    static MethodDeclarationSyntax MemberDeclarationSyntaxForExpression(
        string declarationName,
        StatementSyntax statementSyntax,
        StaticFunctionInterface functionInterface)
    {
        var blockSyntax =
            (statementSyntax as BlockSyntax)
            ??
            SyntaxFactory.Block(statementSyntax);

        return
            SyntaxFactory.MethodDeclaration(
                returnType: SyntaxFactory.IdentifierName(nameof(PineValue)),
                SyntaxFactory.Identifier(declarationName))
            .WithModifiers(
                SyntaxFactory.TokenList(
                    SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                    SyntaxFactory.Token(SyntaxKind.StaticKeyword)))
            .WithParameterList(
                SyntaxFactory.ParameterList(
                    SyntaxFactory.SeparatedList(ComposeParameterList(functionInterface))))
            .WithBody(blockSyntax);
    }

    public static StatementSyntax CompileToCSharpStatement(
        StaticExpression<DeclQualifiedName> expression,
        StaticFunctionInterface functionInterface,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls) =>
        CompileToCSharpStatement(
            expression,
            functionInterface,
            availableFunctions,
            availableValueDecls,
            statementFromResult: ResultThrowOrReturn,
            alreadyDeclared: ImmutableDictionary<StaticExpression<DeclQualifiedName>, string>.Empty);

    public static StatementSyntax CompileToCSharpStatement(
        StaticExpression<DeclQualifiedName> expression,
        StaticFunctionInterface functionInterface,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls,
        System.Func<ExpressionSyntax, StatementSyntax> statementFromResult,
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
                    SyntaxFactory.IdentifierName(nameof(PineValue)))
                .WithVariables(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.VariableDeclarator(
                            SyntaxFactory.Identifier(localName))
                        .WithInitializer(
                            SyntaxFactory.EqualsValueClause(
                                CompileToCSharpExpression(
                                    subexpr,
                                    functionInterface,
                                    availableFunctions,
                                    availableValueDecls,
                                    mutatedDeclared))))));

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
                    functionInterface,
                    availableFunctions,
                    availableValueDecls,
                    newAlreadyDeclared);

            var trueBranchStatement =
                CompileToCSharpStatement(
                    conditionalExpr.TrueBranch,
                    functionInterface,
                    availableFunctions,
                    availableValueDecls,
                    statementFromResult,
                    newAlreadyDeclared);

            var falseBranchStatement =
                CompileToCSharpStatement(
                    conditionalExpr.FalseBranch,
                    functionInterface,
                    availableFunctions,
                    availableValueDecls,
                    statementFromResult,
                    newAlreadyDeclared);

            // If the 'if' block ends with a return/throw, the 'else' is redundant. Emit without 'else' and inline the false branch.
            if (BranchEndsWithReturnOrThrow(trueBranchStatement))
            {
                var ifStatementNoElse =
                    SyntaxFactory.IfStatement(
                        condition: conditionExpr,
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
                    condition: conditionExpr,
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
            var resultExpression =
                CompileToCSharpExpression(
                    expression,
                    functionInterface,
                    availableFunctions,
                    availableValueDecls,
                    newAlreadyDeclared);

            IReadOnlyList<StatementSyntax> allStatements =
                [
                .. newDeclaredStatements,
                statementFromResult(resultExpression)
                ];

            return SyntaxFactory.Block(allStatements);
        }
    }

    private static bool BranchEndsWithReturnOrThrow(StatementSyntax statement)
    {
        if (statement is BlockSyntax block)
        {
            if (block.Statements.Count is 0)
            {
                return false;
            }

            var last = block.Statements.Last();

            return last is ReturnStatementSyntax || last is ThrowStatementSyntax;
        }

        return statement is ReturnStatementSyntax || statement is ThrowStatementSyntax;
    }

    private static IReadOnlyList<StatementSyntax> ExtractStatements(StatementSyntax statement)
    {
        if (statement is BlockSyntax block)
        {
            return block.Statements;
        }

        return [statement];
    }

    public static ExpressionSyntax CompileToCSharpExpression(
        StaticExpression<DeclQualifiedName> expression,
        StaticFunctionInterface functionInterface,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls,
        ImmutableDictionary<StaticExpression<DeclQualifiedName>, string> alreadyDeclared)
    {
        if (alreadyDeclared.TryGetValue(expression, out var existingVarName))
        {
            return SyntaxFactory.IdentifierName(existingVarName);
        }

        ExpressionSyntax? OverrideValueLiteralExpression(PineValue pineValue)
        {
            if (availableValueDecls.TryGetValue(pineValue, out var declName))
            {
                return SyntaxFactory.IdentifierName(declName.FullName);
            }

            return null;
        }

        if (StaticExpressionExtension.TryParseAsPathToExpression(
            expression,
            StaticExpression<DeclQualifiedName>.EnvironmentInstance) is { } pathToEnv)
        {
            if (functionInterface.ParamsPaths
                .Select((p, i) => (path: p, index: i))
                .FirstOrDefault(pi => IntPathEqualityComparer.Instance.Equals(pi.path, pathToEnv)) is { } match)
            {
                var paramRef = RenderParamRef(match.path);

                return SyntaxFactory.IdentifierName(paramRef);
            }
        }

        if (expression is StaticExpression<DeclQualifiedName>.Literal literal)
        {
            if (OverrideValueLiteralExpression(literal.Value) is { } overriddenExpr)
            {
                return overriddenExpr;
            }

            var toLiteral =
                CompileToCSharp.CompileToCSharpLiteralExpression(
                    literal.Value,
                    overrideDefaultExpression: OverrideValueLiteralExpression);

            return toLiteral.exprSyntax;
        }

        if (expression is StaticExpression<DeclQualifiedName>.List list)
        {
            var elementExprs =
                list.Items
                .Select(item =>
                CompileToCSharpExpression(
                    item,
                    functionInterface,
                    availableFunctions,
                    availableValueDecls,
                    alreadyDeclared))
                .ToImmutableArray();

            var collectionExprs =
                SyntaxFactory.CollectionExpression(
                    SyntaxFactory.SeparatedList<CollectionElementSyntax>(
                        elementExprs
                        .Select(SyntaxFactory.ExpressionElement)));

            // Invoke PineValue.List( ... )

            return
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        SyntaxFactory.IdentifierName(nameof(PineValue)),
                        SyntaxFactory.IdentifierName(nameof(PineValue.List))))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.Argument(collectionExprs))));
        }

        if (expression is StaticExpression<DeclQualifiedName>.Conditional conditional)
        {
            var conditionExpr =
                CompileToCSharpExpression(
                    conditional.Condition,
                    functionInterface,
                    availableFunctions,
                    availableValueDecls,
                    alreadyDeclared);

            var trueBranchExpr =
                CompileToCSharpExpression(
                    conditional.TrueBranch,
                    functionInterface,
                    availableFunctions,
                    availableValueDecls,
                    alreadyDeclared);

            var falseBranchExpr =
                CompileToCSharpExpression(
                    conditional.FalseBranch,
                    functionInterface,
                    availableFunctions,
                    availableValueDecls,
                    alreadyDeclared);

            return
                SyntaxFactory.ConditionalExpression(
                    condition: conditionExpr,
                    whenTrue: trueBranchExpr,
                    whenFalse: falseBranchExpr);
        }

        if (expression is StaticExpression<DeclQualifiedName>.KernelApplication kernelApp)
        {
            return
                CompileToCSharpExpression(
                    kernelApp,
                    functionInterface,
                    availableFunctions,
                    availableValueDecls,
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
                    functionInterface,
                    availableFunctions,
                    availableValueDecls,
                    alreadyDeclared))
                .ToImmutableArray();

            return
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.IdentifierName(funcApp.FunctionName.FullName))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList(
                            [
                            .. arguments.Select(SyntaxFactory.Argument)
                            ])));
        }

        if (expression is StaticExpression<DeclQualifiedName>.CrashingParseAndEval parseAndEval)
        {
            var renderedEncodedExpr =
                CompileToCSharpExpression(
                    parseAndEval.Encoded,
                    functionInterface,
                    availableFunctions,
                    availableValueDecls,
                    alreadyDeclared);

            return
                PineCSharpSyntaxFactory.ThrowParseExpressionException(
                    SyntaxFactory.LiteralExpression(
                        SyntaxKind.StringLiteralExpression,
                        SyntaxFactory.Literal("TODO: Include details from encoded and env subexpressions")));
        }

        if (expression is StaticExpression<DeclQualifiedName>.Environment)
        {
            throw new System.NotImplementedException(
                "Encountered Environment which was not resolved to a parameter.");
        }

        throw new System.NotImplementedException(
            "C# code generation for expression type " + expression.GetType() + " is not implemented.");
    }

    public static ExpressionSyntax CompileToCSharpExpression(
        StaticExpression<DeclQualifiedName>.KernelApplication kernelApp,
        StaticFunctionInterface functionInterface,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls,
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
                            functionInterface,
                            availableFunctions,
                            availableValueDecls,
                            alreadyDeclared);

                    var rightExpr =
                        CompileToCSharpExpression(
                            listInput.Items[1],
                            functionInterface,
                            availableFunctions,
                            availableValueDecls,
                            alreadyDeclared);

                    return
                        SyntaxFactory.BinaryExpression(
                            SyntaxKind.EqualsExpression,
                            leftExpr,
                            rightExpr);
                }
            }
        }

        var inputExpr =
            CompileToCSharpExpression(
                kernelApp.Input,
                functionInterface,
                availableFunctions,
                availableValueDecls,
                alreadyDeclared);

        // Generic case: Invoke KernelFunction.ApplyKernelFunctionGeneric

        return
            SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    SyntaxFactory.IdentifierName(nameof(KernelFunction)),
                    SyntaxFactory.IdentifierName(nameof(KernelFunction.ApplyKernelFunctionGeneric))))
            .WithArgumentList(
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SeparatedList([
                        SyntaxFactory.Argument(
                                SyntaxFactory.LiteralExpression(
                                    SyntaxKind.StringLiteralExpression,
                                    SyntaxFactory.Literal(kernelApp.Function))),
                            SyntaxFactory.Argument(inputExpr)
                    ])));
    }

    public static ExpressionSyntax ExpressionForFunctionParam(
        IReadOnlyList<int> paramPath,
        StaticExpression<DeclQualifiedName> argumentExpr,
        StaticFunctionInterface functionInterface,
        IReadOnlyDictionary<DeclQualifiedName, StaticFunctionInterface> availableFunctions,
        IReadOnlyDictionary<PineValue, DeclQualifiedName> availableValueDecls,
        ImmutableDictionary<StaticExpression<DeclQualifiedName>, string> alreadyDeclared)
    {
        if (StaticExpressionExtension.GetSubexpressionAtPath(argumentExpr, paramPath) is { } subexpr)
        {
            var renderedExpr =
                CompileToCSharpExpression(
                    subexpr.subexpr,
                    functionInterface,
                    availableFunctions,
                    availableValueDecls,
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
        StaticFunctionInterface functionInterface)
    {
        var envItemsParameters =
            functionInterface.ParamsPaths
            .Select(paramPath =>
            SyntaxFactory.Parameter(SyntaxFactory.Identifier(RenderParamRef(paramPath)))
            .WithType(SyntaxFactory.IdentifierName(nameof(PineValue))));

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
        return "param_" + string.Join('_', path);
    }
}
