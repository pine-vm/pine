using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Pine.Core.PineVM;
using Pine.Core.PopularEncodings;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Pine.PineVM;

using LocalDeclarations = ImmutableDictionary<StaticExpression<DeclQualifiedName>, (string identifier, LocalType ltype)>;

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
            functionBody
            .EnumerateTailCalls()
            .Where(funcApp => funcApp.FunctionName == selfFunctionName)
            .Any();

        // Build initial blocked names (parameters)
        var initialBlocked =
            selfFunctionInterface.ParamsPaths
            .Select(RenderParamRef)
            .ToImmutableHashSet();

        if (hasTailRecursiveCalls)
        {
            // TODO: Expand to cover cases where we have a common reused derivation from params that does not vary between calls.

            // Map all params to locals that we can update in place.

            var paramsAsConcatBuilders =
                new HashSet<IReadOnlyList<int>>(
                    ComputeParamsAsConcatBuilders(
                        functionBody,
                        selfFunctionName,
                        selfFunctionInterface),
                    IntPathEqualityComparer.Instance);

            var paramsAsSliceBuilders =
                new HashSet<IReadOnlyList<int>>(
                    ComputeParamsAsImmutableSliceBuilders(
                        functionBody,
                        selfFunctionName,
                        selfFunctionInterface),
                    IntPathEqualityComparer.Instance);

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
                    if (paramsAsConcatBuilders.Contains(path))
                    {
                        /*
                         * CSE collection asks for subexpressions, so return instead of throwing exception.
                         * 
                        throw new System.NotImplementedException(
                            "Handling of parameters as ConcatBuilder not implemented yet.");
                        */

                        // Since the current version does not return any type info along with the expression, evaluate first.

                        return
                        PineCSharpSyntaxFactory.EvaluateImmutableConcatBuilderSyntax(
                            SyntaxFactory.IdentifierName(localName));
                    }

                    if (paramsAsSliceBuilders.Contains(path))
                    {
                        return
                        PineCSharpSyntaxFactory.EvaluateImmutableSliceBuilderSyntax(
                            SyntaxFactory.IdentifierName(localName));
                    }

                    return SyntaxFactory.IdentifierName(localName);
                }

                return null;
            }

            CompiledCSharpExpression? GeneralOverride(StaticExpression<DeclQualifiedName> expr)
            {
                if (expr is StaticExpression<DeclQualifiedName>.KernelApplication kernelApp)
                {
                    if (kernelApp.Function is nameof(KernelFunction.length))
                    {
                        if (StaticExpressionExtension.TryParseAsPathToExpression(
                            kernelApp.Input,
                            StaticExpression<DeclQualifiedName>.EnvironmentInstance) is { } pathInEnv)
                        {
                            if (paramsAsSliceBuilders.Contains(pathInEnv))
                            {
                                if (!paramToLocalMap.TryGetValue(pathInEnv, out var localName))
                                    throw new System.Exception("Internal error: Missing local for param path.");

                                var lengthExpr =
                                    PineCSharpSyntaxFactory.ImmutableSliceBuilderGetLengthSyntax(
                                        SyntaxFactory.IdentifierName(localName));

                                return CompiledCSharpExpression.Integer(lengthExpr);
                            }
                        }
                    }

                    if (kernelApp.Function is nameof(KernelFunction.head))
                    {
                        if (StaticExpressionExtension.TryParseAsPathToExpression(
                            kernelApp.Input,
                            StaticExpression<DeclQualifiedName>.EnvironmentInstance) is { } pathInEnv)
                        {
                            if (paramsAsSliceBuilders.Contains(pathInEnv))
                            {
                                if (!paramToLocalMap.TryGetValue(pathInEnv, out var localName))
                                    throw new System.Exception("Internal error: Missing local for param path.");

                                var headExpr =
                                    PineCSharpSyntaxFactory.ImmutableSliceBuilderGetHeadSyntax(
                                        SyntaxFactory.IdentifierName(localName));

                                return CompiledCSharpExpression.Generic(headExpr);
                            }
                        }
                    }
                }

                if (StaticExpressionExtension.TryParseAsEqualPineValueEmptyList(expr) is { } checkEqualEmptyList)
                {
                    if (StaticExpressionExtension.TryParseAsPathToExpression(
                        checkEqualEmptyList,
                        StaticExpression<DeclQualifiedName>.EnvironmentInstance) is { } pathInEnv)
                    {
                        if (paramsAsSliceBuilders.Contains(pathInEnv))
                        {
                            if (!paramToLocalMap.TryGetValue(pathInEnv, out var localName))
                                throw new System.Exception("Internal error: Missing local for param path.");

                            var isEmptyExpr =
                                PineCSharpSyntaxFactory.ImmutableSliceBuilderIsEmptyListSyntax(
                                    SyntaxFactory.IdentifierName(localName));

                            return CompiledCSharpExpression.Boolean(isEmptyExpr);
                        }
                    }
                }

                if (expr is StaticExpression<DeclQualifiedName>.KernelApplication reverseKernelApp)
                {
                    if (reverseKernelApp.Function is nameof(KernelFunction.reverse))
                    {
                        // Check if the input is a reference to a concat builder (either parameter or local)
                        if (StaticExpressionExtension.TryParseAsPathToExpression(
                            reverseKernelApp.Input,
                            StaticExpression<DeclQualifiedName>.EnvironmentInstance) is { } pathInEnv)
                        {
                            if (paramsAsConcatBuilders.Contains(pathInEnv))
                            {
                                if (!paramToLocalMap.TryGetValue(pathInEnv, out var localName))
                                    throw new System.Exception("Internal error: Missing local for param path.");

                                var evaluateReverseExpr =
                                    PineCSharpSyntaxFactory.EvaluateImmutableConcatBuilderReverseSyntax(
                                        SyntaxFactory.IdentifierName(localName));

                                return CompiledCSharpExpression.Generic(evaluateReverseExpr);
                            }
                        }
                    }
                }

                return null;
            }

            IReadOnlyList<StatementSyntax> StatementsFromResult(
                StaticExpression<DeclQualifiedName> expr,
                LocalDeclarations alreadyDeclared)
            {
                var exprEmitEnv =
                    new ExpressionEmitEnv(
                        new FunctionEmitEnv(
                            availableFunctions,
                            availableValueDecls,
                            declarationSyntaxContext,
                            SelfFunctionInterfaceDelegate,
                            GeneralOverride),
                        alreadyDeclared);

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

                    var mutatingUpdates = new List<StatementSyntax>();

                    var assignments = new List<StatementSyntax>();

                    foreach (var paramPath in selfFunctionInterface.ParamsPaths)
                    {
                        if (paramsAsConcatBuilders.Contains(paramPath))
                        {
                            var mutation =
                                StaticExpressionExtension.ParseParamPathAsConcatBuilderMutationInFunctionApplication(
                                    funcApp,
                                    paramPath)
                                ?? throw new System.Exception(
                                    "Internal error: Failed to parse concat builder mutation for parameter.");

                            if (!paramToLocalMap.TryGetValue(paramPath, out var localName))
                            {
                                throw new System.Exception("Internal error: Missing local for param path.");
                            }

                            var appendedItemsCompiled =
                                mutation.Items
                                .Select(appendedItemExpr =>
                                    EnumerateExpressions(
                                        appendedItemExpr,
                                        exprEmitEnv)
                                    .AsGenericValue(declarationSyntaxContext))
                                .ToImmutableArray();

                            var updatedBuilderExpr =
                                mutation.Kind switch
                                {
                                    StaticExpressionExtension.ConcatBuilderMutationKind.Append =>
                                        PineCSharpSyntaxFactory.AppendItemsToImmutableConcatBuilderSyntax(
                                            SyntaxFactory.IdentifierName(localName),
                                            appendedItemsCompiled),

                                    StaticExpressionExtension.ConcatBuilderMutationKind.Prepend =>
                                        PineCSharpSyntaxFactory.PrependItemsToImmutableConcatBuilderSyntax(
                                            SyntaxFactory.IdentifierName(localName),
                                            appendedItemsCompiled),

                                    _ =>
                                        throw new System.NotImplementedException(
                                            "Unknown concat builder mutation kind: " + mutation.Kind)
                                };

                            mutatingUpdates.Add(
                                SyntaxFactory.ExpressionStatement(
                                    SyntaxFactory.AssignmentExpression(
                                        SyntaxKind.SimpleAssignmentExpression,
                                        SyntaxFactory.IdentifierName(localName),
                                        updatedBuilderExpr)));
                        }
                        else if (paramsAsSliceBuilders.Contains(paramPath))
                        {
                            if (!paramToLocalMap.TryGetValue(paramPath, out var localName))
                            {
                                throw new System.Exception("Internal error: Missing local for param path.");
                            }

                            if (TryParseParamPathAsSliceOperationsInFunctionApplication(
                                funcApp,
                                paramPath) is not { } sliceOperations)
                            {
                                throw new System.Exception(
                                    "Internal error: Failed to parse slice operations for parameter.");
                            }

                            if (sliceOperations.Length is 0)
                            {
                                continue;
                            }

                            ExpressionSyntax updatedBuilderExpr = SyntaxFactory.IdentifierName(localName);

                            foreach (var sliceOperation in sliceOperations)
                            {
                                // Check if count is a compile-time constant integer
                                ExpressionSyntax countArgument;

                                if (sliceOperation.CountExpression is StaticExpression<DeclQualifiedName>.Literal literal &&
                                    KernelFunction.SignedIntegerFromValueRelaxed(literal.Value) is { } intValue &&
                                    intValue >= int.MinValue &&
                                    intValue <= int.MaxValue)
                                {
                                    // Use int overload with compile-time constant
                                    countArgument = PineCSharpSyntaxFactory.ExpressionSyntaxForIntegerLiteral((long)intValue);
                                }
                                else
                                {
                                    // Use PineValue overload for non-constant expressions
                                    countArgument =
                                        EnumerateExpressions(
                                            sliceOperation.CountExpression,
                                            exprEmitEnv)
                                        .AsGenericValue(declarationSyntaxContext);
                                }

                                var invocationTarget = updatedBuilderExpr;

                                var methodName =
                                    sliceOperation.Kind switch
                                    {
                                        SliceOperationKind.Skip =>
                                        nameof(Core.DotNet.Builtins.ImmutableSliceBuilder.Skip),

                                        SliceOperationKind.Take =>
                                        nameof(Core.DotNet.Builtins.ImmutableSliceBuilder.Take),

                                        _
                                        => throw new System.NotImplementedException(
                                            "Unknown slice operation kind: " + sliceOperation.Kind)
                                    };

                                updatedBuilderExpr =
                                    SyntaxFactory.InvocationExpression(
                                        SyntaxFactory.MemberAccessExpression(
                                            SyntaxKind.SimpleMemberAccessExpression,
                                            invocationTarget,
                                            SyntaxFactory.IdentifierName(methodName)))
                                    .WithArgumentList(
                                        SyntaxFactory.ArgumentList(
                                            SyntaxFactory.SingletonSeparatedList(
                                                SyntaxFactory.Argument(countArgument))));
                            }

                            assignments.Add(
                                SyntaxFactory.ExpressionStatement(
                                    SyntaxFactory.AssignmentExpression(
                                        SyntaxKind.SimpleAssignmentExpression,
                                        SyntaxFactory.IdentifierName(localName),
                                        updatedBuilderExpr)));
                        }
                        else
                        {
                            if (!paramToLocalMap.TryGetValue(paramPath, out var localName))
                            {
                                throw new System.Exception("Internal error: Missing local for param path.");
                            }

                            var argumentExpr =
                                ExpressionsForFunctionArgument(
                                    paramPath,
                                    funcApp.Arguments,
                                    exprEmitEnv)
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
                    }

                    // Contain the local declarations in their own scope to avoid name clashes

                    var tempDeclarationsBlock =
                        SyntaxFactory.Block(
                            (SyntaxList<StatementSyntax>)
                            [
                            .. tempDeclarations,
                            .. mutatingUpdates,
                            .. assignments
                            ]);

                    return
                        [
                        tempDeclarationsBlock,
                        SyntaxFactory.ContinueStatement()
                        ];
                }

                // Special-case: returning a builder param — evaluate then return.
                if (StaticExpressionExtension.TryParseAsPathToExpression(
                    expr,
                    StaticExpression<DeclQualifiedName>.EnvironmentInstance) is { } retPath
                    && paramsAsConcatBuilders.Contains(retPath))
                {
                    if (!paramToLocalMap.TryGetValue(retPath, out var localName))
                        throw new System.Exception("Internal error: Missing local for param path.");

                    var evaluated =
                        PineCSharpSyntaxFactory.EvaluateImmutableConcatBuilderSyntax(
                            SyntaxFactory.IdentifierName(localName));

                    return [ResultThrowOrReturn(evaluated)];
                }

                if (StaticExpressionExtension.TryParseAsPathToExpression(
                    expr,
                    StaticExpression<DeclQualifiedName>.EnvironmentInstance) is { } retPathSlice
                    && paramsAsSliceBuilders.Contains(retPathSlice))
                {
                    if (!paramToLocalMap.TryGetValue(retPathSlice, out var localName))
                        throw new System.Exception("Internal error: Missing local for param path.");

                    var evaluated =
                        PineCSharpSyntaxFactory.EvaluateImmutableSliceBuilderSyntax(
                            SyntaxFactory.IdentifierName(localName));

                    return [ResultThrowOrReturn(evaluated)];
                }

                var resultExpression =
                    EnumerateExpressions(
                        expr,
                        exprEmitEnv)
                    .AsGenericValue(declarationSyntaxContext);

                return [ResultThrowOrReturn(resultExpression)];
            }

            (System.Type declType, ExpressionSyntax initExpr) ParamDeclarationInitExpr(IReadOnlyList<int> paramPath)
            {
                var origParamRef = SyntaxFactory.IdentifierName(RenderParamRef(paramPath));

                if (paramsAsConcatBuilders.Contains(paramPath))
                {
                    var initExpr =
                        PineCSharpSyntaxFactory.CreateImmutableConcatBuilderSyntax(
                            SyntaxFactory.CollectionExpression(
                                SyntaxFactory.SeparatedList<CollectionElementSyntax>(
                                    [SyntaxFactory.ExpressionElement(origParamRef)])),
                            declarationSyntaxContext);

                    return (typeof(Core.DotNet.Builtins.ImmutableConcatBuilder), initExpr);
                }

                if (paramsAsSliceBuilders.Contains(paramPath))
                {
                    var initExpr =
                        PineCSharpSyntaxFactory.CreateImmutableSliceBuilderSyntax(
                            origParamRef,
                            declarationSyntaxContext);

                    return (typeof(Core.DotNet.Builtins.ImmutableSliceBuilder), initExpr);
                }

                return (typeof(PineValue), origParamRef);
            }

            LocalDeclarationStatementSyntax ParamDeclarationStatement(IReadOnlyList<int> paramPath)
            {
                var localName = paramToLocalMap[paramPath];

                var (declType, initExpr) = ParamDeclarationInitExpr(paramPath);

                return
                    SyntaxFactory.LocalDeclarationStatement(
                        SyntaxFactory.VariableDeclaration(
                            CompileTypeSyntax.TypeSyntaxFromType(declType, declarationSyntaxContext))
                    .WithVariables(
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.VariableDeclarator(
                                SyntaxFactory.Identifier(localName))
                            .WithInitializer(
                                SyntaxFactory.EqualsValueClause(
                                    initExpr)))));
            }

            var paramDeclarations =
                selfFunctionInterface.ParamsPaths
                .Select(ParamDeclarationStatement)
                .ToImmutableArray();

            // Extend blocked with our param locals as well
            var blockedWithParamLocals =
                initialBlocked
                .Union(paramToLocalMap.Values);

            var loopBodyCompiled =
                CompileToCSharpStatement(
                    functionBody,
                    new ExpressionEmitEnv(
                        new FunctionEmitEnv(
                            availableFunctions,
                            availableValueDecls,
                            declarationSyntaxContext,
                            SelfFunctionInterfaceDelegate,
                            GeneralOverride),
                        LocalDeclarations.Empty),
                    statementsFromResult: StatementsFromResult,
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
                LocalDeclarations alreadyDeclared)
            {
                var exprEmitEnv =
                    new ExpressionEmitEnv(
                        new FunctionEmitEnv(
                            availableFunctions,
                            availableValueDecls,
                            declarationSyntaxContext,
                            SelfFunctionInterfaceDelegate,
                            GeneralOverride: _ => null),
                        alreadyDeclared);

                var resultExpression =
                    EnumerateExpressions(
                        expr,
                        exprEmitEnv)
                    .AsGenericValue(declarationSyntaxContext);

                return [ResultThrowOrReturn(resultExpression)];
            }

            var compiled =
                CompileToCSharpStatement(
                    functionBody,
                    new ExpressionEmitEnv(
                        new FunctionEmitEnv(
                            availableFunctions,
                            availableValueDecls,
                            declarationSyntaxContext,
                            SelfFunctionInterfaceDelegate,
                            GeneralOverride: _ => null),
                        LocalDeclarations.Empty),
                    statementsFromResult: StatementsFromResult,
                    blockedDeclarations: initialBlocked);

            return compiled.Statement;
        }
    }

    private static bool IsGoodForConcatBuilderCandidate(
        StaticExpression<DeclQualifiedName> expr,
        DeclQualifiedName selfFunctionName,
        IReadOnlyList<int> paramPath,
        bool isTailPosition,
        ref bool sawConcatBuilderMutation)
    {
        switch (expr)
        {
            case StaticExpression<DeclQualifiedName>.Literal:
                return true;

            case StaticExpression<DeclQualifiedName>.Environment:
                // Only allow returning the parameter as a tail leaf.
                return isTailPosition && paramPath.Count is 0;

            case StaticExpression<DeclQualifiedName>.List list:
                {
                    // List itself is only a tail leaf if it's the whole expression.
                    // Any mention of the param inside list construction disqualifies.
                    foreach (var item in list.Items)
                    {
                        if (!IsGoodForConcatBuilderCandidate(item, selfFunctionName, paramPath, false, ref sawConcatBuilderMutation))
                            return false;
                    }

                    return true;
                }

            case StaticExpression<DeclQualifiedName>.Conditional cond:
                {
                    // Condition must never mention the param path.
                    if (StaticExpressionExtension.MentionsEnvPath(cond.Condition, paramPath))
                        return false;

                    // Both branches are in tail position relative to the function.
                    return
                        IsGoodForConcatBuilderCandidate(
                            cond.TrueBranch,
                            selfFunctionName,
                            paramPath,
                            isTailPosition,
                            ref sawConcatBuilderMutation) &&
                            IsGoodForConcatBuilderCandidate(
                                cond.FalseBranch,
                                selfFunctionName,
                                paramPath,
                                isTailPosition,
                                ref sawConcatBuilderMutation);
                }

            case StaticExpression<DeclQualifiedName>.KernelApplication kernelApp:
                {
                    {
                        // Check for special case: the param appears directly inside an invocation of 'reverse'.

                        if (kernelApp.Function is nameof(KernelFunction.reverse) &&
                            StaticExpressionExtension.TryParseAsPathToExpression(
                                kernelApp.Input,
                                StaticExpression<DeclQualifiedName>.EnvironmentInstance) is { } reverseEnvPath)
                        {
                            if (reverseEnvPath.SequenceEqual(paramPath))
                            {
                                return true;
                            }
                        }
                    }

                    // Any mention in kernel is disallowed (we cannot read the builder).
                    return !StaticExpressionExtension.MentionsEnvPath(kernelApp.Input, paramPath);
                }

            case StaticExpression<DeclQualifiedName>.FunctionApplication funcApp:
                {
                    if (isTailPosition && funcApp.FunctionName == selfFunctionName)
                    {
                        var mutation =
                            StaticExpressionExtension.ParseParamPathAsConcatBuilderMutationInFunctionApplication(
                                funcApp,
                                paramPath);

                        if (mutation is not { } concatMutation)
                            return false;

                        for (var i = 0; i < concatMutation.Items.Count; i++)
                        {
                            if (!IsGoodForConcatBuilderCandidate(concatMutation.Items[i], selfFunctionName, paramPath, false, ref sawConcatBuilderMutation))
                                return false;
                        }

                        sawConcatBuilderMutation = true;
                        return true;
                    }

                    // Any non-tail function application (or call to other function) that mentions the param is disallowed.
                    return !StaticExpressionExtension.MentionsEnvPath(funcApp, paramPath);
                }

            case StaticExpression<DeclQualifiedName>.CrashingParseAndEval crashing:
                {
                    return
                        !StaticExpressionExtension.MentionsEnvPath(crashing.Encoded, paramPath) &&
                        !StaticExpressionExtension.MentionsEnvPath(crashing.EnvironmentExpr, paramPath);
                }

            default:
                throw new System.NotImplementedException(
                    "Internal error: Unknown expression type in IsGoodForConcatBuilderCandidate: " +
                    expr.GetType().FullName);
        }
    }

    private static IEnumerable<IReadOnlyList<int>> ComputeParamsAsConcatBuilders(
        StaticExpression<DeclQualifiedName> functionBody,
        DeclQualifiedName selfFunctionName,
        StaticFunctionInterface selfFunctionInterface)
    {
        foreach (var path in selfFunctionInterface.ParamsPaths)
        {
            var sawConcatBuilderMutation = false;

            var ok =
                IsGoodForConcatBuilderCandidate(
                    functionBody,
                    selfFunctionName,
                    path,
                    isTailPosition: true,
                    ref sawConcatBuilderMutation);

            if (ok && sawConcatBuilderMutation)
            {
                yield return path;
            }
        }
    }

    private enum SliceOperationKind
    {
        Skip = 10,
        Take = 20,
    }

    private readonly record struct ImmutableSliceOperation(
        SliceOperationKind Kind,
        StaticExpression<DeclQualifiedName> CountExpression);

    private static IEnumerable<IReadOnlyList<int>> ComputeParamsAsImmutableSliceBuilders(
        StaticExpression<DeclQualifiedName> functionBody,
        DeclQualifiedName selfFunctionName,
        StaticFunctionInterface selfFunctionInterface)
    {
        foreach (var path in selfFunctionInterface.ParamsPaths)
        {
            var ok = true;
            var sawSliceOperation = false;

            foreach (var tailCall in functionBody.EnumerateTailCalls())
            {
                if (tailCall.FunctionName != selfFunctionName)
                {
                    continue;
                }

                if (TryParseParamPathAsSliceOperationsInFunctionApplication(
                    tailCall,
                    path) is not { } operations)
                {
                    ok = false;
                    break;
                }

                if (operations.Length > 0)
                {
                    sawSliceOperation = true;
                }
            }

            if (ok && sawSliceOperation)
            {
                yield return path;
            }
        }
    }

    private static ImmutableArray<ImmutableSliceOperation>?
        TryParseParamPathAsSliceOperationsInFunctionApplication(
        StaticExpression<DeclQualifiedName>.FunctionApplication funcAppExpr,
        IReadOnlyList<int> paramPath)
    {
        var (subexpr, pathRemaining) =
            StaticExpressionExtension.GetSubexpressionAtPath(funcAppExpr.Arguments, paramPath);

        if (pathRemaining.Count is not 0)
        {
            return null;
        }

        var builder = new List<ImmutableSliceOperation>();

        if (!TryParseImmutableSliceOperationsRecursive(subexpr, paramPath, builder))
        {
            return null;
        }

        return builder.ToImmutableArray();
    }

    private static bool TryParseImmutableSliceOperationsRecursive(
        StaticExpression<DeclQualifiedName> expr,
        IReadOnlyList<int> paramPath,
        List<ImmutableSliceOperation> operations)
    {
        if (StaticExpressionExtension.TryParseAsPathToExpression(
            expr,
            StaticExpression<DeclQualifiedName>.EnvironmentInstance) is { } exprPath)
        {
            return IntPathEqualityComparer.Instance.Equals(exprPath, paramPath);
        }

        if (expr is StaticExpression<DeclQualifiedName>.KernelApplication kernelApp &&
            kernelApp.Input is StaticExpression<DeclQualifiedName>.List argsList &&
            argsList.Items.Count is 2)
        {
            var kind = kernelApp.Function switch
            {
                nameof(KernelFunction.skip) => SliceOperationKind.Skip,
                nameof(KernelFunction.take) => SliceOperationKind.Take,
                _ => (SliceOperationKind?)null,
            };

            if (kind is null)
            {
                return false;
            }

            var checkpoint = operations.Count;

            if (!TryParseImmutableSliceOperationsRecursive(argsList.Items[1], paramPath, operations))
            {
                if (operations.Count > checkpoint)
                {
                    operations.RemoveRange(checkpoint, operations.Count - checkpoint);
                }

                return false;
            }

            operations.Add(new ImmutableSliceOperation(kind.Value, argsList.Items[0]));
            return true;
        }

        return false;
    }

    public readonly record struct CompiledStatement(
        StatementSyntax Statement,
        ImmutableHashSet<string> DeclaredLocals);

    public static CompiledStatement CompileToCSharpStatement(
        StaticExpression<DeclQualifiedName> expression,
        ExpressionEmitEnv emitEnv,
        System.Func<StaticExpression<DeclQualifiedName>, LocalDeclarations, IReadOnlyList<StatementSyntax>> statementsFromResult,
        IReadOnlySet<string> blockedDeclarations)
    {
        bool IgnoreSubexpressionCollectingForCSE(
            StaticExpression<DeclQualifiedName> expr)
        {
            if (expr is StaticExpression<DeclQualifiedName>.Literal)
            {
                return true;
            }

            if (emitEnv.AlreadyDeclared.ContainsKey(expr))
            {
                return true;
            }

            if (StaticExpressionExtension.TryParseAsPathToExpression(
                expr,
                StaticExpression<DeclQualifiedName>.EnvironmentInstance) is { } pathToEnv)
            {
                // TODO: Precise condition (will change when we lift the 2-level limit on parameters).

                if (pathToEnv.Count <= 2 &&
                   emitEnv.FunctionEnv.SelfFunctionInterface(pathToEnv) is { })
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

        var mutatedDeclared = emitEnv.AlreadyDeclared;

        // The current approach to common subexpression elimination has a drawback:
        // We collect subexpressions for common subexpression elimination (CSE) and declare them as locals up-front
        // in 'newDeclaredStatements'. Later, while enumerating expressions, we sometimes replace these generic
        // subexpressions with more specialized forms (e.g., via kernel function specialization or parameter mapping).
        // As a consequence, some of the CSE locals are never referenced by the emitted statements anymore and become
        // superfluous.
        //
        // Workaround implemented here:
        // - Still declare potential CSE locals first and extend 'alreadyDeclared' so downstream compilation can reuse them.
        // - After the branches/statements are emitted, analyze the actual identifier usage in those statements and
        //   filter 'newDeclaredStatements' to only those locals that are actually referenced. We also compute the
        //   transitive closure of dependencies between locals to ensure we keep any prerequisite locals.
        // - Finally, we prepend only the filtered declarations before the emitted statements.

        // TODO: For a precise solution: Figure out which subexpressions will be needed in the final emitted statements first.
        // This is necessary also because we are seeing the oposite problem: Some subexpressions are now computed multiple times
        // because they did not get their entries in CSE.

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
                    CompileTypeSyntax.TypeSyntaxFromType(typeof(PineValue), emitEnv.FunctionEnv.DeclarationSyntaxContext))
                .WithVariables(
                    SyntaxFactory.SingletonSeparatedList(
                        SyntaxFactory.VariableDeclarator(
                            SyntaxFactory.Identifier(localName))
                        .WithInitializer(
                            SyntaxFactory.EqualsValueClause(
                                CompileToCSharpExpression(
                                    subexpr,
                                    emitEnv.AddAlreadyDeclared(mutatedDeclared))
                                .AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext))))));

            newDeclaredStatements.Add(statement);

            mutatedDeclared =
                mutatedDeclared.SetItem(subexpr, (localName, LocalType.Evaluated));
        }

        var newEmitEnv =
            emitEnv.AddAlreadyDeclared(mutatedDeclared);

        if (expression is StaticExpression<DeclQualifiedName>.Conditional conditionalExpr)
        {
            var conditionExpr =
                BuildConditionExpression(
                    conditionalExpr.Condition,
                    newEmitEnv);

            // Compile true branch first, using current blocked + newly declared names
            var blockedAfterPrefix = blockedDeclarations.Union(newlyDeclaredLocals).ToImmutableHashSet();

            var trueBranchCompiled =
                CompileToCSharpStatement(
                    conditionalExpr.TrueBranch,
                    newEmitEnv,
                    statementsFromResult,
                    blockedAfterPrefix);

            var falseBranchCompiled =
                CompileToCSharpStatement(
                    conditionalExpr.FalseBranch,
                    newEmitEnv,
                    statementsFromResult,
                    blockedAfterPrefix.Union(trueBranchCompiled.DeclaredLocals));

            var filtered =
                FilteredLocalsResult.FilterDeclarationsByUsage(
                    newDeclaredStatements,
                    [conditionExpr, trueBranchCompiled.Statement, falseBranchCompiled.Statement],
                    newlyDeclaredLocals.ToImmutable());

            var unionLocals =
                filtered.KeptLocalNames
                .Union(trueBranchCompiled.DeclaredLocals)
                .Union(falseBranchCompiled.DeclaredLocals);

            // If the 'if' block ends with a return/throw/continue, inline false branch into outer scope
            if (BranchEndsWithExitOrLoop(trueBranchCompiled.Statement))
            {
                var ifStatementNoElse =
                    SyntaxFactory.IfStatement(
                        condition: conditionExpr,
                        statement: trueBranchCompiled.Statement,
                        @else: null);

                IReadOnlyList<StatementSyntax> allStatementsNoElse =
                    [
                        .. filtered.Declarations,
                        ifStatementNoElse,
                        .. ExtractStatements(falseBranchCompiled.Statement)
                    ];

                return new CompiledStatement(SyntaxFactory.Block(allStatementsNoElse), unionLocals);
            }

            // Else branch compiled normally; still pass union to keep names globally unique

            var ifStatement =
                SyntaxFactory.IfStatement(
                    condition: conditionExpr,
                    statement: trueBranchCompiled.Statement,
                    @else: SyntaxFactory.ElseClause(falseBranchCompiled.Statement));

            IReadOnlyList<StatementSyntax> allStatements =
                [
                .. filtered.Declarations,
                ifStatement,
                ];

            return new CompiledStatement(SyntaxFactory.Block(allStatements), unionLocals);
        }

        {
            var resultStatements = statementsFromResult(expression, newEmitEnv.AlreadyDeclared);

            var filtered =
                FilteredLocalsResult.FilterDeclarationsByUsage(
                    newDeclaredStatements,
                    resultStatements,
                    newlyDeclaredLocals.ToImmutable());

            IReadOnlyList<StatementSyntax> allStatements =
                [
                .. filtered.Declarations,
                .. resultStatements
                ];

            return new CompiledStatement(SyntaxFactory.Block(allStatements), filtered.KeptLocalNames);
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
        ExpressionEmitEnv emitEnv)
    {
        return
            EnumerateExpressions(
                expression,
                emitEnv)
            .FirstOrDefault()!;
    }

    public static IEnumerable<CompiledCSharpExpression> EnumerateExpressions(
        StaticExpression<DeclQualifiedName> expression,
        ExpressionEmitEnv emitEnv)
    {
        if (emitEnv.FunctionEnv.GeneralOverride(expression) is { } overridden)
        {
            return [overridden];
        }

        // Check for reverse(concat_builder) pattern with local concat builders
        if (expression is StaticExpression<DeclQualifiedName>.KernelApplication reverseKernelApp &&
            reverseKernelApp.Function is nameof(KernelFunction.reverse))
        {
            // Check if the input is a concat builder local
            if (emitEnv.AlreadyDeclared.TryGetValue(reverseKernelApp.Input, out var builderLocal) &&
                builderLocal.ltype is LocalType.ImmutableConcatBuilder)
            {
                var evaluateReverseExpr =
                    PineCSharpSyntaxFactory.EvaluateImmutableConcatBuilderReverseSyntax(
                        SyntaxFactory.IdentifierName(builderLocal.identifier));

                return [CompiledCSharpExpression.Generic(evaluateReverseExpr)];
            }
        }

        if (emitEnv.AlreadyDeclared.TryGetValue(expression, out var existingVar))
        {
            return existingVar.ltype switch
            {
                LocalType.Evaluated =>
                [CompiledCSharpExpression.Generic(
                    SyntaxFactory.IdentifierName(existingVar.identifier))
                ],

                LocalType.ImmutableConcatBuilder =>
                [CompiledCSharpExpression.Generic(
                    PineCSharpSyntaxFactory.EvaluateImmutableConcatBuilderSyntax(
                        SyntaxFactory.IdentifierName(existingVar.identifier)))
                ],

                LocalType.ImmutableSliceBuilder =>
                [CompiledCSharpExpression.Generic(
                    PineCSharpSyntaxFactory.EvaluateImmutableSliceBuilderSyntax(
                        SyntaxFactory.IdentifierName(existingVar.identifier)))
                ],

                _ =>
                throw new System.NotImplementedException(
                    "Internal error: Unknown local type: " + existingVar.ltype),
            };
        }

        (ExpressionSyntax, IReadOnlyList<int>)? FindNearestParameterForPathInEnv(
            IReadOnlyList<int> pathInEnv)
        {
            // Try each possible prefix of pathInEnv to find a parameter

            for (var len = pathInEnv.Count; len >= 1; len--)
            {
                var prefix = pathInEnv.Take(len).ToImmutableArray();

                if (emitEnv.FunctionEnv.SelfFunctionInterface(prefix) is { } paramRef)
                {
                    var remainingPath = pathInEnv.Skip(len).ToImmutableArray();

                    return (paramRef, remainingPath);
                }
            }

            return null;
        }

        var pathsFromParametersAndLocals =
            new List<(ExpressionSyntax paramRef, IReadOnlyList<int> remainingPath)>();

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
                        emitEnv.FunctionEnv.DeclarationSyntaxContext);

                pathsFromParametersAndLocals.Add((paramRef, remainingPath));
            }
        }

        {
            (IReadOnlyList<int> pathInSubexpr, StaticExpression<DeclQualifiedName> subexpr)? lastSubExpr = null;

            foreach (var current in StaticExpressionExtension.InterpretAsPathReversed(expression))
            {
                lastSubExpr = current;

                if (emitEnv.AlreadyDeclared.TryGetValue(current.subexpr, out var typelLocal))
                {
                    if (typelLocal.ltype == LocalType.Evaluated)
                    {
                        pathsFromParametersAndLocals.Add(
                            (SyntaxFactory.IdentifierName(typelLocal.identifier),
                            current.pathInSubexpr));
                    }
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
                            emitEnv);

                    pathsFromParametersAndLocals.Add(
                        (subExprCompiled.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext),
                        lastSubExpr.Value.pathInSubexpr));
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
                    emitEnv.FunctionEnv.DeclarationSyntaxContext);

            return [CompiledCSharpExpression.Generic(fromPathGeneric)];
        }

        if (expression is StaticExpression<DeclQualifiedName>.Literal literal)
        {
            return
                EnumerateExpressionsForLiteral(
                    literal,
                    emitEnv.FunctionEnv.AvailableValueDeclarations,
                    emitEnv.FunctionEnv.DeclarationSyntaxContext);
        }

        if (expression is StaticExpression<DeclQualifiedName>.List list)
        {
            var itemExprs =
                list.Items
                .Select(item =>
                CompileToCSharpExpression(
                    item,
                    emitEnv))
                .ToImmutableArray();

            var collectionExprs =
                SyntaxFactory.CollectionExpression(
                    SyntaxFactory.SeparatedList<CollectionElementSyntax>(
                        itemExprs
                        .Select(itemExpr => SyntaxFactory.ExpressionElement(itemExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext)))));

            // Invoke PineValue.List( ... )

            var genericCSharpExpr =
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        CompileTypeSyntax.TypeSyntaxFromType(
                            typeof(PineValue),
                            emitEnv.FunctionEnv.DeclarationSyntaxContext),
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
                    emitEnv);
        }

        if (expression is StaticExpression<DeclQualifiedName>.KernelApplication kernelApp)
        {
            return
                EnumerateExpressionsForKernelApp(
                    kernelApp,
                    emitEnv);
        }

        if (expression is StaticExpression<DeclQualifiedName>.FunctionApplication funcApp)
        {
            if (!emitEnv.FunctionEnv.AvailableFunctions.TryGetValue(funcApp.FunctionName, out var funcInterface))
            {
                throw new System.Exception(
                    "Function application references unknown function '" + funcApp.FunctionName + ".");
            }

            var arguments =
                funcInterface.ParamsPaths
                .Select(argumentPath =>
                ExpressionsForFunctionArgument(
                    argumentPath,
                    funcApp.Arguments,
                    emitEnv))
                .ToImmutableArray();

            var genericCSharpExpr =
                SyntaxFactory.InvocationExpression(
                    SyntaxFactory.ParseName(funcApp.FunctionName.FullName))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList(
                            [
                            .. arguments.Select(argExpr =>
                            SyntaxFactory.Argument(argExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext)))
                            ])));

            return [CompiledCSharpExpression.Generic(genericCSharpExpr)];
        }

        if (expression is StaticExpression<DeclQualifiedName>.CrashingParseAndEval parseAndEval)
        {
            var renderedEncodedExpr =
                CompileToCSharpExpression(
                    parseAndEval.Encoded,
                    emitEnv);

            var genericCSharpExpr =
                PineCSharpSyntaxFactory.ThrowParseExpressionException(
                    SyntaxFactory.LiteralExpression(
                        SyntaxKind.StringLiteralExpression,
                        SyntaxFactory.Literal("TODO: Include details from encoded and env subexpressions")),
                    emitEnv.FunctionEnv.DeclarationSyntaxContext);

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
        ExpressionEmitEnv emitEnv)
    {
        var conditionExpr =
            BuildConditionExpression(
                conditional.Condition,
                emitEnv);

        var trueBranchExprs =
            EnumerateExpressions(
                conditional.TrueBranch,
                emitEnv)
            .ToImmutableArray();

        var falseBranchExprs =
            EnumerateExpressions(
                conditional.FalseBranch,
                emitEnv)
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
                    trueBranchExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext)),
                whenFalse:
                CompiledCSharpExpression.EnsureIsParenthesizedForComposition(
                    falseBranchExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext)));

        yield return CompiledCSharpExpression.Generic(genericCSharpExpr);
    }

    public static ExpressionSyntax BuildConditionExpression(
        StaticExpression<DeclQualifiedName> condition,
        ExpressionEmitEnv emitEnv)
    {
        if (emitEnv.FunctionEnv.GeneralOverride(condition) is { } overridden)
        {
            return overridden.AsBooleanValue(emitEnv.FunctionEnv.DeclarationSyntaxContext);
        }

        var conditionAsAndChain = ParseAsAndChain(condition);

        var andChainConjunctsCompiled =
            conditionAsAndChain
            .Select(expr =>
                CompileToCSharpExpression(
                    expr,
                    emitEnv))
            .ToImmutableArray();

        if (andChainConjunctsCompiled.Length is 0)
        {
            return PineCSharpSyntaxFactory.ExpressionSyntaxForBooleanLiteral(true);
        }

        if (andChainConjunctsCompiled.Length is 1)
        {
            return andChainConjunctsCompiled[0].AsBooleanValue(emitEnv.FunctionEnv.DeclarationSyntaxContext);
        }

        // Combine conjuncts with '&&'

        return
            andChainConjunctsCompiled
            .Select(e => e.AsBooleanValue(emitEnv.FunctionEnv.DeclarationSyntaxContext))
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
        ExpressionEmitEnv emitEnv)
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
                            emitEnv)
                        .ToImmutableArray();

                    var rightExprs =
                        EnumerateExpressions(
                            listInput.Items[1],
                            emitEnv)
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
                                    leftExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext)),
                                CompiledCSharpExpression.EnsureIsParenthesizedForComposition(
                                    rightExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext)));

                        yield return CompiledCSharpExpression.Boolean(booleanCSharpExpr);
                    }
                }
            }
        }

        if (kernelApp.Function is nameof(KernelFunction.negate))
        {
            if (kernelApp.Input is StaticExpression<DeclQualifiedName> inputExpr)
            {
                var inputSequences =
                    EnumerateExpressions(
                        inputExpr,
                        emitEnv);

                foreach (var inputSequence in inputSequences)
                {
                    if (inputSequence.Type is CompiledCSharpExpression.ValueType.Boolean)
                    {
                        var booleanCSharpExpr =
                             SyntaxFactory.PrefixUnaryExpression(
                                 SyntaxKind.LogicalNotExpression,
                                 CompiledCSharpExpression.EnsureIsParenthesizedForComposition(
                                     inputSequence.ExpressionSyntax));

                        yield return CompiledCSharpExpression.Boolean(booleanCSharpExpr);

                        break;
                    }
                }
            }
        }

        var resultsFromFusion =
            CompileKernelFunctionApplication.TryCompileKernelFusion(
                kernelApp,
                emitEnv);

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
                            emitEnv)
                        .AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext);

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
                    CompileKernelFunctionApplication.EnumerateMatchingSpecializedInterface(
                        specializedInterfaces,
                        [kernelApp.Input],
                        isCommutative: false,
                        TryRenderArgument,
                        emitEnv.FunctionEnv.DeclarationSyntaxContext);

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
                    CompileKernelFunctionApplication.EnumerateMatchingSpecializedInterface(
                        specializedInterfaces,
                        argumentsList.Items,
                        isCommutative,
                        TryRenderArgument,
                        emitEnv.FunctionEnv.DeclarationSyntaxContext);

                foreach (var match in matches)
                {
                    yield return match;
                }
            }
        }

        {
            var inputExpr =
                CompileToCSharpExpression(
                    kernelApp.Input,
                    emitEnv);

            if (PineKernelFunctions.CompileKernelFunctionGenericInvocation(
                kernelApp.Function,
                inputExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext),
                emitEnv.FunctionEnv.DeclarationSyntaxContext)
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
                            emitEnv.FunctionEnv.DeclarationSyntaxContext),
                        SyntaxFactory.IdentifierName(nameof(KernelFunction.ApplyKernelFunctionGeneric))))
                .WithArgumentList(
                    SyntaxFactory.ArgumentList(
                        SyntaxFactory.SeparatedList(
                            [
                            SyntaxFactory.Argument(
                            SyntaxFactory.LiteralExpression(
                                SyntaxKind.StringLiteralExpression,
                                SyntaxFactory.Literal(kernelApp.Function))),
                        SyntaxFactory.Argument(inputExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext))
                            ])));

            yield return CompiledCSharpExpression.Generic(genericCSharpExpr);
        }
    }

    public static IEnumerable<CompiledCSharpExpression> ExpressionsForFunctionArgument(
        IReadOnlyList<int> paramPath,
        StaticExpression<DeclQualifiedName> argumentExpr,
        ExpressionEmitEnv emitEnv)
    {
        if (StaticExpressionExtension.GetSubexpressionAtPath(argumentExpr, paramPath) is { } subexpr)
        {
            var renderedExpr =
                EnumerateExpressions(
                    subexpr.subexpr,
                    emitEnv);

            if (subexpr.pathRemaining.Count is 0)
            {
                return renderedExpr;
            }

            var withRemainingPathExpr =
                PineCSharpSyntaxFactory.BuildCSharpExpressionToGetItemFromPathOrEmptyList(
                    renderedExpr.AsGenericValue(emitEnv.FunctionEnv.DeclarationSyntaxContext),
                    subexpr.pathRemaining,
                    emitEnv.FunctionEnv.DeclarationSyntaxContext);

            return
                [CompiledCSharpExpression.Generic(withRemainingPathExpr)];
        }

        throw new System.NotImplementedException(
            "Failed to find subexpression at path [" + string.Join(',', paramPath) + "] in argument expression.");
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
         * 
         * Additionally, expressions that appear in ALL branches of a conditional should be treated as
         * effectively unconditional relative to that conditional.
         * */

        // First pass: identify expressions that appear in all branches of conditionals
        var expressionsCommonToAllBranches = FindExpressionsCommonToAllBranches(rootExpressions, ignoreExpr);

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

            // Treat expressions common to all branches as unconditional
            var effectivelyConditional = current.conditional && !expressionsCommonToAllBranches.Contains(current.expr);

            if (seenOnceUnconditional.Contains(current.expr) ||
                (seenOnceConditional.Contains(current.expr) && !effectivelyConditional))
            {
                yield return current.expr;

                collected.Add(current.expr);

                continue;
            }

            if (effectivelyConditional)
            {
                seenOnceConditional.Add(current.expr);
            }
            else
            {
                seenOnceUnconditional.Add(current.expr);
            }

            ProcessSubexpressions(
                current.expr,
                EnqueueIfNoRoot,
                current.conditional);
        }
    }

    private static HashSet<StaticExpression<FuncId>> FindExpressionsCommonToAllBranches<FuncId>(
        IReadOnlyList<StaticExpression<FuncId>> rootExpressions,
        System.Func<StaticExpression<FuncId>, bool> ignoreExpr)
    {
        var result = new HashSet<StaticExpression<FuncId>>();

        foreach (var rootExpr in rootExpressions)
        {
            CollectCommonSubexpressionsRecursive(
                rootExpr,
                ignoreExpr,
                commonExprs: result,
                visited: []);
        }

        return result;
    }

    private static void ProcessSubexpressions<FuncId>(
        StaticExpression<FuncId> expr,
        System.Action<StaticExpression<FuncId>, bool> processSubexpression,
        bool parentIsConditional)
    {
        if (expr is StaticExpression<FuncId>.Conditional conditional)
        {
            processSubexpression(conditional.Condition, parentIsConditional);
            processSubexpression(conditional.TrueBranch, true);
            processSubexpression(conditional.FalseBranch, true);
            return;
        }

        if (expr is StaticExpression<FuncId>.Literal)
        {
            return;
        }

        if (expr is StaticExpression<FuncId>.Environment)
        {
            return;
        }

        if (expr is StaticExpression<FuncId>.List list)
        {
            foreach (var item in list.Items)
            {
                processSubexpression(item, parentIsConditional);
            }
            return;
        }

        if (expr is StaticExpression<FuncId>.KernelApplication kernelApp)
        {
            processSubexpression(kernelApp.Input, parentIsConditional);
            return;
        }

        if (expr is StaticExpression<FuncId>.FunctionApplication funcApp)
        {
            processSubexpression(funcApp.Arguments, parentIsConditional);
            return;
        }

        if (expr is StaticExpression<FuncId>.CrashingParseAndEval parseAndEval)
        {
            processSubexpression(parseAndEval.Encoded, parentIsConditional);
            processSubexpression(parseAndEval.EnvironmentExpr, parentIsConditional);
            return;
        }

        throw new System.NotImplementedException(
            "CSE collection for expression type " + expr.GetType() + " is not implemented.");
    }

    private static void CollectCommonSubexpressionsRecursive<FuncId>(
        StaticExpression<FuncId> expr,
        System.Func<StaticExpression<FuncId>, bool> ignoreExpr,
        HashSet<StaticExpression<FuncId>> commonExprs,
        HashSet<StaticExpression<FuncId>> visited)
    {
        if (!visited.Add(expr) || ignoreExpr(expr))
        {
            return;
        }

        if (expr is StaticExpression<FuncId>.Conditional conditional)
        {
            // Recursively process the condition
            CollectCommonSubexpressionsRecursive(conditional.Condition, ignoreExpr, commonExprs, visited);

            // Collect all subexpressions from both branches
            var trueBranchExprs = new HashSet<StaticExpression<FuncId>>();
            CollectAllSubexpressionsRecursive(conditional.TrueBranch, ignoreExpr, trueBranchExprs, []);

            var falseBranchExprs = new HashSet<StaticExpression<FuncId>>();
            CollectAllSubexpressionsRecursive(conditional.FalseBranch, ignoreExpr, falseBranchExprs, []);

            // Find expressions common to both branches and add them to the result
            var branchCommonExprs = trueBranchExprs.Intersect(falseBranchExprs);

            commonExprs.UnionWith(branchCommonExprs);

            // Continue recursing into both branches to find nested conditionals
            CollectCommonSubexpressionsRecursive(conditional.TrueBranch, ignoreExpr, commonExprs, visited);
            CollectCommonSubexpressionsRecursive(conditional.FalseBranch, ignoreExpr, commonExprs, visited);

            return;
        }

        // For non-conditional expressions, recurse using the unified traversal
        ProcessSubexpressions(
            expr,
            (subExpr, _) => CollectCommonSubexpressionsRecursive(subExpr, ignoreExpr, commonExprs, visited),
            parentIsConditional: false);
    }

    private static void CollectAllSubexpressionsRecursive<FuncId>(
        StaticExpression<FuncId> expr,
        System.Func<StaticExpression<FuncId>, bool> ignoreExpr,
        HashSet<StaticExpression<FuncId>> allExprs,
        HashSet<StaticExpression<FuncId>> visited)
    {
        if (!visited.Add(expr) || ignoreExpr(expr))
        {
            return;
        }

        allExprs.Add(expr);

        // Use the unified traversal to process subexpressions
        ProcessSubexpressions(
            expr,
            (subExpr, _) => CollectAllSubexpressionsRecursive(subExpr, ignoreExpr, allExprs, visited),
            parentIsConditional: false);
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
}
