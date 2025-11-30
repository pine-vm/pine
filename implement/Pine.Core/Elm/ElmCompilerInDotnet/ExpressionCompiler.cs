using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Compiles Elm expressions to Pine expressions.
/// Uses the visitor pattern for type-safe expression handling.
/// </summary>
public class ExpressionCompiler
    : SyntaxTypes.ExpressionVisitorBase<ExpressionCompilationContext, Result<CompilationError, Expression>>
{
    /// <summary>
    /// Shared instance of the expression compiler.
    /// </summary>
    public static ExpressionCompiler Instance { get; } = new();

    /// <summary>
    /// Compiles an Elm expression to a Pine expression.
    /// </summary>
    /// <param name="expression">The Elm expression to compile.</param>
    /// <param name="context">The compilation context.</param>
    /// <returns>A result containing the compiled Pine expression or a compilation error.</returns>
    public Result<CompilationError, Expression> Compile(
        SyntaxTypes.Expression expression,
        ExpressionCompilationContext context) =>
        Visit(expression, context);

    /// <inheritdoc/>
    protected override Result<CompilationError, Expression> VisitUnknown(
        SyntaxTypes.Expression expr,
        ExpressionCompilationContext context) =>
        new CompilationError.UnsupportedExpression(expr.GetType().Name);

    /// <inheritdoc/>
    public override Result<CompilationError, Expression> VisitInteger(
        SyntaxTypes.Expression.Integer expr,
        ExpressionCompilationContext context) =>
        Expression.LiteralInstance(EmitIntegerLiteral(expr.Value));

    /// <inheritdoc/>
    public override Result<CompilationError, Expression> VisitLiteral(
        SyntaxTypes.Expression.Literal expr,
        ExpressionCompilationContext context) =>
        Expression.LiteralInstance(EmitStringLiteral(expr.Value));

    /// <inheritdoc/>
    public override Result<CompilationError, Expression> VisitCharLiteral(
        SyntaxTypes.Expression.CharLiteral expr,
        ExpressionCompilationContext context) =>
        Expression.LiteralInstance(EmitCharLiteral(expr.Value));

    /// <inheritdoc/>
    public override Result<CompilationError, Expression> VisitFunctionOrValue(
        SyntaxTypes.Expression.FunctionOrValue expr,
        ExpressionCompilationContext context)
    {
        if (expr.ModuleName.Count is 0)
        {
            // Check if it's a local binding from pattern matching first
            if (context.TryGetLocalBinding(expr.Name, out var bindingExpr) && bindingExpr is not null)
            {
                return bindingExpr;
            }

            // Check if it's a parameter reference
            if (context.TryGetParameterIndex(expr.Name, out var paramIndex))
            {
                return BuiltinHelpers.BuildPathToParameter(paramIndex);
            }

            // Check if it's a choice type tag (starts with uppercase letter)
            if (ElmValueEncoding.StringIsValidTagName(expr.Name))
            {
                // This is a choice type constructor with no arguments
                return Expression.LiteralInstance(ElmValueEncoding.TagAsPineValue(expr.Name, []));
            }
        }

        if (expr.ModuleName.Count is 1 && expr.ModuleName[0] is "Basics")
        {
            if (expr.Name is "True")
            {
                return Expression.LiteralInstance(EmitBooleanLiteral(true));
            }

            if (expr.Name is "False")
            {
                return Expression.LiteralInstance(EmitBooleanLiteral(false));
            }
        }

        // After canonicalization, tags have a module name but are still recognized
        // by having an uppercase first letter
        if (ElmValueEncoding.StringIsValidTagName(expr.Name))
        {
            return Expression.LiteralInstance(ElmValueEncoding.TagAsPineValue(expr.Name, []));
        }

        return new CompilationError.UnresolvedReference(expr.Name, context.CurrentModuleName);
    }

    /// <inheritdoc/>
    public override Result<CompilationError, Expression> VisitApplication(
        SyntaxTypes.Expression.Application expr,
        ExpressionCompilationContext context)
    {
        if (expr.Arguments.Count < 2)
        {
            return new CompilationError.ApplicationTooFewArguments(expr.Arguments.Count);
        }

        var firstArg = expr.Arguments[0].Value;

        // Check if this is a Pine_kernel application
        if (firstArg is SyntaxTypes.Expression.FunctionOrValue kernelFunc &&
            kernelFunc.ModuleName.Count is 1 &&
            context.ModuleCompilationContext.IsPineKernelModule(kernelFunc.ModuleName[0]))
        {
            var kernelInput = expr.Arguments[1].Value;
            var compiledInputResult = Visit(kernelInput, context);

            if (compiledInputResult.IsErrOrNull() is { } err)
            {
                return err;
            }

            return Expression.KernelApplicationInstance(
                kernelFunc.Name,
                compiledInputResult.IsOkOrNull()!);
        }

        // Check if this is a function application or choice type tag application
        if (firstArg is SyntaxTypes.Expression.FunctionOrValue funcRef)
        {
            // Check if this is a choice type tag application
            if (ElmValueEncoding.StringIsValidTagName(funcRef.Name))
            {
                var tagNameValue = Expression.LiteralInstance(StringEncoding.ValueFromString(funcRef.Name));

                var compiledArguments = new List<Expression>();
                for (var i = 1; i < expr.Arguments.Count; i++)
                {
                    var argResult = Visit(expr.Arguments[i].Value, context);
                    if (argResult.IsErrOrNull() is { } err)
                    {
                        return err;
                    }
                    compiledArguments.Add(argResult.IsOkOrNull()!);
                }

                return Expression.ListInstance(
                [
                    tagNameValue,
                    Expression.ListInstance(compiledArguments)
                ]);
            }

            // Determine qualified function name
            string qualifiedFunctionName = funcRef.ModuleName.Count > 0
                ? string.Join(".", funcRef.ModuleName) + "." + funcRef.Name
                : context.CurrentModuleName + "." + funcRef.Name;

            var functionIndex = context.GetFunctionIndexInLayout(qualifiedFunctionName);
            if (functionIndex < 0)
            {
                return new CompilationError.FunctionNotInDependencyLayout(qualifiedFunctionName);
            }

            var argumentResult = Visit(expr.Arguments[1].Value, context);
            if (argumentResult.IsErrOrNull() is { } argErr)
            {
                return argErr;
            }

            var functionRef = ExpressionBuilder.BuildExpressionForPathInExpression(
                [0, functionIndex],
                Expression.EnvironmentInstance);

            var functionList = ExpressionBuilder.BuildExpressionForPathInExpression(
                [0],
                Expression.EnvironmentInstance);

            var callEnvironment = Expression.ListInstance(
            [
                functionList,
                Expression.ListInstance([argumentResult.IsOkOrNull()!])
            ]);

            return new Expression.ParseAndEval(
                encoded: functionRef,
                environment: callEnvironment);
        }

        return new CompilationError.UnsupportedApplicationType();
    }

    /// <inheritdoc/>
    public override Result<CompilationError, Expression> VisitListExpr(
        SyntaxTypes.Expression.ListExpr expr,
        ExpressionCompilationContext context)
    {
        var compiledElements = new List<Expression>();
        foreach (var elem in expr.Elements)
        {
            var result = Visit(elem.Value, context);
            if (result.IsErrOrNull() is { } err)
            {
                return err;
            }
            compiledElements.Add(result.IsOkOrNull()!);
        }

        return Expression.ListInstance(compiledElements);
    }

    /// <inheritdoc/>
    public override Result<CompilationError, Expression> VisitOperatorApplication(
        SyntaxTypes.Expression.OperatorApplication expr,
        ExpressionCompilationContext context) =>
        OperatorCompiler.Compile(expr, context, this);

    /// <inheritdoc/>
    public override Result<CompilationError, Expression> VisitParenthesizedExpression(
        SyntaxTypes.Expression.ParenthesizedExpression expr,
        ExpressionCompilationContext context) =>
        Visit(expr.Expression.Value, context);

    /// <inheritdoc/>
    public override Result<CompilationError, Expression> VisitNegation(
        SyntaxTypes.Expression.Negation expr,
        ExpressionCompilationContext context)
    {
        if (expr.Expression.Value is SyntaxTypes.Expression.Integer intLiteral)
        {
            return Expression.LiteralInstance(EmitIntegerLiteral(-intLiteral.Value));
        }

        var innerResult = Visit(expr.Expression.Value, context);
        if (innerResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        var negativeOne = Expression.LiteralInstance(EmitIntegerLiteral(-1));
        return BuiltinHelpers.ApplyBuiltinIntMul([negativeOne, innerResult.IsOkOrNull()!]);
    }

    /// <inheritdoc/>
    public override Result<CompilationError, Expression> VisitIfBlock(
        SyntaxTypes.Expression.IfBlock expr,
        ExpressionCompilationContext context)
    {
        var conditionResult = Visit(expr.Condition.Value, context);
        if (conditionResult.IsErrOrNull() is { } condErr)
        {
            return condErr;
        }

        var trueBranchResult = Visit(expr.ThenBlock.Value, context);
        if (trueBranchResult.IsErrOrNull() is { } trueErr)
        {
            return trueErr;
        }

        var falseBranchResult = Visit(expr.ElseBlock.Value, context);
        if (falseBranchResult.IsErrOrNull() is { } falseErr)
        {
            return falseErr;
        }

        return Expression.ConditionalInstance(
            condition: conditionResult.IsOkOrNull()!,
            falseBranch: falseBranchResult.IsOkOrNull()!,
            trueBranch: trueBranchResult.IsOkOrNull()!);
    }

    /// <inheritdoc/>
    public override Result<CompilationError, Expression> VisitCaseExpression(
        SyntaxTypes.Expression.CaseExpression expr,
        ExpressionCompilationContext context) =>
        PatternCompiler.CompileCaseExpression(expr.CaseBlock, context, this);

    /// <inheritdoc/>
    public override Result<CompilationError, Expression> VisitLetExpression(
        SyntaxTypes.Expression.LetExpression expr,
        ExpressionCompilationContext context) =>
        CompileLetExpression(expr.Value, context);

    private Result<CompilationError, Expression> CompileLetExpression(
        SyntaxTypes.Expression.LetBlock letBlock,
        ExpressionCompilationContext context)
    {
        var newBindings = new Dictionary<string, Expression>();

        if (context.LocalBindings is { } existingBindings)
        {
            foreach (var kvp in existingBindings)
            {
                newBindings[kvp.Key] = kvp.Value;
            }
        }

        var declarations = letBlock.Declarations;
        var declarationInfos = new List<(int index, HashSet<string> names, HashSet<string> deps)>();

        for (var i = 0; i < declarations.Count; i++)
        {
            var decl = declarations[i].Value;
            var names = new HashSet<string>();
            var deps = new HashSet<string>();

            switch (decl)
            {
                case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                    var funcName = letFunc.Function.Declaration.Value.Name.Value;
                    names.Add(funcName);
                    CollectExpressionReferences(letFunc.Function.Declaration.Value.Expression.Value, deps);
                    break;

                case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestructuring:
                    PatternCompiler.CollectPatternNames(letDestructuring.Pattern.Value, names);
                    CollectExpressionReferences(letDestructuring.Expression.Value, deps);
                    break;
            }

            declarationInfos.Add((i, names, deps));
        }

        var allBoundNames = new HashSet<string>();
        foreach (var info in declarationInfos)
        {
            foreach (var name in info.names)
            {
                allBoundNames.Add(name);
            }
        }

        var sortedIndicesResult = TopologicalSortDeclarations(declarationInfos);
        if (sortedIndicesResult.IsErrOrNull() is { } sortErr)
        {
            return sortErr;
        }

        var sortedIndices = sortedIndicesResult.IsOkOrNull()!;
        var letContext = context.WithReplacedLocalBindings(newBindings);

        foreach (var idx in sortedIndices)
        {
            var decl = declarations[idx].Value;

            switch (decl)
            {
                case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                    var funcName = letFunc.Function.Declaration.Value.Name.Value;
                    var funcBody = letFunc.Function.Declaration.Value.Expression.Value;
                    var funcArgs = letFunc.Function.Declaration.Value.Arguments;

                    if (funcArgs.Count is 0)
                    {
                        var compiledBodyResult = Visit(funcBody, letContext);

                        if (compiledBodyResult.IsErrOrNull() is { } bodyErr)
                        {
                            return bodyErr;
                        }

                        newBindings[funcName] = compiledBodyResult.IsOkOrNull()!;
                    }
                    else
                    {
                        return new CompilationError.UnsupportedLetFunctionWithParameters(funcName);
                    }
                    break;

                case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestructuring:
                    var destructuredResult = Visit(letDestructuring.Expression.Value, letContext);
                    if (destructuredResult.IsErrOrNull() is { } destrErr)
                    {
                        return destrErr;
                    }

                    var patternBindings = PatternCompiler.ExtractPatternBindings(
                        letDestructuring.Pattern.Value,
                        destructuredResult.IsOkOrNull()!);

                    foreach (var kvp in patternBindings)
                    {
                        newBindings[kvp.Key] = kvp.Value;
                    }
                    break;
            }
        }

        return Visit(letBlock.Expression.Value, letContext);
    }

    #region Helper Methods

    internal static void CollectExpressionReferences(
        SyntaxTypes.Expression expression,
        HashSet<string> refs)
    {
        switch (expression)
        {
            case SyntaxTypes.Expression.FunctionOrValue funcOrVal:
                if (funcOrVal.ModuleName.Count is 0)
                {
                    refs.Add(funcOrVal.Name);
                }
                break;

            case SyntaxTypes.Expression.Application app:
                foreach (var arg in app.Arguments)
                {
                    CollectExpressionReferences(arg.Value, refs);
                }
                break;

            case SyntaxTypes.Expression.ListExpr listExpr:
                foreach (var elem in listExpr.Elements)
                {
                    CollectExpressionReferences(elem.Value, refs);
                }
                break;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                CollectExpressionReferences(opApp.Left.Value, refs);
                CollectExpressionReferences(opApp.Right.Value, refs);
                break;

            case SyntaxTypes.Expression.ParenthesizedExpression paren:
                CollectExpressionReferences(paren.Expression.Value, refs);
                break;

            case SyntaxTypes.Expression.Negation neg:
                CollectExpressionReferences(neg.Expression.Value, refs);
                break;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                CollectExpressionReferences(ifBlock.Condition.Value, refs);
                CollectExpressionReferences(ifBlock.ThenBlock.Value, refs);
                CollectExpressionReferences(ifBlock.ElseBlock.Value, refs);
                break;

            case SyntaxTypes.Expression.LetExpression letExpr:
                var localNames = new HashSet<string>();
                foreach (var decl in letExpr.Value.Declarations)
                {
                    switch (decl.Value)
                    {
                        case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                            localNames.Add(letFunc.Function.Declaration.Value.Name.Value);
                            break;

                        case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                            PatternCompiler.CollectPatternNames(letDestr.Pattern.Value, localNames);
                            break;
                    }
                }

                var innerRefs = new HashSet<string>();
                foreach (var decl in letExpr.Value.Declarations)
                {
                    switch (decl.Value)
                    {
                        case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                            CollectExpressionReferences(letFunc.Function.Declaration.Value.Expression.Value, innerRefs);
                            break;

                        case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                            CollectExpressionReferences(letDestr.Expression.Value, innerRefs);
                            break;
                    }
                }
                CollectExpressionReferences(letExpr.Value.Expression.Value, innerRefs);

                foreach (var innerRef in innerRefs)
                {
                    if (!localNames.Contains(innerRef))
                    {
                        refs.Add(innerRef);
                    }
                }
                break;
        }
    }

    /// <summary>
    /// Performs topological sort of declarations based on their dependencies.
    /// Returns a Result with either the sorted indices or a CyclicDependency error.
    /// </summary>
    private static Result<CompilationError, List<int>> TopologicalSortDeclarations(
        List<(int index, HashSet<string> names, HashSet<string> deps)> declarations)
    {
        var (result, cycleNames) = TopologicalSortDeclarationsCore(declarations);

        if (cycleNames is not null)
        {
            return new CompilationError.CyclicDependency(cycleNames);
        }

        return result;
    }

    /// <summary>
    /// Performs topological sort of declarations based on their dependencies.
    /// Throws InvalidOperationException if a cycle is detected.
    /// </summary>
    internal static List<int> TopologicalSortDeclarationsOrThrow(
        List<(int index, HashSet<string> names, HashSet<string> deps)> declarations)
    {
        var (result, cycleNames) = TopologicalSortDeclarationsCore(declarations);

        if (cycleNames is not null)
        {
            throw new InvalidOperationException(
                $"Circular dependency detected in let declarations: {string.Join(", ", cycleNames)}");
        }

        return result;
    }

    private static (List<int> result, IReadOnlyList<string>? cycleNames) TopologicalSortDeclarationsCore(
        List<(int index, HashSet<string> names, HashSet<string> deps)> declarations)
    {
        var declarationCount = declarations.Count;
        var inDegree = new int[declarationCount];
        var dependents = new List<int>[declarationCount];

        for (var i = 0; i < declarationCount; i++)
        {
            dependents[i] = [];
        }

        for (var i = 0; i < declarationCount; i++)
        {
            var deps = declarations[i].deps;

            for (var j = 0; j < declarationCount; j++)
            {
                if (i == j)
                    continue;

                var otherNames = declarations[j].names;

                if (deps.Overlaps(otherNames))
                {
                    inDegree[i]++;
                    dependents[j].Add(i);
                }
            }
        }

        var result = new List<int>();
        var queue = new Queue<int>();

        for (var i = 0; i < declarationCount; i++)
        {
            if (inDegree[i] is 0)
            {
                queue.Enqueue(i);
            }
        }

        while (queue.Count > 0)
        {
            var current = queue.Dequeue();
            result.Add(current);

            foreach (var dependent in dependents[current])
            {
                inDegree[dependent]--;

                if (inDegree[dependent] is 0)
                {
                    queue.Enqueue(dependent);
                }
            }
        }

        if (result.Count != declarationCount)
        {
            var cycleNames = declarations
                .Where((_, i) => !result.Contains(i))
                .SelectMany(d => d.names)
                .ToList();
            return (result, cycleNames);
        }

        return (result, null);
    }

    internal static PineValue EmitStringLiteral(string str) =>
        ElmValueEncoding.StringAsPineValue(str);

    internal static PineValue EmitIntegerLiteral(BigInteger value) =>
        IntegerEncoding.EncodeSignedInteger(value);

    internal static PineValue EmitCharLiteral(int value) =>
        ElmValueEncoding.ElmCharAsPineValue(value);

    internal static PineValue EmitBooleanLiteral(bool value) =>
        KernelFunction.ValueFromBool(value);

    #endregion
}
