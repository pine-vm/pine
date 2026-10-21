using Pine.Core.CodeGen;
using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

using SyntaxTypes = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Compiles Elm compiler IR expressions to Pine expressions.
/// Uses recursive mapping for type-safe expression handling.
/// </summary>
public partial class ExpressionCompiler
{
    /// <summary>
    /// Compiles an Elm expression to a Pine expression.
    /// </summary>
    /// <param name="expression">The Elm expression to compile.</param>
    /// <param name="context">The compilation context.</param>
    /// <returns>A result containing the compiled Pine expression or a compilation error.</returns>
    public static Result<CompilationError, Expression> Compile(
        SyntaxTypes.Expression expression,
        ExpressionCompilationContext context) =>
        expression switch
        {
            SyntaxTypes.Expression.Identifier expr =>
            CompileResolvedReference(expr.Reference, context),

            SyntaxTypes.Expression.LiteralValue expr =>
            Expression.LitralInst(expr.Value),

            SyntaxTypes.Expression.Application expr =>
            CompileApplication(expr, context),

            SyntaxTypes.Expression.ListExpr expr =>
            CompileListExpr(expr, context),

            SyntaxTypes.Expression.ProjectItem expr =>
            CompileProjectItem(expr, context),

            SyntaxTypes.Expression.RecordExpr expr =>
            CompileRecordExpr(expr, context),

            SyntaxTypes.Expression.OperatorApplication expr =>
            CompileOperatorApplication(expr, context),

            SyntaxTypes.Expression.PrefixOperator expr =>
            CompilePrefixOperator(expr),

            SyntaxTypes.Expression.Negation expr =>
            CompileNegation(expr, context),

            SyntaxTypes.Expression.IfBlock expr =>
            CompileIfBlock(expr, context),

            SyntaxTypes.Expression.CaseExpression expr =>
            CompileCaseExpression(expr, context),

            SyntaxTypes.Expression.LetExpression expr =>
            CompileLetExpression(expr, context),

            SyntaxTypes.Expression.RecordUpdateExpression expr =>
            CompileRecordUpdateExpression(expr, context),

            SyntaxTypes.Expression.RecordAccess expr =>
            CompileRecordAccess(expr, context),

            SyntaxTypes.Expression.RecordAccessFunction expr =>
            CompileRecordAccessFunction(expr),

            SyntaxTypes.Expression.GLSLExpression =>
            CompilationError.UnsupportedExpression(expression.GetType().Name),

            _ =>
            throw new NotImplementedException(
                $"{nameof(Compile)} does not handle expression variant: {expression.GetType().Name}")
        };

    private static Result<CompilationError, Expression> CompileProjectItem(
        SyntaxTypes.Expression.ProjectItem projection,
        ExpressionCompilationContext context) =>
        Compile(projection.Value, context)
        .Map(value => BuiltinHelpers.ApplyBuiltinHead(BuiltinHelpers.ApplyBuiltinSkip(projection.Index, value)));

    private static Result<CompilationError, Expression> CompileListExpr(
        SyntaxTypes.Expression.ListExpr expr,
        ExpressionCompilationContext context)
    {
        var compiledItems = new Expression[expr.Elements.Count];

        for (var i = 0; i < expr.Elements.Count; i++)
        {
            var item = expr.Elements[i];

            var itemResult = Compile(item, context);

            if (itemResult.IsErrOrNull() is { } err)
            {
                return
                    CompilationError.Scoped(
                        "When compiling list element [" + i + "]",
                        err);
            }

            if (itemResult.IsOkOrNull() is not { } itemOk)
            {
                throw new NotImplementedException(
                    "Unexpected result type when compiling list element [" + i + "]: " + item.GetType());
            }

            compiledItems[i] = itemOk;
        }

        return Expression.ListInst(compiledItems);
    }

    private static Result<CompilationError, Expression> CompileRecordExpr(
        SyntaxTypes.Expression.RecordExpr expr,
        ExpressionCompilationContext context)
    {
        // Sort fields by name alphabetically (as per Elm record encoding)
        var sortedFields =
            expr.Fields
            .OrderBy(f => f.FieldName, StringComparer.Ordinal)
            .ToList();

        var compiledFieldItems = new List<Expression>();

        foreach (var field in sortedFields)
        {
            var fieldName = field.FieldName;
            var fieldValueExpr = field.Value;

            var compiledValue = Compile(fieldValueExpr, context);

            if (compiledValue.IsErrOrNull() is { } err)
            {
                return err;
            }

            // New flat layout: emit name and value as adjacent items in the outer list.
            compiledFieldItems.Add(
                Expression.LitralInst(StringEncoding.ValueFromString(fieldName)));

            compiledFieldItems.Add(compiledValue.IsOkOrNull()!);
        }

        // A record is encoded as [recordTypeTag, name0, value0, name1, value1, ...]
        return
            Expression.ListInst(
                [
                Expression.LitralInst(ElmValue.ElmRecordTypeTagNameAsValue),
                ..compiledFieldItems
                ]);
    }

    private static Result<CompilationError, Expression> CompileRecordUpdateExpression(
        SyntaxTypes.Expression.RecordUpdateExpression expr,
        ExpressionCompilationContext context)
    {
        var receiver = Compile(expr.Receiver, context);

        if (receiver.IsErrOrNull() is { } receiverError)
            return receiverError;

        var recordExpr = receiver.Extract(error => throw new InvalidOperationException(error.ToString()));

        // Compile each field update and sort by field name alphabetically.
        // This ordering is required because the runtime update function relies on both 
        // the record fields and updates being sorted in the same order to efficiently 
        // merge them in a single pass.
        var sortedFields =
            expr.Fields
            .OrderBy(f => f.FieldName, StringComparer.Ordinal)
            .ToList();

        // Compile update values
        var compiledUpdateValues = new List<(string fieldName, Expression valueExpr)>();

        foreach (var field in sortedFields)
        {
            var fieldName = field.FieldName;
            var fieldValueExpr = field.Value;

            var compiledValue = Compile(fieldValueExpr, context);

            if (compiledValue.IsErrOrNull() is { } err)
            {
                return err;
            }

            compiledUpdateValues.Add((fieldName, compiledValue.IsOkOrNull()!));
        }

        // Fall back to runtime field update when type is not known
        var compiledUpdates = new List<Expression>();

        foreach (var (fieldName, valueExpr) in compiledUpdateValues)
        {
            // Each update is encoded as [fieldName, newValue]
            var updatePair =
                Expression.ListInst(
                    [
                    Expression.LitralInst(StringEncoding.ValueFromString(fieldName)),
                    valueExpr
                    ]);

            compiledUpdates.Add(updatePair);
        }

        var updatesListExpr = Expression.ListInst(compiledUpdates);

        // Build the call to the record update function
        // The environment for the function is: [record, updates]
        var callEnv = Expression.ListInst([recordExpr, updatesListExpr]);

        return
            new Expression.Eval(
                encoded: Expression.LitralInst(RecordRuntime.PineFunctionForRecordUpdateAsValue),
                environment: callEnv);
    }

    private static Result<CompilationError, Expression> CompileRecordAccess(
        SyntaxTypes.Expression.RecordAccess expr,
        ExpressionCompilationContext context)
    {
        // Compile the record expression
        var recordResult = Compile(expr.Record, context);

        if (recordResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        var recordExpr = recordResult.IsOkOrNull()!;
        var fieldName = expr.FieldName;

        // Static reads have already become ProjectItem nodes in the IR.
        // Build the call to the record access function
        // The environment for the function is: [record, fieldName]
        var callEnv =
            Expression.ListInst(
                [
                recordExpr,
                Expression.LitralInst(StringEncoding.ValueFromString(fieldName))
                ]);

        return
            new Expression.Eval(
                encoded: Expression.LitralInst(RecordRuntime.PineFunctionForRecordAccessAsValue),
                environment: callEnv);
    }

    /// <summary>
    /// Compiles a record access function expression like <c>.fieldName</c> to a one-argument
    /// function value that extracts a field from a record.
    /// </summary>
    private static Result<CompilationError, Expression> CompileRecordAccessFunction(
        SyntaxTypes.Expression.RecordAccessFunction expr)
    {
        // The parser stores RecordAccessFunction.FunctionName including the
        // leading dot (e.g. ".fieldName") to mirror the upstream elm-syntax
        // representation. The runtime field-lookup function in
        // RecordRuntime.PineFunctionForRecordAccessAsValue compares against
        // the stored field name, which does NOT include the dot, so we
        // strip it here. Without this, the lookup never matches and falls
        // through to the "field not found" branch, producing
        // PineValue.EmptyList — which then crashes the surrounding
        // ParseAndEval with an "expressionValue is string ''" / empty-list
        // diagnostic when the result is invoked or used as a function.
        var fieldName = expr.FieldName;

        // Build a 1-argument function: \record -> record.fieldName
        // env = [envFunctions, record], so the parameter is at env[1]
        var recordParam = BuiltinHelpers.BuildPathToParameter(0);

        // Build the record field lookup body using the runtime function
        var callEnv =
            Expression.ListInst(
                [
                recordParam,
                Expression.LitralInst(StringEncoding.ValueFromString(fieldName))
                ]);

        var body =
            new Expression.Eval(
                encoded: Expression.LitralInst(RecordRuntime.PineFunctionForRecordAccessAsValue),
                environment: callEnv);

        var functionValue =
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                body,
                parameterCount: 1,
                envFunctions: [])
            ??
            throw new NotImplementedException(
                "Failed to build function value template for record access function.");

        return Expression.LitralInst(functionValue);
    }

    private static Result<CompilationError, Expression> CompileOperatorApplication(
        SyntaxTypes.Expression.OperatorApplication expr,
        ExpressionCompilationContext context) =>
        OperatorCompiler.Compile(expr, context);

    /// <summary>
    /// Compiles a prefix operator expression like <c>(+)</c> to a function value.
    /// Prefix operators are operators used as functions, e.g., <c>(+) 1 2</c> instead of <c>1 + 2</c>.
    /// </summary>
    private static Result<CompilationError, Expression> CompilePrefixOperator(
        SyntaxTypes.Expression.PrefixOperator expr)
    {
        // Handle :: (cons) operator specially - it's from List, not Basics
        if (expr.Operator is "::")
        {
            return Expression.LitralInst(s_consFunction.Value);
        }

        // Delegate to BasicArithmetic for centralized operator handling
        var prefixExpr = CoreLibraryModule.CoreBasics.GetPrefixOperatorExpression(expr.Operator);

        if (prefixExpr is null)
        {
            return CompilationError.UnsupportedOperator(expr.Operator);
        }

        return prefixExpr;
    }

    private static readonly Lazy<PineValue> s_consFunction = new(BuildConsFunction);

    /// <summary>
    /// Builds a cons (::) function value: a 2-argument function that prepends head to tail list.
    /// </summary>
    private static PineValue BuildConsFunction()
    {
        var headExpr = BuiltinHelpers.BuildPathToParameter(0);

        var tailExpr = BuiltinHelpers.BuildPathToParameter(1);

        // head :: tail  ==>  concat([[head], tail])
        var singletonList = Expression.ListInst([headExpr]);

        var body =
            BuiltinHelpers.ApplyBuiltinConcat(
                [singletonList, tailExpr]);

        return
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                body,
                parameterCount: 2,
                envFunctions: [])
            ??
            throw new NotImplementedException(
                "Failed to build function value template for cons (::) operator.");
    }

    private static Result<CompilationError, Expression> CompileNegation(
        SyntaxTypes.Expression.Negation expr,
        ExpressionCompilationContext context)
    {
        var innerResult = Compile(expr.Expression, context);

        if (innerResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        return CoreLibraryModule.CoreBasics.Generic_Negate(innerResult.IsOkOrNull()!);
    }

    private static Result<CompilationError, Expression> CompileIfBlock(
        SyntaxTypes.Expression.IfBlock expr,
        ExpressionCompilationContext context)
    {
        var conditionResult = Compile(expr.Condition, context);

        if (conditionResult.IsErrOrNull() is { } condErr)
        {
            return condErr;
        }

        var trueBranchResult = Compile(expr.ThenBlock, context);

        if (trueBranchResult.IsErrOrNull() is { } trueErr)
        {
            return trueErr;
        }

        var falseBranchResult = Compile(expr.ElseBlock, context);

        if (falseBranchResult.IsErrOrNull() is { } falseErr)
        {
            return falseErr;
        }

        return
            Expression.ConditionalInst(
                condition: conditionResult.IsOkOrNull()!,
                falseBranch: falseBranchResult.IsOkOrNull()!,
                trueBranch: trueBranchResult.IsOkOrNull()!);
    }

    private static Result<CompilationError, Expression> CompileCaseExpression(
        SyntaxTypes.Expression.CaseExpression expr,
        ExpressionCompilationContext context) =>
        PatternCompiler.CompileCaseExpression(expr, context);

    private static Result<CompilationError, Expression> CompileLetExpression(
        SyntaxTypes.Expression.LetExpression expr,
        ExpressionCompilationContext context) =>
        CompileLetExpressionBody(expr, context);

    private static Result<CompilationError, Expression> CompileLetExpressionBody(
        SyntaxTypes.Expression.LetExpression letBlock, ExpressionCompilationContext context)
    {
        var bindings = new Dictionary<CompilerModel.BindingId, Expression>(context.LocalBindings);

        var infos =
            letBlock.Declarations.Select(
                (declaration, index) =>
                {
                    var names = new HashSet<CompilerModel.BindingId>();
                    PatternCompiler.CollectPatternBindings(declaration.Pattern, names);
                    var dependencies = new HashSet<CompilerModel.BindingId>();
                    CollectBindingReferences(declaration.Expression, dependencies);
                    return (index, names, dependencies);
                }).ToList();

        var sorted = TopologicalSortDeclarations(infos);

        if (sorted.IsErrOrNull() is { } error)
            return error;

        var inner = context.WithReplacedLocalBindings(bindings);

        foreach (var index in sorted.Extract(error => throw new InvalidOperationException(error.ToString())))
        {
            var declaration = letBlock.Declarations[index];
            var result = Compile(declaration.Expression, inner);

            if (result.IsErrOrNull() is { } compileError)
                return compileError;

            foreach (var (id, value) in PatternCompiler.ExtractPatternBindings(
                declaration.Pattern,
                result.Extract(error => throw new InvalidOperationException(error.ToString()))))
            {
                bindings[id] = value;
            }
        }

        return Compile(letBlock.Expression, inner);
    }

    internal static void CollectBindingReferences(
        SyntaxTypes.Expression expression,
        HashSet<CompilerModel.BindingId> refs)
    {
        if (expression is SyntaxTypes.Expression.Identifier { Reference: CompilerModel.Reference.Local local })
        {
            refs.Add(local.Binding);
            return;
        }

        if (expression is SyntaxTypes.Expression.LetExpression let)
        {
            var free = new HashSet<CompilerModel.BindingId>();

            foreach (var declaration in let.Declarations)
                CollectBindingReferences(declaration.Expression, free);

            CollectBindingReferences(let.Expression, free);

            foreach (var declaration in let.Declarations)
                free.ExceptWith(CompilerModel.ExpressionTransform.PatternBindings(declaration.Pattern));

            refs.UnionWith(free);
            return;
        }

        if (expression is SyntaxTypes.Expression.CaseExpression match)
        {
            CollectBindingReferences(match.Expression, refs);

            foreach (var branch in match.Cases)
            {
                var free = new HashSet<CompilerModel.BindingId>();
                CollectBindingReferences(branch.Expression, free);
                free.ExceptWith(CompilerModel.ExpressionTransform.PatternBindings(branch.Pattern));
                refs.UnionWith(free);
            }

            return;
        }

        foreach (var child in SyntaxTypes.ExpressionTraversal.Children(expression))
            CollectBindingReferences(child, refs);
    }

    /// <summary>
    /// Performs topological sort of declarations based on their dependencies.
    /// Returns a Result with either the sorted indices or a CyclicDependency error.
    /// </summary>
    private static Result<CompilationError, List<int>> TopologicalSortDeclarations(
        List<(int index, HashSet<CompilerModel.BindingId> names, HashSet<CompilerModel.BindingId> deps)> declarations)
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
        List<(int index, HashSet<CompilerModel.BindingId> names, HashSet<CompilerModel.BindingId> deps)> declarations)
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
        List<(int index, HashSet<CompilerModel.BindingId> names, HashSet<CompilerModel.BindingId> deps)> declarations)
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
            var cycleNames =
                declarations
                .Where((_, i) => !result.Contains(i))
                .SelectMany(d => d.names)
                .Select(name => name.ToString())
                .ToList();

            return (result, cycleNames);
        }

        return (result, null);
    }

    /// <summary>
    /// Compiles a reference to a top-level function as a function value.
    /// This is used when a function is used as a value (not applied), such as:
    /// - Returning a function from a case expression branch
    /// - Passing a function as an argument
    /// - Storing a function in a data structure
    /// </summary>
    private static Expression EmitChoiceTypeTagPartialApplicationExpression(
        string tagName,
        int totalArgCount,
        IReadOnlyList<Expression> capturedArgExpressions)
    {
        var remainingArgCount = totalArgCount - capturedArgExpressions.Count;

        if (remainingArgCount <= 0)
        {
            // All arguments are already captured - just build the value directly
            var tagNameValue = Expression.LitralInst(StringEncoding.ValueFromString(tagName));

            return
                Expression.ListInst(
                    [
                    Expression.LitralInst(ElmValue.ElmChoiceTypeTagNameAsValue),
                    tagNameValue,
                    ..capturedArgExpressions
                    ]);
        }

        var tagNameLiteral = Expression.LitralInst(StringEncoding.ValueFromString(tagName));

        // Build the inner expression for the FULL tag arity. At invocation time the inner expression is
        // evaluated with environment [envFunctions, arg_0, ..., arg_{totalArgCount-1}], i.e. arg_i is at
        // path [1 + i]. This is the same inner expression used by the plain (zero captured args) function
        // value, so the partial application below produces an intermediate value identical to applying the
        // captured arguments to the plain function value.
        var allArgExpressions = new Expression[totalArgCount];

        for (var i = 0; i < totalArgCount; i++)
        {
            allArgExpressions[i] =
                ExpressionBuilder.BuildExpressionForPathInExpression(
                    [1 + i],
                    Expression.EnvironmentInstance);
        }

        // Build the choice type value expression for when all arguments are collected.
        var choiceTypeValueExpr =
            Expression.ListInst(
                [
                Expression.LitralInst(ElmValue.ElmChoiceTypeTagNameAsValue),
                tagNameLiteral,
                ..allArgExpressions
                ]);

        // Emit a compact template-based function value that only nests the remaining parameters and embeds
        // the already available (captured) argument expressions directly.
        return
            FunctionValueBuilder.EmitCurriedFunctionTemplateWithLeadingArgs(
                choiceTypeValueExpr,
                totalArgCount,
                capturedArgExpressions);
    }

    /// <summary>
    /// Emits a function value for a choice type tag that accepts arguments, with no captured arguments.
    /// This is used when the tag is used as a function value without any application.
    /// When fully applied, the function constructs the choice type value
    /// [ChoiceTypeMarker, TagName, arg0, arg1, ...].
    /// </summary>
    /// <param name="tagName">The name of the choice type tag.</param>
    /// <param name="argCount">The number of arguments the tag expects.</param>
    /// <returns>An expression that evaluates to a function value.</returns>
    private static Expression EmitChoiceTypeTagFunctionValue(
        string tagName,
        int argCount)
    {
        // Build the inner expression that constructs the choice type value
        // env = [envFunctions, arg0, arg1, ..., argN-1]
        var argExpressions = new Expression[argCount];

        for (var i = 0; i < argCount; i++)
        {
            argExpressions[i] = BuiltinHelpers.BuildPathToParameter(i);
        }

        var tagNameExpr = Expression.LitralInst(StringEncoding.ValueFromString(tagName));

        // Build the choice type value expression for when all arguments are collected
        var choiceTypeValueExpr =
            Expression.ListInst(
                [
                Expression.LitralInst(ElmValue.ElmChoiceTypeTagNameAsValue),
                tagNameExpr,
                ..argExpressions
                ]);

        // Build the function value that wraps this expression
        var functionValue =
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                choiceTypeValueExpr,
                argCount)
            ??
            throw new NotImplementedException(
                "Failed to build function value template for choice tag function.");

        return Expression.LitralInst(functionValue);
    }

    /// <summary>
    /// Builds a record value expression from field names and argument expressions.
    /// The field names are in declaration order and are mapped to the argument expressions,
    /// then sorted alphabetically for the record representation.
    /// </summary>
    private static Expression BuildRecordValueExpression(
        IReadOnlyList<string> fieldNamesInDeclOrder,
        IReadOnlyList<Expression> argExpressions)
    {
        // Pair field names (declaration order) with argument expressions, then sort alphabetically
        var sortedPairs =
            fieldNamesInDeclOrder
            .Select((fieldName, index) => (fieldName, expr: argExpressions[index]))
            .OrderBy(pair => pair.fieldName, StringComparer.Ordinal)
            .ToList();

        var recordFieldItems = new List<Expression>(sortedPairs.Count * 2);

        foreach (var pair in sortedPairs)
        {
            recordFieldItems.Add(
                Expression.LitralInst(StringEncoding.ValueFromString(pair.fieldName)));

            recordFieldItems.Add(pair.expr);
        }

        return
            Expression.ListInst(
                [
                Expression.LitralInst(ElmValue.ElmRecordTypeTagNameAsValue),
                ..recordFieldItems
                ]);
    }

    /// <summary>
    /// Emits a function value for a record type alias constructor that accepts all field arguments.
    /// This is used when the record constructor is used as a function value without any application,
    /// e.g., passing <c>Point</c> to a higher-order function like <c>List.map2 Point xs ys</c>.
    /// When fully applied, the function constructs the record with fields sorted alphabetically.
    /// </summary>
    /// <param name="fieldNamesInDeclOrder">Field names in the order they appear in the type alias declaration.</param>
    /// <returns>An expression that evaluates to a function value.</returns>
    private static Expression EmitRecordConstructorFunctionValue(
        IReadOnlyList<string> fieldNamesInDeclOrder)
    {
        var argCount = fieldNamesInDeclOrder.Count;

        // Build expressions that reference arguments from the environment.
        // At invocation time, environment is [arg0, arg1, ..., argN]
        var argExpressions = new Expression[argCount];

        for (var i = 0; i < argCount; i++)
        {
            argExpressions[i] = BuiltinHelpers.BuildPathToParameter(i);
        }

        // Build the record value expression
        var recordValueExpr = BuildRecordValueExpression(fieldNamesInDeclOrder, argExpressions);

        // Build the function value that wraps this expression
        var functionValue =
            FunctionValueBuilder.TryBuildCurriedFunctionValueAsTemplate(
                recordValueExpr,
                argCount)
            ??
            throw new NotImplementedException(
                "Failed to build function value template for record constructor function.");

        return Expression.LitralInst(functionValue);
    }

    /// <summary>
    /// Emits an expression that, when evaluated, produces a function value for a partially applied 
    /// record type alias constructor. The captured arguments are evaluated at runtime and stored 
    /// in the function value's environment.
    /// </summary>
    /// <param name="fieldNamesInDeclOrder">Field names in the order they appear in the type alias declaration.</param>
    /// <param name="capturedArgExpressions">Expressions for arguments already provided (to be evaluated at runtime).</param>
    /// <returns>An expression that evaluates to a function value.</returns>
    private static Expression EmitRecordConstructorPartialApplicationExpression(
        IReadOnlyList<string> fieldNamesInDeclOrder,
        IReadOnlyList<Expression> capturedArgExpressions)
    {
        var totalArgCount = fieldNamesInDeclOrder.Count;
        var remainingArgCount = totalArgCount - capturedArgExpressions.Count;

        if (remainingArgCount <= 0)
        {
            // All arguments are already captured - just build the record value directly.
            return BuildRecordValueExpression(fieldNamesInDeclOrder, capturedArgExpressions);
        }

        // Build the inner expression for the FULL constructor arity. At invocation time the inner expression
        // is evaluated with environment [envFunctions, arg_0, ..., arg_{totalArgCount-1}], i.e. arg_i is at
        // path [1 + i]. This is the same inner expression used by the plain (zero captured args) function
        // value, so the partial application below produces an intermediate value identical to applying the
        // captured arguments to the plain function value.
        var allArgExpressions = new Expression[totalArgCount];

        for (var i = 0; i < totalArgCount; i++)
        {
            allArgExpressions[i] = BuiltinHelpers.BuildPathToParameter(i);
        }

        // Build the record value expression
        var recordValueExpr = BuildRecordValueExpression(fieldNamesInDeclOrder, allArgExpressions);

        // Emit a compact template-based function value that only nests the remaining parameters and embeds
        // the already available (captured) argument expressions directly.
        return
            FunctionValueBuilder.EmitCurriedFunctionTemplateWithLeadingArgs(
                recordValueExpr,
                totalArgCount,
                capturedArgExpressions);
    }

    /// <summary>
    /// Compiles a generic function application where the function is a value
    /// (e.g., from a parameter or local binding).
    /// Uses ParseAndEval to apply each argument one at a time to the function value.
    /// Per the spec: "For function applications where the function is a value of unknown origin,
    /// the compiler emits an expression that adds the given arguments using a form that allows
    /// for generic partial application."
    /// </summary>
    private static Expression CompileGenericFunctionApplication(
        Expression functionExpr,
        IReadOnlyList<Expression> arguments)
    {
        // Apply arguments one at a time using ParseAndEval
        // Each application evaluates the function value (which is an encoded expression)
        // with the argument as the environment
        var result = functionExpr;

        foreach (var argument in arguments)
        {
            // ParseAndEval where:
            // - encoded = the function value (which is an encoded expression)
            // - environment = the argument value
            result =
                new Expression.Eval(
                    encoded: result,
                    environment: argument);
        }

        return result;
    }

    internal static PineValue EmitStringLiteral(string str) =>
        ElmValueEncoding.StringAsPineValue(str);

    internal static PineValue EmitIntegerLiteral(BigInteger value) =>
        IntegerEncoding.EncodeSignedInteger(value);

    internal static PineValue EmitCharLiteral(int value) =>
        ElmValueEncoding.ElmCharAsPineValue(value);

    internal static PineValue EmitBooleanLiteral(bool value) =>
        BuiltinFunction.ValueFromBool(value);

}
