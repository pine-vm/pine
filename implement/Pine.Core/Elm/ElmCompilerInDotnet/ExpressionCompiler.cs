using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Compiles Elm expressions to Pine expressions.
/// Uses recursive mapping for type-safe expression handling.
/// </summary>
public class ExpressionCompiler
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
    public static Result<CompilationError, Expression> Compile(
        SyntaxTypes.Expression expression,
        ExpressionCompilationContext context) =>
        expression switch
        {
            SyntaxTypes.Expression.Integer expr =>
                CompileInteger(expr),

            SyntaxTypes.Expression.Literal expr =>
                CompileLiteral(expr),

            SyntaxTypes.Expression.CharLiteral expr =>
                CompileCharLiteral(expr),

            SyntaxTypes.Expression.FunctionOrValue expr =>
                CompileFunctionOrValue(expr, context),

            SyntaxTypes.Expression.Application expr =>
                CompileApplication(expr, context),

            SyntaxTypes.Expression.ListExpr expr =>
                CompileListExpr(expr, context),

            SyntaxTypes.Expression.TupledExpression expr =>
                CompileTupledExpression(expr, context),

            SyntaxTypes.Expression.RecordExpr expr =>
                CompileRecordExpr(expr, context),

            SyntaxTypes.Expression.OperatorApplication expr =>
                CompileOperatorApplication(expr, context),

            SyntaxTypes.Expression.ParenthesizedExpression expr =>
                CompileParenthesizedExpression(expr, context),

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

            _ =>
                new CompilationError.UnsupportedExpression(expression.GetType().Name)
        };

    private static Result<CompilationError, Expression> CompileInteger(
        SyntaxTypes.Expression.Integer expr) =>
        Expression.LiteralInstance(EmitIntegerLiteral(expr.Value));

    private static Result<CompilationError, Expression> CompileLiteral(
        SyntaxTypes.Expression.Literal expr) =>
        Expression.LiteralInstance(EmitStringLiteral(expr.Value));

    private static Result<CompilationError, Expression> CompileCharLiteral(
        SyntaxTypes.Expression.CharLiteral expr) =>
        Expression.LiteralInstance(EmitCharLiteral(expr.Value));

    private static Result<CompilationError, Expression> CompileFunctionOrValue(
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

    private static Result<CompilationError, Expression> CompileApplication(
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
            var compiledInputResult = Compile(kernelInput, context);

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
            // Check if this is a record type alias constructor application
            // Record type alias constructors also have uppercase names, so check this first
            var qualifiedConstructorName =
                funcRef.ModuleName.Count > 0
                ?
                string.Join(".", funcRef.ModuleName) + "." + funcRef.Name
                :
                context.CurrentModuleName + "." + funcRef.Name;

            if (context.ModuleCompilationContext.TryGetRecordConstructorFieldNames(qualifiedConstructorName) is { } fieldNamesInDeclOrder)
            {
                // This is a record type alias constructor application
                // Arguments are in declaration order (same as fieldNamesInDeclOrder)
                // We need to construct a record with fields sorted alphabetically

                var argumentCount = expr.Arguments.Count - 1;  // First arg is the constructor itself
                if (argumentCount != fieldNamesInDeclOrder.Count)
                {
                    // Partial application not supported yet - fall through to other handling
                    // For now, only handle full application
                    return new CompilationError.UnsupportedExpression(
                        $"Record constructor partial application not supported: {funcRef.Name} expects {fieldNamesInDeclOrder.Count} arguments but got {argumentCount}");
                }

                // Compile all arguments
                var compiledArguments = new List<Expression>();
                for (var i = 1; i < expr.Arguments.Count; i++)
                {
                    var argResult = Compile(expr.Arguments[i].Value, context);
                    if (argResult.IsErrOrNull() is { } err)
                    {
                        return err;
                    }
                    compiledArguments.Add(argResult.IsOkOrNull()!);
                }

                // Create pairs of (fieldName, argExpression) in declaration order
                var fieldArgPairs = fieldNamesInDeclOrder
                    .Select((fieldName, index) => (fieldName, expr: compiledArguments[index]))
                    .ToList();

                // Sort pairs alphabetically by field name for the record representation
                var sortedPairs = fieldArgPairs
                    .OrderBy(pair => pair.fieldName, StringComparer.Ordinal)
                    .ToList();

                // Build the record field expressions
                var recordFieldExprs = sortedPairs
                    .Select(pair => Expression.ListInstance(
                    [
                        Expression.LiteralInstance(StringEncoding.ValueFromString(pair.fieldName)),
                        pair.expr
                    ]))
                    .ToList();

                // Build the record: [ElmRecordTag, [[field1, field2, ...]]]
                return Expression.ListInstance(
                [
                    Expression.LiteralInstance(ElmValue.ElmRecordTypeTagNameAsValue),
                    Expression.ListInstance([Expression.ListInstance(recordFieldExprs)])
                ]);
            }

            // Check if this is a choice type tag application
            if (ElmValueEncoding.StringIsValidTagName(funcRef.Name))
            {
                var tagNameValue = Expression.LiteralInstance(StringEncoding.ValueFromString(funcRef.Name));

                var compiledArguments = new List<Expression>();
                for (var i = 1; i < expr.Arguments.Count; i++)
                {
                    var argResult = Compile(expr.Arguments[i].Value, context);
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
            var qualifiedFunctionName =
                funcRef.ModuleName.Count > 0
                ?
                string.Join(".", funcRef.ModuleName) + "." + funcRef.Name
                :
                context.CurrentModuleName + "." + funcRef.Name;

            if (context.GetFunctionIndexInLayout(qualifiedFunctionName) is not { } functionIndex)
            {
                return new CompilationError.FunctionNotInDependencyLayout(qualifiedFunctionName);
            }

            var argumentResult = Compile(expr.Arguments[1].Value, context);
            if (argumentResult.IsErrOrNull() is { } argErr)
            {
                return argErr;
            }

            var functionRef = ExpressionBuilder.BuildExpressionForPathInExpression(
                [0, functionIndex],
                Expression.EnvironmentInstance);

            // Build the environment for the called function
            // We need to construct an environment that matches what the called function expects
            Expression callEnvFunctions;

            // Check if we have pre-computed dependency layout for the called function
            if (context.ModuleCompilationContext.TryGetDependencyLayout(qualifiedFunctionName) is { } calledFuncLayout &&
                calledFuncLayout.Count > 0)
            {
                // Build environment by mapping the called function's dependency layout
                // to indices in the current function's environment
                var envFunctionExpressions = new List<Expression>();

                foreach (var depName in calledFuncLayout)
                {
                    if (context.GetFunctionIndexInLayout(depName) is { } depIndex)
                    {
                        // Get this dependency from the current environment
                        var depRef =
                            ExpressionBuilder.BuildExpressionForPathInExpression(
                                [0, depIndex],
                                Expression.EnvironmentInstance);

                        envFunctionExpressions.Add(depRef);
                    }
                    else
                    {
                        // Dependency not found in current layout - this shouldn't happen
                        // if dependencies are properly analyzed
                        return new CompilationError.FunctionNotInDependencyLayout(depName);
                    }
                }

                callEnvFunctions = Expression.ListInstance(envFunctionExpressions);
            }
            else
            {
                // No dependency info available, fall back to passing current env functions
                callEnvFunctions = ExpressionBuilder.BuildExpressionForPathInExpression(
                    [0],
                    Expression.EnvironmentInstance);
            }

            var callEnvironment = Expression.ListInstance(
            [
                callEnvFunctions,
                Expression.ListInstance([argumentResult.IsOkOrNull()!])
            ]);

            return new Expression.ParseAndEval(
                encoded: functionRef,
                environment: callEnvironment);
        }

        return new CompilationError.UnsupportedApplicationType();
    }

    private static Result<CompilationError, Expression> CompileListExpr(
        SyntaxTypes.Expression.ListExpr expr,
        ExpressionCompilationContext context)
    {
        var compiledElements = new List<Expression>();
        foreach (var elem in expr.Elements)
        {
            var result = Compile(elem.Value, context);
            if (result.IsErrOrNull() is { } err)
            {
                return err;
            }
            compiledElements.Add(result.IsOkOrNull()!);
        }

        return Expression.ListInstance(compiledElements);
    }

    private static Result<CompilationError, Expression> CompileTupledExpression(
        SyntaxTypes.Expression.TupledExpression expr,
        ExpressionCompilationContext context)
    {
        // A tuple in Elm is represented as a list in Pine
        var compiledElements = new List<Expression>();
        foreach (var elem in expr.Elements)
        {
            var result = Compile(elem.Value, context);
            if (result.IsErrOrNull() is { } err)
            {
                return err;
            }
            compiledElements.Add(result.IsOkOrNull()!);
        }

        return Expression.ListInstance(compiledElements);
    }

    private static Result<CompilationError, Expression> CompileRecordExpr(
        SyntaxTypes.Expression.RecordExpr expr,
        ExpressionCompilationContext context)
    {
        // Sort fields by name alphabetically (as per Elm record encoding)
        var sortedFields = expr.Fields
            .OrderBy(f => f.Value.fieldName.Value, StringComparer.Ordinal)
            .ToList();

        var compiledFieldPairs = new List<Expression>();
        foreach (var field in sortedFields)
        {
            var fieldName = field.Value.fieldName.Value;
            var fieldValueExpr = field.Value.valueExpr.Value;

            var compiledValue = Compile(fieldValueExpr, context);
            if (compiledValue.IsErrOrNull() is { } err)
            {
                return err;
            }

            // Each field is encoded as [fieldName, fieldValue]
            var fieldPair = Expression.ListInstance(
            [
                Expression.LiteralInstance(StringEncoding.ValueFromString(fieldName)),
                compiledValue.IsOkOrNull()!
            ]);

            compiledFieldPairs.Add(fieldPair);
        }

        // A record is encoded as [recordTypeTag, [[field1, field2, ...]]]
        var fieldsListExpr = Expression.ListInstance(compiledFieldPairs);
        var innerListExpr = Expression.ListInstance([fieldsListExpr]);

        return Expression.ListInstance(
        [
            Expression.LiteralInstance(ElmValue.ElmRecordTypeTagNameAsValue),
            innerListExpr
        ]);
    }

    private static Result<CompilationError, Expression> CompileRecordUpdateExpression(
        SyntaxTypes.Expression.RecordUpdateExpression expr,
        ExpressionCompilationContext context)
    {
        // Get the original record value from the record name variable
        var recordName = expr.RecordName.Value;

        Expression recordExpr;
        TypeInference.InferredType.RecordType? recordType = null;

        // Check if it's a local binding first
        if (context.TryGetLocalBinding(recordName, out var bindingExpr) && bindingExpr is not null)
        {
            recordExpr = bindingExpr;
            // Try to get the type from local bindings
            if (context.TryGetLocalBindingType(recordName, out var localType) &&
                localType is TypeInference.InferredType.RecordType localRecordType)
            {
                recordType = localRecordType;
            }
        }
        // Check if it's a parameter reference
        else if (context.TryGetParameterIndex(recordName, out var paramIndex))
        {
            recordExpr = BuiltinHelpers.BuildPathToParameter(paramIndex);
            // Try to get the type from parameter types
            if (context.ParameterTypes.TryGetValue(recordName, out var paramType) &&
                paramType is TypeInference.InferredType.RecordType paramRecordType)
            {
                recordType = paramRecordType;
            }
        }
        else
        {
            return new CompilationError.UnresolvedReference(recordName, context.CurrentModuleName);
        }

        // Compile each field update and sort by field name alphabetically.
        // This ordering is required because the runtime update function relies on both 
        // the record fields and updates being sorted in the same order to efficiently 
        // merge them in a single pass.
        var sortedFields = expr.Fields
            .OrderBy(f => f.Value.fieldName.Value, StringComparer.Ordinal)
            .ToList();

        // Compile update values
        var compiledUpdateValues = new List<(string fieldName, Expression valueExpr)>();
        foreach (var field in sortedFields)
        {
            var fieldName = field.Value.fieldName.Value;
            var fieldValueExpr = field.Value.valueExpr.Value;

            var compiledValue = Compile(fieldValueExpr, context);
            if (compiledValue.IsErrOrNull() is { } err)
            {
                return err;
            }

            compiledUpdateValues.Add((fieldName, compiledValue.IsOkOrNull()!));
        }

        // If we know the record type at compile time, use direct field update
        if (recordType is not null)
        {
            // Build a map of field name to index
            var fieldIndices = recordType.Fields
                .Select((field, idx) => (field.FieldName, idx))
                .ToDictionary(x => x.FieldName, x => x.idx);

            // Check if all update field names exist in the record type
            var allFieldsKnown = compiledUpdateValues.All(u => fieldIndices.ContainsKey(u.fieldName));

            if (allFieldsKnown)
            {
                // Use compile-time field index computation
                return CompileRecordUpdateWithKnownIndices(
                    recordExpr,
                    recordType.Fields.Count,
                    compiledUpdateValues.Select(u => (fieldIndices[u.fieldName], u.valueExpr)).ToList());
            }
        }

        // Fall back to runtime field update when type is not known
        var compiledUpdates = new List<Expression>();
        foreach (var (fieldName, valueExpr) in compiledUpdateValues)
        {
            // Each update is encoded as [fieldName, newValue]
            var updatePair = Expression.ListInstance(
            [
                Expression.LiteralInstance(StringEncoding.ValueFromString(fieldName)),
                valueExpr
            ]);

            compiledUpdates.Add(updatePair);
        }

        var updatesListExpr = Expression.ListInstance(compiledUpdates);

        // Build the call to the record update function
        // The environment for the function is: [record, updates]
        var callEnv = Expression.ListInstance([recordExpr, updatesListExpr]);

        return new Expression.ParseAndEval(
            encoded: Expression.LiteralInstance(RecordRuntime.PineFunctionForRecordUpdateAsValue),
            environment: callEnv);
    }

    /// <summary>
    /// Compiles a record update when field indices are known at compile time.
    /// This avoids runtime iteration through fields.
    /// </summary>
    private static Expression CompileRecordUpdateWithKnownIndices(
        Expression recordExpr,
        int totalFieldCount,
        IReadOnlyList<(int fieldIndex, Expression newValue)> updates)
    {
        // Record structure: [ElmRecordTag, [[field1, field2, ...]]]
        // Each field is [fieldName, fieldValue]
        // 
        // We need to construct a new record with the same structure but updated values.
        // Strategy: Get the fields list, then construct a new list with updated values.

        // Get record tag
        var recordTagExpr = BuiltinHelpers.ApplyBuiltinHead(recordExpr);

        // Get record[1] (skip tag)
        var recordContentExpr = BuiltinHelpers.ApplyBuiltinHead(
            BuiltinHelpers.ApplyBuiltinSkip(1, recordExpr));

        // Get record[1][0] = [field1, field2, ...]
        var fieldsListExpr = BuiltinHelpers.ApplyBuiltinHead(recordContentExpr);

        // Build the updated fields list
        // For each field position, either use the original field or the new value
        var updatesDict = updates.ToDictionary(u => u.fieldIndex, u => u.newValue);

        var updatedFieldExprs = new List<Expression>();
        for (var i = 0; i < totalFieldCount; i++)
        {
            Expression fieldPairExpr;

            // Get the original field at index i
            if (i == 0)
            {
                fieldPairExpr = BuiltinHelpers.ApplyBuiltinHead(fieldsListExpr);
            }
            else
            {
                fieldPairExpr = BuiltinHelpers.ApplyBuiltinHead(
                    BuiltinHelpers.ApplyBuiltinSkip(i, fieldsListExpr));
            }

            if (updatesDict.TryGetValue(i, out var newValueExpr))
            {
                // This field is being updated
                // Get the field name from the original field
                var fieldNameExpr = BuiltinHelpers.ApplyBuiltinHead(fieldPairExpr);

                // Create new field pair with updated value
                updatedFieldExprs.Add(Expression.ListInstance([fieldNameExpr, newValueExpr]));
            }
            else
            {
                // Keep the original field unchanged
                updatedFieldExprs.Add(fieldPairExpr);
            }
        }

        // Construct the new record: [tag, [[updatedFields]]]
        var updatedFieldsListExpr = Expression.ListInstance(updatedFieldExprs);
        return Expression.ListInstance(
        [
            recordTagExpr,
            Expression.ListInstance([updatedFieldsListExpr])
        ]);
    }

    private static Result<CompilationError, Expression> CompileRecordAccess(
        SyntaxTypes.Expression.RecordAccess expr,
        ExpressionCompilationContext context)
    {
        // Compile the record expression
        var recordResult = Compile(expr.Record.Value, context);
        if (recordResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        var recordExpr = recordResult.IsOkOrNull()!;
        var fieldName = expr.FieldName.Value;

        // Try to get the record type to compute field index at compile time
        var recordType = TryGetRecordType(expr.Record.Value, context);
        if (recordType is not null)
        {
            // We know the record field layout at compile time
            // Find the index of the field in the sorted field list
            var fieldIndex = recordType.Fields
                .Select((field, idx) => (field.FieldName, idx))
                .FirstOrDefault(x => x.FieldName == fieldName);

            if (fieldIndex.FieldName == fieldName)
            {
                // Emit direct field access using the known index
                // Record structure: [ElmRecordTag, [[field1, field2, ...]]]
                // Each field is [fieldName, fieldValue]
                // To get field at index N: skip N fields, take head, get value (index 1)
                return CompileRecordAccessWithKnownIndex(recordExpr, fieldIndex.idx);
            }
        }

        // Fall back to runtime field lookup when type is not known
        // Build the call to the record access function
        // The environment for the function is: [record, fieldName]
        var callEnv = Expression.ListInstance(
        [
            recordExpr,
            Expression.LiteralInstance(StringEncoding.ValueFromString(fieldName))
        ]);

        return new Expression.ParseAndEval(
            encoded: Expression.LiteralInstance(RecordRuntime.PineFunctionForRecordAccessAsValue),
            environment: callEnv);
    }

    /// <summary>
    /// Compiles a record field access when the field index is known at compile time.
    /// This avoids runtime iteration through fields.
    /// </summary>
    private static Expression CompileRecordAccessWithKnownIndex(Expression recordExpr, int fieldIndex)
    {
        // Record structure: [ElmRecordTag, [[field1, field2, ...]]]
        // field at index N: record[1][0][N][1]
        // 
        // record[1] = [[field1, field2, ...]]
        // record[1][0] = [field1, field2, ...]
        // record[1][0][N] = [fieldName_N, fieldValue_N]
        // record[1][0][N][1] = fieldValue_N

        // Get record[1] (skip tag)
        var recordContentExpr = BuiltinHelpers.ApplyBuiltinHead(
            BuiltinHelpers.ApplyBuiltinSkip(1, recordExpr));

        // Get record[1][0] (unwrap the inner list)
        var fieldsListExpr = BuiltinHelpers.ApplyBuiltinHead(recordContentExpr);

        // Get field at index N
        Expression fieldPairExpr;
        if (fieldIndex == 0)
        {
            fieldPairExpr = BuiltinHelpers.ApplyBuiltinHead(fieldsListExpr);
        }
        else
        {
            fieldPairExpr = BuiltinHelpers.ApplyBuiltinHead(
                BuiltinHelpers.ApplyBuiltinSkip(fieldIndex, fieldsListExpr));
        }

        // Get the field value (index 1 in the pair)
        return BuiltinHelpers.ApplyBuiltinHead(
            BuiltinHelpers.ApplyBuiltinSkip(1, fieldPairExpr));
    }

    /// <summary>
    /// Tries to determine if an expression has a known record type with field names.
    /// </summary>
    private static TypeInference.InferredType.RecordType? TryGetRecordType(
        SyntaxTypes.Expression expression,
        ExpressionCompilationContext context)
    {
        // Check if the expression is a simple variable reference
        if (expression is SyntaxTypes.Expression.FunctionOrValue funcOrValue &&
            funcOrValue.ModuleName.Count == 0)
        {
            var varName = funcOrValue.Name;

            // Check local binding types first
            if (context.TryGetLocalBindingType(varName, out var localType) &&
                localType is TypeInference.InferredType.RecordType localRecordType)
            {
                return localRecordType;
            }

            // Check parameter types
            if (context.ParameterTypes.TryGetValue(varName, out var paramType) &&
                paramType is TypeInference.InferredType.RecordType paramRecordType)
            {
                return paramRecordType;
            }
        }

        return null;
    }

    private static Result<CompilationError, Expression> CompileOperatorApplication(
        SyntaxTypes.Expression.OperatorApplication expr,
        ExpressionCompilationContext context) =>
        OperatorCompiler.Compile(expr, context);

    private static Result<CompilationError, Expression> CompileParenthesizedExpression(
        SyntaxTypes.Expression.ParenthesizedExpression expr,
        ExpressionCompilationContext context) =>
        Compile(expr.Expression.Value, context);

    private static Result<CompilationError, Expression> CompileNegation(
        SyntaxTypes.Expression.Negation expr,
        ExpressionCompilationContext context)
    {
        if (expr.Expression.Value is SyntaxTypes.Expression.Integer intLiteral)
        {
            return Expression.LiteralInstance(EmitIntegerLiteral(-intLiteral.Value));
        }

        var innerResult = Compile(expr.Expression.Value, context);
        if (innerResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        var negativeOne = Expression.LiteralInstance(EmitIntegerLiteral(-1));
        return BuiltinHelpers.ApplyBuiltinIntMul([negativeOne, innerResult.IsOkOrNull()!]);
    }

    private static Result<CompilationError, Expression> CompileIfBlock(
        SyntaxTypes.Expression.IfBlock expr,
        ExpressionCompilationContext context)
    {
        var conditionResult = Compile(expr.Condition.Value, context);
        if (conditionResult.IsErrOrNull() is { } condErr)
        {
            return condErr;
        }

        var trueBranchResult = Compile(expr.ThenBlock.Value, context);
        if (trueBranchResult.IsErrOrNull() is { } trueErr)
        {
            return trueErr;
        }

        var falseBranchResult = Compile(expr.ElseBlock.Value, context);
        if (falseBranchResult.IsErrOrNull() is { } falseErr)
        {
            return falseErr;
        }

        return Expression.ConditionalInstance(
            condition: conditionResult.IsOkOrNull()!,
            falseBranch: falseBranchResult.IsOkOrNull()!,
            trueBranch: trueBranchResult.IsOkOrNull()!);
    }

    private static Result<CompilationError, Expression> CompileCaseExpression(
        SyntaxTypes.Expression.CaseExpression expr,
        ExpressionCompilationContext context) =>
        PatternCompiler.CompileCaseExpression(expr.CaseBlock, context);

    private static Result<CompilationError, Expression> CompileLetExpression(
        SyntaxTypes.Expression.LetExpression expr,
        ExpressionCompilationContext context) =>
        CompileLetExpressionBody(expr.Value, context);

    private static Result<CompilationError, Expression> CompileLetExpressionBody(
        SyntaxTypes.Expression.LetBlock letBlock,
        ExpressionCompilationContext context)
    {
        var newBindings = new Dictionary<string, Expression>();
        var newBindingTypes = context.LocalBindingTypes is { } existingBindingTypes
            ? existingBindingTypes.ToImmutableDictionary()
            : [];

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
        var letContext = context.WithReplacedLocalBindingsAndTypes(newBindings, newBindingTypes);

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
                        // Infer the type of the bound expression BEFORE compiling
                        // This allows us to propagate types from parameters through let bindings
                        var bindingType = TypeInference.InferExpressionType(
                            funcBody,
                            context.ParameterNames,
                            context.ParameterTypes,
                            newBindingTypes.Count > 0 ? newBindingTypes : null,
                            context.CurrentModuleName,
                            context.FunctionReturnTypes);

                        newBindingTypes = newBindingTypes.SetItem(funcName, bindingType);

                        // Update the let context with the new type before compiling
                        letContext = letContext.WithReplacedLocalBindingsAndTypes(newBindings, newBindingTypes);

                        var compiledBodyResult = Compile(funcBody, letContext);

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
                    // Infer the type of the expression being destructured
                    var destructuredExprType = TypeInference.InferExpressionType(
                        letDestructuring.Expression.Value,
                        context.ParameterNames,
                        context.ParameterTypes,
                        newBindingTypes.Count > 0 ? newBindingTypes : null,
                        context.CurrentModuleName,
                        context.FunctionReturnTypes);

                    // Extract binding types from the pattern using the inferred type
                    newBindingTypes = TypeInference.ExtractPatternBindingTypesFromInferred(
                        letDestructuring.Pattern.Value,
                        destructuredExprType,
                        newBindingTypes);

                    // Update the let context with the new types
                    letContext = letContext.WithReplacedLocalBindingsAndTypes(newBindings, newBindingTypes);

                    var destructuredResult = Compile(letDestructuring.Expression.Value, letContext);
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

                    // Update the let context with the new bindings
                    letContext = letContext.WithReplacedLocalBindingsAndTypes(newBindings, newBindingTypes);
                    break;
            }
        }

        return Compile(letBlock.Expression.Value, letContext);
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
