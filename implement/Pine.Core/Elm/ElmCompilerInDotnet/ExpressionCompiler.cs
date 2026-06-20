using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Compiles Elm expressions to Pine expressions.
/// Uses recursive mapping for type-safe expression handling.
/// </summary>
public class ExpressionCompiler
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
            SyntaxTypes.Expression.UnitExpr =>
            Expression.LiteralInstance(PineValue.EmptyList),

            SyntaxTypes.Expression.Integer expr =>
            CompileInteger(expr),

            SyntaxTypes.Expression.FloatLiteral expr =>
            CompileFloatLiteral(expr),

            SyntaxTypes.Expression.StringLiteral expr =>
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

            _ =>
            CompilationError.UnsupportedExpression(expression.GetType().Name)
        };

    private static Result<CompilationError, Expression> CompileInteger(
        SyntaxTypes.Expression.Integer expr) =>
        Expression.LiteralInstance(EmitIntegerLiteral(expr.Value));

    private static Result<CompilationError, Expression> CompileFloatLiteral(
        SyntaxTypes.Expression.FloatLiteral expr)
    {
        var floatValue =
            ElmValue.ElmFloat.Convert(
                ((double)expr.Numerator / (double)expr.Denominator));

        var pineValue =
            ElmValueEncoding.ElmValueAsPineValue(floatValue);

        return Expression.LiteralInstance(pineValue);
    }

    private static Result<CompilationError, Expression> CompileLiteral(
        SyntaxTypes.Expression.StringLiteral expr) =>
        Expression.LiteralInstance(EmitStringLiteral(expr.Value));

    private static Result<CompilationError, Expression> CompileCharLiteral(
        SyntaxTypes.Expression.CharLiteral expr) =>
        Expression.LiteralInstance(EmitCharLiteral(expr.Value));

    internal static Result<CompilationError, Expression> CompileFunctionOrValue(
        SyntaxTypes.Expression.FunctionOrValue expr,
        ExpressionCompilationContext context)
    {
        if (expr.QualifiedName.Namespaces.Count is 0)
        {
            // Check if it's a local binding from pattern matching first
            if (context.TryGetLocalBinding(expr.QualifiedName.DeclName) is { } bindingExpr)
            {
                return bindingExpr;
            }

            // Check if it's a parameter reference
            if (context.TryGetParameterIndex(expr.QualifiedName.DeclName) is { } paramIndex)
            {
                return BuiltinHelpers.BuildPathToParameter(paramIndex);
            }
        }

        if (expr.QualifiedName.Namespaces.Count is 1 && expr.QualifiedName.Namespaces[0] is "Basics")
        {
            if (expr.QualifiedName.DeclName is "True")
            {
                return Expression.LiteralInstance(EmitBooleanLiteral(true));
            }

            if (expr.QualifiedName.DeclName is "False")
            {
                return Expression.LiteralInstance(EmitBooleanLiteral(false));
            }
        }

        // Check if this is a record type alias constructor used as a function value
        // Record constructors also have uppercase names, so check this before the tag name check
        {
            if (context.ModuleCompilationContext.TryGetRecordConstructorFieldNames(expr.QualifiedName.FullName) is { } fieldNamesInDeclOrder)
            {
                // Record constructor used as a function value (no application yet)
                // Build a function value that accepts all field arguments and constructs the record
                return EmitRecordConstructorFunctionValue(fieldNamesInDeclOrder);
            }
        }

        // After canonicalization, tags have a module name but are still recognized
        // by having an uppercase first letter
        if (ElmValueEncoding.StringIsValidTagName(expr.QualifiedName.DeclName))
        {
            var qualifiedConstructorName =
                expr.QualifiedName.Namespaces.Count > 0
                ?
                QualifiedNameHelper.ToQualifiedNameString(expr.QualifiedName.Namespaces, expr.QualifiedName.DeclName)
                :
                QualifiedNameHelper.ToQualifiedNameString([context.CurrentModuleName], expr.QualifiedName.DeclName);

            var expectedArgCount =
                context.ModuleCompilationContext.TryGetChoiceTypeConstructorArgumentCount(qualifiedConstructorName);

            if (expectedArgCount is null || expectedArgCount.Value is 0)
            {
                // Zero-argument tag: emit as a literal value
                return Expression.LiteralInstance(ElmValueEncoding.TagAsPineValue(expr.QualifiedName.DeclName, []));
            }

            // Tag with arguments used as a function value (no application yet)
            // Build a function value that accepts all arguments
            return EmitChoiceTypeTagFunctionValue(expr.QualifiedName.DeclName, expectedArgCount.Value);
        }

        // Check if this is a reference to a top-level function (function value without application)
        // This happens when a function is returned as a value, like in:
        //   case x of
        //     1 -> functionA
        //     _ -> functionB
        {
            var qualifiedFunctionName =
                expr.QualifiedName.Namespaces.Count > 0
                ?
                string.Join(".", expr.QualifiedName.Namespaces) + "." + expr.QualifiedName.DeclName
                :
                context.CurrentModuleName + "." + expr.QualifiedName.DeclName;

            if (context.GetFunctionIndexInLayout(qualifiedFunctionName) is { } functionIndex)
            {
                // Same-SCC reference: build the function value via the
                // env-relative encoded body and env-functions list.
                return CompileFunctionReference(qualifiedFunctionName, functionIndex, context);
            }

            // §7.6b: cross-SCC reference. The callee is not in the caller's
            // (members-only) layout but it has already been compiled (SCCs
            // are processed in dependency order).
            //
            // For functions with paramCount > 0 the callee's WrapperValue
            // *is* the function value, and inlining it as a literal is the
            // direct counterpart of the same-SCC `CompileFunctionReference`
            // path. For zero-parameter declarations the callee's wrapper
            // value is an *encoded ParseAndEval that, when evaluated, yields
            // the value*; the consumer expects the value itself, not the
            // wrapper, so we evaluate the wrapper here by emitting the same
            // literal-based ParseAndEval that `CompileFunctionReference`
            // emits for paramCount <= 0.
            if (context.ModuleCompilationContext.TryGetCompiledFunctionInfo(
                qualifiedFunctionName) is { } crossSccCalleeInfo)
            {
                if (crossSccCalleeInfo.ParameterCount <= 0)
                {
                    // Phase 1 of Approach A1: all callees use WithEnvFunctions
                    // layout. The body always expects [envFunctionsList], which
                    // may be empty for non-recursive single-member SCCs.
                    var crossSccEnvFuncsLiteral =
                        Expression.LiteralInstance(
                            PineValue.List([.. crossSccCalleeInfo.EnvFunctions]));

                    var crossSccCallEnvironment =
                        Expression.ListInstance([crossSccEnvFuncsLiteral]);

                    return
                        (Result<CompilationError, Expression>)new Expression.ParseAndEval(
                            encoded: Expression.LiteralInstance(crossSccCalleeInfo.EncodedBody),
                            environment: crossSccCallEnvironment);
                }

                return Expression.LiteralInstance(crossSccCalleeInfo.CompiledValue);
            }
        }

        // Handle references to natively-implemented Basics functions used as values
        // (e.g., passing `min` or `max` as an argument to a higher-order function like `foldl`)
        if (expr.QualifiedName.Namespaces.Count is 1 && expr.QualifiedName.Namespaces[0] is "Basics")
        {
            if (CoreLibraryModule.CoreBasics.GetFunctionValue(expr.QualifiedName.DeclName) is { } basicsFunctionValue)
            {
                return Expression.LiteralInstance(basicsFunctionValue);
            }
        }

        // Handle List.cons used as a value reference (the :: operator after canonicalization)
        // This handles cases like passing (::) as a function value: `applyFunc (::) 1 [2, 3]`
        if (expr.QualifiedName.Namespaces.Count is 1 && expr.QualifiedName.Namespaces[0] is "List" &&
            expr.QualifiedName.DeclName is "cons")
        {
            return Expression.LiteralInstance(s_consFunction.Value);
        }

        return new CompilationError.UnresolvedReference(expr.QualifiedName.DeclName, context.CurrentModuleName);
    }

    private static Result<CompilationError, Expression> CompileApplication(
        SyntaxTypes.Expression.Application expr,
        ExpressionCompilationContext context)
    {
        if (expr.Arguments.Count < 1)
        {
            return new CompilationError.ApplicationTooFewArguments(expr.Arguments.Count + 1);
        }

        var functionExpr = expr.Function;

        // Check if this is a Pine_kernel application
        if (functionExpr is SyntaxTypes.Expression.FunctionOrValue kernelFunc &&
            kernelFunc.QualifiedName.Namespaces.Count is 1 &&
            context.ModuleCompilationContext.IsPineKernelModule(kernelFunc.QualifiedName.Namespaces[0]))
        {
            var kernelInput = expr.Arguments[0];
            var compiledInputResult = Compile(kernelInput, context);

            if (compiledInputResult.IsErrOrNull() is { } err)
            {
                return err;
            }

            return
                Expression.KernelApplicationInstance(
                    kernelFunc.QualifiedName.DeclName,
                    compiledInputResult.IsOkOrNull()!);
        }

        // Compile all arguments
        var compiledArguments = new Expression[expr.Arguments.Count];

        for (var i = 0; i < expr.Arguments.Count; i++)
        {
            var argResult = Compile(expr.Arguments[i], context);

            if (argResult.IsErrOrNull() is { } argErr)
            {
                return argErr;
            }

            if (argResult.IsOkOrNull() is not { } argOk)
            {
                throw new NotImplementedException(
                    "Unexpected result type when compiling application argument: " + argResult.GetType());
            }

            compiledArguments[i] = argOk;
        }

        // Check if this is a prefix operator application with 2 arguments (e.g., (&&) x y)
        // For and/or/xor, emit conditionals directly instead of function application
        if (functionExpr is SyntaxTypes.Expression.PrefixOperator prefixOp && compiledArguments.Length is 2)
        {
            var left = compiledArguments[0];
            var right = compiledArguments[1];

            if (prefixOp.Operator is "&&")
            {
                return CoreLibraryModule.CoreBasics.Generic_And(left, right);
            }

            if (prefixOp.Operator is "||")
            {
                return CoreLibraryModule.CoreBasics.Generic_Or(left, right);
            }
        }

        // Check if this is a function application or choice type tag application
        if (functionExpr is SyntaxTypes.Expression.FunctionOrValue funcRef)
        {
            // Check if this is a record type alias constructor application
            // Record type alias constructors also have uppercase names, so check this first
            var qualifiedConstructorName =
                funcRef.QualifiedName.Namespaces.Count > 0
                ?
                string.Join(".", funcRef.QualifiedName.Namespaces) + "." + funcRef.QualifiedName.DeclName
                :
                context.CurrentModuleName + "." + funcRef.QualifiedName.DeclName;

            if (context.ModuleCompilationContext.TryGetRecordConstructorFieldNames(qualifiedConstructorName) is { } fieldNamesInDeclOrder)
            {
                // This is a record type alias constructor application
                // Arguments are in declaration order (same as fieldNamesInDeclOrder)
                // We need to construct a record with fields sorted alphabetically

                var argumentCount = expr.Arguments.Count; // First arg is the constructor itself

                if (argumentCount < fieldNamesInDeclOrder.Count)
                {
                    // Partial application: fewer arguments than fields
                    // Build a function value that captures the provided arguments and
                    // accepts the remaining ones
                    return
                        EmitRecordConstructorPartialApplicationExpression(
                            fieldNamesInDeclOrder,
                            compiledArguments);
                }

                // Create pairs of (fieldName, argExpression) in declaration order
                var fieldArgPairs =
                    fieldNamesInDeclOrder
                    .Select((fieldName, index) => (fieldName, expr: compiledArguments[index]))
                    .ToList();

                // Sort pairs alphabetically by field name for the record representation
                var sortedPairs =
                    fieldArgPairs
                    .OrderBy(pair => pair.fieldName, StringComparer.Ordinal)
                    .ToList();

                // Build the record field expressions interleaved as
                // [name0, value0, name1, value1, ...] for the new flat layout.
                var recordFieldItems = new List<Expression>(sortedPairs.Count * 2);

                foreach (var pair in sortedPairs)
                {
                    recordFieldItems.Add(
                        Expression.LiteralInstance(StringEncoding.ValueFromString(pair.fieldName)));

                    recordFieldItems.Add(pair.expr);
                }

                // Build the record: [ElmRecordTag, name0, value0, name1, value1, ...]
                return
                    Expression.ListInstance(
                        [
                        Expression.LiteralInstance(ElmValue.ElmRecordTypeTagNameAsValue),
                        ..recordFieldItems
                        ]);
            }

            // Check if this is a choice type tag application
            if (ElmValueEncoding.StringIsValidTagName(funcRef.QualifiedName.DeclName))
            {
                var tagNameValue = Expression.LiteralInstance(StringEncoding.ValueFromString(funcRef.QualifiedName.DeclName));

                var qualifiedTagName =
                    funcRef.QualifiedName.Namespaces.Count > 0
                    ?
                    QualifiedNameHelper.ToQualifiedNameString(funcRef.QualifiedName.Namespaces, funcRef.QualifiedName.DeclName)
                    :
                    QualifiedNameHelper.ToQualifiedNameString([context.CurrentModuleName], funcRef.QualifiedName.DeclName);

                var expectedArgCount =
                    context.ModuleCompilationContext.TryGetChoiceTypeConstructorArgumentCount(qualifiedTagName);

                // If we know the expected argument count and have all arguments, build the value directly
                if (expectedArgCount is null || compiledArguments.Length >= expectedArgCount.Value)
                {
                    // Full application: build the choice type value directly
                    return
                        Expression.ListInstance(
                            [
                            tagNameValue,
                            Expression.ListInstance(compiledArguments)
                            ]);
                }

                // Partial application: we have fewer arguments than expected
                // Build an expression that evaluates to a function value
                return
                    EmitChoiceTypeTagPartialApplicationExpression(
                        funcRef.QualifiedName.DeclName,
                        expectedArgCount.Value,
                        compiledArguments);
            }

            // Check if the function is a parameter or local binding (higher-order function application)
            // This handles cases like: `applyTwoArgs func a b = func a b`
            // where `func` is a parameter that holds a function value
            if (funcRef.QualifiedName.Namespaces.Count is 0)
            {
                if (context.TryGetLocalBinding(funcRef.QualifiedName.DeclName) is { } bindingExpr)
                {
                    // Apply arguments to the function value from local binding using generic application
                    return CompileGenericFunctionApplication(bindingExpr, compiledArguments);
                }

                if (context.TryGetParameterIndex(funcRef.QualifiedName.DeclName) is { } paramIndex)
                {
                    // Apply arguments to the function value from parameter using generic application
                    var funcExpr = BuiltinHelpers.BuildPathToParameter(paramIndex);

                    return CompileGenericFunctionApplication(funcExpr, compiledArguments);
                }
            }

            // Handle Basics module functions that are implemented in BasicArithmetic
            // Check using the structured module name list rather than string manipulation
            if (funcRef.QualifiedName.Namespaces.Count is 1 && funcRef.QualifiedName.Namespaces[0] is "Basics" &&
                CoreLibraryModule.CoreBasics.GetBasicsFunctionInfo(funcRef.QualifiedName.DeclName) is { } basicsFuncInfo &&
                basicsFuncInfo.FunctionType.Count - compiledArguments.Length - 1 is 0)
            {
                // For polymorphic numeric functions (add, sub, mul), use type inference to decide
                // whether to use the integer-specific implementation
                if (funcRef.QualifiedName.DeclName is "add" or "sub" or "mul")
                {
                    // Use type inference to determine the expression type
                    var expressionType =
                        TypeInference.InferExpressionType(
                            expr,
                            context.ParameterNames,
                            context.ParameterTypes,
                            context.LocalBindingTypes,
                            context.CurrentModuleName,
                            context.FunctionTypes);

                    if (expressionType is TypeInference.InferredType.IntType)
                    {
                        // Use integer-specific implementation
                        if (funcRef.QualifiedName.DeclName is "add")
                            return BuiltinHelpers.ApplyBuiltinIntAdd(compiledArguments);

                        if (funcRef.QualifiedName.DeclName is "sub")
                            return CoreLibraryModule.CoreBasics.Int_sub(compiledArguments[0], compiledArguments[1]);

                        if (funcRef.QualifiedName.DeclName is "mul")
                            return BuiltinHelpers.ApplyBuiltinIntMul(compiledArguments);
                    }
                }

                return basicsFuncInfo.CompileApplication(compiledArguments);
            }

            // Determine qualified function name
            var qualifiedFunctionName =
                funcRef.QualifiedName.Namespaces.Count > 0
                ?
                string.Join(".", funcRef.QualifiedName.Namespaces) + "." + funcRef.QualifiedName.DeclName
                :
                context.CurrentModuleName + "." + funcRef.QualifiedName.DeclName;

            // Resolve callee in the compiled-functions cache. After §7.6b the
            // caller's layout contains only its SCC members, so a cross-SCC
            // callee will not appear in the layout but will be in the cache
            // (because SCCs are compiled in dependency order).
            var functionIndexOpt = context.GetFunctionIndexInLayout(qualifiedFunctionName);
            CompiledFunctionInfo? cachedCalleeInfo = null;

            if (functionIndexOpt is null)
            {
                cachedCalleeInfo =
                    context.ModuleCompilationContext.TryGetCompiledFunctionInfo(
                        qualifiedFunctionName);
            }

            if (functionIndexOpt is null && cachedCalleeInfo is null)
            {
                // Handle references to natively-implemented Basics functions
                // that are partially applied or not in the dependency layout
                if (funcRef.QualifiedName.Namespaces.Count is 1 && funcRef.QualifiedName.Namespaces[0] is "Basics" &&
                    CoreLibraryModule.CoreBasics.GetFunctionValue(funcRef.QualifiedName.DeclName) is { } basicsFuncValue)
                {
                    return
                        CompileGenericFunctionApplication(
                            Expression.LiteralInstance(basicsFuncValue),
                            compiledArguments);
                }

                // Handle List.cons (the :: operator after canonicalization)
                if (funcRef.QualifiedName.Namespaces.Count is 1 && funcRef.QualifiedName.Namespaces[0] is "List" &&
                    funcRef.QualifiedName.DeclName is "cons")
                {
                    if (compiledArguments.Length >= 2)
                    {
                        // Full application: cons head tail  ==>  concat([[head], tail])
                        var singletonList = Expression.ListInstance([compiledArguments[0]]);

                        var consResult =
                            BuiltinHelpers.ApplyBuiltinConcat(
                                [singletonList, compiledArguments[1]]);

                        // Handle over-application (unlikely but consistent)
                        if (compiledArguments.Length > 2)
                        {
                            return
                                CompileGenericFunctionApplication(
                                    consResult,
                                    compiledArguments[2..]);
                        }

                        return consResult;
                    }

                    // Partial or no application: return the cons function value
                    return
                        CompileGenericFunctionApplication(
                            Expression.LiteralInstance(s_consFunction.Value),
                            compiledArguments);
                }

                return new CompilationError.FunctionNotInDependencyLayout(qualifiedFunctionName);
            }

            // Get function info to determine parameter count
            if (context.ModuleCompilationContext.TryGetFunctionInfo(qualifiedFunctionName) is not { } funcInfo)
            {
                // Function exists in dependency layout but info is not available
                // This shouldn't happen if the function is properly registered
                return new CompilationError.UnresolvedReference(funcRef.QualifiedName.DeclName, context.CurrentModuleName);
            }

            var paramCount = funcInfo.declaration.Function.Declaration.Arguments.Count;
            var argCount = compiledArguments.Length;

            if (argCount >= paramCount && paramCount > 0)
            {
                // Full or over-application: use optimized [envFuncs, arg0, arg1, ...] flat structure
                // for the first paramCount arguments.
                //
                // §7.6b: pick env-relative or literal-based references based on
                // whether the callee is same-SCC (in caller's members-only layout)
                // or cross-SCC (only in the compiled-functions cache).
                Expression functionRef;
                Expression callEnvFunctions;

                if (functionIndexOpt is { } functionIndex)
                {
                    // Same-SCC callee — read the encoded body from the caller's
                    // runtime env at [0, functionIndex], and forward the entire
                    // env-functions list (env[0]) as a whole.
                    //
                    // All members of an SCC share the same env-functions list
                    // layout (the SCC member list, in stable order). The caller
                    // is itself an SCC member, so its env[0] is exactly the
                    // env-functions list the callee expects. Forwarding env[0]
                    // as a single value avoids materialising a fresh list with
                    // one Build_List + N path-reads per call.
                    functionRef =
                        ExpressionBuilder.BuildExpressionForPathInExpression(
                            [0, functionIndex],
                            Expression.EnvironmentInstance);

                    callEnvFunctions =
                        ExpressionBuilder.BuildExpressionForPathInExpression(
                            [0],
                            Expression.EnvironmentInstance);
                }
                else
                {
                    // §7.6b: cross-SCC callee — inline the cached encoded
                    // body and env-functions list as literals. No reads from
                    // the caller's env at index 0.
                    //
                    // Phase 1 of Approach A1: all callees use WithEnvFunctions
                    // layout. Always emit [envFunctionsList, arg0, ..., argN-1]
                    // as the call environment (the env-functions list may be
                    // empty for non-recursive single-member SCCs).
                    var cachedInfo = cachedCalleeInfo!;

                    functionRef = Expression.LiteralInstance(cachedInfo.EncodedBody);

                    callEnvFunctions =
                        Expression.LiteralInstance(
                            PineValue.List([.. cachedInfo.EnvFunctions]));
                }

                // Use only the first paramCount arguments for the optimized full application
                var fullApplicationArgs = compiledArguments.Take(paramCount).ToList();

                var callEnvironment =
                    Expression.ListInstance(
                        [callEnvFunctions, .. fullApplicationArgs]);

                Expression fullApplicationResult =
                    new Expression.ParseAndEval(
                        encoded: functionRef,
                        environment: callEnvironment);

                // If there are remaining arguments (over-application), apply them generically
                if (argCount > paramCount)
                {
                    var remainingArgs = compiledArguments.Skip(paramCount).ToList();
                    return CompileGenericFunctionApplication(fullApplicationResult, remainingArgs);
                }

                return fullApplicationResult;
            }
        }

        {
            var compileFunctionExprResult = Compile(functionExpr, context);

            if (compileFunctionExprResult.IsErrOrNull() is { } funcErr)
            {
                return
                    CompilationError.Scoped(
                        "When compiling function part of application",
                        funcErr);
            }

            if (compileFunctionExprResult.IsOkOrNull() is not { } funcOk)
            {
                throw new NotImplementedException(
                    "Unexpected result type when compiling function part of application: " +
                    compileFunctionExprResult.GetType());
            }

            // Apply provided arguments using generic application
            return CompileGenericFunctionApplication(funcOk, compiledArguments);
        }
    }

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

        return Expression.ListInstance(compiledItems);
    }

    private static Result<CompilationError, Expression> CompileTupledExpression(
        SyntaxTypes.Expression.TupledExpression expr,
        ExpressionCompilationContext context)
    {
        // A tuple in Elm is represented as a list in Pine
        var compiledItems = new Expression[expr.Elements.Count];

        for (var i = 0; i < expr.Elements.Count; i++)
        {
            var item = expr.Elements[i];

            var itemResult = Compile(item, context);

            if (itemResult.IsErrOrNull() is { } err)
            {
                return
                    CompilationError.Scoped(
                        "When compiling tuple element [" + i + "]",
                        err);
            }

            if (itemResult.IsOkOrNull() is not { } itemOk)
            {
                throw new NotImplementedException(
                    "Unexpected result type when compiling tuple element [" + i + "]: " + item.GetType());
            }

            compiledItems[i] = itemOk;
        }

        return Expression.ListInstance(compiledItems);
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
                Expression.LiteralInstance(StringEncoding.ValueFromString(fieldName)));

            compiledFieldItems.Add(compiledValue.IsOkOrNull()!);
        }

        // A record is encoded as [recordTypeTag, name0, value0, name1, value1, ...]
        return
            Expression.ListInstance(
                [
                Expression.LiteralInstance(ElmValue.ElmRecordTypeTagNameAsValue),
                ..compiledFieldItems
                ]);
    }

    private static Result<CompilationError, Expression> CompileRecordUpdateExpression(
        SyntaxTypes.Expression.RecordUpdateExpression expr,
        ExpressionCompilationContext context)
    {
        // Get the original record value from the record name variable
        var recordName = expr.RecordName;

        Expression recordExpr;
        TypeInference.InferredType.RecordType? recordType = null;

        // Check if it's a local binding first
        if (context.TryGetLocalBinding(recordName) is { } bindingExpr)
        {
            recordExpr = bindingExpr;
            // Try to get the type from local bindings
            if (context.TryGetLocalBindingType(recordName) is TypeInference.InferredType.RecordType localRecordType)
            {
                recordType = localRecordType;
            }
        }
        // Check if it's a parameter reference
        else if (context.TryGetParameterIndex(recordName) is { } paramIndex)
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

        // If we know the record type at compile time, use direct field update
        if (recordType is not null)
        {
            // Build a map of field name to index
            var fieldIndices =
                recordType.Fields
                .Select((field, idx) => (field.FieldName, idx))
                .ToDictionary(x => x.FieldName, x => x.idx);

            // Check if all update field names exist in the record type
            var allFieldsKnown = compiledUpdateValues.All(u => fieldIndices.ContainsKey(u.fieldName));

            if (allFieldsKnown)
            {
                // Use compile-time field index computation
                return
                    CompileRecordUpdateWithKnownIndices(
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
            var updatePair =
                Expression.ListInstance(
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

        return
            new Expression.ParseAndEval(
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
        // New flat record structure: [tag, name0, value0, name1, value1, ...].
        // Field i has its name at outer slot (2*i + 1) and its value at outer slot (2*i + 2).
        //
        // Strategy: build a fresh outer list whose tag and field names are read from the
        // original record (so we don't have to know the name strings here), and whose
        // values are either the original value (for unchanged slots) or the new value
        // expression (for updated slots).

        var updatesDict = updates.ToDictionary(u => u.fieldIndex, u => u.newValue);

        var resultItems =
            new List<Expression>(totalFieldCount * 2 + 1)
            {
                // Tag: read from the original record so we don't bake the constant in twice.
                BuiltinHelpers.ApplyBuiltinHead(recordExpr),
            };

        for (var i = 0; i < totalFieldCount; i++)
        {
            // Field name at outer slot (2*i + 1) - always read from original.
            var nameSlot = 2 * i + 1;

            var nameExpr =
                BuiltinHelpers.ApplyBuiltinHead(
                    BuiltinHelpers.ApplyBuiltinSkip(nameSlot, recordExpr));

            resultItems.Add(nameExpr);

            // Field value at outer slot (2*i + 2) - either the original value or the update.
            if (updatesDict.TryGetValue(i, out var newValueExpr))
            {
                resultItems.Add(newValueExpr);
            }
            else
            {
                var valueSlot = 2 * i + 2;

                resultItems.Add(
                    BuiltinHelpers.ApplyBuiltinHead(
                        BuiltinHelpers.ApplyBuiltinSkip(valueSlot, recordExpr)));
            }
        }

        return Expression.ListInstance(resultItems);
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

        // Try to get the record type to compute field index at compile time
        var recordType = TryGetRecordType(expr.Record, context);

        if (recordType is not null)
        {
            // We know the record field layout at compile time
            // Find the index of the field in the sorted field list
            var fieldIndex =
                recordType.Fields
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
        var callEnv =
            Expression.ListInstance(
                [
                recordExpr,
                Expression.LiteralInstance(StringEncoding.ValueFromString(fieldName))
                ]);

        return
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(RecordRuntime.PineFunctionForRecordAccessAsValue),
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
            Expression.ListInstance(
                [
                recordParam,
                Expression.LiteralInstance(StringEncoding.ValueFromString(fieldName))
                ]);

        var body =
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(RecordRuntime.PineFunctionForRecordAccessAsValue),
                environment: callEnv);

        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                body,
                parameterCount: 1,
                envFunctions: []);

        return Expression.LiteralInstance(functionValue);
    }

    /// <summary>
    /// Compiles a record field access when the field index is known at compile time.
    /// This avoids runtime iteration through fields.
    /// </summary>
    private static Expression CompileRecordAccessWithKnownIndex(Expression recordExpr, int fieldIndex)
    {
        // New flat record layout: [tag, name0, value0, name1, value1, ...].
        // Field value at sorted index N is at position 2*N + 2 in the outer list.
        var valueSlot = 2 * fieldIndex + 2;

        return
            BuiltinHelpers.ApplyBuiltinHead(
                BuiltinHelpers.ApplyBuiltinSkip(valueSlot, recordExpr));
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
            funcOrValue.QualifiedName.Namespaces.Count is 0)
        {
            var varName = funcOrValue.QualifiedName.DeclName;

            // Check local binding types first
            if (context.TryGetLocalBindingType(varName) is TypeInference.InferredType.RecordType localRecordType)
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
            return Expression.LiteralInstance(s_consFunction.Value);
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
        var singletonList = Expression.ListInstance([headExpr]);

        var body =
            BuiltinHelpers.ApplyBuiltinConcat(
                [singletonList, tailExpr]);

        return
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                body,
                parameterCount: 2,
                envFunctions: []);
    }

    private static Result<CompilationError, Expression> CompileNegation(
        SyntaxTypes.Expression.Negation expr,
        ExpressionCompilationContext context)
    {
        if (expr.Expression is SyntaxTypes.Expression.Integer intLiteral)
        {
            return Expression.LiteralInstance(EmitIntegerLiteral(-intLiteral.Value));
        }

        var innerResult = Compile(expr.Expression, context);

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
            Expression.ConditionalInstance(
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
        SyntaxTypes.Expression.LetExpression letBlock,
        ExpressionCompilationContext context)
    {
        var newBindings = new Dictionary<string, Expression>();

        var newBindingTypes =
            context.LocalBindingTypes is { } existingBindingTypes
            ?
            existingBindingTypes.ToImmutableDictionary()
            :
            [];

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
            var decl = declarations[i];
            var names = new HashSet<string>();
            var deps = new HashSet<string>();

            switch (decl)
            {
                case SyntaxTypes.LetDeclaration.LetFunction letFunc:
                    var funcName = letFunc.Function.Declaration.Name;
                    names.Add(funcName);
                    CollectExpressionReferences(letFunc.Function.Declaration.Expression, deps);
                    break;

                case SyntaxTypes.LetDeclaration.LetDestructuring letDestructuring:
                    PatternCompiler.CollectPatternNames(letDestructuring.Pattern, names);
                    CollectExpressionReferences(letDestructuring.Expression, deps);
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

        if (sortedIndicesResult.IsOkOrNull() is not { } sortedIndices)
        {
            throw new NotImplementedException(
                "Unexpected type of result from TopologicalSortDeclarations: " + sortedIndicesResult.GetType().FullName);
        }

        var letContext = context.WithReplacedLocalBindingsAndTypes(newBindings, newBindingTypes);

        foreach (var idx in sortedIndices)
        {
            var decl = declarations[idx];

            switch (decl)
            {
                case SyntaxTypes.LetDeclaration.LetFunction letFunc:
                    var funcName = letFunc.Function.Declaration.Name;
                    var funcBody = letFunc.Function.Declaration.Expression;
                    var funcArgs = letFunc.Function.Declaration.Arguments;

                    if (funcArgs.Count is 0)
                    {
                        // Infer the type of the bound expression BEFORE compiling
                        // This allows us to propagate types from parameters through let bindings
                        var bindingType =
                            TypeInference.InferExpressionType(
                                funcBody,
                                context.ParameterNames,
                                context.ParameterTypes,
                                newBindingTypes.Count > 0 ? newBindingTypes : null,
                                context.CurrentModuleName,
                                context.FunctionTypes);

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

                case SyntaxTypes.LetDeclaration.LetDestructuring letDestructuring:

                    // Infer the type of the expression being destructured
                    var destructuredExprType =
                        TypeInference.InferExpressionType(
                            letDestructuring.Expression,
                            context.ParameterNames,
                            context.ParameterTypes,
                            newBindingTypes.Count > 0 ? newBindingTypes : null,
                            context.CurrentModuleName,
                            context.FunctionTypes);

                    // Extract binding types from the pattern using the inferred type
                    newBindingTypes =
                        TypeInference.ExtractPatternBindingTypesFromInferred(
                            letDestructuring.Pattern,
                            destructuredExprType,
                            newBindingTypes);

                    // Update the let context with the new types
                    letContext = letContext.WithReplacedLocalBindingsAndTypes(newBindings, newBindingTypes);

                    var destructuredResult = Compile(letDestructuring.Expression, letContext);

                    if (destructuredResult.IsErrOrNull() is { } destrErr)
                    {
                        return destrErr;
                    }

                    if (destructuredResult.IsOkOrNull() is not { } destrExpr)
                    {
                        throw new NotImplementedException(
                            "Unexpected type of result from Compile: " + destructuredResult.GetType().FullName);
                    }

                    var patternBindings =
                        PatternCompiler.ExtractPatternBindings(
                            letDestructuring.Pattern,
                            destrExpr,
                            scrutineeType: destructuredExprType,
                            recordTypeAliasFields:
                            context.ModuleCompilationContext.RecordTypeAliasConstructors,
                            choiceTagArgumentTypes:
                            context.ModuleCompilationContext.ChoiceTagArgumentTypes);

                    foreach (var kvp in patternBindings)
                    {
                        newBindings[kvp.Key] = kvp.Value;
                    }

                    // Update the let context with the new bindings
                    letContext = letContext.WithReplacedLocalBindingsAndTypes(newBindings, newBindingTypes);

                    break;
            }
        }

        return Compile(letBlock.Expression, letContext);
    }

    #region Helper Methods

    internal static void CollectExpressionReferences(
        SyntaxTypes.Expression expression,
        HashSet<string> refs)
    {
        switch (expression)
        {
            case SyntaxTypes.Expression.FunctionOrValue funcOrVal:
                if (funcOrVal.QualifiedName.Namespaces.Count is 0)
                {
                    refs.Add(funcOrVal.QualifiedName.DeclName);
                }

                break;

            case SyntaxTypes.Expression.Application app:
                CollectExpressionReferences(app.Function, refs);

                foreach (var arg in app.Arguments)
                {
                    CollectExpressionReferences(arg, refs);
                }

                break;

            case SyntaxTypes.Expression.ListExpr listExpr:
                foreach (var elem in listExpr.Elements)
                {
                    CollectExpressionReferences(elem, refs);
                }

                break;

            case SyntaxTypes.Expression.OperatorApplication opApp:
                CollectExpressionReferences(opApp.Left, refs);
                CollectExpressionReferences(opApp.Right, refs);
                break;

            case SyntaxTypes.Expression.Negation neg:
                CollectExpressionReferences(neg.Expression, refs);
                break;

            case SyntaxTypes.Expression.IfBlock ifBlock:
                CollectExpressionReferences(ifBlock.Condition, refs);
                CollectExpressionReferences(ifBlock.ThenBlock, refs);
                CollectExpressionReferences(ifBlock.ElseBlock, refs);
                break;

            case SyntaxTypes.Expression.LetExpression letExpr:
                var localNames = new HashSet<string>();

                foreach (var decl in letExpr.Declarations)
                {
                    switch (decl)
                    {
                        case SyntaxTypes.LetDeclaration.LetFunction letFunc:
                            localNames.Add(letFunc.Function.Declaration.Name);
                            break;

                        case SyntaxTypes.LetDeclaration.LetDestructuring letDestr:
                            PatternCompiler.CollectPatternNames(letDestr.Pattern, localNames);
                            break;
                    }
                }

                var innerRefs = new HashSet<string>();

                foreach (var decl in letExpr.Declarations)
                {
                    switch (decl)
                    {
                        case SyntaxTypes.LetDeclaration.LetFunction letFunc:
                            CollectExpressionReferences(letFunc.Function.Declaration.Expression, innerRefs);
                            break;

                        case SyntaxTypes.LetDeclaration.LetDestructuring letDestr:
                            CollectExpressionReferences(letDestr.Expression, innerRefs);
                            break;
                    }
                }

                CollectExpressionReferences(letExpr.Expression, innerRefs);

                foreach (var innerRef in innerRefs)
                {
                    if (!localNames.Contains(innerRef))
                    {
                        refs.Add(innerRef);
                    }
                }

                break;

            case SyntaxTypes.Expression.RecordExpr recordExpr:
                foreach (var field in recordExpr.Fields)
                {
                    CollectExpressionReferences(field.Value, refs);
                }

                break;

            case SyntaxTypes.Expression.RecordUpdateExpression recordUpdate:
                refs.Add(recordUpdate.RecordName);

                foreach (var field in recordUpdate.Fields)
                {
                    CollectExpressionReferences(field.Value, refs);
                }

                break;

            case SyntaxTypes.Expression.RecordAccess recordAccess:
                CollectExpressionReferences(recordAccess.Record, refs);
                break;

            case SyntaxTypes.Expression.CaseExpression caseExpr:
                CollectExpressionReferences(caseExpr.Expression, refs);

                foreach (var caseItem in caseExpr.Cases)
                {
                    var caseLocalNames = new HashSet<string>();
                    PatternCompiler.CollectPatternNames(caseItem.Pattern, caseLocalNames);

                    var caseRefs = new HashSet<string>();
                    CollectExpressionReferences(caseItem.Expression, caseRefs);

                    foreach (var caseRef in caseRefs)
                    {
                        if (!caseLocalNames.Contains(caseRef))
                        {
                            refs.Add(caseRef);
                        }
                    }
                }

                break;

            case SyntaxTypes.Expression.TupledExpression tupled:
                foreach (var elem in tupled.Elements)
                {
                    CollectExpressionReferences(elem, refs);
                }

                break;

            case SyntaxTypes.Expression.LambdaExpression lambdaExpr:
                var lambdaParamNames = new HashSet<string>();

                foreach (var param in lambdaExpr.Arguments)
                {
                    PatternCompiler.CollectPatternNames(param, lambdaParamNames);
                }

                var lambdaRefs = new HashSet<string>();
                CollectExpressionReferences(lambdaExpr.Expression, lambdaRefs);

                foreach (var lambdaRef in lambdaRefs)
                {
                    if (!lambdaParamNames.Contains(lambdaRef))
                    {
                        refs.Add(lambdaRef);
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
            var cycleNames =
                declarations
                .Where((_, i) => !result.Contains(i))
                .SelectMany(d => d.names)
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
    private static Result<CompilationError, Expression> CompileFunctionReference(
        string qualifiedFunctionName,
        int functionIndex,
        ExpressionCompilationContext context)
    {
        // Get the function's info to determine parameter count
        if (context.ModuleCompilationContext.TryGetFunctionInfo(qualifiedFunctionName) is not { } funcInfo)
        {
            return new CompilationError.UnresolvedReference(qualifiedFunctionName, context.CurrentModuleName);
        }

        var paramCount = funcInfo.declaration.Function.Declaration.Arguments.Count;

        // Get the encoded body from the environment.
        // For declarations compiled with env functions, the root environment uses the flat layout
        // [envFunctions, arg0, arg1, ...], so the encoded bodies are in the env-functions list at [0][i].
        var encodedBodyExpr =
            ExpressionBuilder.BuildExpressionForPathInExpression(
                [0, functionIndex],
                Expression.EnvironmentInstance);

        // Build the env functions list expressions based on the function's dependency layout
        var envFunctionsExprs = new List<Expression>();

        if (context.ModuleCompilationContext.TryGetDependencyLayout(qualifiedFunctionName) is { } funcLayout)
        {
            foreach (var depName in funcLayout)
            {
                if (context.GetFunctionIndexInLayout(depName) is { } depIndex)
                {
                    // Get this dependency from the current environment
                    var depRef =
                        ExpressionBuilder.BuildExpressionForPathInExpression(
                            [0, depIndex],
                            Expression.EnvironmentInstance);

                    envFunctionsExprs.Add(depRef);
                }
                else
                {
                    return new CompilationError.FunctionNotInDependencyLayout(depName);
                }
            }
        }

        if (paramCount <= 0)
        {
            // Zero-parameter value declaration: invoke the body immediately
            // to produce the actual value rather than returning an unevaluated function wrapper.
            var envFuncsListExpr = Expression.ListInstance(envFunctionsExprs);

            var callEnvironment =
                Expression.ListInstance([envFuncsListExpr]);

            return
                new Expression.ParseAndEval(
                    encoded: encodedBodyExpr,
                    environment: callEnvironment);
        }

        // Emit the function value expression using FunctionValueBuilder
        return
            FunctionValueBuilder.EmitFunctionExpressionFromEncodedBody(
                encodedBodyExpr,
                paramCount,
                envFunctionsExprs);
    }

    /// <summary>
    /// Emits an expression that, when evaluated, produces a function value for a partially applied 
    /// choice type tag. The captured arguments are evaluated at runtime and stored in the function value's closure.
    /// </summary>
    /// <param name="tagName">The name of the choice type tag.</param>
    /// <param name="totalArgCount">The total number of arguments the tag expects.</param>
    /// <param name="capturedArgExpressions">Expressions for arguments already provided (to be evaluated at runtime).</param>
    /// <returns>An expression that evaluates to a function value.</returns>
    private static Expression EmitChoiceTypeTagPartialApplicationExpression(
        string tagName,
        int totalArgCount,
        IReadOnlyList<Expression> capturedArgExpressions)
    {
        var remainingArgCount = totalArgCount - capturedArgExpressions.Count;

        if (remainingArgCount <= 0)
        {
            // All arguments are already captured - just build the value directly
            var tagNameValue = Expression.LiteralInstance(StringEncoding.ValueFromString(tagName));

            return
                Expression.ListInstance(
                    [
                    tagNameValue,
                    Expression.ListInstance(capturedArgExpressions)
                    ]);
        }

        // Build the inner expression that constructs the choice type value.
        // The inner expression expects flat env layout:
        // - env[0] = captured args list (stored in "env functions" slot)
        // - env[1..N] = remaining args
        //
        // We use FunctionValueBuilder with the captured args stored in the "env functions" slot.

        var tagNameLiteral = Expression.LiteralInstance(StringEncoding.ValueFromString(tagName));

        // Build the inner expression that constructs [TagName, [capturedArg0, capturedArg1, ..., remainingArg0, remainingArg1, ...]]
        // At invocation time, environment is [[capturedArgs], [remainingArgs]]
        var allArgExpressions = new List<Expression>();

        // Build expression to get captured args from env[0]
        for (var i = 0; i < capturedArgExpressions.Count; i++)
        {
            allArgExpressions.Add(
                ExpressionBuilder.BuildExpressionForPathInExpression(
                    [0, i],
                    Expression.EnvironmentInstance));
        }

        // Build expressions to get remaining args from env (flat layout)
        for (var i = 0; i < remainingArgCount; i++)
        {
            allArgExpressions.Add(
                ExpressionBuilder.BuildExpressionForPathInExpression(
                    [1 + i],
                    Expression.EnvironmentInstance));
        }

        // Build the choice type value expression
        var choiceTypeValueExpr =
            Expression.ListInstance(
                [
                tagNameLiteral,
                Expression.ListInstance(allArgExpressions)
                ]);

        // Use EmitFunctionExpressionFromEncodedBody to build a function value with the captured args
        // stored in the "env functions" slot. This allows the captured args to be evaluated at
        // the point where the partial application expression is evaluated, not when it's compiled.
        return
            FunctionValueBuilder.EmitFunctionExpression(
                choiceTypeValueExpr,
                remainingArgCount,
                capturedArgExpressions);
    }

    /// <summary>
    /// Emits a function value for a choice type tag that accepts arguments, with no captured arguments.
    /// This is used when the tag is used as a function value without any application.
    /// When fully applied, the function constructs the choice type value [TagName, [arg0, arg1, ...]].
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

        var tagNameExpr = Expression.LiteralInstance(StringEncoding.ValueFromString(tagName));

        // Build the choice type value expression for when all arguments are collected
        var choiceTypeValueExpr =
            Expression.ListInstance(
                [
                tagNameExpr,
                Expression.ListInstance(argExpressions)
                ]);

        // Build the function value that wraps this expression
        var functionValue =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                choiceTypeValueExpr,
                argCount,
                envFunctions: []);

        return Expression.LiteralInstance(functionValue);
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
                Expression.LiteralInstance(StringEncoding.ValueFromString(pair.fieldName)));

            recordFieldItems.Add(pair.expr);
        }

        return
            Expression.ListInstance(
                [
                Expression.LiteralInstance(ElmValue.ElmRecordTypeTagNameAsValue),
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
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                recordValueExpr,
                argCount,
                envFunctions: []);

        return Expression.LiteralInstance(functionValue);
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

        // Build the inner expression that constructs the record.
        // At invocation time, flat env layout: env[0] = capturedArgs list, env[1..N] = remainingArgs
        var allArgExpressions = new List<Expression>();

        // Build expressions to get captured args from env[0]
        for (var i = 0; i < capturedArgExpressions.Count; i++)
        {
            allArgExpressions.Add(
                ExpressionBuilder.BuildExpressionForPathInExpression(
                    [0, i],
                    Expression.EnvironmentInstance));
        }

        // Build expressions to get remaining args from env (flat layout)
        for (var i = 0; i < remainingArgCount; i++)
        {
            allArgExpressions.Add(
                ExpressionBuilder.BuildExpressionForPathInExpression(
                    [1 + i],
                    Expression.EnvironmentInstance));
        }

        // Build the record value expression
        var recordValueExpr = BuildRecordValueExpression(fieldNamesInDeclOrder, allArgExpressions);

        // Use EmitFunctionExpression to build a function value with the captured args
        return
            FunctionValueBuilder.EmitFunctionExpression(
                recordValueExpr,
                remainingArgCount,
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
                new Expression.ParseAndEval(
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
        KernelFunction.ValueFromBool(value);

    #endregion
}
