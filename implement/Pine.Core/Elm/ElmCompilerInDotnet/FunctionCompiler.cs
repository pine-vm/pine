using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Compiles Elm function declarations to Pine values.
/// </summary>
public class FunctionCompiler
{
    /// <summary>
    /// Shared instance of the function compiler.
    /// </summary>
    public static FunctionCompiler Instance { get; } = new();

    /// <summary>
    /// Compiles a function declaration and returns both the compiled value and the updated context.
    /// </summary>
    /// <param name="functionDeclaration">The function declaration to compile.</param>
    /// <param name="currentModuleName">The name of the module containing the function.</param>
    /// <param name="context">The compilation context.</param>
    /// <returns>A result containing the compiled Pine value and updated context, or a compilation error.</returns>
    public static Result<CompilationError, (PineValue value, ModuleCompilationContext updatedContext)> Compile(
        SyntaxTypes.Declaration.FunctionDeclaration functionDeclaration,
        string currentModuleName,
        ModuleCompilationContext context)
    {
        var functionName = functionDeclaration.Function.Declaration.Value.Name.Value;
        var qualifiedFunctionName = currentModuleName + "." + functionName;

        // Check if we've already compiled this function
        if (context.TryGetCompiledFunction(qualifiedFunctionName, out var cachedValue) && cachedValue is not null)
        {
            return (cachedValue, context);
        }

        var arguments = functionDeclaration.Function.Declaration.Value.Arguments;
        var parameterCount = arguments.Count;
        var functionBody = functionDeclaration.Function.Declaration.Value.Expression.Value;

        // Build parameter name mapping
        var parameterNames = new Dictionary<string, int>();
        var localBindings = new Dictionary<string, Expression>();

        for (var i = 0; i < arguments.Count; i++)
        {
            var argPattern = arguments[i].Value;

            if (argPattern is SyntaxTypes.Pattern.VarPattern varPattern)
            {
                parameterNames[varPattern.Name] = i;
            }
            else
            {
                var paramExpr = BuiltinHelpers.BuildPathToParameter(i);
                var analysis = PatternCompiler.AnalyzePattern(argPattern, paramExpr);
                foreach (var kvp in analysis.Bindings)
                {
                    localBindings[kvp.Key] = kvp.Value;
                }
            }
        }

        // Extract parameter types
        var parameterTypes = ExtractParameterTypes(functionDeclaration.Function);

        // Analyze dependencies
        var dependencies = AnalyzeFunctionDependencies(functionBody, currentModuleName, context);

        // Create dependency layout
        var dependencyLayout = new List<string> { qualifiedFunctionName };
        dependencyLayout.AddRange(dependencies);

        // Create expression compilation context
        var expressionContext = new ExpressionCompilationContext(
            ParameterNames: parameterNames,
            ParameterTypes: parameterTypes,
            CurrentModuleName: currentModuleName,
            CurrentFunctionName: functionName,
            LocalBindings: localBindings.Count > 0 ? localBindings : null,
            LocalBindingTypes: null,
            DependencyLayout: dependencyLayout,
            ModuleCompilationContext: context,
            FunctionReturnTypes: context.FunctionReturnTypes);

        // Compile the function body
        var compiledBodyResult = ExpressionCompiler.Compile(functionBody, expressionContext);
        if (compiledBodyResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        var compiledBodyExpression = compiledBodyResult.IsOkOrNull()!;

        // For zero-parameter functions with literal value
        if (compiledBodyExpression is Expression.Literal literalExpr)
        {
            var result = EmitPlainValueDeclaration(literalExpr.Value);
            return (result, context.WithCompiledFunction(qualifiedFunctionName, result, dependencyLayout));
        }

        // For functions with parameters, create a FunctionRecord
        var encodedFunction = ExpressionEncoding.EncodeExpressionAsValue(compiledBodyExpression);
        var envFunctionsList = new List<PineValue> { encodedFunction };

        var placeholderResult = FunctionRecord.EncodeFunctionRecordInValueTagged(
            new FunctionRecord(
                InnerFunction: compiledBodyExpression,
                ParameterCount: parameterCount,
                EnvFunctions: envFunctionsList.ToArray(),
                ArgumentsAlreadyCollected: ReadOnlyMemory<PineValue>.Empty));

        context = context.WithCompiledFunction(qualifiedFunctionName, placeholderResult, dependencyLayout);

        // Compile dependencies
        foreach (var depQualifiedName in dependencies)
        {
            if (context.TryGetFunctionInfo(depQualifiedName, out var depInfo))
            {
                var depResult = Compile(depInfo.declaration, depInfo.moduleName, context);
                if (depResult.IsErrOrNull() is { } depErr)
                {
                    return depErr;
                }

                // Since we checked for error above, we know it's Ok
                if (depResult is Result<CompilationError, (PineValue value, ModuleCompilationContext updatedContext)>.Ok ok)
                {
                    var (depCompiled, updatedContext) = ok.Value;
                    context = updatedContext;

                    var parseCache = new PineVMParseCache();
                    var parsedFuncResult = FunctionRecord.ParseFunctionRecordTagged(depCompiled, parseCache);

                    if (parsedFuncResult.IsOkOrNull() is { } parsedFunc)
                    {
                        var encodedDepExpr = ExpressionEncoding.EncodeExpressionAsValue(parsedFunc.InnerFunction);
                        envFunctionsList.Add(encodedDepExpr);
                    }
                    else
                    {
                        envFunctionsList.Add(depCompiled);
                    }
                }
            }
        }

        // Create final result
        var finalResult = FunctionRecord.EncodeFunctionRecordInValueTagged(
            new FunctionRecord(
                InnerFunction: compiledBodyExpression,
                ParameterCount: parameterCount,
                EnvFunctions: envFunctionsList.ToArray(),
                ArgumentsAlreadyCollected: ReadOnlyMemory<PineValue>.Empty));

        return (finalResult, context.WithCompiledFunction(qualifiedFunctionName, finalResult, dependencyLayout));
    }

    private static Dictionary<string, TypeInference.InferredType> ExtractParameterTypes(
        SyntaxTypes.FunctionStruct function)
    {
        var parameterTypes = new Dictionary<string, TypeInference.InferredType>();

        if (function.Signature?.Value is { } signature)
        {
            var typeAnnotation = signature.TypeAnnotation.Value;
            var parameters = function.Declaration.Value.Arguments;

            var currentType = typeAnnotation;
            var paramIndex = 0;

            while (currentType is SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation funcType &&
                   paramIndex < parameters.Count)
            {
                var paramPattern = parameters[paramIndex].Value;
                var paramTypeAnnotation = funcType.ArgumentType.Value;

                // Extract binding types from the pattern
                // This handles both simple VarPattern and complex patterns like TuplePattern
                TypeInference.ExtractPatternBindingTypes(paramPattern, paramTypeAnnotation, parameterTypes);

                currentType = funcType.ReturnType.Value;
                paramIndex++;
            }
        }

        return parameterTypes;
    }

    private static List<string> AnalyzeFunctionDependencies(
        SyntaxTypes.Expression expression,
        string currentModuleName,
        ModuleCompilationContext context)
    {
        var dependencies = new HashSet<string>();

        void AnalyzeExpression(SyntaxTypes.Expression expr)
        {
            switch (expr)
            {
                case SyntaxTypes.Expression.Application application:
                    if (application.Arguments.Count >= 2 &&
                        application.Arguments[0].Value is SyntaxTypes.Expression.FunctionOrValue funcRef)
                    {
                        if (funcRef.ModuleName.Count is 1 && context.IsPineKernelModule(funcRef.ModuleName[0]))
                        {
                            foreach (var arg in application.Arguments)
                            {
                                AnalyzeExpression(arg.Value);
                            }
                            break;
                        }

                        if (ElmValueEncoding.StringIsValidTagName(funcRef.Name))
                        {
                            foreach (var arg in application.Arguments)
                            {
                                AnalyzeExpression(arg.Value);
                            }
                            break;
                        }

                        var qualifiedName =
                            funcRef.ModuleName.Count > 0
                            ?
                            string.Join(".", funcRef.ModuleName) + "." + funcRef.Name
                            :
                            currentModuleName + "." + funcRef.Name;

                        dependencies.Add(qualifiedName);
                    }

                    foreach (var arg in application.Arguments)
                    {
                        AnalyzeExpression(arg.Value);
                    }
                    break;

                case SyntaxTypes.Expression.OperatorApplication operatorApp:
                    AnalyzeExpression(operatorApp.Left.Value);
                    AnalyzeExpression(operatorApp.Right.Value);
                    break;

                case SyntaxTypes.Expression.IfBlock ifBlock:
                    AnalyzeExpression(ifBlock.Condition.Value);
                    AnalyzeExpression(ifBlock.ThenBlock.Value);
                    AnalyzeExpression(ifBlock.ElseBlock.Value);
                    break;

                case SyntaxTypes.Expression.ListExpr listExpr:
                    foreach (var elem in listExpr.Elements)
                    {
                        AnalyzeExpression(elem.Value);
                    }
                    break;

                case SyntaxTypes.Expression.ParenthesizedExpression parenthesized:
                    AnalyzeExpression(parenthesized.Expression.Value);
                    break;

                case SyntaxTypes.Expression.Negation negation:
                    AnalyzeExpression(negation.Expression.Value);
                    break;
            }
        }

        AnalyzeExpression(expression);

        return [.. dependencies];
    }

    private static PineValue EmitPlainValueDeclaration(PineValue value) =>
        FunctionRecord.EncodeFunctionRecordInValueTagged(
            new FunctionRecord(
                InnerFunction: Expression.LiteralInstance(value),
                ParameterCount: 0,
                EnvFunctions: ReadOnlyMemory<PineValue>.Empty,
                ArgumentsAlreadyCollected: ReadOnlyMemory<PineValue>.Empty));
}
