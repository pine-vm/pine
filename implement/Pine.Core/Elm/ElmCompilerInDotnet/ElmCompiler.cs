using Pine.Core.CodeAnalysis;
using Pine.Core.Files;
using Pine.Core.CommonEncodings;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;
using System.Text;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Methods for compiling Elm source code and environments into Pine values, using the standard packaging for
/// Elm functions and modules as found at
/// <see href="https://github.com/pine-vm/pine/blob/391100e6734a50d2bede29ee49bca1afc8868fed/implement/pine/Elm/elm-compiler/src/FirCompiler.elm#L2179-L2242"></see>
/// and
/// <see href="https://github.com/pine-vm/pine/blob/391100e6734a50d2bede29ee49bca1afc8868fed/implement/pine/Elm/elm-compiler/src/ElmCompiler.elm"></see>
/// </summary>
public class ElmCompiler
{
    private static readonly FrozenSet<string> s_pineKernelModuleNamesDefault =
        FrozenSet.Create(
            [
            "Pine_builtin",
            "Pine_kernel",
            ]);

    public static Result<string, PineValue> CompileInteractiveEnvironment(
        FileTree appCodeTree,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths,
        bool disableInlining)
    {
        /*
         * WIP TODO:
         * Implement filtering, ordering.
         */

        var elmModuleFiles =
            appCodeTree.EnumerateFilesTransitive()
            .Where(file => file.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .ToImmutableArray();

        // First pass: Parse all modules and collect function declarations
        var parsedModulesBeforeCanonicalize =
            new List<SyntaxTypes.File>();

        foreach (var moduleFile in elmModuleFiles)
        {
            var moduleText =
                Encoding.UTF8.GetString(moduleFile.fileContent.Span);

            var parseResult =
                ElmSyntax.ElmSyntaxParser.ParseModuleText(moduleText);

            {
                if (parseResult.IsErrOrNull() is { } err)
                {
                    return err;
                }
            }

            if (parseResult.IsOkOrNull() is not { } parseModuleOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse result type: " + parseResult.GetType().Name);
            }

            var moduleName =
                SyntaxTypes.Module.GetModuleName(parseModuleOk.ModuleDefinition.Value).Value;

            var moduleNameFlattened =
                string.Join(".", moduleName);

            parsedModulesBeforeCanonicalize.Add(parseModuleOk);
        }

        var canonicalizationResult =
            Canonicalization.Canonicalize(parsedModulesBeforeCanonicalize);

        {
            if (canonicalizationResult.IsErrOrNull() is { } err)
            {
                return err;
            }
        }

        if (canonicalizationResult.IsOkOrNull() is not { } canonicalizedModulesDict)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + canonicalizationResult.GetType().Name);
        }

        // Check if any module has canonicalization errors
        var moduleErrors = new List<string>();
        foreach (var (moduleName, moduleResult) in canonicalizedModulesDict)
        {
            if (moduleResult.IsErrOrNull() is { } moduleErr)
            {
                var moduleNameStr = string.Join(".", moduleName);
                moduleErrors.Add($"In module {moduleNameStr}:\n{moduleErr}");
            }
        }

        if (moduleErrors.Any())
        {
            return string.Join("\n\n", moduleErrors);
        }

        // Extract all successfully canonicalized modules
        var canonicalizedModules = canonicalizedModulesDict
            .Select(kvp => kvp.Value.IsOkOrNull())
            .WhereNotNull()
            .ToList();

        var allFunctions =
            new Dictionary<string, (string moduleName, string functionName, SyntaxTypes.Declaration.FunctionDeclaration declaration)>();

        foreach (var elmModuleSyntax in canonicalizedModules)
        {
            var moduleName =
                SyntaxTypes.Module.GetModuleName(elmModuleSyntax.ModuleDefinition.Value).Value;

            var moduleNameFlattened =
                string.Join(".", moduleName);

            var declarations =
                elmModuleSyntax.Declarations
                .Select(declNode => declNode.Value)
                .OfType<SyntaxTypes.Declaration.FunctionDeclaration>();

            foreach (var declaration in declarations)
            {
                var functionName =
                    declaration.Function.Declaration.Value.Name.Value;

                var qualifiedName =
                    moduleNameFlattened + "." + functionName;

                allFunctions[qualifiedName] = (moduleNameFlattened, functionName, declaration);
            }
        }

        // Create compilation context with all available functions and a cache
        var compilationContext =
            new CompilationContext(
                allFunctions,
                CompiledFunctionsCache: [],
                PineKernelModuleNames: s_pineKernelModuleNamesDefault);

        // Second pass: Compile each module with access to all functions
        var compiledModuleEntries = new List<PineValue>();

        foreach (var parsedModule in canonicalizedModules)
        {
            var moduleNameFlattened =
                string.Join('.', SyntaxTypes.Module.GetModuleName(parsedModule.ModuleDefinition.Value).Value);

            var moduleValue = CompileModule(parsedModule, moduleNameFlattened, compilationContext);

            var namedModuleEntry =
                PineValue.List(
                    [
                    StringEncoding.ValueFromString(moduleNameFlattened),
                    moduleValue
                    ]);

            compiledModuleEntries.Add(namedModuleEntry);
        }

        var compiledEnvValue =
            PineValue.List(
                [
                ..compiledModuleEntries
                ]);

        return compiledEnvValue;
    }

    private static PineValue CompileModule(
        SyntaxTypes.File parsedModule,
        string currentModuleName,
        CompilationContext context)
    {
        var declarations =
            parsedModule.Declarations
            .Select(declNode => declNode.Value)
            .OfType<SyntaxTypes.Declaration.FunctionDeclaration>();

        IReadOnlyList<(string, PineValue)> compiledFunctions =
            [..
            declarations
            .Select(declaration =>
            (declaration.Function.Declaration.Value.Name.Value,
            CompileFunctionDeclaration(declaration, currentModuleName, context)))
            ];

        var compiledFunctionDeclaration =
            new ElmModuleInCompilation(
                FunctionDeclarations: compiledFunctions);

        var moduleValue =
            EmitModuleValue(compiledFunctionDeclaration);

        return moduleValue;
    }

    /*
    type alias ElmModuleInCompilation =
        { functionDeclarations : List ( String, Pine.Value )
        , typeDeclarations : List ( String, ElmModuleTypeDeclaration )
        }
     * */

    private record CompilationContext(
        IReadOnlyDictionary<string, (string moduleName, string functionName, SyntaxTypes.Declaration.FunctionDeclaration declaration)> AllFunctions,
        Dictionary<string, PineValue> CompiledFunctionsCache,
        FrozenSet<string> PineKernelModuleNames);

    private record ElmModuleInCompilation(
        IReadOnlyList<(string declName, PineValue)> FunctionDeclarations);

    private static PineValue CompileFunctionDeclaration(
        SyntaxTypes.Declaration.FunctionDeclaration functionDeclaration,
        string currentModuleName,
        CompilationContext context)
    {
        var functionName =
            functionDeclaration.Function.Declaration.Value.Name.Value;

        var qualifiedFunctionName =
            currentModuleName + "." + functionName;

        // Check if we've already compiled this function (to avoid infinite recursion)
        if (context.CompiledFunctionsCache.TryGetValue(qualifiedFunctionName, out var cachedValue))
        {
            return cachedValue;
        }

        var arguments = functionDeclaration.Function.Declaration.Value.Arguments;
        var parameterCount = arguments.Count;

        var functionBody =
            functionDeclaration.Function.Declaration.Value.Expression.Value;

        // Build parameter name mapping: parameter names to their indices
        var parameterNames = new Dictionary<string, int>();

        for (var i = 0; i < arguments.Count; i++)
        {
            if (arguments[i].Value is SyntaxTypes.Pattern.VarPattern varPattern)
            {
                parameterNames[varPattern.Name] = i;
            }
        }

        // Extract parameter types from the function signature if available
        var parameterTypes = ExtractParameterTypes(functionDeclaration.Function);

        // Analyze dependencies to determine which functions this function calls
        var dependencies =
            AnalyzeFunctionDependencies(
                functionBody,
                currentModuleName,
                context);

        // Create the function dependency layout: [self, dependencies...]
        // The current function is always at index 0
        var dependencyLayout = new List<string> { qualifiedFunctionName };

        dependencyLayout.AddRange(dependencies);

        // Compile the function body with knowledge of the dependency layout
        var compiledBodyExpression =
            CompileExpression(
                functionBody,
                parameterNames,
                parameterTypes,
                currentModuleName,
                functionName,
                context,
                dependencyLayout);

        // For zero-parameter functions, check if it's a simple literal value
        if (compiledBodyExpression is Expression.Literal literalExpr)
        {
            var result = EmitPlainValueDeclaration(literalExpr.Value);
            context.CompiledFunctionsCache[qualifiedFunctionName] = result;
            return result;
        }

        // For functions with parameters, create a FunctionRecord
        var encodedFunction =
            ExpressionEncoding.EncodeExpressionAsValue(compiledBodyExpression);

        // Build the EnvFunctions list with encoded dependencies
        var envFunctionsList = new List<PineValue> { encodedFunction };

        // Add placeholder for self-reference first
        var placeholderResult =
            FunctionRecord.EncodeFunctionRecordInValueTagged(
                new FunctionRecord(
                    InnerFunction: compiledBodyExpression,
                    ParameterCount: parameterCount,
                    EnvFunctions: envFunctionsList.ToArray(),
                    ArgumentsAlreadyCollected: ReadOnlyMemory<PineValue>.Empty));

        context.CompiledFunctionsCache[qualifiedFunctionName] = placeholderResult;

        // Now compile dependencies (they can reference this function via the cache)
        foreach (var depQualifiedName in dependencies)
        {
            if (context.AllFunctions.TryGetValue(depQualifiedName, out var depInfo))
            {
                // Compile the dependency function
                CompileFunctionDeclaration(depInfo.declaration, depInfo.moduleName, context);

                // Get the compiled function from cache
                if (context.CompiledFunctionsCache.TryGetValue(depQualifiedName, out var depCompiled))
                {
                    // Parse the FunctionRecord to extract the InnerFunction expression
                    var parseCache = new PineVMParseCache();
                    var parsedFuncResult = FunctionRecord.ParseFunctionRecordTagged(depCompiled, parseCache);

                    if (parsedFuncResult.IsOkOrNull() is { } parsedFunc)
                    {
                        // Encode the InnerFunction expression
                        var encodedDepExpr = ExpressionEncoding.EncodeExpressionAsValue(parsedFunc.InnerFunction);
                        envFunctionsList.Add(encodedDepExpr);
                    }
                    else
                    {
                        // If parsing fails, just use the compiled value (may cause issues)
                        envFunctionsList.Add(depCompiled);
                    }
                }
            }
        }

        // Create the final result with all dependencies
        var finalResult =
            FunctionRecord.EncodeFunctionRecordInValueTagged(
                new FunctionRecord(
                    InnerFunction: compiledBodyExpression,
                    ParameterCount: parameterCount,
                    EnvFunctions: envFunctionsList.ToArray(),
                    ArgumentsAlreadyCollected: ReadOnlyMemory<PineValue>.Empty));

        context.CompiledFunctionsCache[qualifiedFunctionName] = finalResult;

        return finalResult;
    }

    private static List<string> AnalyzeFunctionDependencies(
        SyntaxTypes.Expression expression,
        string currentModuleName,
        CompilationContext context)
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
                        // Skip Pine_kernel functions
                        if (funcRef.ModuleName.Count is 1 && context.PineKernelModuleNames.Contains(funcRef.ModuleName[0]))
                        {
                            // Still recurse into arguments
                            foreach (var arg in application.Arguments)
                            {
                                AnalyzeExpression(arg.Value);
                            }
                            break;
                        }

                        // Skip choice type tag constructors
                        if (ElmValueEncoding.StringIsValidTagName(funcRef.Name))
                        {
                            // Still recurse into arguments
                            foreach (var arg in application.Arguments)
                            {
                                AnalyzeExpression(arg.Value);
                            }
                            break;
                        }

                        // Determine qualified name
                        string qualifiedName;

                        if (funcRef.ModuleName.Count > 0)
                        {
                            qualifiedName = string.Join(".", funcRef.ModuleName) + "." + funcRef.Name;
                        }
                        else
                        {
                            qualifiedName = currentModuleName + "." + funcRef.Name;
                        }

                        dependencies.Add(qualifiedName);
                    }

                    // Recurse into arguments
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

    private static Dictionary<string, TypeInference.InferredType> ExtractParameterTypes(
        SyntaxTypes.FunctionStruct function)
    {
        var parameterTypes = new Dictionary<string, TypeInference.InferredType>();

        if (function.Signature?.Value is { } signature)
        {
            var typeAnnotation = signature.TypeAnnotation.Value;
            var parameters = function.Declaration.Value.Arguments;

            // Walk through the function type annotation to extract parameter types
            var currentType = typeAnnotation;
            var paramIndex = 0;

            while (currentType is SyntaxTypes.TypeAnnotation.FunctionTypeAnnotation funcType &&
                   paramIndex < parameters.Count)
            {
                if (parameters[paramIndex].Value is SyntaxTypes.Pattern.VarPattern varPattern)
                {
                    var paramType = TypeInference.TypeAnnotationToInferredType(funcType.ArgumentType.Value);
                    parameterTypes[varPattern.Name] = paramType;
                }

                currentType = funcType.ReturnType.Value;
                paramIndex++;
            }
        }

        return parameterTypes;
    }

    private static Expression CompileExpression(
        SyntaxTypes.Expression expression,
        IReadOnlyDictionary<string, int> parameterNames,
        IReadOnlyDictionary<string, TypeInference.InferredType> parameterTypes,
        string currentModuleName,
        string? currentFunctionName,
        CompilationContext context,
        IReadOnlyList<string> dependencyLayout,
        IReadOnlyDictionary<string, Expression>? localBindings = null)
    {
        if (expression is SyntaxTypes.Expression.Integer integerLiteral)
        {
            return Expression.LiteralInstance(
                EmitIntegerLiteral(integerLiteral.Value));
        }

        if (expression is SyntaxTypes.Expression.Literal stringLiteral)
        {
            return Expression.LiteralInstance(
                EmitStringLiteral(stringLiteral.Value));
        }

        if (expression is SyntaxTypes.Expression.CharLiteral charLiteral)
        {
            return Expression.LiteralInstance(
                EmitCharLiteral(charLiteral.Value));
        }

        if (expression is SyntaxTypes.Expression.FunctionOrValue functionOrValue)
        {
            if (functionOrValue.ModuleName.Count is 0)
            {
                // Check if it's a local binding from pattern matching first
                if (localBindings is not null &&
                    localBindings.TryGetValue(functionOrValue.Name, out var bindingExpr))
                {
                    return bindingExpr;
                }

                // Check if it's a parameter reference
                if (parameterNames.TryGetValue(functionOrValue.Name, out var paramIndex))
                {
                    // Generate environment path for parameter: [1, paramIndex]
                    // Environment structure: [0] = function list, [1] = parameter list
                    return BuildPathToParameter(paramIndex);
                }

                // Check if it's a choice type tag (starts with uppercase letter)
                if (ElmValueEncoding.StringIsValidTagName(functionOrValue.Name))
                {
                    // This is a choice type constructor with no arguments
                    // Emit as: [tagName, []]
                    return Expression.LiteralInstance(
                        ElmValueEncoding.TagAsPineValue(functionOrValue.Name, []));
                }
            }

            if (functionOrValue.ModuleName.Count is 1 &&
                functionOrValue.ModuleName[0] is "Basics")
            {
                if (functionOrValue.Name is "True")
                {
                    return Expression.LiteralInstance(
                        EmitBooleanLiteral(true));
                }

                if (functionOrValue.Name is "False")
                {
                    return Expression.LiteralInstance(
                        EmitBooleanLiteral(false));
                }
            }

            // After canonicalization, tags have a module name but are still recognized
            // by having an uppercase first letter
            if (ElmValueEncoding.StringIsValidTagName(functionOrValue.Name))
            {
                // This is a choice type constructor with no arguments (from any module)
                // Emit as: [tagName, []]
                return Expression.LiteralInstance(
                    ElmValueEncoding.TagAsPineValue(functionOrValue.Name, []));
            }
        }

        if (expression is SyntaxTypes.Expression.Application application)
        {
            return
                CompileApplication(
                    application,
                    parameterNames,
                    parameterTypes,
                    currentModuleName,
                    currentFunctionName,
                    context,
                    dependencyLayout,
                    localBindings);
        }

        if (expression is SyntaxTypes.Expression.ListExpr listExpr)
        {
            var compiledElements =
                listExpr.Elements
                .Select(elem =>
                CompileExpression(
                    elem.Value,
                    parameterNames,
                    parameterTypes,
                    currentModuleName,
                    currentFunctionName,
                    context,
                    dependencyLayout,
                    localBindings))
                .ToList();

            return Expression.ListInstance(compiledElements);
        }

        if (expression is SyntaxTypes.Expression.OperatorApplication operatorApp)
        {
            return
                CompileOperatorApplication(
                    operatorApp,
                    parameterNames,
                    parameterTypes,
                    currentModuleName,
                    currentFunctionName,
                    context,
                    dependencyLayout,
                    localBindings);
        }

        if (expression is SyntaxTypes.Expression.ParenthesizedExpression parenthesized)
        {
            // Parentheses just group expressions, compile the inner expression

            return
                CompileExpression(
                    parenthesized.Expression.Value,
                    parameterNames,
                    parameterTypes,
                    currentModuleName,
                    currentFunctionName,
                    context,
                    dependencyLayout,
                    localBindings);
        }

        if (expression is SyntaxTypes.Expression.Negation negation)
        {
            // Negation: for literals, use the negated value directly
            if (negation.Expression.Value is SyntaxTypes.Expression.Integer intLiteral)
            {
                return Expression.LiteralInstance(EmitIntegerLiteral(-intLiteral.Value));
            }

            // For non-literals, compile as multiplication by -1
            var innerExpression =
                CompileExpression(
                    negation.Expression.Value,
                    parameterNames,
                    parameterTypes,
                    currentModuleName,
                    currentFunctionName,
                    context,
                    dependencyLayout,
                    localBindings);

            var negativeOne = Expression.LiteralInstance(EmitIntegerLiteral(-1));

            return Expression.KernelApplicationInstance(
                nameof(KernelFunction.int_mul),
                Expression.ListInstance([negativeOne, innerExpression]));
        }

        if (expression is SyntaxTypes.Expression.IfBlock ifBlock)
        {
            var condition =
                CompileExpression(
                    ifBlock.Condition.Value,
                    parameterNames,
                    parameterTypes,
                    currentModuleName,
                    currentFunctionName,
                    context,
                    dependencyLayout,
                    localBindings);

            var trueBranch =
                CompileExpression(
                    ifBlock.ThenBlock.Value,
                    parameterNames,
                    parameterTypes,
                    currentModuleName,
                    currentFunctionName,
                    context,
                    dependencyLayout,
                    localBindings);

            var falseBranch =
                CompileExpression(
                    ifBlock.ElseBlock.Value,
                    parameterNames,
                    parameterTypes,
                    currentModuleName,
                    currentFunctionName,
                    context,
                    dependencyLayout,
                    localBindings);

            return
                Expression.ConditionalInstance(
                    condition: condition,
                    falseBranch: falseBranch,
                    trueBranch: trueBranch);
        }

        if (expression is SyntaxTypes.Expression.CaseExpression caseExpression)
        {
            return
                CompileCaseExpression(
                    caseExpression.CaseBlock,
                    parameterNames,
                    parameterTypes,
                    currentModuleName,
                    currentFunctionName,
                    context,
                    dependencyLayout);
        }

        throw new NotImplementedException(
            "Expression type not supported in this temporary implementation: " +
            expression.GetType().Name);
    }

    private static Expression CompileApplication(
        SyntaxTypes.Expression.Application application,
        IReadOnlyDictionary<string, int> parameterNames,
        IReadOnlyDictionary<string, TypeInference.InferredType> parameterTypes,
        string currentModuleName,
        string? currentFunctionName,
        CompilationContext context,
        IReadOnlyList<string> dependencyLayout,
        IReadOnlyDictionary<string, Expression>? localBindings = null)
    {
        if (application.Arguments.Count < 2)
        {
            throw new NotImplementedException(
                "Application must have at least 2 arguments (function and argument)");
        }

        // Check if this is a Pine_kernel application
        var firstArg = application.Arguments[0].Value;

        if (firstArg is SyntaxTypes.Expression.FunctionOrValue kernelFunc &&
            kernelFunc.ModuleName.Count is 1 &&
            context.PineKernelModuleNames.Contains(kernelFunc.ModuleName[0]))
        {
            // This is a kernel function application
            var kernelFunctionName = kernelFunc.Name;

            // The second argument should be the input to the kernel function
            var kernelInput = application.Arguments[1].Value;

            var compiledInput =
                CompileExpression(
                    kernelInput,
                    parameterNames,
                    parameterTypes,
                    currentModuleName,
                    currentFunctionName,
                    context,
                    dependencyLayout,
                    localBindings);

            return Expression.KernelApplicationInstance(
                kernelFunctionName,
                compiledInput);
        }

        // Check if this is a function application or choice type tag application
        if (firstArg is SyntaxTypes.Expression.FunctionOrValue funcRef)
        {
            // Check if this is a choice type tag application (starts with uppercase)
            // Tags are identified by having a valid tag name (starting with uppercase letter)
            if (ElmValueEncoding.StringIsValidTagName(funcRef.Name))
            {
                // This is a choice type constructor with arguments
                // Compile as: [tagName, [arg1, arg2, ...]]
                var tagNameValue = Expression.LiteralInstance(StringEncoding.ValueFromString(funcRef.Name));

                // Compile all arguments (excluding the tag name which is the first element)
                var compiledArguments = new List<Expression>();
                for (var i = 1; i < application.Arguments.Count; i++)
                {
                    var arg = application.Arguments[i].Value;
                    var compiledArg =
                        CompileExpression(
                            arg,
                            parameterNames,
                            parameterTypes,
                            currentModuleName,
                            currentFunctionName,
                            context,
                            dependencyLayout,
                            localBindings);
                    compiledArguments.Add(compiledArg);
                }

                // Create the tag structure: [tagName, [args...]]
                return Expression.ListInstance(
                    [
                        tagNameValue,
                        Expression.ListInstance(compiledArguments)
                    ]);
            }

            // Determine the qualified name of the function being called
            string qualifiedFunctionName;

            if (funcRef.ModuleName.Count > 0)
            {
                // Function from another module
                qualifiedFunctionName = string.Join(".", funcRef.ModuleName) + "." + funcRef.Name;
            }
            else
            {
                // Function from the same module
                qualifiedFunctionName = currentModuleName + "." + funcRef.Name;
            }

            // Find the index of this function in the dependency layout
            var functionIndex = -1;
            for (var i = 0; i < dependencyLayout.Count; i++)
            {
                if (dependencyLayout[i] == qualifiedFunctionName)
                {
                    functionIndex = i;
                    break;
                }
            }

            if (functionIndex < 0)
            {
                throw new NotImplementedException(
                    $"Function '{qualifiedFunctionName}' not found in dependency layout");
            }

            // Compile the argument
            var argument = application.Arguments[1].Value;

            var compiledArgument =
                CompileExpression(
                    argument,
                    parameterNames,
                    parameterTypes,
                    currentModuleName,
                    currentFunctionName,
                    context,
                    dependencyLayout,
                    localBindings);

            // Build reference to the encoded function at environment[0][functionIndex]
            // Start with environment
            var functionRef =
                ExpressionBuilder.BuildExpressionForPathInExpression(
                    [0, functionIndex],
                    Expression.EnvironmentInstance);

            // Get the function list from current environment for the new environment
            var functionList =
                ExpressionBuilder.BuildExpressionForPathInExpression(
                    [0],
                    Expression.EnvironmentInstance);

            // Construct environment for the function call: [functionList, [argument]]
            var callEnvironment =
                Expression.ListInstance(
                    [
                    functionList,
                    Expression.ListInstance([compiledArgument])
                    ]);

            // Use ParseAndEval to apply the function
            return new Expression.ParseAndEval(
                encoded: functionRef,
                environment: callEnvironment);
        }

        throw new NotImplementedException(
            "Only Pine_kernel applications and function references are supported in this temporary implementation");
    }

    private static Expression CompileOperatorApplication(
        SyntaxTypes.Expression.OperatorApplication operatorApp,
        IReadOnlyDictionary<string, int> parameterNames,
        IReadOnlyDictionary<string, TypeInference.InferredType> parameterTypes,
        string currentModuleName,
        string? currentFunctionName,
        CompilationContext context,
        IReadOnlyList<string> dependencyLayout,
        IReadOnlyDictionary<string, Expression>? localBindings = null)
    {
        // Use type inference to determine the operation type
        var expressionType =
            TypeInference.InferExpressionType(operatorApp, parameterNames, parameterTypes);

        if (expressionType is TypeInference.InferredType.IntType)
        {
            /*
             * Pine offers integer addition and multiplication as kernel functions.
             * Therefore, if we have proven the type to be Int, we can map to
             * kernel functions directly instead of 'Basics.*' core library functions.
             * */

            // Handle subtraction specially: a - b = a + (-1 * b)
            if (operatorApp.Operator is "-")
            {
                var leftCompiled =
                    CompileExpression(operatorApp.Left.Value, parameterNames, parameterTypes, currentModuleName, currentFunctionName, context, dependencyLayout, localBindings);

                var rightCompiled =
                    CompileExpression(operatorApp.Right.Value, parameterNames, parameterTypes, currentModuleName, currentFunctionName, context, dependencyLayout, localBindings);

                // Create: -1 * rightCompiled
                var negatedRight = Expression.KernelApplicationInstance(
                    nameof(KernelFunction.int_mul),
                    Expression.ListInstance([
                        Expression.LiteralInstance(EmitIntegerLiteral(-1)),
                        rightCompiled
                    ]));

                // Create: leftCompiled + negatedRight
                return Expression.KernelApplicationInstance(
                    nameof(KernelFunction.int_add),
                    Expression.ListInstance([leftCompiled, negatedRight]));
            }

            var kernelFunctionName = operatorApp.Operator switch
            {
                "+" => nameof(KernelFunction.int_add),
                "*" => nameof(KernelFunction.int_mul),
                _ => null
            };

            if (kernelFunctionName is not null)
            {
                // Compile the operands
                var leftCompiled =
                    CompileExpression(
                        operatorApp.Left.Value,
                        parameterNames,
                        parameterTypes,
                        currentModuleName,
                        currentFunctionName,
                        context,
                        dependencyLayout,
                        localBindings);

                var rightCompiled =
                    CompileExpression(
                        operatorApp.Right.Value,
                        parameterNames,
                        parameterTypes,
                        currentModuleName,
                        currentFunctionName,
                        context,
                        dependencyLayout,
                        localBindings);

                // Create a list with both operands
                var operandsList = Expression.ListInstance([leftCompiled, rightCompiled]);

                // Return the kernel application
                return Expression.KernelApplicationInstance(
                    kernelFunctionName,
                    operandsList);
            }
        }

        throw new NotImplementedException(
            $"Operator '{operatorApp.Operator}' is not yet supported in this temporary implementation");
    }

    private static Expression CompileCaseExpression(
        SyntaxTypes.CaseBlock caseBlock,
        IReadOnlyDictionary<string, int> parameterNames,
        IReadOnlyDictionary<string, TypeInference.InferredType> parameterTypes,
        string currentModuleName,
        string? currentFunctionName,
        CompilationContext context,
        IReadOnlyList<string> dependencyLayout)
    {
        // Compile the scrutinee (the expression being matched)
        var scrutinee =
            CompileExpression(
                caseBlock.Expression.Value,
                parameterNames,
                parameterTypes,
                currentModuleName,
                currentFunctionName,
                context,
                dependencyLayout);

        // Compile cases from bottom to top, creating nested conditionals
        // The last case becomes the default/else branch
        // We initialize with null and expect to always replace it through the fold.
        // For well-formed Elm code with exhaustive pattern matching, this should always be replaced.
        Expression? result = null;

        // Process cases in reverse order to build the conditional chain
        for (var i = caseBlock.Cases.Count - 1; i >= 0; i--)
        {
            var caseItem = caseBlock.Cases[i];
            var pattern = caseItem.Pattern.Value;

            // Extract bindings from the pattern
            var patternBindings = ExtractPatternBindings(pattern, scrutinee);

            // Compile the case expression body with the pattern bindings
            var caseBody =
                CompileExpression(
                    caseItem.Expression.Value,
                    parameterNames,
                    parameterTypes,
                    currentModuleName,
                    currentFunctionName,
                    context,
                    dependencyLayout,
                    localBindings: patternBindings.Count > 0 ? patternBindings : null);

            // Generate condition based on pattern type
            var conditionExpr = CompilePatternCondition(pattern, scrutinee);

            if (conditionExpr is null)
            {
                // AllPattern (_) or VarPattern - catch-all, this becomes the default branch
                result = caseBody;
            }
            else
            {
                // Create a conditional: if (pattern matches) then caseBody else result
                // If result is null, this means we don't have a fallback yet - use caseBody as both branches
                // (This handles the case when the first pattern in reverse order is not a catch-all)
                result =
                    Expression.ConditionalInstance(
                        condition: conditionExpr,
                        trueBranch: caseBody,
                        falseBranch: result ?? caseBody);
            }
        }

        // After processing all cases, result should be non-null if patterns are exhaustive
        if (result is null)
        {
            throw new InvalidOperationException(
                "Case expression has no patterns - this should not happen with well-formed Elm code");
        }

        return result;
    }

    /// <summary>
    /// Extract variable bindings from a pattern and return expressions to access their values.
    /// </summary>
    private static Dictionary<string, Expression> ExtractPatternBindings(
        SyntaxTypes.Pattern pattern,
        Expression scrutinee)
    {
        var bindings = new Dictionary<string, Expression>();

        ExtractPatternBindingsRecursive(pattern, scrutinee, bindings);

        return bindings;
    }

    private static void ExtractPatternBindingsRecursive(
        SyntaxTypes.Pattern pattern,
        Expression scrutinee,
        Dictionary<string, Expression> bindings)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.VarPattern varPattern:
                // Variable pattern binds the entire scrutinee to the variable name
                bindings[varPattern.Name] = scrutinee;
                break;

            case SyntaxTypes.Pattern.ParenthesizedPattern parenthesizedPattern:
                // Parenthesized pattern just wraps another pattern - unwrap and recurse
                ExtractPatternBindingsRecursive(parenthesizedPattern.Pattern.Value, scrutinee, bindings);
                break;

            case SyntaxTypes.Pattern.UnConsPattern unConsPattern:
                // For head :: tail pattern:
                // - head binds to head(scrutinee)
                // - tail binds to skip([1, scrutinee])
                var headExpr =
                    Expression.KernelApplicationInstance(
                        nameof(KernelFunction.head),
                        scrutinee);

                var tailExpr =
                    Expression.KernelApplicationInstance(
                        nameof(KernelFunction.skip),
                        Expression.ListInstance(
                            [
                                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                                scrutinee
                            ]));

                ExtractPatternBindingsRecursive(unConsPattern.Head.Value, headExpr, bindings);
                ExtractPatternBindingsRecursive(unConsPattern.Tail.Value, tailExpr, bindings);
                break;

            case SyntaxTypes.Pattern.ListPattern listPattern:
                // For list patterns like [a, b, c], bind each element
                for (var i = 0; i < listPattern.Elements.Count; i++)
                {
                    var elementPattern = listPattern.Elements[i].Value;
                    var elementExpr = GetListElementExpression(scrutinee, i);
                    ExtractPatternBindingsRecursive(elementPattern, elementExpr, bindings);
                }
                break;

            case SyntaxTypes.Pattern.AllPattern:
            case SyntaxTypes.Pattern.IntPattern:
                // These patterns don't introduce bindings
                break;

            default:
                // Other patterns not yet supported for binding extraction
                break;
        }
    }

    /// <summary>
    /// Get an expression that extracts the element at the given index from a list.
    /// Uses skip followed by head: head(skip([index, list]))
    /// </summary>
    private static Expression GetListElementExpression(Expression listExpr, int index)
    {
        if (index == 0)
        {
            return Expression.KernelApplicationInstance(
                nameof(KernelFunction.head),
                listExpr);
        }

        // For index > 0: head(skip([index, list]))
        var skipExpr =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.skip),
                Expression.ListInstance(
                    [
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(index)),
                        listExpr
                    ]));

        return Expression.KernelApplicationInstance(
            nameof(KernelFunction.head),
            skipExpr);
    }

    private static Expression? CompilePatternCondition(
        SyntaxTypes.Pattern pattern,
        Expression scrutinee)
    {
        // AllPattern (_) matches everything - no condition needed, always matches
        if (pattern is SyntaxTypes.Pattern.AllPattern)
        {
            return null;
        }

        // VarPattern matches everything and binds the value - no condition needed
        if (pattern is SyntaxTypes.Pattern.VarPattern)
        {
            return null;
        }

        // ParenthesizedPattern just wraps another pattern - unwrap and recurse
        if (pattern is SyntaxTypes.Pattern.ParenthesizedPattern parenthesizedPattern)
        {
            return CompilePatternCondition(parenthesizedPattern.Pattern.Value, scrutinee);
        }

        // IntPattern matches a specific integer value
        if (pattern is SyntaxTypes.Pattern.IntPattern intPattern)
        {
            var intLiteral =
                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(intPattern.Value));

            return
                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.equal),
                    Expression.ListInstance([scrutinee, intLiteral]));
        }

        // ListPattern matches a list with specific elements
        if (pattern is SyntaxTypes.Pattern.ListPattern listPattern)
        {
            // Empty list pattern []: use length check for efficiency.
            // This also correctly matches empty blobs, which type checking will eventually prevent.
            if (listPattern.Elements.Count == 0)
            {
                var lengthExpr =
                    Expression.KernelApplicationInstance(
                        nameof(KernelFunction.length),
                        scrutinee);

                var zeroLiteral =
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(0));

                return
                    Expression.KernelApplicationInstance(
                        nameof(KernelFunction.equal),
                        Expression.ListInstance([lengthExpr, zeroLiteral]));
            }

            // Optimization: if all elements are constants, use a single equality check
            if (IsConstantPattern(listPattern))
            {
                var patternValue = PatternToConstantValue(listPattern);

                return
                    Expression.KernelApplicationInstance(
                        nameof(KernelFunction.equal),
                        Expression.ListInstance([scrutinee, Expression.LiteralInstance(patternValue)]));
            }

            // For non-constant patterns, check length then each element
            var lengthCheckExpr =
                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.length),
                    scrutinee);

            var expectedLength =
                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(listPattern.Elements.Count));

            var lengthCondition =
                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.equal),
                    Expression.ListInstance([lengthCheckExpr, expectedLength]));

            Expression combinedCondition = lengthCondition;

            for (var i = 0; i < listPattern.Elements.Count; i++)
            {
                var elementPattern = listPattern.Elements[i].Value;
                var elementExpr = GetListElementExpression(scrutinee, i);

                var elementCondition = CompilePatternCondition(elementPattern, elementExpr);

                if (elementCondition is not null)
                {
                    // Chain conditions: if elementCondition then combinedCondition else false
                    combinedCondition =
                        Expression.ConditionalInstance(
                            condition: elementCondition,
                            trueBranch: combinedCondition,
                            falseBranch: Expression.LiteralInstance(KernelFunction.ValueFromBool(false)));
                }
            }

            return combinedCondition;
        }

        // UnConsPattern (head :: tail) matches non-empty list with head matching headPattern
        if (pattern is SyntaxTypes.Pattern.UnConsPattern unConsPattern)
        {
            // First check: length > 0 (list is not empty)
            var lengthExpr =
                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.length),
                    scrutinee);

            var zeroLiteral =
                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(0));

            // Check if list is not empty: (length == 0) == false
            var isEmptyCondition =
                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.equal),
                    Expression.ListInstance([lengthExpr, zeroLiteral]));

            var isNotEmptyCondition =
                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.equal),
                    Expression.ListInstance(
                        [
                            isEmptyCondition,
                            Expression.LiteralInstance(KernelFunction.ValueFromBool(false))
                        ]));

            // Get head and tail expressions
            var headExpr =
                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.head),
                    scrutinee);

            var tailExpr =
                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.skip),
                    Expression.ListInstance(
                        [
                            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                            scrutinee
                        ]));

            // Check head pattern condition
            var headCondition = CompilePatternCondition(unConsPattern.Head.Value, headExpr);

            // Check tail pattern condition
            var tailCondition = CompilePatternCondition(unConsPattern.Tail.Value, tailExpr);

            // Combine all conditions
            Expression combinedCondition = isNotEmptyCondition;

            if (headCondition is not null)
            {
                combinedCondition =
                    Expression.ConditionalInstance(
                        condition: combinedCondition,
                        trueBranch: headCondition,
                        falseBranch: Expression.LiteralInstance(KernelFunction.ValueFromBool(false)));
            }

            if (tailCondition is not null)
            {
                combinedCondition =
                    Expression.ConditionalInstance(
                        condition: combinedCondition,
                        trueBranch: tailCondition,
                        falseBranch: Expression.LiteralInstance(KernelFunction.ValueFromBool(false)));
            }

            return combinedCondition;
        }

        throw new NotImplementedException(
            $"Pattern type not yet supported: {pattern.GetType().Name}");
    }

    /// <summary>
    /// Check if a pattern contains only constant values (no variable bindings).
    /// Constant patterns can be optimized to a single equality comparison.
    /// Supported constant pattern types:
    /// - IntPattern: integer literals
    /// - ListPattern: when all elements are constant patterns
    /// - ParenthesizedPattern: when the inner pattern is constant
    /// </summary>
    private static bool IsConstantPattern(SyntaxTypes.Pattern pattern)
    {
        return pattern switch
        {
            SyntaxTypes.Pattern.IntPattern => true,
            SyntaxTypes.Pattern.ParenthesizedPattern p => IsConstantPattern(p.Pattern.Value),
            SyntaxTypes.Pattern.ListPattern listPattern =>
                listPattern.Elements.All(e => IsConstantPattern(e.Value)),
            // Variable patterns and other patterns are not constant
            _ => false
        };
    }

    /// <summary>
    /// Convert a constant pattern to its corresponding PineValue.
    /// Precondition: <see cref="IsConstantPattern"/> must return true for the pattern.
    /// </summary>
    /// <exception cref="InvalidOperationException">Thrown when the pattern is not a constant pattern.</exception>
    private static PineValue PatternToConstantValue(SyntaxTypes.Pattern pattern)
    {
        return pattern switch
        {
            SyntaxTypes.Pattern.IntPattern intPattern =>
                IntegerEncoding.EncodeSignedInteger(intPattern.Value),
            SyntaxTypes.Pattern.ParenthesizedPattern p =>
                PatternToConstantValue(p.Pattern.Value),
            SyntaxTypes.Pattern.ListPattern listPattern =>
                PineValue.List(
                    listPattern.Elements
                    .Select(e => PatternToConstantValue(e.Value))
                    .ToArray()),
            _ => throw new InvalidOperationException(
                $"Pattern type is not constant: {pattern.GetType().Name}")
        };
    }

    private static Expression BuildPathToParameter(int parameterIndex)
    {
        // Parameters are placed in the environment at path [1, parameterIndex]
        // [0] = functions list (empty in these scenarios)
        // [1] = parameters list

        return
            ExpressionBuilder.BuildExpressionForPathInExpression(
                [1, parameterIndex],
                Expression.EnvironmentInstance);
    }

    private static PineValue EmitPlainValueDeclaration(PineValue value)
    {
        return
            FunctionRecord.EncodeFunctionRecordInValueTagged(
                new FunctionRecord(
                    InnerFunction: Expression.LiteralInstance(value),
                    ParameterCount: 0,
                    EnvFunctions: ReadOnlyMemory<PineValue>.Empty,
                    ArgumentsAlreadyCollected: ReadOnlyMemory<PineValue>.Empty
                ));
    }

    private static PineValue EmitModuleValue(
        ElmModuleInCompilation compiledModule)
    {
        /*
        emitModuleValue : ElmModuleInCompilation -> Pine.Value
        emitModuleValue parsedModule =
            let
                typeDescriptions : List ( String, Pine.Value )
                typeDescriptions =
                    List.foldr
                        (\( typeName, typeDeclaration ) aggregate ->
                            ( typeName, emitTypeDeclarationValue typeDeclaration ) :: aggregate
                        )
                        []
                        parsedModule.typeDeclarations
            in
            Pine.ListValue
                (List.map Pine.valueFromContextExpansionWithName
                    (List.concat [ parsedModule.functionDeclarations, typeDescriptions ])
                )
         * */

        IReadOnlyList<(string declName, PineValue declValue)> typeDescriptions = [];

        PineValue[] entries =
            [.. compiledModule.FunctionDeclarations
            .Concat(typeDescriptions)
            .Select(tuple => ValueFromContextExpansionWithName(tuple.declName, tuple.Item2))
            ];

        return PineValue.List(entries);
    }

    private static PineValue EmitStringLiteral(string str)
    {
        /*
        valueFromString : String -> Pine.Value
        valueFromString string =
            Pine.ListValue
                [ elmStringTypeTagNameAsValue
                , Pine.ListValue [ Pine.computeValueFromString string ]
                ]
         * */

        return ElmValueEncoding.StringAsPineValue(str);
    }

    private static PineValue EmitIntegerLiteral(BigInteger value)
    {
        return IntegerEncoding.EncodeSignedInteger(value);
    }

    private static PineValue EmitCharLiteral(int value)
    {
        return ElmValueEncoding.ElmCharAsPineValue(value);
    }

    private static PineValue EmitBooleanLiteral(bool value)
    {
        return KernelFunction.ValueFromBool(value);
    }

    private static PineValue ValueFromContextExpansionWithName(
        string name,
        PineValue pineValue)
    {
        /*
        valueFromContextExpansionWithName : ( String, Value ) -> Value
        valueFromContextExpansionWithName ( declName, declValue ) =
            ListValue [ computeValueFromString declName, declValue ]
         * */

        return PineValue.List([StringEncoding.ValueFromString(name), pineValue]);
    }

    public static PineValue Compile(
        AppCompilationUnits compilationUnits)
    {
        throw new NotImplementedException();
    }
}
