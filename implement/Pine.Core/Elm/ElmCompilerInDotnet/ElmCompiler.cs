using Pine.Core.CodeAnalysis;
using Pine.Core.Files;
using Pine.Core.CommonEncodings;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Methods for compiling Elm source code and environments into Pine values, using the standard packaging for
/// Elm functions and modules as found at
/// <see href="https://github.com/pine-vm/pine/blob/391100e6734a50d2bede29ee49bca1afc8868fed/implement/pine/Elm/elm-compiler/src/FirCompiler.elm#L2179-L2242"></see>
/// and
/// <see href="https://github.com/pine-vm/pine/blob/391100e6734a50d2bede29ee49bca1afc8868fed/implement/pine/Elm/elm-compiler/src/ElmCompiler.elm"></see>
/// <para>
/// For an overview of the compiler implementation, see the file `elm-compiler-implementation-guide.md`
/// </para>
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
                ElmSyntax.Stil4mConcretized.Module.GetModuleName(parseModuleOk.ModuleDefinition.Value).Value;

            var moduleNameFlattened =
                string.Join(".", moduleName);

            var parseModuleAst =
                SyntaxTypes.FromStil4mConcretized.Convert(parseModuleOk);

            parsedModulesBeforeCanonicalize.Add(parseModuleAst);
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

        // Lambda lifting stage: Transform closures into top-level functions
        var lambdaLiftedModules = canonicalizedModules
            .Select(LambdaLifting.LiftLambdas)
            .ToList();

        var allFunctions =
            new Dictionary<string, (string moduleName, string functionName, SyntaxTypes.Declaration.FunctionDeclaration declaration)>();

        foreach (var elmModuleSyntax in lambdaLiftedModules)
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

        // Build function return types dictionary for type inference
        var functionReturnTypes = new Dictionary<string, TypeInference.InferredType>();
        foreach (var (qualifiedName, (_, _, declaration)) in allFunctions)
        {
            var returnType = TypeInference.GetFunctionReturnType(declaration);
            functionReturnTypes[qualifiedName] = returnType;
        }

        // Build function parameter types dictionary for type inference from function applications
        var functionParameterTypes = new Dictionary<string, IReadOnlyList<TypeInference.InferredType>>();
        foreach (var (qualifiedName, (_, _, declaration)) in allFunctions)
        {
            var paramTypes = TypeInference.GetFunctionParameterTypes(declaration);
            if (paramTypes.Count > 0)
            {
                functionParameterTypes[qualifiedName] = paramTypes;
            }
        }

        // Build constructor argument types dictionary for type inference from NamedPatterns
        var constructorArgumentTypes = new Dictionary<string, IReadOnlyList<TypeInference.InferredType>>();
        foreach (var elmModuleSyntax in lambdaLiftedModules)
        {
            var typeDeclarations =
                elmModuleSyntax.Declarations
                .Select(declNode => declNode.Value)
                .OfType<SyntaxTypes.Declaration.CustomTypeDeclaration>();

            foreach (var typeDecl in typeDeclarations)
            {
                foreach (var ctorNode in typeDecl.TypeDeclaration.Constructors)
                {
                    var ctor = ctorNode.Value;
                    var ctorName = ctor.Name.Value;

                    var argTypes = new List<TypeInference.InferredType>();
                    foreach (var argNode in ctor.Arguments)
                    {
                        var argType = TypeInference.TypeAnnotationToInferredType(argNode.Value);
                        argTypes.Add(argType);
                    }

                    constructorArgumentTypes[ctorName] = argTypes;
                }
            }
        }

        // Build record type alias constructors dictionary
        // A type alias for a record type creates an implicit constructor function
        // where argument order matches the field order in the type alias declaration
        var recordTypeAliasConstructors = new Dictionary<string, IReadOnlyList<string>>();
        foreach (var elmModuleSyntax in lambdaLiftedModules)
        {
            var moduleName =
                SyntaxTypes.Module.GetModuleName(elmModuleSyntax.ModuleDefinition.Value).Value;
            var moduleNameFlattened = string.Join(".", moduleName);

            var aliasDeclarations =
                elmModuleSyntax.Declarations
                .Select(declNode => declNode.Value)
                .OfType<SyntaxTypes.Declaration.AliasDeclaration>();

            foreach (var aliasDecl in aliasDeclarations)
            {
                var aliasName = aliasDecl.TypeAlias.Name.Value;
                var typeAnnotation = aliasDecl.TypeAlias.TypeAnnotation.Value;

                // Check if the type alias is for a record type
                if (typeAnnotation is SyntaxTypes.TypeAnnotation.Record recordType)
                {
                    // Extract field names in declaration order
                    var fieldNames = recordType.RecordDefinition.Fields
                        .Select(f => f.Value.FieldName.Value)
                        .ToList();

                    var qualifiedName = moduleNameFlattened + "." + aliasName;
                    recordTypeAliasConstructors[qualifiedName] = fieldNames;
                }
            }
        }

        // Create initial compilation context with all available functions
        var initialContext =
            new ModuleCompilationContext(
                allFunctions,
                CompiledFunctionsCache: [],
                PineKernelModuleNames: s_pineKernelModuleNamesDefault,
                FunctionReturnTypes: functionReturnTypes,
                ConstructorArgumentTypes: constructorArgumentTypes,
                FunctionParameterTypes: functionParameterTypes,
                RecordTypeAliasConstructors: recordTypeAliasConstructors);

        // Pre-compute dependency layouts for all functions BEFORE compilation
        var dependencyLayouts = ComputeDependencyLayouts(allFunctions, initialContext);

        // Create compilation context with pre-computed dependency layouts
        var compilationContext = initialContext.WithDependencyLayouts(dependencyLayouts);

        // Second pass: Compile each module with access to all functions and dependency layouts
        var compiledModuleEntries = new List<PineValue>();

        foreach (var parsedModule in lambdaLiftedModules)
        {
            var moduleNameFlattened =
                string.Join('.', SyntaxTypes.Module.GetModuleName(parsedModule.ModuleDefinition.Value).Value);

            var (moduleValue, updatedContext) =
                CompileModule(parsedModule, moduleNameFlattened, compilationContext);

            compilationContext = updatedContext;

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

    /// <summary>
    /// Computes dependency layouts for all functions in a module.
    /// This is the first pass of the two-pass compilation approach.
    /// Each function's layout contains: [self, sorted_dependencies...]
    /// Dependencies are sorted alphabetically for consistent ordering.
    /// </summary>
    /// <param name="allFunctions">Dictionary of all functions keyed by qualified name.</param>
    /// <param name="context">The module compilation context.</param>
    /// <returns>Dictionary mapping qualified function names to their dependency layouts.</returns>
    public static IReadOnlyDictionary<string, IReadOnlyList<string>> ComputeDependencyLayouts(
        IReadOnlyDictionary<string, (string moduleName, string functionName, SyntaxTypes.Declaration.FunctionDeclaration declaration)> allFunctions,
        ModuleCompilationContext context)
    {
        // First pass: compute direct dependencies for each function
        var directDependencies = new Dictionary<string, IReadOnlySet<string>>();

        foreach (var (qualifiedName, (moduleName, functionName, declaration)) in allFunctions)
        {
            var functionBody = declaration.Function.Declaration.Value.Expression.Value;
            var dependencies = AnalyzeFunctionDependencies(functionBody, moduleName, context);
            directDependencies[qualifiedName] = dependencies;
        }

        // Second pass: compute transitive closure of dependencies
        // A function needs all functions that any of its callees might need
        var transitiveDependencies = new Dictionary<string, HashSet<string>>();

        foreach (var qualifiedName in directDependencies.Keys)
        {
            transitiveDependencies[qualifiedName] = ComputeTransitiveDependencies(qualifiedName, directDependencies);
        }

        // Build layouts with transitive dependencies
        var dependencyLayouts = new Dictionary<string, IReadOnlyList<string>>();

        foreach (var (qualifiedName, deps) in transitiveDependencies)
        {
            // Sort dependencies alphabetically for consistent ordering
            var sortedDependencies = deps.OrderBy(d => d).ToList();

            // Layout is: [self, sorted_dependencies...]
            // Self is always first for snapshot test name rendering
            var layout = new List<string> { qualifiedName };
            layout.AddRange(sortedDependencies);
            dependencyLayouts[qualifiedName] = layout;
        }

        return dependencyLayouts;
    }

    /// <summary>
    /// Computes the transitive closure of dependencies for a function.
    /// This includes all functions that the function directly or indirectly calls,
    /// excluding the function itself (which is handled separately as index 0).
    /// </summary>
    private static HashSet<string> ComputeTransitiveDependencies(
        string functionName,
        IReadOnlyDictionary<string, IReadOnlySet<string>> directDependencies)
    {
        var visited = new HashSet<string>();
        var result = new HashSet<string>();
        var stack = new Stack<string>();

        // Mark self as visited to avoid including it in transitive dependencies
        // (self is always at index 0 in the layout)
        visited.Add(functionName);

        // Start with direct dependencies of the function
        if (directDependencies.TryGetValue(functionName, out var directDeps))
        {
            foreach (var dep in directDeps)
            {
                // Skip self-references
                if (dep != functionName)
                {
                    stack.Push(dep);
                }
            }
        }

        while (stack.Count > 0)
        {
            var current = stack.Pop();

            if (!visited.Add(current))
            {
                continue;
            }

            result.Add(current);

            // Add dependencies of this function
            if (directDependencies.TryGetValue(current, out var currentDeps))
            {
                foreach (var dep in currentDeps)
                {
                    if (!visited.Contains(dep))
                    {
                        stack.Push(dep);
                    }
                }
            }
        }

        return result;
    }

    private static (PineValue moduleValue, ModuleCompilationContext updatedContext) CompileModule(
        SyntaxTypes.File parsedModule,
        string currentModuleName,
        ModuleCompilationContext context)
    {
        var declarations =
            parsedModule.Declarations
            .Select(declNode => declNode.Value)
            .OfType<SyntaxTypes.Declaration.FunctionDeclaration>();

        var compiledFunctions = new List<(string, PineValue)>();

        foreach (var declaration in declarations)
        {
            var funcName = declaration.Function.Declaration.Value.Name.Value;
            var (compiledValue, updatedContext) = CompileFunctionDeclaration(declaration, currentModuleName, context);
            compiledFunctions.Add((funcName, compiledValue));
            context = updatedContext;
        }

        var compiledFunctionDeclaration =
            new ElmModuleInCompilation(
                FunctionDeclarations: compiledFunctions);

        var moduleValue =
            EmitModuleValue(compiledFunctionDeclaration);

        return (moduleValue, context);
    }

    /*
    type alias ElmModuleInCompilation =
        { functionDeclarations : List ( String, Pine.Value )
        , typeDeclarations : List ( String, ElmModuleTypeDeclaration )
        }
     * */

    private record ElmModuleInCompilation(
        IReadOnlyList<(string declName, PineValue)> FunctionDeclarations);

    private static (PineValue value, ModuleCompilationContext updatedContext) CompileFunctionDeclaration(
        SyntaxTypes.Declaration.FunctionDeclaration functionDeclaration,
        string currentModuleName,
        ModuleCompilationContext context)
    {
        var functionName =
            functionDeclaration.Function.Declaration.Value.Name.Value;

        var qualifiedFunctionName =
            currentModuleName + "." + functionName;

        // Check if we've already compiled this function (to avoid infinite recursion)
        if (context.TryGetCompiledFunctionValue(qualifiedFunctionName) is { } cachedValue)
        {
            return (cachedValue, context);
        }

        var arguments = functionDeclaration.Function.Declaration.Value.Arguments;

        var functionBody =
            functionDeclaration.Function.Declaration.Value.Expression.Value;

        // Build parameter name mapping: parameter names to their indices
        var parameterNames = new Dictionary<string, int>();

        // Build local bindings for pattern-matched parameters
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
                // For non-VarPattern arguments (including TuplePattern), extract bindings from the pattern
                // The scrutinee is the parameter at index i
                var paramExpr = BuiltinHelpers.BuildPathToParameter(i);
                // Analyze the pattern and merge bindings (ignoring the condition)
                var analysis = PatternCompiler.AnalyzePattern(argPattern, paramExpr);
                foreach (var kvp in analysis.Bindings)
                {
                    localBindings[kvp.Key] = kvp.Value;
                }
            }
        }

        var parameterCount = arguments.Count;

        // Extract parameter types from the function signature if available, and from NamedPatterns using constructor types
        var parameterTypes = ExtractParameterTypes(
            functionDeclaration.Function,
            context.ConstructorArgumentTypes,
            context.FunctionParameterTypes,
            currentModuleName);

        // Use the pre-computed dependency layout which includes transitive dependencies
        // This is needed so that when calling another function, all of its dependencies
        // are available in our environment
        IReadOnlyList<string> dependencyLayout;

        if (context.TryGetDependencyLayout(qualifiedFunctionName) is { } precomputedLayout)
        {
            dependencyLayout = precomputedLayout;
        }
        else
        {
            // Fallback: compute direct dependencies (shouldn't happen if layouts are pre-computed)
            var dependencies =
                AnalyzeFunctionDependencies(
                    functionBody,
                    currentModuleName,
                    context);

            // Create the function dependency layout: [self, dependencies...]
            // The current function is always at index 0
            var layout = new List<string> { qualifiedFunctionName };
            layout.AddRange(dependencies);
            dependencyLayout = layout;
        }

        // Create expression compilation context using the immutable context types
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

        // Compile the function body with knowledge of the dependency layout
        var compiledBodyExpression =
            CompileExpression(
                functionBody,
                expressionContext);

        // Create a parse cache to be reused for reduction and dependency parsing
        var parseCache = new PineVMParseCache();

        // Apply reduction to simplify expressions like head([a, b]) → a
        compiledBodyExpression =
            ReducePineExpression.ReduceExpressionBottomUp(
                compiledBodyExpression,
                parseCache);

        // For zero-parameter functions, check if it's a simple literal value
        if (compiledBodyExpression is Expression.Literal literalExpr)
        {
            var result = EmitPlainValueDeclaration(literalExpr.Value);
            return (result, context.WithCompiledFunction(qualifiedFunctionName, result, dependencyLayout));
        }

        // For functions with parameters, build env functions list
        var encodedFunction =
            ExpressionEncoding.EncodeExpressionAsValue(compiledBodyExpression);

        // Build the EnvFunctions list with encoded dependencies
        var envFunctionsList = new List<PineValue> { encodedFunction };

        // Create a placeholder result using FunctionValueBuilder for self-reference during recursive compilation.
        // This placeholder is necessary because:
        // 1. Recursive functions may reference themselves through the context cache
        // 2. Mutually recursive functions need a placeholder to break the circular dependency
        // The placeholder allows dependencies to be compiled and can reference this function if needed.
        var placeholderResult =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                compiledBodyExpression,
                parameterCount,
                envFunctionsList);

        context = context.WithCompiledFunction(qualifiedFunctionName, placeholderResult, dependencyLayout);

        // Now compile dependencies (they can reference this function via the cache)
        // Skip the first element (self) in the dependency layout
        foreach (var depQualifiedName in dependencyLayout.Skip(1))
        {
            if (context.TryGetFunctionInfo(depQualifiedName, out var depInfo))
            {
                // Compile the dependency function
                var (depCompiled, updatedContext) = CompileFunctionDeclaration(depInfo.declaration, depInfo.moduleName, context);
                context = updatedContext;

                // Parse the FunctionRecord to extract the InnerFunction expression
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

        // Create the final result using FunctionValueBuilder for incremental argument application
        var finalResult =
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                compiledBodyExpression,
                parameterCount,
                envFunctionsList);

        return (finalResult, context.WithCompiledFunction(qualifiedFunctionName, finalResult, dependencyLayout));
    }

    private static IReadOnlySet<string> AnalyzeFunctionDependencies(
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
                        // Skip Pine_kernel functions
                        if (funcRef.ModuleName.Count is 1 && context.IsPineKernelModule(funcRef.ModuleName[0]))
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

                case SyntaxTypes.Expression.LetExpression letExpr:
                    foreach (var decl in letExpr.Value.Declarations)
                    {
                        switch (decl.Value)
                        {
                            case SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc:
                                AnalyzeExpression(letFunc.Function.Declaration.Value.Expression.Value);
                                break;

                            case SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr:
                                AnalyzeExpression(letDestr.Expression.Value);
                                break;
                        }
                    }
                    AnalyzeExpression(letExpr.Value.Expression.Value);
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

        return dependencies;
    }

    private static ImmutableDictionary<string, TypeInference.InferredType> ExtractParameterTypes(
        SyntaxTypes.FunctionStruct function,
        IReadOnlyDictionary<string, IReadOnlyList<TypeInference.InferredType>>? constructorArgumentTypes,
        IReadOnlyDictionary<string, IReadOnlyList<TypeInference.InferredType>>? functionParameterTypes,
        string currentModuleName)
    {
        var parameterTypes = ImmutableDictionary<string, TypeInference.InferredType>.Empty;

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
                var paramPattern = parameters[paramIndex].Value;
                var paramTypeAnnotation = funcType.ArgumentType.Value;

                // Extract binding types from the pattern
                // This handles both simple VarPattern and complex patterns like TuplePattern
                parameterTypes = TypeInference.ExtractPatternBindingTypes(paramPattern, paramTypeAnnotation, parameterTypes);

                currentType = funcType.ReturnType.Value;
                paramIndex++;
            }
        }

        // Also extract types from NamedPattern parameters using constructor argument types
        // This handles cases where the function has no type signature but uses choice type patterns
        if (constructorArgumentTypes is not null)
        {
            var parameters = function.Declaration.Value.Arguments;
            foreach (var paramNode in parameters)
            {
                parameterTypes = TypeInference.ExtractPatternBindingTypesWithConstructors(
                    paramNode.Value,
                    constructorArgumentTypes,
                    parameterTypes);
            }

            // Also analyze the function body for tag applications that constrain parameter types
            // For example, in `alfa a b = let c = TagAlfa a in ...`, we infer that `a` is Int
            var functionBody = function.Declaration.Value.Expression.Value;
            parameterTypes = TypeInference.ExtractTypeConstraintsFromTagApplications(
                functionBody,
                constructorArgumentTypes,
                parameterTypes);
        }

        // Also analyze the function body for function applications that constrain parameter types
        // For example, in `beta a b = let c = alfa a in ...` where `alfa : Int -> String`, we infer that `a` is Int
        if (functionParameterTypes is not null)
        {
            var functionBody = function.Declaration.Value.Expression.Value;
            parameterTypes = TypeInference.ExtractTypeConstraintsFromFunctionApplications(
                functionBody,
                functionParameterTypes,
                currentModuleName,
                parameterTypes);
        }

        return parameterTypes;
    }

    private static Expression CompileExpression(
        SyntaxTypes.Expression expression,
        ExpressionCompilationContext context)
    {
        var result = ExpressionCompiler.Compile(expression, context);

        if (result.IsErrOrNull() is { } error)
        {
            throw new NotImplementedException(error.ToString());
        }

        return result.IsOkOrNull()!;
    }

    private static PineValue EmitPlainValueDeclaration(PineValue value)
    {
        return
            FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                Expression.LiteralInstance(value),
                parameterCount: 0,
                envFunctions: []);
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
