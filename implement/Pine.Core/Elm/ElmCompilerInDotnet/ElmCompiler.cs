using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Files;
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

    /// <summary>
    /// Names of Elm modules that are implemented natively in the .NET assembly and must not be
    /// compiled from Elm source files (e.g. bundled elm-kernel-modules).
    /// The .NET assembly ships its own implementation of these modules via classes such as
    /// <see cref="CoreLibraryModule.CoreBasics"/>, superseding the corresponding .elm source.
    /// </summary>
    private static readonly FrozenSet<string> s_nativelyImplementedModuleNames =
        FrozenSet.Create(
            [
            "Basics",
            ]);

    public static Result<string, PineValue> CompileInteractiveEnvironment(
        FileTree appCodeTree,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths,
        bool disableInlining)
    {
        var elmModuleFiles =
            appCodeTree.EnumerateFilesTransitive()
            .Where(file => file.path.Last().EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .ToImmutableArray();

        // Step 1: Parse all modules, building a map from file path to module name.
        var successfullyParsedModules = new Dictionary<string, SyntaxTypes.File>();
        var parseFailures = new HashSet<string>();

        var filePathToModuleName =
            new Dictionary<IReadOnlyList<string>, string>(
                EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>());

        foreach (var moduleFile in elmModuleFiles)
        {
            var moduleText =
                Encoding.UTF8.GetString(moduleFile.fileContent.Span);

            // First try to get the module name from the header
            var headerResult =
                ElmSyntax.ElmSyntaxParser.ParseModuleHeader(moduleText);

            if (headerResult.IsErrOrNull() is not null)
            {
                // Can't even parse the header - skip this file entirely
                continue;
            }

            if (headerResult.IsOkOrNull() is not { } header)
            {
                throw new NotImplementedException(
                    "Unexpected header result type: " + headerResult.GetType().Name);
            }

            var moduleNameFlattened = string.Join(".", header.ModuleName);

            // Record the path→name mapping before any module-level filtering.
            filePathToModuleName[moduleFile.path] = moduleNameFlattened;

            // Natively-implemented modules (e.g. "Basics") are superseded by C# code.
            // Do not parse or compile them from Elm source.
            if (s_nativelyImplementedModuleNames.Contains(moduleNameFlattened))
                continue;

            // Now try full parsing
            var parseResult =
                ElmSyntax.ElmSyntaxParser.ParseModuleText(moduleText);

            if (parseResult.IsErrOrNull() is not null)
            {
                parseFailures.Add(moduleNameFlattened);
                continue;
            }

            if (parseResult.IsOkOrNull() is not { } parseModuleOk)
            {
                throw new NotImplementedException(
                    "Unexpected parse result type: " + parseResult.GetType().Name);
            }

            var parseModuleAst =
                SyntaxTypes.FromFullSyntaxModel.Convert(parseModuleOk);

            successfullyParsedModules[moduleNameFlattened] = parseModuleAst;
        }

        // Step 2: Compute the transitive dependency closure of the root modules.
        // Only modules reachable from roots that parsed successfully are compiled.
        // Modules outside the dependency graph (unreachable from the selected roots)
        // are completely ignored — their parse failures or other issues are irrelevant.
        // If a reachable module has an import that failed to parse, that is an error:
        // a required dependency is broken.
        var rootModuleNames =
            rootFilePaths
            .Select(path =>
                filePathToModuleName.TryGetValue(path, out var name) ? name : null)
            .Where(name => name is not null)
            .ToHashSet()!;

        // Start the closure with roots that parsed successfully.
        // Roots that failed to parse are silently skipped (they were never needed).
        var modulesToCompile =
            new HashSet<string>(rootModuleNames.Where(successfullyParsedModules.ContainsKey));

        {
            bool changed;

            do
            {
                changed = false;

                foreach (var moduleName in modulesToCompile.ToList())
                {
                    if (!successfullyParsedModules.TryGetValue(moduleName, out var parsedModule))
                        continue;

                    foreach (var import in parsedModule.Imports)
                    {
                        var importedName = string.Join(".", import.Value.ModuleName.Value);

                        if (s_nativelyImplementedModuleNames.Contains(importedName))
                            continue; // Natively implemented in .NET; not a compilation dep.

                        if (parseFailures.Contains(importedName))
                        {
                            // A module in the dep graph depends on a module that failed to parse.
                            return
                                "Module '" + importedName +
                                "' is required by '" + moduleName +
                                "' but failed to parse.";
                        }

                        // Unknown modules (not in our file tree) are external packages; skip.
                        if (successfullyParsedModules.ContainsKey(importedName) &&
                            !modulesToCompile.Contains(importedName))
                        {
                            modulesToCompile.Add(importedName);
                            changed = true;
                        }
                    }
                }
            } while (changed);
        }

        // Step 3: Canonicalize the dependency modules and surface any errors.
        var modulesForCanonicalization =
            modulesToCompile
            .Where(successfullyParsedModules.ContainsKey)
            .Select(name => successfullyParsedModules[name])
            .ToList();

        var canonicalizationResult =
            Canonicalization.CanonicalizeAllowingErrors(modulesForCanonicalization);

        if (canonicalizationResult.IsErrOrNull() is { } canonErr)
        {
            return canonErr;
        }

        if (canonicalizationResult.IsOkOrNull() is not { } canonicalizedModulesDict)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + canonicalizationResult.GetType().Name);
        }

        // Surface canonicalization errors for any module in the dependency set.
        var moduleErrors = new List<string>();

        foreach (var (moduleName, (_, errors)) in canonicalizedModulesDict)
        {
            if (errors.Count > 0)
            {
                var moduleNameStr = string.Join(".", moduleName);
                var errMessages = string.Join("\n", errors.Select(e => e.ReferencedName));
                moduleErrors.Add("In module " + moduleNameStr + ":\n" + errMessages);
            }
        }

        if (moduleErrors.Count > 0)
        {
            return string.Join("\n\n", moduleErrors);
        }

        // All dependency modules canonicalized successfully.
        var canonicalizedModules =
            canonicalizedModulesDict.Values.Select(v => v.File).ToList();

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

        // Build choice type tag argument types dictionary for type inference from NamedPatterns
        var choiceTagArgumentTypes = new Dictionary<string, IReadOnlyList<TypeInference.InferredType>>();
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

                    choiceTagArgumentTypes[ctorName] = argTypes;
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
                ChoiceTagArgumentTypes: choiceTagArgumentTypes,
                FunctionParameterTypes: functionParameterTypes,
                RecordTypeAliasConstructors: recordTypeAliasConstructors);

        // Pre-compute dependency layouts and SCCs for all functions BEFORE compilation
        var (dependencyLayouts, functionToScc, sccsInDependencyOrder) =
            ComputeDependencyLayoutsAndSccs(allFunctions, initialContext);

        // Create compilation context with pre-computed layouts and SCCs
        var compilationContext =
            initialContext
            .WithDependencyLayouts(dependencyLayouts);

        // Second pass: Compile all SCCs in dependency order
        // This ensures all dependencies are compiled before they are needed
        foreach (var scc in sccsInDependencyOrder)
        {
            // Skip if all members are already compiled
            if (scc.Members.All(m => compilationContext.TryGetCompiledFunctionValue(m) is not null))
                continue;

            var sharedLayout = scc.GetLayout();
            var sccMembersSet = new HashSet<string>(scc.Members);

            // Skip explicitly if any layout dependency outside this SCC was not compiled.
            // This happens when a transitive dependency was excluded from the compilation roots.
            var allLayoutDepsCompiled =
                sharedLayout
                .Where(dep => !sccMembersSet.Contains(dep))
                .All(dep => compilationContext.TryGetCompiledFunctionValue(dep) is not null);

            if (!allLayoutDepsCompiled)
                continue;

            compilationContext = CompileSCC(scc, compilationContext);
        }

        // Third pass: Build module values from compiled functions
        var compiledModuleEntries = new List<PineValue>();

        foreach (var parsedModule in lambdaLiftedModules)
        {
            var moduleNameFlattened =
                string.Join('.', SyntaxTypes.Module.GetModuleName(parsedModule.ModuleDefinition.Value).Value);

            var moduleValue = BuildModuleValue(parsedModule, moduleNameFlattened, compilationContext);

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
    /// Pre-computes the dependency layouts and SCC mappings for all functions.
    /// For mutually recursive functions (strongly connected components), all functions
    /// share the same layout ordering as per the implementation guide.
    /// </summary>
    /// <param name="allFunctions">Dictionary of all functions keyed by qualified name.</param>
    /// <param name="context">The module compilation context.</param>
    /// <returns>
    /// A tuple containing:
    /// - Dictionary mapping qualified function names to their dependency layouts
    /// - Dictionary mapping qualified function names to their SCC
    /// - List of SCCs in dependency order (dependencies first)
    /// </returns>
    public static (IReadOnlyDictionary<string, IReadOnlyList<string>> layouts, IReadOnlyDictionary<string, FunctionScc> functionToScc, IReadOnlyList<FunctionScc> sccsInDependencyOrder)
        ComputeDependencyLayoutsAndSccs(
            IReadOnlyDictionary<string, (string moduleName, string functionName, SyntaxTypes.Declaration.FunctionDeclaration declaration)> allFunctions,
            ModuleCompilationContext context)
    {
        // First pass: compute direct dependencies for each function
        // Only include dependencies that are in allFunctions (we can't compile external functions)
        var directDependencies =
            allFunctions
            .ToImmutableDictionary(
                kvp => kvp.Key,
                kvp =>
                {
                    var (moduleName, functionName, declaration) = kvp.Value;
                    var functionBody = declaration.Function.Declaration.Value.Expression.Value;
                    var dependencies = AnalyzeFunctionDependencies(functionBody, moduleName, context);
                    // Filter to only include functions that are in allFunctions
                    IReadOnlySet<string> filtered = dependencies
                        .Where(d => allFunctions.ContainsKey(d))
                        .ToHashSet();
                    return filtered;
                });

        // Detect strongly connected components (SCCs) - groups of mutually recursive functions
        // Tarjan's algorithm returns SCCs in topological order (dependencies first)
        var sccsFromTarjan = FindStronglyConnectedComponents([.. allFunctions.Keys], directDependencies);

        // Build FunctionScc records - SCCs are already in dependency order from Tarjan
        var sccsInDependencyOrder = new List<FunctionScc>();

        // Compute transitive dependencies for each function
        var transitiveDependencies =
            directDependencies
            .ToDictionary(
                kvp => kvp.Key,
                kvp => ComputeTransitiveDependencies(kvp.Key, directDependencies));

        // Build SCCs and layouts
        var dependencyLayouts = new Dictionary<string, IReadOnlyList<string>>();
        var functionToScc = new Dictionary<string, FunctionScc>();

        foreach (var scc in sccsFromTarjan)
        {
            // Sort SCC members alphabetically for consistent ordering
            var sortedSccMembers =
                scc
                .Order(StringComparer.Ordinal)
                .ToImmutableList();

            // Compute the additional dependencies for this SCC
            // These are transitive dependencies that are not SCC members
            var allOtherDeps = new HashSet<string>();
            foreach (var member in scc)
            {
                if (transitiveDependencies.TryGetValue(member, out var deps))
                {
                    foreach (var d in deps)
                    {
                        if (!scc.Contains(d))
                        {
                            allOtherDeps.Add(d);
                        }
                    }
                }
            }

            var sortedOtherDeps =
                allOtherDeps
                .Order(StringComparer.Ordinal)
                .ToImmutableList();

            // Create SCC with members and additional dependencies
            // The complete layout is: Members ++ AdditionalDependencies
            var sccRecord = new FunctionScc(sortedSccMembers, sortedOtherDeps);
            var sharedLayout = sccRecord.GetLayout();

            foreach (var member in scc)
            {
                functionToScc[member] = sccRecord;
                dependencyLayouts[member] = sharedLayout;
            }

            sccsInDependencyOrder.Add(sccRecord);
        }

        // No need to reverse - Tarjan already returns SCCs in dependency order

        return (dependencyLayouts, functionToScc, sccsInDependencyOrder);
    }

    /// <summary>
    /// Finds strongly connected components (SCCs) in the function call graph.
    /// Uses Tarjan's algorithm.
    /// </summary>
    private static List<HashSet<string>> FindStronglyConnectedComponents(
        IReadOnlyList<string> functions,
        ImmutableDictionary<string, IReadOnlySet<string>> dependencies)
    {
        var index = 0;
        var stack = new Stack<string>();
        var indices = new Dictionary<string, int>();
        var lowLinks = new Dictionary<string, int>();
        var onStack = new HashSet<string>();
        var sccs = new List<HashSet<string>>();

        void StrongConnect(string v)
        {
            indices[v] = index;
            lowLinks[v] = index;
            index++;
            stack.Push(v);
            onStack.Add(v);

            // Consider successors
            if (dependencies.TryGetValue(v, out var deps))
            {
                foreach (var w in deps)
                {
                    // Only consider functions we know about
                    if (!functions.Contains(w))
                        continue;

                    if (!indices.TryGetValue(w, out var value))
                    {
                        // Successor w has not yet been visited; recurse on it
                        StrongConnect(w);
                        lowLinks[v] = Math.Min(lowLinks[v], lowLinks[w]);
                    }
                    else if (onStack.Contains(w))
                    {
                        // Successor w is in stack S and hence in the current SCC
                        lowLinks[v] = Math.Min(lowLinks[v], value);
                    }
                }
            }

            // If v is a root node, pop the stack and generate an SCC
            if (lowLinks[v] == indices[v])
            {
                var scc = new HashSet<string>();
                string w;
                do
                {
                    w = stack.Pop();
                    onStack.Remove(w);
                    scc.Add(w);
                } while (w != v);
                sccs.Add(scc);
            }
        }

        foreach (var v in functions)
        {
            if (!indices.ContainsKey(v))
            {
                StrongConnect(v);
            }
        }

        return sccs;
    }

    /// <summary>
    /// Computes the transitive closure of dependencies for a function.
    /// This includes all functions that the function directly or indirectly calls,
    /// excluding the function itself (which is handled separately as index 0).
    /// </summary>
    private static HashSet<string> ComputeTransitiveDependencies(
        string functionName,
        ImmutableDictionary<string, IReadOnlySet<string>> directDependencies)
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

    /// <summary>
    /// Builds the module value from already-compiled functions.
    /// All functions should have been compiled in the SCC compilation pass.
    /// </summary>
    private static PineValue BuildModuleValue(
        SyntaxTypes.File parsedModule,
        string currentModuleName,
        ModuleCompilationContext context)
    {
        // Get declarations in this module (for building the final module value)
        var declarations =
            parsedModule.Declarations
            .Select(declNode => declNode.Value)
            .OfType<SyntaxTypes.Declaration.FunctionDeclaration>()
            .ToList();

        // Collect compiled functions for this module (in declaration order)
        var compiledFunctions = new List<(string, PineValue)>();

        foreach (var declaration in declarations)
        {
            var funcName = declaration.Function.Declaration.Value.Name.Value;

            var qualifiedName = currentModuleName + "." + funcName;

            var compiledValue =
                context.TryGetCompiledFunctionValue(qualifiedName)
                ?? throw new InvalidOperationException($"Function {qualifiedName} was not compiled.");

            compiledFunctions.Add((funcName, compiledValue));
        }

        var compiledFunctionDeclaration =
            new ElmModuleInCompilation(
                FunctionDeclarations: compiledFunctions);

        return EmitModuleValue(compiledFunctionDeclaration);
    }

    /*
    type alias ElmModuleInCompilation =
        { functionDeclarations : List ( String, Pine.Value )
        , typeDeclarations : List ( String, ElmModuleTypeDeclaration )
        }
     * */

    private record ElmModuleInCompilation(
        IReadOnlyList<(string declName, PineValue)> FunctionDeclarations);

    /// <summary>
    /// Compiles all functions in a strongly connected component (SCC) together.
    /// This ensures that all mutually recursive functions share the same envFunctionsList
    /// with correct references to each other.
    /// 
    /// Since SCCs are compiled in dependency order, all dependencies of the current SCC
    /// are already compiled and can be retrieved from the context.
    /// </summary>
    private static ModuleCompilationContext CompileSCC(
        FunctionScc scc,
        ModuleCompilationContext context)
    {
        var sharedLayout = scc.GetLayout();
        var sccMembers = scc.Members;

        // Pre-compute index lookup for efficient access
        var layoutIndexMap = new Dictionary<string, int>();

        for (var i = 0; i < sharedLayout.Count; i++)
        {
            layoutIndexMap[sharedLayout[i]] = i;
        }

        var parseCache = new PineVMParseCache();

        // Phase 1: Compile all function bodies in the SCC (without building wrappers yet)
        var compiledBodies = new Dictionary<string, (Expression body, int paramCount)>();

        foreach (var memberName in sccMembers)
        {
            if (!context.TryGetFunctionInfo(memberName, out var funcInfo))
                continue;

            var declaration = funcInfo.declaration;
            var moduleName = funcInfo.moduleName;
            var functionBody = declaration.Function.Declaration.Value.Expression.Value;
            var arguments = declaration.Function.Declaration.Value.Arguments;
            var paramCount = arguments.Count;

            // Build parameter mappings
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
            var parameterTypes = ExtractParameterTypes(
                declaration.Function,
                context.ChoiceTagArgumentTypes,
                context.FunctionParameterTypes,
                moduleName);

            // Create expression context
            var expressionContext = new ExpressionCompilationContext(
                ParameterNames: parameterNames,
                ParameterTypes: parameterTypes,
                CurrentModuleName: moduleName,
                CurrentFunctionName: declaration.Function.Declaration.Value.Name.Value,
                LocalBindings: localBindings.Count > 0 ? localBindings : null,
                LocalBindingTypes: null,
                DependencyLayout: sharedLayout,
                ModuleCompilationContext: context,
                FunctionReturnTypes: context.FunctionReturnTypes);

            // Compile the body
            var compiledBody = CompileExpression(functionBody, expressionContext);
            compiledBody = ReducePineExpression.ReduceExpressionBottomUp(compiledBody, parseCache);

            compiledBodies[memberName] = (compiledBody, paramCount);
        }

        var encodedBodies =
            compiledBodies
            .ToDictionary(
                kvp => kvp.Key,
                kvp => ExpressionEncoding.EncodeExpressionAsValue(kvp.Value.body));

        // Phase 2: Build the envFunctionsList
        // Since SCCs are compiled in dependency order, all dependencies are already compiled.
        var envFunctionsList =
            sharedLayout
            .Select((declName, depIndex) =>
            {
                if (encodedBodies.TryGetValue(declName, out var encodedBodyDep))
                {
                    return encodedBodyDep;
                }

                // Dependencies are already compiled - retrieve encoded body from cache
                if (context.TryGetCompiledFunctionInfo(declName, out var depInfo) && depInfo is not null)
                {
                    return depInfo.EncodedBody;
                }

                throw new InvalidOperationException(
                    $"Dependency {declName} not found in cache when compiling SCC {string.Join(", ", scc.Members)}. " +
                    "SCCs should be compiled in dependency order.");
            })
            .ToList();

        // Phase 3: Build final wrappers and cache all compiled functions
        foreach (var memberName in sccMembers)
        {
            if (!compiledBodies.TryGetValue(memberName, out var bodyInfo))
                continue;

            var wrapper =
                FunctionValueBuilder.EmitFunctionValueWithEnvFunctions(
                    bodyInfo.body,
                    bodyInfo.paramCount,
                    envFunctionsList);

            var encodedBody = encodedBodies[memberName];
            context = context.WithCompiledFunction(memberName, wrapper, encodedBody, sharedLayout);
        }

        return context;
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

                case SyntaxTypes.Expression.CaseExpression caseExpr:
                    AnalyzeExpression(caseExpr.CaseBlock.Expression.Value);
                    foreach (var caseItem in caseExpr.CaseBlock.Cases)
                    {
                        AnalyzeExpression(caseItem.Expression.Value);
                    }
                    break;

                case SyntaxTypes.Expression.TupledExpression tupledExpr:
                    foreach (var elem in tupledExpr.Elements)
                    {
                        AnalyzeExpression(elem.Value);
                    }
                    break;

                case SyntaxTypes.Expression.RecordExpr recordExpr:
                    foreach (var field in recordExpr.Fields)
                    {
                        AnalyzeExpression(field.Value.valueExpr.Value);
                    }
                    break;

                case SyntaxTypes.Expression.RecordUpdateExpression recordUpdateExpr:
                    foreach (var setter in recordUpdateExpr.Fields)
                    {
                        AnalyzeExpression(setter.Value.valueExpr.Value);
                    }
                    break;

                case SyntaxTypes.Expression.FunctionOrValue funcOrValue:

                    if (funcOrValue.Name.Length < 0)
                    {
                        break;
                    }

                    if (char.IsUpper(funcOrValue.Name[0]))
                    {
                        // This must be a choice type tag constructor or record constructor - cannot contribute to dependencies
                        break;
                    }

                    {
                        // Handle standalone function references (escaping functions)
                        // This is when a function is used as a value, not applied

                        string qualifiedName;

                        if (funcOrValue.ModuleName.Count > 0)
                        {
                            // Skip Pine_kernel module functions
                            if (funcOrValue.ModuleName.Count is 1 &&
                                context.IsPineKernelModule(funcOrValue.ModuleName[0]))
                            {
                                break;
                            }

                            qualifiedName = string.Join(".", funcOrValue.ModuleName) + "." + funcOrValue.Name;
                        }
                        else
                        {
                            qualifiedName = currentModuleName + "." + funcOrValue.Name;
                        }

                        // Only add if it's a known function (not a local variable)
                        if (context.TryGetFunctionInfo(qualifiedName, out _))
                        {
                            dependencies.Add(qualifiedName);
                        }
                    }
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
