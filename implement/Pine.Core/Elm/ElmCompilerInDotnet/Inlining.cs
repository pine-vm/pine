using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// See the file 'explore-early-instantiation-stage.md' for design notes.
/// </summary>
public class Inlining
{
    /// <summary>
    /// Zero-based location used for generated syntax nodes.
    /// Per design notes: "we use the value 0 for all locations (row, column) and ranges for newly created syntax nodes."
    /// These will be used in future cross-module inlining when creating new syntax nodes.
    /// </summary>
    private static readonly SyntaxTypes.Location s_zeroLocation = new(Row: 0, Column: 0);

    /// <summary>
    /// Zero range used for generated syntax nodes. See <see cref="s_zeroLocation"/> for details.
    /// </summary>
    private static readonly SyntaxTypes.Range s_zeroRange = new(Start: s_zeroLocation, End: s_zeroLocation);

    /// <summary>
    /// Singleton comparer for module name tuples to avoid repeated allocations.
    /// </summary>
    private static readonly ModuleNameTupleComparer s_moduleNameTupleComparer = new();

    public abstract record Config
    {
        /// <summary>
        /// Attempt inlining only for function applications which supply functions as arguments.
        /// </summary>
        public static readonly Config OnlyFunctions = new InlineOnlyFunctions();

        /// <summary>
        /// Attempt inlining only for function applications which supply functions as arguments.
        /// </summary>
        internal sealed record InlineOnlyFunctions
            : Config;
    }

    /// <summary>
    /// Represents a function declaration with its containing module for inlining purposes.
    /// </summary>
    private record FunctionInfo(
        ModuleName ModuleName,
        SyntaxTypes.FunctionStruct Function,
        bool IsRecursive);

    /// <summary>
    /// Context for inlining operations, including all function definitions and configuration.
    /// </summary>
    private record InliningContext(
        ImmutableDictionary<(ModuleName ModuleName, string FunctionName), FunctionInfo> FunctionsByQualifiedName,
        Config Config,
        ImmutableHashSet<(ModuleName ModuleName, string FunctionName)> InliningStack);

    public static Result<string, IReadOnlyDictionary<ModuleName, SyntaxTypes.File>> Inline(
        IReadOnlyList<SyntaxTypes.File> modules,
        Config config)
    {
        // Build a dictionary of all function declarations across all modules
        var functionsByQualifiedName = BuildFunctionDictionary(modules);

        // Mark recursive functions
        var functionsWithRecursionInfo = MarkRecursiveFunctions(functionsByQualifiedName);

        var context = new InliningContext(
            functionsWithRecursionInfo,
            config,
            ImmutableHashSet<(ModuleName ModuleName, string FunctionName)>.Empty.WithComparer(s_moduleNameTupleComparer));

        var result =
            new Dictionary<ModuleName, SyntaxTypes.File>(
                EnumerableExtensions.EqualityComparer<ModuleName>());

        foreach (var module in modules)
        {
            var moduleName = SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value;
            var inlinedModule = InlineModule(module, context);
            result[moduleName] = inlinedModule;
        }

        return result;
    }

    private static ImmutableDictionary<(ModuleName ModuleName, string FunctionName), FunctionInfo> MarkRecursiveFunctions(
        ImmutableDictionary<(ModuleName ModuleName, string FunctionName), FunctionInfo> functions)
    {
        var builder = functions.ToBuilder();

        foreach (var kvp in functions)
        {
            var funcKey = kvp.Key;
            var funcInfo = kvp.Value;

            // Check if this function is recursive (directly or indirectly references itself)
            var isRecursive = IsRecursiveFunction(funcKey, funcInfo.Function, functions, ImmutableHashSet<(ModuleName, string)>.Empty.WithComparer(s_moduleNameTupleComparer));

            builder[funcKey] = funcInfo with { IsRecursive = isRecursive };
        }

        return builder.ToImmutable();
    }

    private static bool IsRecursiveFunction(
        (ModuleName ModuleName, string FunctionName) funcKey,
        SyntaxTypes.FunctionStruct func,
        ImmutableDictionary<(ModuleName ModuleName, string FunctionName), FunctionInfo> allFunctions,
        ImmutableHashSet<(ModuleName, string)> visited)
    {
        // Collect all function references in the function body
        var references = CollectFunctionReferences(func.Declaration.Value.Expression.Value);

        foreach (var refKey in references)
        {
            // Check if this reference is the function itself (direct recursion)
            if (s_moduleNameTupleComparer.Equals(refKey, funcKey))
            {
                return true;
            }

            // Check if we've already visited this function in the current path (indirect recursion)
            if (visited.Contains(refKey))
            {
                continue; // Skip to avoid infinite loop in analysis, but this doesn't indicate funcKey is recursive
            }

            // Check if referenced function eventually calls back to this function (indirect recursion)
            if (allFunctions.TryGetValue(refKey, out var refFuncInfo))
            {
                var newVisited = visited.Add(refKey);
                if (IsRecursiveFunction(funcKey, refFuncInfo.Function, allFunctions, newVisited))
                {
                    return true;
                }
            }
        }

        return false;
    }

    private static IEnumerable<(ModuleName ModuleName, string FunctionName)> CollectFunctionReferences(
        SyntaxTypes.Expression expr)
    {
        return expr switch
        {
            SyntaxTypes.Expression.FunctionOrValue funcOrValue =>
                [(funcOrValue.ModuleName, funcOrValue.Name)],

            SyntaxTypes.Expression.Application app =>
                app.Arguments.SelectMany(a => CollectFunctionReferences(a.Value)),

            SyntaxTypes.Expression.ParenthesizedExpression paren =>
                CollectFunctionReferences(paren.Expression.Value),

            SyntaxTypes.Expression.IfBlock ifBlock =>
                CollectFunctionReferences(ifBlock.Condition.Value)
                    .Concat(CollectFunctionReferences(ifBlock.ThenBlock.Value))
                    .Concat(CollectFunctionReferences(ifBlock.ElseBlock.Value)),

            SyntaxTypes.Expression.CaseExpression caseExpr =>
                CollectFunctionReferences(caseExpr.CaseBlock.Expression.Value)
                    .Concat(caseExpr.CaseBlock.Cases.SelectMany(c => CollectFunctionReferences(c.Expression.Value))),

            SyntaxTypes.Expression.LetExpression letExpr =>
                letExpr.Value.Declarations.SelectMany(d => CollectFunctionReferencesFromLetDeclaration(d.Value))
                    .Concat(CollectFunctionReferences(letExpr.Value.Expression.Value)),

            SyntaxTypes.Expression.LambdaExpression lambda =>
                CollectFunctionReferences(lambda.Lambda.Expression.Value),

            SyntaxTypes.Expression.ListExpr listExpr =>
                listExpr.Elements.SelectMany(e => CollectFunctionReferences(e.Value)),

            SyntaxTypes.Expression.TupledExpression tupled =>
                tupled.Elements.SelectMany(e => CollectFunctionReferences(e.Value)),

            SyntaxTypes.Expression.RecordExpr recordExpr =>
                recordExpr.Fields.SelectMany(f => CollectFunctionReferences(f.Value.valueExpr.Value)),

            SyntaxTypes.Expression.RecordUpdateExpression recordUpdate =>
                recordUpdate.Fields.SelectMany(f => CollectFunctionReferences(f.Value.valueExpr.Value)),

            SyntaxTypes.Expression.RecordAccess recordAccess =>
                CollectFunctionReferences(recordAccess.Record.Value),

            SyntaxTypes.Expression.Negation negation =>
                CollectFunctionReferences(negation.Expression.Value),

            SyntaxTypes.Expression.OperatorApplication opApp =>
                CollectFunctionReferences(opApp.Left.Value)
                    .Concat(CollectFunctionReferences(opApp.Right.Value)),

            _ => []
        };
    }

    private static IEnumerable<(ModuleName ModuleName, string FunctionName)> CollectFunctionReferencesFromLetDeclaration(
        SyntaxTypes.Expression.LetDeclaration letDecl)
    {
        return letDecl switch
        {
            SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc =>
                CollectFunctionReferences(letFunc.Function.Declaration.Value.Expression.Value),

            SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr =>
                CollectFunctionReferences(letDestr.Expression.Value),

            _ => []
        };
    }

    private static ImmutableDictionary<(ModuleName ModuleName, string FunctionName), FunctionInfo> BuildFunctionDictionary(
        IReadOnlyList<SyntaxTypes.File> modules)
    {
        var builder = ImmutableDictionary.CreateBuilder<(ModuleName ModuleName, string FunctionName), FunctionInfo>(
            s_moduleNameTupleComparer);

        foreach (var module in modules)
        {
            var moduleName = SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value;

            foreach (var decl in module.Declarations)
            {
                if (decl.Value is SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
                {
                    var funcName = funcDecl.Function.Declaration.Value.Name.Value;
                    var key = (moduleName, funcName);
                    builder[key] = new FunctionInfo(moduleName, funcDecl.Function, IsRecursive: false);
                }
            }
        }

        return builder.ToImmutable();
    }

    private static SyntaxTypes.File InlineModule(SyntaxTypes.File module, InliningContext context)
    {
        var inlinedDeclarations = module.Declarations
            .Select(decl => InlineDeclaration(decl, context))
            .ToList();

        return module with { Declarations = inlinedDeclarations };
    }

    private static SyntaxTypes.Node<SyntaxTypes.Declaration> InlineDeclaration(
        SyntaxTypes.Node<SyntaxTypes.Declaration> declNode,
        InliningContext context)
    {
        if (declNode.Value is not SyntaxTypes.Declaration.FunctionDeclaration funcDecl)
        {
            return declNode;
        }

        var inlinedFunction = InlineFunctionStruct(funcDecl.Function, context);
        var inlinedDeclaration = new SyntaxTypes.Declaration.FunctionDeclaration(inlinedFunction);

        return new SyntaxTypes.Node<SyntaxTypes.Declaration>(declNode.Range, inlinedDeclaration);
    }

    private static SyntaxTypes.FunctionStruct InlineFunctionStruct(
        SyntaxTypes.FunctionStruct func,
        InliningContext context)
    {
        var impl = func.Declaration.Value;
        var inlinedExpr = InlineExpression(impl.Expression, context);

        var inlinedImpl = new SyntaxTypes.FunctionImplementation(
            Name: impl.Name,
            Arguments: impl.Arguments,
            Expression: inlinedExpr);

        return func with
        {
            Declaration = new SyntaxTypes.Node<SyntaxTypes.FunctionImplementation>(
                func.Declaration.Range,
                inlinedImpl)
        };
    }

    private static SyntaxTypes.Node<SyntaxTypes.Expression> InlineExpression(
        SyntaxTypes.Node<SyntaxTypes.Expression> exprNode,
        InliningContext context)
    {
        var expr = exprNode.Value;

        var inlinedExpr = expr switch
        {
            SyntaxTypes.Expression.Application app =>
                InlineApplication(app, context),

            SyntaxTypes.Expression.ParenthesizedExpression paren =>
                new SyntaxTypes.Expression.ParenthesizedExpression(
                    InlineExpression(paren.Expression, context)),

            SyntaxTypes.Expression.IfBlock ifBlock =>
                new SyntaxTypes.Expression.IfBlock(
                    InlineExpression(ifBlock.Condition, context),
                    InlineExpression(ifBlock.ThenBlock, context),
                    InlineExpression(ifBlock.ElseBlock, context)),

            SyntaxTypes.Expression.CaseExpression caseExpr =>
                new SyntaxTypes.Expression.CaseExpression(
                    InlineCaseBlock(caseExpr.CaseBlock, context)),

            SyntaxTypes.Expression.LetExpression letExpr =>
                new SyntaxTypes.Expression.LetExpression(
                    InlineLetBlock(letExpr.Value, context)),

            SyntaxTypes.Expression.LambdaExpression lambda =>
                new SyntaxTypes.Expression.LambdaExpression(
                    InlineLambdaStruct(lambda.Lambda, context)),

            SyntaxTypes.Expression.ListExpr listExpr =>
                new SyntaxTypes.Expression.ListExpr(
                    [.. listExpr.Elements.Select(e => InlineExpression(e, context))]),

            SyntaxTypes.Expression.TupledExpression tupled =>
                new SyntaxTypes.Expression.TupledExpression(
                    [.. tupled.Elements.Select(e => InlineExpression(e, context))]),

            SyntaxTypes.Expression.RecordExpr recordExpr =>
                new SyntaxTypes.Expression.RecordExpr(
                    [.. recordExpr.Fields.Select(f => InlineRecordField(f, context))]),

            SyntaxTypes.Expression.RecordUpdateExpression recordUpdate =>
                new SyntaxTypes.Expression.RecordUpdateExpression(
                    recordUpdate.RecordName,
                    [.. recordUpdate.Fields.Select(f => InlineRecordField(f, context))]),

            SyntaxTypes.Expression.RecordAccess recordAccess =>
                new SyntaxTypes.Expression.RecordAccess(
                    InlineExpression(recordAccess.Record, context),
                    recordAccess.FieldName),

            SyntaxTypes.Expression.Negation negation =>
                new SyntaxTypes.Expression.Negation(
                    InlineExpression(negation.Expression, context)),

            SyntaxTypes.Expression.OperatorApplication opApp =>
                new SyntaxTypes.Expression.OperatorApplication(
                    opApp.Operator,
                    opApp.Direction,
                    InlineExpression(opApp.Left, context),
                    InlineExpression(opApp.Right, context)),

            // Leaf expressions that don't need transformation
            SyntaxTypes.Expression.UnitExpr or
            SyntaxTypes.Expression.Literal or
            SyntaxTypes.Expression.CharLiteral or
            SyntaxTypes.Expression.Integer or
            SyntaxTypes.Expression.Hex or
            SyntaxTypes.Expression.Floatable or
            SyntaxTypes.Expression.FunctionOrValue or
            SyntaxTypes.Expression.PrefixOperator or
            SyntaxTypes.Expression.RecordAccessFunction =>
                expr,

            _ => expr
        };

        return new SyntaxTypes.Node<SyntaxTypes.Expression>(exprNode.Range, inlinedExpr);
    }

    private static SyntaxTypes.Expression InlineApplication(
        SyntaxTypes.Expression.Application app,
        InliningContext context)
    {
        if (app.Arguments.Count < 2)
        {
            // No actual arguments, just recursively inline
            return new SyntaxTypes.Expression.Application(
                [.. app.Arguments.Select(a => InlineExpression(a, context))]);
        }

        var funcExpr = app.Arguments[0].Value;

        // Check if this is a call to a known function
        if (funcExpr is SyntaxTypes.Expression.FunctionOrValue funcOrValue)
        {
            var qualifiedName = (funcOrValue.ModuleName, funcOrValue.Name);

            if (context.FunctionsByQualifiedName.TryGetValue(qualifiedName, out var funcInfo))
            {
                // Skip inlining recursive functions
                if (funcInfo.IsRecursive)
                {
                    return new SyntaxTypes.Expression.Application(
                        [.. app.Arguments.Select(a => InlineExpression(a, context))]);
                }

                // Skip if we're already in the process of inlining this function (prevents infinite recursion)
                if (context.InliningStack.Contains(qualifiedName))
                {
                    return new SyntaxTypes.Expression.Application(
                        [.. app.Arguments.Select(a => InlineExpression(a, context))]);
                }

                var funcImpl = funcInfo.Function.Declaration.Value;
                var funcParams = funcImpl.Arguments;
                var appArgs = app.Arguments.Skip(1).ToList();

                // Check if we should inline based on config
                if (ShouldInline(funcParams, appArgs, context))
                {
                    // Add this function to the inlining stack to prevent infinite recursion
                    var newContext = context with { InliningStack = context.InliningStack.Add(qualifiedName) };

                    // Inline the function: substitute parameters with arguments
                    var inlinedResult = InlineFunctionCall(funcImpl, appArgs, newContext);
                    return inlinedResult;
                }
            }
        }

        // Default: recursively inline arguments
        return new SyntaxTypes.Expression.Application(
            [.. app.Arguments.Select(a => InlineExpression(a, context))]);
    }

    private static bool ShouldInline(
        IReadOnlyList<SyntaxTypes.Node<SyntaxTypes.Pattern>> funcParams,
        IReadOnlyList<SyntaxTypes.Node<SyntaxTypes.Expression>> appArgs,
        InliningContext context)
    {
        if (context.Config is not Config.InlineOnlyFunctions)
        {
            return false;
        }

        // Check if any argument is a function (lambda expression) or if the parameter
        // uses constructor pattern matching (indicating it expects a wrapped function type)
        var argCount = Math.Min(funcParams.Count, appArgs.Count);

        for (var i = 0; i < argCount; i++)
        {
            var param = funcParams[i].Value;
            var arg = appArgs[i].Value;

            // An argument is considered a "function" if it's a lambda or a known function reference
            if (IsFunctionExpression(arg, context))
            {
                return true;
            }

            // If the parameter uses a constructor pattern (like `(Parser f)`), it's likely
            // extracting a function from a wrapped type - inline to allow direct use
            if (IsConstructorPattern(param))
            {
                return true;
            }
        }

        return false;
    }

    private static bool IsConstructorPattern(SyntaxTypes.Pattern pattern)
    {
        return pattern switch
        {
            SyntaxTypes.Pattern.NamedPattern => true,
            SyntaxTypes.Pattern.ParenthesizedPattern paren => IsConstructorPattern(paren.Pattern.Value),
            _ => false
        };
    }

    private static bool IsFunctionExpression(SyntaxTypes.Expression expr, InliningContext context)
    {
        return expr switch
        {
            SyntaxTypes.Expression.LambdaExpression => true,

            // Only consider FunctionOrValue as a function if it's a known function in our function dictionary
            // This excludes data constructors and local variables
            SyntaxTypes.Expression.FunctionOrValue funcOrValue =>
                context.FunctionsByQualifiedName.ContainsKey((funcOrValue.ModuleName, funcOrValue.Name)),

            SyntaxTypes.Expression.ParenthesizedExpression paren =>
                IsFunctionExpression(paren.Expression.Value, context),

            _ => false
        };
    }

    private static SyntaxTypes.Expression InlineFunctionCall(
        SyntaxTypes.FunctionImplementation funcImpl,
        IReadOnlyList<SyntaxTypes.Node<SyntaxTypes.Expression>> args,
        InliningContext context)
    {
        var funcParams = funcImpl.Arguments;
        var funcBody = funcImpl.Expression;

        // First, recursively inline the arguments
        var inlinedArgs = args.Select(a => InlineExpression(a, context)).ToList();

        // Check if any parameter uses constructor pattern matching - if so, we need let bindings
        var letDeclarations = new List<SyntaxTypes.Node<SyntaxTypes.Expression.LetDeclaration>>();
        var substitutions = new Dictionary<string, SyntaxTypes.Node<SyntaxTypes.Expression>>();

        var count = Math.Min(funcParams.Count, inlinedArgs.Count);

        for (var i = 0; i < count; i++)
        {
            var param = funcParams[i];
            var arg = inlinedArgs[i];

            if (param.Value is SyntaxTypes.Pattern.VarPattern varPattern)
            {
                // Simple variable pattern - direct substitution
                substitutions[varPattern.Name] = arg;
            }
            else if (IsConstructorPattern(param.Value))
            {
                // Constructor pattern - create a let destructuring binding
                var letDestr = new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                    Pattern: param,
                    Expression: arg);
                letDeclarations.Add(new SyntaxTypes.Node<SyntaxTypes.Expression.LetDeclaration>(s_zeroRange, letDestr));
            }
            // Note: Other complex patterns (like tuples, records) would need additional handling
            // For now, we only support VarPattern and NamedPattern (constructor patterns)
        }

        // Substitute in the function body
        var substitutedBody = SubstituteInExpression(funcBody, substitutions);

        // Recursively inline in the substituted body
        var inlinedBody = InlineExpression(substitutedBody, context);

        // If we have let declarations, wrap the body in a let expression
        SyntaxTypes.Expression resultExpr;

        if (letDeclarations.Count > 0)
        {
            var letBlock =
                new SyntaxTypes.Expression.LetBlock(
                    Declarations: [.. letDeclarations],
                    Expression: inlinedBody);

            resultExpr = new SyntaxTypes.Expression.LetExpression(letBlock);
        }
        else
        {
            resultExpr = inlinedBody.Value;
        }

        // If we have more arguments than parameters, we need to create an application
        if (args.Count > funcParams.Count)
        {
            var remainingArgs =
                args.Skip(funcParams.Count).Select(a => InlineExpression(a, context))
                .ToList();

            var allArgs = new List<SyntaxTypes.Node<SyntaxTypes.Expression>>
            {
                new(s_zeroRange, resultExpr)
            };

            allArgs.AddRange(remainingArgs);

            return new SyntaxTypes.Expression.Application([.. allArgs]);
        }

        // If we have fewer arguments than parameters, we create a partial application (lambda)
        if (args.Count < funcParams.Count)
        {
            var remainingParams = funcParams.Skip(args.Count).ToList();
            return new SyntaxTypes.Expression.LambdaExpression(
                new SyntaxTypes.LambdaStruct(
                    Arguments: remainingParams,
                    Expression: new SyntaxTypes.Node<SyntaxTypes.Expression>(s_zeroRange, resultExpr)));
        }

        return resultExpr;
    }

    private static SyntaxTypes.Node<SyntaxTypes.Expression> SubstituteInExpression(
        SyntaxTypes.Node<SyntaxTypes.Expression> exprNode,
        IReadOnlyDictionary<string, SyntaxTypes.Node<SyntaxTypes.Expression>> substitutions)
    {
        var expr = exprNode.Value;

        var substitutedExpr = expr switch
        {
            SyntaxTypes.Expression.FunctionOrValue funcOrValue when
                funcOrValue.ModuleName.Count is 0 &&
                substitutions.TryGetValue(funcOrValue.Name, out var replacement) =>
                replacement.Value,

            SyntaxTypes.Expression.Application app =>
                new SyntaxTypes.Expression.Application(
                    [.. app.Arguments.Select(a => SubstituteInExpression(a, substitutions))]),

            SyntaxTypes.Expression.ParenthesizedExpression paren =>
                new SyntaxTypes.Expression.ParenthesizedExpression(
                    SubstituteInExpression(paren.Expression, substitutions)),

            SyntaxTypes.Expression.IfBlock ifBlock =>
                new SyntaxTypes.Expression.IfBlock(
                    SubstituteInExpression(ifBlock.Condition, substitutions),
                    SubstituteInExpression(ifBlock.ThenBlock, substitutions),
                    SubstituteInExpression(ifBlock.ElseBlock, substitutions)),

            SyntaxTypes.Expression.CaseExpression caseExpr =>
                new SyntaxTypes.Expression.CaseExpression(
                    SubstituteInCaseBlock(caseExpr.CaseBlock, substitutions)),

            SyntaxTypes.Expression.LetExpression letExpr =>
                new SyntaxTypes.Expression.LetExpression(
                    SubstituteInLetBlock(letExpr.Value, substitutions)),

            SyntaxTypes.Expression.LambdaExpression lambda =>
                new SyntaxTypes.Expression.LambdaExpression(
                    SubstituteInLambdaStruct(lambda.Lambda, substitutions)),

            SyntaxTypes.Expression.ListExpr listExpr =>
                new SyntaxTypes.Expression.ListExpr(
                    [.. listExpr.Elements.Select(e => SubstituteInExpression(e, substitutions))]),

            SyntaxTypes.Expression.TupledExpression tupled =>
                new SyntaxTypes.Expression.TupledExpression(
                    [.. tupled.Elements.Select(e => SubstituteInExpression(e, substitutions))]),

            SyntaxTypes.Expression.RecordExpr recordExpr =>
                new SyntaxTypes.Expression.RecordExpr(
                    [.. recordExpr.Fields.Select(f => SubstituteInRecordField(f, substitutions))]),

            SyntaxTypes.Expression.RecordUpdateExpression recordUpdate =>
                new SyntaxTypes.Expression.RecordUpdateExpression(
                    recordUpdate.RecordName,
                    [.. recordUpdate.Fields.Select(f => SubstituteInRecordField(f, substitutions))]),

            SyntaxTypes.Expression.RecordAccess recordAccess =>
                new SyntaxTypes.Expression.RecordAccess(
                    SubstituteInExpression(recordAccess.Record, substitutions),
                    recordAccess.FieldName),

            SyntaxTypes.Expression.Negation negation =>
                new SyntaxTypes.Expression.Negation(
                    SubstituteInExpression(negation.Expression, substitutions)),

            SyntaxTypes.Expression.OperatorApplication opApp =>
                new SyntaxTypes.Expression.OperatorApplication(
                    opApp.Operator,
                    opApp.Direction,
                    SubstituteInExpression(opApp.Left, substitutions),
                    SubstituteInExpression(opApp.Right, substitutions)),

            // Leaf expressions
            _ => expr
        };

        return new SyntaxTypes.Node<SyntaxTypes.Expression>(exprNode.Range, substitutedExpr);
    }

    private static SyntaxTypes.CaseBlock SubstituteInCaseBlock(
        SyntaxTypes.CaseBlock caseBlock,
        IReadOnlyDictionary<string, SyntaxTypes.Node<SyntaxTypes.Expression>> substitutions)
    {
        return new SyntaxTypes.CaseBlock(
            Expression: SubstituteInExpression(caseBlock.Expression, substitutions),
            Cases: [.. caseBlock.Cases.Select(c => SubstituteInCase(c, substitutions))]);
    }

    private static SyntaxTypes.Case SubstituteInCase(
        SyntaxTypes.Case caseItem,
        IReadOnlyDictionary<string, SyntaxTypes.Node<SyntaxTypes.Expression>> substitutions)
    {
        // Remove substitutions shadowed by the pattern
        var shadowedNames = CollectPatternNames(caseItem.Pattern.Value);
        var filteredSubstitutions = substitutions
            .Where(kvp => !shadowedNames.Contains(kvp.Key))
            .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

        return new SyntaxTypes.Case(
            Pattern: caseItem.Pattern,
            Expression: SubstituteInExpression(caseItem.Expression, filteredSubstitutions));
    }

    private static SyntaxTypes.Expression.LetBlock SubstituteInLetBlock(
        SyntaxTypes.Expression.LetBlock letBlock,
        IReadOnlyDictionary<string, SyntaxTypes.Node<SyntaxTypes.Expression>> substitutions)
    {
        // Collect names introduced by let declarations
        var letNames = new HashSet<string>();
        foreach (var decl in letBlock.Declarations)
        {
            if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc)
            {
                letNames.Add(letFunc.Function.Declaration.Value.Name.Value);
            }
            else if (decl.Value is SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr)
            {
                foreach (var name in CollectPatternNames(letDestr.Pattern.Value))
                {
                    letNames.Add(name);
                }
            }
        }

        // Filter substitutions for the body (names introduced by let shadow outer substitutions)
        var filteredSubstitutions = substitutions
            .Where(kvp => !letNames.Contains(kvp.Key))
            .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

        return new SyntaxTypes.Expression.LetBlock(
            Declarations: [.. letBlock.Declarations.Select(d => SubstituteInLetDeclaration(d, substitutions))],
            Expression: SubstituteInExpression(letBlock.Expression, filteredSubstitutions));
    }

    private static SyntaxTypes.Node<SyntaxTypes.Expression.LetDeclaration> SubstituteInLetDeclaration(
        SyntaxTypes.Node<SyntaxTypes.Expression.LetDeclaration> declNode,
        IReadOnlyDictionary<string, SyntaxTypes.Node<SyntaxTypes.Expression>> substitutions)
    {
        var decl = declNode.Value;

        var substitutedDecl =
            decl switch
            {
                SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc =>
                    new SyntaxTypes.Expression.LetDeclaration.LetFunction(
                        SubstituteInFunctionStruct(letFunc.Function, substitutions)),

                SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr =>
                    new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                        letDestr.Pattern,
                        SubstituteInExpression(letDestr.Expression, substitutions)),

                _ => decl
            };

        return new SyntaxTypes.Node<SyntaxTypes.Expression.LetDeclaration>(declNode.Range, substitutedDecl);
    }

    private static SyntaxTypes.FunctionStruct SubstituteInFunctionStruct(
        SyntaxTypes.FunctionStruct func,
        IReadOnlyDictionary<string, SyntaxTypes.Node<SyntaxTypes.Expression>> substitutions)
    {
        var impl = func.Declaration.Value;

        // Remove substitutions shadowed by function parameters
        var paramNames = new HashSet<string>();

        foreach (var param in impl.Arguments)
        {
            foreach (var name in CollectPatternNames(param.Value))
            {
                paramNames.Add(name);
            }
        }

        var filteredSubstitutions = substitutions
            .Where(kvp => !paramNames.Contains(kvp.Key))
            .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

        var substitutedImpl = new SyntaxTypes.FunctionImplementation(
            Name: impl.Name,
            Arguments: impl.Arguments,
            Expression: SubstituteInExpression(impl.Expression, filteredSubstitutions));

        return func with
        {
            Declaration = new SyntaxTypes.Node<SyntaxTypes.FunctionImplementation>(
                func.Declaration.Range,
                substitutedImpl)
        };
    }

    private static SyntaxTypes.LambdaStruct SubstituteInLambdaStruct(
        SyntaxTypes.LambdaStruct lambda,
        IReadOnlyDictionary<string, SyntaxTypes.Node<SyntaxTypes.Expression>> substitutions)
    {
        // Remove substitutions shadowed by lambda parameters
        var paramNames = new HashSet<string>();

        foreach (var param in lambda.Arguments)
        {
            foreach (var name in CollectPatternNames(param.Value))
            {
                paramNames.Add(name);
            }
        }

        var filteredSubstitutions = substitutions
            .Where(kvp => !paramNames.Contains(kvp.Key))
            .ToDictionary(kvp => kvp.Key, kvp => kvp.Value);

        return new SyntaxTypes.LambdaStruct(
            Arguments: lambda.Arguments,
            Expression: SubstituteInExpression(lambda.Expression, filteredSubstitutions));
    }

    private static SyntaxTypes.Node<(SyntaxTypes.Node<string>, SyntaxTypes.Node<SyntaxTypes.Expression>)> SubstituteInRecordField(
        SyntaxTypes.Node<(SyntaxTypes.Node<string> fieldName, SyntaxTypes.Node<SyntaxTypes.Expression> valueExpr)> fieldNode,
        IReadOnlyDictionary<string, SyntaxTypes.Node<SyntaxTypes.Expression>> substitutions)
    {
        var (fieldName, valueExpr) = fieldNode.Value;
        return new SyntaxTypes.Node<(SyntaxTypes.Node<string>, SyntaxTypes.Node<SyntaxTypes.Expression>)>(
            fieldNode.Range,
            (fieldName, SubstituteInExpression(valueExpr, substitutions)));
    }

    private static HashSet<string> CollectPatternNames(SyntaxTypes.Pattern pattern)
    {
        var names = new HashSet<string>();

        CollectPatternNamesRecursive(pattern, names);

        return names;
    }

    private static void CollectPatternNamesRecursive(SyntaxTypes.Pattern pattern, HashSet<string> names)
    {
        switch (pattern)
        {
            case SyntaxTypes.Pattern.VarPattern varPattern:
                names.Add(varPattern.Name);
                break;

            case SyntaxTypes.Pattern.TuplePattern tuplePattern:
                foreach (var elem in tuplePattern.Elements)
                {
                    CollectPatternNamesRecursive(elem.Value, names);
                }
                break;

            case SyntaxTypes.Pattern.RecordPattern recordPattern:
                foreach (var field in recordPattern.Fields)
                {
                    names.Add(field.Value);
                }
                break;

            case SyntaxTypes.Pattern.UnConsPattern unconsPattern:
                CollectPatternNamesRecursive(unconsPattern.Head.Value, names);
                CollectPatternNamesRecursive(unconsPattern.Tail.Value, names);
                break;

            case SyntaxTypes.Pattern.ListPattern listPattern:
                foreach (var elem in listPattern.Elements)
                {
                    CollectPatternNamesRecursive(elem.Value, names);
                }
                break;

            case SyntaxTypes.Pattern.NamedPattern namedPattern:
                foreach (var arg in namedPattern.Arguments)
                {
                    CollectPatternNamesRecursive(arg.Value, names);
                }
                break;

            case SyntaxTypes.Pattern.AsPattern asPattern:
                names.Add(asPattern.Name.Value);
                CollectPatternNamesRecursive(asPattern.Pattern.Value, names);
                break;

            case SyntaxTypes.Pattern.ParenthesizedPattern parenPattern:
                CollectPatternNamesRecursive(parenPattern.Pattern.Value, names);
                break;

                // Other pattern types don't introduce names
        }
    }

    private static SyntaxTypes.CaseBlock InlineCaseBlock(
        SyntaxTypes.CaseBlock caseBlock,
        InliningContext context)
    {
        return new SyntaxTypes.CaseBlock(
            Expression: InlineExpression(caseBlock.Expression, context),
            Cases: [.. caseBlock.Cases.Select(c => InlineCase(c, context))]);
    }

    private static SyntaxTypes.Case InlineCase(
        SyntaxTypes.Case caseItem,
        InliningContext context)
    {
        return new SyntaxTypes.Case(
            Pattern: caseItem.Pattern,
            Expression: InlineExpression(caseItem.Expression, context));
    }

    private static SyntaxTypes.Expression.LetBlock InlineLetBlock(
        SyntaxTypes.Expression.LetBlock letBlock,
        InliningContext context)
    {
        return new SyntaxTypes.Expression.LetBlock(
            Declarations: [.. letBlock.Declarations.Select(d => InlineLetDeclaration(d, context))],
            Expression: InlineExpression(letBlock.Expression, context));
    }

    private static SyntaxTypes.Node<SyntaxTypes.Expression.LetDeclaration> InlineLetDeclaration(
        SyntaxTypes.Node<SyntaxTypes.Expression.LetDeclaration> declNode,
        InliningContext context)
    {
        var decl = declNode.Value;

        var inlinedDecl = decl switch
        {
            SyntaxTypes.Expression.LetDeclaration.LetFunction letFunc =>
                new SyntaxTypes.Expression.LetDeclaration.LetFunction(
                    InlineFunctionStruct(letFunc.Function, context)),

            SyntaxTypes.Expression.LetDeclaration.LetDestructuring letDestr =>
                new SyntaxTypes.Expression.LetDeclaration.LetDestructuring(
                    letDestr.Pattern,
                    InlineExpression(letDestr.Expression, context)),

            _ => decl
        };

        return new SyntaxTypes.Node<SyntaxTypes.Expression.LetDeclaration>(declNode.Range, inlinedDecl);
    }

    private static SyntaxTypes.LambdaStruct InlineLambdaStruct(
        SyntaxTypes.LambdaStruct lambda,
        InliningContext context)
    {
        return new SyntaxTypes.LambdaStruct(
            Arguments: lambda.Arguments,
            Expression: InlineExpression(lambda.Expression, context));
    }

    private static SyntaxTypes.Node<(SyntaxTypes.Node<string>, SyntaxTypes.Node<SyntaxTypes.Expression>)> InlineRecordField(
        SyntaxTypes.Node<(SyntaxTypes.Node<string> fieldName, SyntaxTypes.Node<SyntaxTypes.Expression> valueExpr)> fieldNode,
        InliningContext context)
    {
        var (fieldName, valueExpr) = fieldNode.Value;

        return new SyntaxTypes.Node<(SyntaxTypes.Node<string>, SyntaxTypes.Node<SyntaxTypes.Expression>)>(
            fieldNode.Range,
            (fieldName, InlineExpression(valueExpr, context)));
    }

    /// <summary>
    /// Comparer for module name tuples that handles collection equality properly.
    /// </summary>
    private sealed class ModuleNameTupleComparer : IEqualityComparer<(ModuleName ModuleName, string FunctionName)>
    {
        public bool Equals((ModuleName ModuleName, string FunctionName) x, (ModuleName ModuleName, string FunctionName) y)
        {
            return
                x.FunctionName == y.FunctionName &&
                x.ModuleName.SequenceEqual(y.ModuleName);
        }

        public int GetHashCode((ModuleName ModuleName, string FunctionName) obj)
        {
            var hash = new System.HashCode();

            foreach (var part in obj.ModuleName)
            {
                hash.Add(part);
            }

            hash.Add(obj.FunctionName);

            return hash.ToHashCode();
        }
    }
}
