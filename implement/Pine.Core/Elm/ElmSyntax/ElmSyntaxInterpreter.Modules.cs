using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Internal;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;


namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// Multi-module evaluation overloads. These accept a collection of Elm module source
/// texts (or pre-parsed <see cref="SyntaxModel.File"/> values), apply the existing
/// <see cref="Canonicalization"/> implementation to qualify all references in
/// expressions, and then dispatch evaluation against the canonicalized declarations
/// keyed by their full module-qualified name.
/// </summary>
public partial class ElmSyntaxInterpreter
{
    /// <summary>
    /// Program code of an app prepared to run functions in the interpreter.
    /// </summary>
    public record Prepared(
        IReadOnlyDictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration> Declarations);

    /// <summary>
    /// Parses the supplied <paramref name="moduleSourceTexts"/> to qualify every reference in every module,
    /// and then interprets the canonicalized root expression against a declaration
    /// dictionary keyed by full module-qualified name.
    /// </summary>
    public static Result<ElmInterpretationError, ElmValue> ParseAndInterpretAsElmValue(
        string rootExpressionText,
        IReadOnlyList<string> moduleSourceTexts)
    {
        var prepareModulesResult = PrepareModules(moduleSourceTexts);

        if (prepareModulesResult.IsErrOrNull() is { } prepareErr)
        {
            return new ElmInterpretationError(prepareErr, []);
        }

        if (prepareModulesResult.IsOkOrNull() is not { } preprocessed)
        {
            throw new System.NotImplementedException(
                "Unexpected result type from PrepareModules: " + prepareModulesResult.GetType().FullName);
        }

        return InterpretAsElmValue(rootExpressionText, preprocessed);
    }

    /// <summary>
    /// As <see cref="InterpretAsElmValue(string, Prepared)"/>, but additionally records an
    /// <see cref="ElmSyntaxInterpreterPerformanceCounters"/> snapshot, optionally forwards every
    /// observed function application to <paramref name="onApplication"/>, and lets the caller
    /// choose whether the interpreter's default builtins (such as the
    /// <c>Basics.compare</c> entry registered by <see cref="BuildBuiltinFunctionResolvers"/>)
    /// short-circuit the corresponding user-defined Elm declarations.
    /// <para>
    /// The root expression is parsed and canonicalized with the default implicit imports, so an
    /// unqualified reference such as <c>compare</c> resolves to <c>Basics.compare</c> and is
    /// served by the builtin when <paramref name="enableDefaultBuiltins"/> is <c>true</c>.
    /// </para>
    /// </summary>
    /// <param name="rootExpressionText">The Elm expression text to parse and evaluate.</param>
    /// <param name="prepared">Canonicalized declarations the root expression can reference.</param>
    /// <param name="onApplication">
    /// Optional callback invoked once for each <see cref="ApplicationLogEntry"/> the interpreter
    /// dispatches. Tests typically supply <c>list.Add</c> here to capture a trace they can later
    /// search or render.
    /// </param>
    /// <param name="enableDefaultBuiltins">
    /// When <c>true</c> (the default), applications matching a registered builtin in
    /// <see cref="BuildBuiltinFunctionResolvers"/> are computed directly on the value model
    /// rather than by interpreting the user-defined declaration. When <c>false</c>, builtins
    /// are bypassed and every application resolves against <paramref name="prepared"/>.
    /// </param>
    public static (Result<ElmInterpretationError, PineValueInProcess> Result, ElmSyntaxInterpreterPerformanceCounters Counters)
        ParseAndInterpretWithCounters(
        string rootExpressionText,
        Prepared prepared,
        System.Action<ApplicationLogEntry>? onApplication = null,
        bool enableDefaultBuiltins = true)
    {
        var parseResult = ParseAndCanonicalizeExpressionWithDefaultImports(rootExpressionText);

        if (parseResult.IsErrOrNull() is { } parseErr)
        {
            return (new ElmInterpretationError(parseErr, []), default);
        }

        if (parseResult.IsOkOrNull() is not { } rootExpression)
        {
            throw new System.NotImplementedException(
                "Unexpected parse result type: " + parseResult.GetType().FullName);
        }

        var invocationCounter = new InvocationCounter(onApplication);

        var resolvers = new List<System.Func<Application, ApplicationResolution?>>();

        if (enableDefaultBuiltins)
        {
            resolvers.Add(ApplicationResolver(s_builtinFunctionResolvers));
        }

        resolvers.Add(app => PineBuiltinResolverCounting(app, invocationCounter));
        resolvers.Add(app => UserDefinedResolver(app, prepared.Declarations));

        var combined = CombineResolvers(resolvers);

        var rootContext =
            new ApplicationContext(
                CurrentTopLevel: DeclQualifiedName.Create([], ""),
                LocalBindings: ImmutableDictionary<string, PineValueInProcess>.Empty);

        var result =
            RunTrampoline(
                initialExpression: ElmSyntaxAbstract.ConvertFromConcrete.FromExpression(rootExpression),
                initialEnv: rootContext,
                initialApplication: null,
                resolveApplication: combined,
                infixOperators: BuildInfixOperatorMap(prepared.Declarations),
                invocationLogger: invocationCounter);

        return (result, invocationCounter.ToReadOnly());
    }

    /// <summary>
    /// Parses the supplied <paramref name="expressionText"/> and qualifies every reference.
    /// </summary>
    public static Result<string, SyntaxModel.Expression> ParseAndCanonicalizeExpressionWithDefaultImports(
        string expressionText)
    {
        var parseResult = ElmSyntaxParser.ParseExpression(expressionText);

        if (parseResult.IsErrOrNull() is { } parseRootErr)
        {
            return "Failed to parse root expression: " + parseRootErr;
        }

        if (parseResult.IsOkOrNull() is not { } rootExpression)
        {
            throw new System.NotImplementedException(
                "Unexpected parse result type: " + parseResult.GetType().FullName);
        }

        var canonicalizeRootResult = CanonicalizeExpression(rootExpression);

        if (canonicalizeRootResult.Errors.Count is not 0)
        {
            return "Canonicalization of root expression failed: " + string.Join("; ", canonicalizeRootResult.Errors);
        }

        return canonicalizeRootResult.Value;
    }

    /// <summary>
    /// Parses the supplied <paramref name="moduleSourceTexts"/> to qualify every reference in every module.
    /// </summary>
    public static Result<string, Prepared> PrepareModules(
        IReadOnlyList<string> moduleSourceTexts)
    {
        // Parse every module text into the full SyntaxModel form.
        var parsedFiles = new List<SyntaxModel.File>(moduleSourceTexts.Count);

        for (var i = 0; i < moduleSourceTexts.Count; i++)
        {
            var parseResult = ElmSyntaxParser.ParseModuleText(moduleSourceTexts[i]);

            if (parseResult.IsErrOrNullable() is { } parseErr)
            {
                return "Failed to parse module #" + i + ": " + parseErr;
            }

            if (parseResult.IsOkOrNull() is not { } parsedFile)
            {
                throw new System.NotImplementedException(
                    "Unexpected parse result type: " + parseResult.GetType().FullName);
            }

            parsedFiles.Add(parsedFile);
        }

        return PrepareModules(parsedFiles);
    }

    /// <summary>
    /// As <see cref="PrepareModules(IReadOnlyList{string})"/>, but accepts
    /// pre-parsed modules.
    /// </summary>
    public static Result<string, Prepared> PrepareModules(
        IReadOnlyList<SyntaxModel.File> modules)
    {
        if (modules.Count is 0)
        {
            return
                "ParseAndInterpret with modules requires at least one module " +
                "(the synthetic root module containing 'pine_root_expression').";
        }

        var canonicalizeResult = Canonicalization.CanonicalizeAllowingErrors(modules);

        if (canonicalizeResult.IsErrOrNull() is { } canonErr)
        {
            return "Canonicalization failed: " + canonErr;
        }

        if (canonicalizeResult.IsOkOrNull() is not { } canonicalized)
        {
            throw new System.NotImplementedException(
                "Unexpected canonicalization result type: " + canonicalizeResult.GetType().FullName);
        }

        // Build a declarations dictionary keyed by full module-qualified name. For
        // InfixDeclaration entries, the key uses the operator symbol as DeclName (so
        // BuildInfixOperatorMap finds them).
        var declarations = new Dictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration>();

        foreach (var (moduleNameKey, (canonicalizedFile, errors, shadowings)) in canonicalized)
        {
            if (errors.Count is not 0)
            {
                var moduleNameStr = string.Join(".", moduleNameKey);

                var errMessages = string.Join("\n", errors.Select(ElmCompiler.RenderCanonicalizationError));

                return "Failed canonicalization in module " + moduleNameStr + ":\n" + errMessages;
            }

            var abstractFile = ElmSyntaxAbstract.ConvertFromConcrete.FromFile(canonicalizedFile);

            var abstractFileOptimized = OptimizeSyntax(abstractFile);

            var moduleNameParts = moduleNameKey.ToList();

            foreach (var declNode in abstractFileOptimized.Declarations)
            {
                if (declNode is ElmSyntaxAbstract.Declaration.InfixDeclaration infixDecl)
                {
                    declarations[DeclQualifiedName.Create(moduleNameParts, infixDecl.Infix.Operator)] = declNode;

                    continue;
                }

                var declName = DeclarationSimpleName(declNode);

                if (declName is null)
                    continue;

                declarations[DeclQualifiedName.Create(moduleNameParts, declName)] = declNode;
            }
        }

        return new Prepared(declarations);
    }

    private static ElmSyntaxAbstract.File OptimizeSyntax(ElmSyntaxAbstract.File file)
    {
        var operatorLoweringConfig =
            new ElmSyntaxAbstract.OperatorLowering.Config(
                LowerPipes: true,
                LowerBasicsArithmeticOperators: true,
                LowerBasicsComparisonOperators: true,
                LowerBasicsEqualityOperators: true,
                LowerBasicsLogicalOperators: true);

        return
            ElmSyntaxAbstract.OperatorLowering.RewriteOperators(file, operatorLoweringConfig);
    }

    /// <summary>
    /// Invokes the function identified by <paramref name="functionName"/> with the given
    /// <paramref name="arguments"/>, resolving against the supplied <paramref name="prepared"/>
    /// declarations (and the default builtins) via <see cref="BuildResolvers(IReadOnlyDictionary{DeclQualifiedName, ElmSyntaxAbstract.Declaration})"/>.
    /// </summary>
    public static Result<ElmInterpretationError, PineValueInProcess> Interpret(
        DeclQualifiedName functionName,
        IReadOnlyList<PineValueInProcess> arguments,
        Prepared prepared)
    {
        var resolver = BuildResolvers(prepared.Declarations);

        return
            Interpret(
                functionName,
                arguments,
                resolver,
                BuildInfixOperatorMap(prepared.Declarations));
    }

    private static CanonicalizationResult<SyntaxModel.Expression> CanonicalizeExpression(
        SyntaxModel.Expression expr)
    {
        return Canonicalization.CanonicalizeExpression(expr, ImplicitImportConfig.Default);
    }

    /// <summary>
    /// Returns the simple top-level name of <paramref name="declaration"/>, or null when
    /// the declaration kind does not carry such a name (e.g. infix or destructuring).
    /// </summary>
    private static string? DeclarationSimpleName(ElmSyntaxAbstract.Declaration declaration) =>
        declaration switch
        {
            ElmSyntaxAbstract.Declaration.FunctionDeclaration functionDeclaration =>
            functionDeclaration.Function.Declaration.Name,

            ElmSyntaxAbstract.Declaration.AliasDeclaration aliasDeclaration =>
            aliasDeclaration.TypeAlias.Name,

            ElmSyntaxAbstract.Declaration.ChoiceTypeDeclaration choiceTypeDeclaration =>
            choiceTypeDeclaration.TypeDeclaration.Name,

            _ =>
            null,
        };
}
