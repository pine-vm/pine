using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Internal;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Stil4mFile = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.File;
using Stil4mFromFull = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel;
using Stil4mToFull = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.ToFullSyntaxModel;

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
            return prepareErr;
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
    public static Result<ElmInterpretationError, Prepared> PrepareModules(
        IReadOnlyList<string> moduleSourceTexts)
    {
        // Parse every module text into the full SyntaxModel form.
        var parsedFiles = new List<SyntaxModel.File>(moduleSourceTexts.Count);

        for (var i = 0; i < moduleSourceTexts.Count; i++)
        {
            var parseResult = ElmSyntaxParser.ParseModuleText(moduleSourceTexts[i]);

            if (parseResult.IsErrOrNull() is { } parseErr)
            {
                return
                    new ElmInterpretationError(
                        "Failed to parse module #" + i + ": " + parseErr,
                        []);
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
    public static Result<ElmInterpretationError, Prepared> PrepareModules(
        IReadOnlyList<SyntaxModel.File> modules)
    {
        if (modules.Count is 0)
        {
            return
                new ElmInterpretationError(
                    "ParseAndInterpret with modules requires at least one module "
                    + "(the synthetic root module containing 'pine_root_expression').",
                    []);
        }

        // Convert to the Stil4mElmSyntax7 form expected by Canonicalization.
        var stil4mFiles = new List<Stil4mFile>(modules.Count);

        for (var i = 0; i < modules.Count; i++)
        {
            stil4mFiles.Add(Stil4mFromFull.Convert(modules[i]));
        }

        var canonicalizeResult = Canonicalization.CanonicalizeAllowingErrors(stil4mFiles);

        if (canonicalizeResult.IsErrOrNull() is { } canonErr)
        {
            return
                new ElmInterpretationError(
                    "Canonicalization failed: " + canonErr,
                    []);
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
            var fullModuleFile = Stil4mToFull.Convert(canonicalizedFile);

            var abstractFile = ElmSyntaxAbstract.ConvertFromConcrete.FromFile(fullModuleFile);

            var moduleNameParts = moduleNameKey.ToList();

            foreach (var declNode in abstractFile.Declarations)
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
        var stil4mExpr = Stil4mFromFull.Convert(expr);

        return
            Canonicalization.CanonicalizeExpression(stil4mExpr, ImplicitImportConfig.Default)
            .MapValue(stil4mCanonicalExpr => Stil4mToFull.Convert(stil4mCanonicalExpr));
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
