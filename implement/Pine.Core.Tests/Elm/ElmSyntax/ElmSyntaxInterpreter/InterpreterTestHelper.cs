using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Text;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;
using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;
using PineValueInProcess = Pine.Core.Internal.PineValueInProcess;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

internal static class InterpreterTestHelper
{
    /// <summary>
    /// Empty declarations dictionary used by the no-module evaluation overloads. Reused as a
    /// shared singleton so tests that don't depend on any user-defined declarations don't
    /// allocate a fresh empty dictionary on every invocation.
    /// </summary>
    private static readonly IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> s_emptyDeclarations =
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration>.Empty;

    /// <summary>
    /// Parses an Elm module text and returns a dictionary of its top-level declarations
    /// keyed by their simple declaration name (the bare top-level identifier, with no
    /// module-name qualification). Infix declarations are indexed under their operator
    /// symbol.
    /// </summary>
    /// <remarks>
    /// Use <see cref="ParseDeclarationsRemovingModuleNames(string)"/> instead when the
    /// result needs to be passed to <see cref="ElmInterpreter.ParseAndInterpret(string,
    /// IReadOnlyDictionary{DeclQualifiedName, SyntaxTypes.Declaration})"/>; that overload
    /// returns the same logical mapping but keyed by <see cref="DeclQualifiedName"/> with
    /// empty namespaces.
    /// </remarks>
    public static IReadOnlyDictionary<string, SyntaxTypes.Declaration>
        ParseDeclarations(string elmModuleText)
    {
        var parsedFile =
            ElmSyntaxParser.ParseModuleText(elmModuleText)
            .Extract(err => throw new Exception("Failed to parse module: " + err));

        var declarations = new Dictionary<string, SyntaxTypes.Declaration>();

        foreach (var declNode in parsedFile.Declarations)
        {
            // Infix metadata declarations don't have a "name" in the usual sense — they
            // map an operator symbol like `+` or `<+>` to an underlying function. Index
            // them under their operator symbol so the interpreter can later resolve
            // OperatorApplication / PrefixOperator nodes to the user-supplied function.
            if (declNode.Value is SyntaxTypes.Declaration.InfixDeclaration infixDecl)
            {
                declarations[infixDecl.Infix.Operator.Value] = declNode.Value;
                continue;
            }

            var name = DeclarationName(declNode.Value);

            if (name is null)
                continue;

            declarations[name] = declNode.Value;
        }

        return declarations;
    }

    /// <summary>
    /// As <see cref="ParseDeclarations(string)"/>, but returns a dictionary keyed by
    /// <see cref="DeclQualifiedName"/> with empty namespaces. Suitable for passing
    /// directly to <see cref="ElmInterpreter.ParseAndInterpret(string,
    /// IReadOnlyDictionary{DeclQualifiedName, SyntaxTypes.Declaration})"/> when the
    /// interpreter should resolve the parsed declarations as if they came from a single
    /// module whose name is irrelevant — i.e. unqualified references in the evaluated
    /// expression match the parsed declarations directly.
    /// </summary>
    public static IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration>
        ParseDeclarationsRemovingModuleNames(string elmModuleText)
    {
        var byName = ParseDeclarations(elmModuleText);

        var result = new Dictionary<DeclQualifiedName, SyntaxTypes.Declaration>(byName.Count);

        foreach (var (name, declaration) in byName)
        {
            result[DeclQualifiedName.Create([], name)] = declaration;
        }

        return result;
    }

    /// <summary>
    /// Returns the simple name of the top-level declaration, or null if the declaration kind
    /// does not carry a name relevant for lookup.
    /// </summary>
    public static string? DeclarationName(SyntaxTypes.Declaration declaration) =>
        declaration switch
        {
            SyntaxTypes.Declaration.FunctionDeclaration functionDeclaration =>
            functionDeclaration.Function.Declaration.Value.Name.Value,

            SyntaxTypes.Declaration.AliasDeclaration aliasDeclaration =>
            aliasDeclaration.TypeAlias.Name.Value,

            SyntaxTypes.Declaration.ChoiceTypeDeclaration choiceTypeDeclaration =>
            choiceTypeDeclaration.TypeDeclaration.Name.Value,

            _ =>
            null,
        };

    /// <summary>
    /// Retrieves the body expression of the function declaration with the given name from
    /// the declarations dictionary.
    /// </summary>
    public static SyntaxTypes.Expression GetFunctionBody(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxTypes.Declaration> declarations,
        string functionName)
    {
        var declaration = declarations[DeclQualifiedName.Create([], functionName)];

        if (declaration is SyntaxTypes.Declaration.FunctionDeclaration functionDeclaration)
        {
            return functionDeclaration.Function.Declaration.Value.Expression.Value;
        }

        throw new InvalidOperationException(
            "Declaration '" + functionName + "' is not a function declaration.");
    }

    /// <summary>
    /// Parses and interprets <paramref name="expression"/> with no user-defined declarations
    /// (only the built-in <c>Pine_builtin</c> / <c>Pine_kernel</c> resolvers apply).
    /// Returns the interpreter <see cref="Result{ErrT, OkT}"/>; use <see cref="EvaluateOrCrash(string)"/>
    /// when the test only cares about the success value.
    /// </summary>
    public static Result<ElmInterpretationError, PineValueInProcess> Evaluate(string expression) =>
        ElmInterpreter.ParseAndInterpret(expression, s_emptyDeclarations);

    /// <summary>
    /// As <see cref="Evaluate(string)"/>, but throws a <see cref="Exception"/>
    /// if interpretation fails. Intended for tests whose scenario should never reach a
    /// runtime error; tests that want to assert the error itself should use the
    /// <see cref="Result{ErrT, OkT}"/>-returning <see cref="Evaluate(string)"/> variant.
    /// </summary>
    public static PineValueInProcess EvaluateOrCrash(string expression) =>
        Evaluate(expression)
        .Extract(err => throw new Exception(err.ToString()));

    public static ElmValue EvaluateOrCrashAsElmValue(string expression) =>
        ElmInterpreter.ToElm(
            Evaluate(expression)
            .Extract(err => throw new Exception(err.ToString())));

    /// <summary>
    /// Parses <paramref name="elmModuleText"/> for its top-level declarations and then parses
    /// and interprets <paramref name="expression"/> against those declarations.
    /// Returns the interpreter <see cref="Result{ErrT, OkT}"/>; use
    /// <see cref="EvaluateInModuleOrCrash(string, string)"/> when the test only cares about
    /// the success value.
    /// </summary>
    public static Result<ElmInterpretationError, PineValueInProcess> EvaluateInModule(
        string expression,
        string elmModuleText) =>
        ElmInterpreter.ParseAndInterpret(expression, ParseDeclarationsRemovingModuleNames(elmModuleText));

    /// <summary>
    /// As <see cref="EvaluateInModule(string, string)"/>, but throws a
    /// <see cref="Exception"/> if interpretation fails. Intended for tests whose
    /// scenario should never reach a runtime error; tests that want to assert the error
    /// itself should use the <see cref="Result{ErrT, OkT}"/>-returning
    /// <see cref="EvaluateInModule(string, string)"/> variant.
    /// </summary>
    /// <summary>
    /// As <see cref="EvaluateInModule(string, string)"/>, but throws a
    /// <see cref="Exception"/> if interpretation fails. Intended for tests whose
    /// scenario should never reach a runtime error; tests that want to assert the error
    /// itself should use the <see cref="Result{ErrT, OkT}"/>-returning
    /// <see cref="EvaluateInModule(string, string)"/> variant.
    /// </summary>
    public static PineValueInProcess EvaluateInModuleOrCrash(
        string expression,
        string elmModuleText) =>
        EvaluateInModule(expression, elmModuleText)
        .Extract(err => throw new Exception(err.ToString()));

    public static ElmValue EvaluateInModuleOrCrashAsElmValue(
        string expression,
        string elmModuleText) =>
        ElmInterpreter.ToElm(
            EvaluateInModule(expression, elmModuleText)
            .Extract(err => throw new Exception(err.ToString())));

    /// <summary>
    /// As <see cref="EvaluateOrCrash(string)"/>, but additionally renders the resulting value
    /// to its Elm-expression text via <see cref="ElmValue.RenderAsElmExpression(ElmValue)"/>.
    /// Use for snapshot-style assertions on composed values (records, tuples, lists, tag
    /// instances) where comparing the rendered text reads more clearly than constructing the
    /// expected value out of nested <see cref="ElmValue"/> factory calls.
    /// </summary>
    public static string EvaluateOrCrashRendered(string expression) =>
        ElmInterpreter.RenderAsElmExpression(EvaluateOrCrash(expression)).expressionString;

    /// <summary>
    /// As <see cref="EvaluateInModuleOrCrash(string, string)"/>, but additionally renders the
    /// resulting value to its Elm-expression text via
    /// <see cref="ElmValue.RenderAsElmExpression(ElmValue)"/>.
    /// Use for snapshot-style assertions on composed values (records, tuples, lists, tag
    /// instances) where comparing the rendered text reads more clearly than constructing the
    /// expected value out of nested <see cref="ElmValue"/> factory calls.
    /// </summary>
    public static string EvaluateInModuleOrCrashRendered(
        string expression,
        string elmModuleText) =>
        ElmInterpreter.RenderAsElmExpression(
            EvaluateInModuleOrCrash(expression, elmModuleText))
        .expressionString;

    /// <summary>
    /// Loads the verbatim source text of one of the bundled Elm kernel modules (e.g.
    /// <c>Basics.elm</c>, <c>Dict.elm</c>, <c>String.elm</c>, <c>List.elm</c>) from
    /// <see cref="BundledFiles.ElmKernelModulesDefault"/>.
    /// </summary>
    public static string LoadKernelModuleSource(string fileName)
    {
        var node =
            BundledFiles.ElmKernelModulesDefault.Value
            .GetNodeAtPath([fileName])
            ?? throw new Exception("Did not find elm-kernel-modules/" + fileName + " in bundled files.");

        if (node is not Files.FileTree.FileNode fileNode)
        {
            throw new Exception(
                "Expected elm-kernel-modules/" + fileName + " to be a file node, but got: " + node.GetType());
        }

        return Encoding.UTF8.GetString(fileNode.Bytes.Span);
    }

    /// <summary>
    /// Loads the verbatim source text of one of the bundled compiler-source-container modules
    /// (e.g. <c>src/Base64/Encode.elm</c>) from
    /// <see cref="BundledFiles.CompilerSourceContainerFilesDefault"/>. The path is given as the
    /// sequence of directory/file segments from the container root (e.g.
    /// <c>"src", "Base64", "Encode.elm"</c>).
    /// </summary>
    public static string LoadCompilerSourceModule(params string[] path)
    {
        var node =
            BundledFiles.CompilerSourceContainerFilesDefault.Value
            .GetNodeAtPath(path)
            ?? throw new Exception("Did not find compiler source module at " + string.Join("/", path) + ".");

        if (node is not Files.FileTree.FileNode fileNode)
        {
            throw new Exception(
                "Expected compiler source " + string.Join("/", path) + " to be a file node, but got: " + node.GetType());
        }

        return Encoding.UTF8.GetString(fileNode.Bytes.Span);
    }

    /// <summary>
    /// Canonicalizes the supplied bundled kernel modules (identified by file name) into a
    /// single <see cref="ElmInterpreter.Prepared"/> program. References between modules are
    /// resolved by their fully-qualified canonical names, so a root expression evaluated
    /// against the result must address kernel functions with their module-qualified names
    /// (e.g. <c>String.split</c>, <c>Dict.get</c>).
    /// </summary>
    public static ElmInterpreter.Prepared PrepareKernelModules(params string[] fileNames)
    {
        var modules = new List<string>(fileNames.Length);

        foreach (var fileName in fileNames)
        {
            modules.Add(LoadKernelModuleSource(fileName));
        }

        return
            ElmInterpreter.PrepareModules(modules)
            .Extract(err => throw new Exception(err.ToString()));
    }

    /// <summary>
    /// As <see cref="PrepareKernelModules(string[])"/>, but canonicalizes an explicit list of
    /// already-loaded module source texts. Use this to combine bundled kernel modules with
    /// additional source-container modules (such as the <c>Base64</c> modules under
    /// <c>src/Base64</c>) in a single prepared program.
    /// </summary>
    public static ElmInterpreter.Prepared PrepareModulesFromSources(IReadOnlyList<string> moduleSources) =>
        ElmInterpreter.PrepareModules(moduleSources)
        .Extract(err => throw new Exception(err.ToString()));

    /// <summary>
    /// Evaluates <paramref name="expression"/> against the prepared modules and materializes
    /// the result into a concrete <see cref="PineValue"/> via
    /// <see cref="PineValueInProcess.Evaluate"/>. Operating on the <see cref="PineValue"/>
    /// directly (rather than converting to <see cref="ElmValue"/>) keeps the assertions
    /// robust for any plain-data result; as in Elm, values that contain functions are not
    /// supported and would surface as a runtime exception.
    /// </summary>
    public static PineValue EvaluateInModulesToPineValue(
        string expression,
        ElmInterpreter.Prepared prepared)
    {
        var (result, _) =
            ElmInterpreter.ParseAndInterpretWithCounters(expression, prepared);

        return
            result
            .Extract(err => throw new Exception(err.ToString()))
            .Evaluate();
    }

    /// <summary>
    /// Evaluates <paramref name="expression"/> with the default builtin function resolvers
    /// disabled, so that the evaluation falls through to the user-defined (Elm-source) kernel
    /// implementations. Used to assert that a builtin produces exactly the same value as the
    /// Elm implementation it short-circuits.
    /// </summary>
    public static PineValue EvaluateInModulesWithoutBuiltinsToPineValue(
        string expression,
        ElmInterpreter.Prepared prepared)
    {
        var (result, _) =
            ElmInterpreter.ParseAndInterpretWithCounters(
                expression,
                prepared,
                onApplication: null,
                enableDefaultBuiltins: false);

        return
            result
            .Extract(err => throw new Exception(err.ToString()))
            .Evaluate();
    }
}
