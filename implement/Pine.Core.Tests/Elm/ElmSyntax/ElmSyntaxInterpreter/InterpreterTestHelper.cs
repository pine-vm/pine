using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;
using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;

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
            result[new DeclQualifiedName([], name)] = declaration;
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
        var declaration = declarations[new DeclQualifiedName([], functionName)];

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
    public static Result<ElmInterpretationError, ElmValue> Evaluate(string expression) =>
        ElmInterpreter.ParseAndInterpret(expression, s_emptyDeclarations);

    /// <summary>
    /// As <see cref="Evaluate(string)"/>, but throws a <see cref="Exception"/>
    /// if interpretation fails. Intended for tests whose scenario should never reach a
    /// runtime error; tests that want to assert the error itself should use the
    /// <see cref="Result{ErrT, OkT}"/>-returning <see cref="Evaluate(string)"/> variant.
    /// </summary>
    public static ElmValue EvaluateOrCrash(string expression) =>
        Evaluate(expression)
        .Extract(err => throw new Exception(err.ToString()));

    /// <summary>
    /// Parses <paramref name="elmModuleText"/> for its top-level declarations and then parses
    /// and interprets <paramref name="expression"/> against those declarations.
    /// Returns the interpreter <see cref="Result{ErrT, OkT}"/>; use
    /// <see cref="EvaluateInModuleOrCrash(string, string)"/> when the test only cares about
    /// the success value.
    /// </summary>
    public static Result<ElmInterpretationError, ElmValue> EvaluateInModule(
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
    public static ElmValue EvaluateInModuleOrCrash(
        string expression,
        string elmModuleText) =>
        EvaluateInModule(expression, elmModuleText)
        .Extract(err => throw new Exception(err.ToString()));

    /// <summary>
    /// As <see cref="EvaluateOrCrash(string)"/>, but additionally renders the resulting value
    /// to its Elm-expression text via <see cref="ElmValue.RenderAsElmExpression(ElmValue)"/>.
    /// Use for snapshot-style assertions on composed values (records, tuples, lists, tag
    /// instances) where comparing the rendered text reads more clearly than constructing the
    /// expected value out of nested <see cref="ElmValue"/> factory calls.
    /// </summary>
    public static string EvaluateOrCrashRendered(string expression) =>
        ElmValue.RenderAsElmExpression(EvaluateOrCrash(expression)).expressionString;

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
        ElmValue.RenderAsElmExpression(
            EvaluateInModuleOrCrash(expression, elmModuleText))
        .expressionString;
}
