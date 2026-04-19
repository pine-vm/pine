using Pine.Core.CodeAnalysis;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// One frame of an Elm call stack: the function declaration that was active and the values
/// it was called with. Used inside <see cref="ElmInterpretationError"/> to describe where a
/// runtime error was raised and, when the interpreter detects infinite recursion, to describe
/// the detected cycle.
/// </summary>
public sealed record ElmCallStackFrame(
    DeclQualifiedName FunctionName,
    IReadOnlyList<ElmValue> Arguments)
{
    /// <inheritdoc/>
    public bool Equals(ElmCallStackFrame? other)
    {
        if (other is null)
            return false;

        if (!FunctionName.Equals(other.FunctionName))
            return false;

        if (Arguments.Count != other.Arguments.Count)
            return false;

        for (var i = 0; i < Arguments.Count; i++)
        {
            if (!Arguments[i].Equals(other.Arguments[i]))
                return false;
        }

        return true;
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hash = new System.HashCode();
        hash.Add(FunctionName);

        foreach (var argument in Arguments)
        {
            hash.Add(argument);
        }

        return hash.ToHashCode();
    }

    /// <summary>
    /// Renders the frame as an Elm-style function application: the fully-qualified function
    /// name followed by each argument's Elm-expression rendering, parenthesising arguments
    /// whose own rendering would be ambiguous in an application context.
    /// </summary>
    public override string ToString()
    {
        if (Arguments.Count is 0)
            return FunctionName.FullName;

        var sb = new System.Text.StringBuilder(FunctionName.FullName);

        foreach (var argument in Arguments)
        {
            var (rendered, needsParens) = ElmValue.RenderAsElmExpression(argument);

            sb.Append(' ');

            if (needsParens)
                sb.Append('(').Append(rendered).Append(')');

            else
                sb.Append(rendered);
        }

        return sb.ToString();
    }
}

/// <summary>
/// Structured representation of a runtime error raised while interpreting Elm code.
/// Returned on the error branch of every <see cref="ElmSyntaxInterpreter.Interpret(SyntaxModel.Expression, IReadOnlyDictionary{DeclQualifiedName, SyntaxModel.Declaration})"/>
/// overload. Carries the Elm call stack (<see cref="CallStack"/>) that was active at the moment
/// of failure, innermost first. Built-ins such as <c>Debug.todo</c> surface errors of this kind,
/// as do pattern-match failures, tag-arity mismatches, and detected infinite-recursion cycles.
/// </summary>
public sealed record ElmInterpretationError(
    string Message,
    IReadOnlyList<ElmCallStackFrame> CallStack)
{
    /// <inheritdoc/>
    public bool Equals(ElmInterpretationError? other)
    {
        if (other is null)
            return false;

        if (Message != other.Message)
            return false;

        if (CallStack.Count != other.CallStack.Count)
            return false;

        for (var i = 0; i < CallStack.Count; i++)
        {
            if (!CallStack[i].Equals(other.CallStack[i]))
                return false;
        }

        return true;
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hash = new System.HashCode();
        hash.Add(Message);

        foreach (var frame in CallStack)
        {
            hash.Add(frame);
        }

        return hash.ToHashCode();
    }

    /// <summary>
    /// Renders the error as a human-readable string: the <see cref="Message"/> on the first
    /// line, followed by the Elm call stack, innermost frame first, with one "<c>  at &lt;frame&gt;</c>"
    /// line per frame.
    /// </summary>
    public override string ToString()
    {
        if (CallStack.Count is 0)
            return Message;

        var sb = new System.Text.StringBuilder(Message);
        sb.Append("\nElm call stack (innermost first):");

        for (var i = 0; i < CallStack.Count; i++)
        {
            sb.Append("\n  at ").Append(CallStack[i]);
        }

        return sb.ToString();
    }
}

/// <summary>
/// Internal signalling mechanism used by the trampoline to unwind through arbitrary C# frames
/// (including user-supplied resolver callbacks) when an Elm-level runtime error occurs. Caught
/// at the public interpreter boundary and surfaced to callers as
/// <see cref="Result{ErrT, OkT}.Err"/> carrying an <see cref="ElmInterpretationError"/>.
/// </summary>
internal sealed class ElmInterpretationException : System.Exception
{
    public ElmInterpretationError Error { get; }

    public ElmInterpretationException(
        ElmInterpretationError error,
        System.Exception? innerException = null)
        : base(error.ToString(), innerException)
    {
        Error = error;
    }
}

/// <summary>
/// Evaluates <see cref="SyntaxModel.Expression"/> trees directly into <see cref="ElmValue"/> results.
/// This interpreter is intentionally small: it covers enough of the Elm surface to support
/// tests for literal evaluation, <c>Pine_builtin</c> / <c>Pine_kernel</c> forwarding, let blocks,
/// full function application, record type alias construction, and choice type tag construction.
/// </summary>
public partial class ElmSyntaxInterpreter
{
    /// <summary>
    /// Represents a fully-evaluated application whose function part and all arguments have been
    /// reduced to <see cref="ElmValue"/> instances.
    /// </summary>
    public record Application(
        DeclQualifiedName FunctionName,
        IReadOnlyList<ElmValue> Arguments,
        ApplicationContext Context);

    /// <summary>
    /// Contextual information accompanying an <see cref="Application"/>: the top-level declaration
    /// currently being evaluated and the local bindings visible at the call site.
    /// </summary>
    public record ApplicationContext(
        DeclQualifiedName CurrentTopLevel,
        IReadOnlyDictionary<string, ElmValue> LocalBindings);

    /// <summary>
    /// Outcome of attempting to resolve an <see cref="Application"/>: either a final value
    /// has been produced, or the interpreter should continue by evaluating a function body
    /// with parameters bound to argument values.
    /// </summary>
    public abstract record ApplicationResolution
    {
        /// <summary>A fully-resolved Elm value.</summary>
        public sealed record Resolved(ElmValue Value)
            : ApplicationResolution;

        /// <summary>
        /// The application resolved to a user-defined function. The interpreter will bind
        /// the function's argument patterns to the call's arguments and then evaluate the body.
        /// </summary>
        public sealed record ContinueWithFunction(
            SyntaxModel.FunctionImplementation Function)
            : ApplicationResolution;
    }

    /// <summary>
    /// Interprets <paramref name="rootExpression"/> using a resolver that combines
    /// <see cref="PineBuiltinResolver(Application)"/> with <see cref="UserDefinedResolver(Application, IReadOnlyDictionary{DeclQualifiedName, SyntaxModel.Declaration})"/>
    /// backed by the supplied <paramref name="declarations"/>.
    /// </summary>
    /// <returns>
    /// A <see cref="Result{ErrT, OkT}"/> containing the evaluated <see cref="ElmValue"/> on success,
    /// or an <see cref="ElmInterpretationError"/> describing the runtime failure (with its Elm
    /// call stack) on the error branch.
    /// </returns>
    public static Result<ElmInterpretationError, ElmValue> Interpret(
        SyntaxModel.Expression rootExpression,
        IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration> declarations)
    {
        var interpreter = new ElmSyntaxInterpreter();

        var combined =
            interpreter.CombineResolvers(
                [
                interpreter.PineBuiltinResolver,
                app => interpreter.UserDefinedResolver(app, declarations),
                ]);

        return Interpret(rootExpression, combined, BuildInfixOperatorMap(declarations));
    }

    /// <summary>
    /// Interprets <paramref name="rootExpression"/> using a caller-supplied <paramref name="resolveApplication"/>
    /// callback that is invoked whenever the interpreter encounters a function application or a bare
    /// function/value reference that is not resolved by a local binding.
    /// </summary>
    /// <returns>
    /// A <see cref="Result{ErrT, OkT}"/> containing the evaluated <see cref="ElmValue"/> on success,
    /// or an <see cref="ElmInterpretationError"/> describing the runtime failure (with its Elm
    /// call stack) on the error branch.
    /// </returns>
    public static Result<ElmInterpretationError, ElmValue> Interpret(
        SyntaxModel.Expression rootExpression,
        System.Func<Application, ApplicationResolution> resolveApplication) =>
        Interpret(rootExpression, resolveApplication, infixOperators: null);

    /// <summary>
    /// As <see cref="Interpret(SyntaxModel.Expression, System.Func{Application, ApplicationResolution})"/>,
    /// but additionally consults <paramref name="infixOperators"/> when translating
    /// <see cref="SyntaxModel.Expression.OperatorApplication"/> and
    /// <see cref="SyntaxModel.Expression.PrefixOperator"/> nodes to ordinary function
    /// applications. This lets the interpreter dispatch operators through the user-defined
    /// <c>infix</c> declarations from the Elm source code rather than a hard-coded table.
    /// </summary>
    public static Result<ElmInterpretationError, ElmValue> Interpret(
        SyntaxModel.Expression rootExpression,
        System.Func<Application, ApplicationResolution> resolveApplication,
        IReadOnlyDictionary<string, DeclQualifiedName>? infixOperators)
    {
        var context =
            new ApplicationContext(
                CurrentTopLevel: new DeclQualifiedName([], ""),
                LocalBindings: ImmutableDictionary<string, ElmValue>.Empty);

        return
            RunTrampolineAsResult(
                initialExpression: rootExpression,
                initialEnv: context,
                initialApplication: null,
                resolveApplication: resolveApplication,
                infixOperators: infixOperators);
    }

    /// <summary>
    /// Invokes the function identified by <paramref name="functionName"/> with the given
    /// <paramref name="arguments"/>, resolving against the supplied <paramref name="declarations"/>
    /// via the combined <see cref="PineBuiltinResolver(Application)"/> /
    /// <see cref="UserDefinedResolver(Application, IReadOnlyDictionary{DeclQualifiedName, SyntaxModel.Declaration})"/> resolver.
    /// </summary>
    /// <returns>
    /// A <see cref="Result{ErrT, OkT}"/> containing the evaluated <see cref="ElmValue"/> on success,
    /// or an <see cref="ElmInterpretationError"/> describing the runtime failure (with its Elm
    /// call stack) on the error branch.
    /// </returns>
    public static Result<ElmInterpretationError, ElmValue> Interpret(
        DeclQualifiedName functionName,
        IReadOnlyList<ElmValue> arguments,
        IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration> declarations)
    {
        var interpreter = new ElmSyntaxInterpreter();

        var combined =
            interpreter.CombineResolvers(
                [
                interpreter.PineBuiltinResolver,
                app => interpreter.UserDefinedResolver(app, declarations),
                ]);

        return Interpret(functionName, arguments, combined, BuildInfixOperatorMap(declarations));
    }

    /// <summary>
    /// Invokes the function identified by <paramref name="functionName"/> with the given
    /// <paramref name="arguments"/>, using <paramref name="resolveApplication"/> to resolve
    /// the call (and any further applications reached during evaluation of the body).
    /// </summary>
    /// <returns>
    /// A <see cref="Result{ErrT, OkT}"/> containing the evaluated <see cref="ElmValue"/> on success,
    /// or an <see cref="ElmInterpretationError"/> describing the runtime failure (with its Elm
    /// call stack) on the error branch.
    /// </returns>
    public static Result<ElmInterpretationError, ElmValue> Interpret(
        DeclQualifiedName functionName,
        IReadOnlyList<ElmValue> arguments,
        System.Func<Application, ApplicationResolution> resolveApplication) =>
        Interpret(functionName, arguments, resolveApplication, infixOperators: null);

    /// <summary>
    /// As <see cref="Interpret(DeclQualifiedName, IReadOnlyList{ElmValue}, System.Func{Application, ApplicationResolution})"/>,
    /// but additionally consults <paramref name="infixOperators"/> for source-defined infix
    /// operator dispatch.
    /// </summary>
    public static Result<ElmInterpretationError, ElmValue> Interpret(
        DeclQualifiedName functionName,
        IReadOnlyList<ElmValue> arguments,
        System.Func<Application, ApplicationResolution> resolveApplication,
        IReadOnlyDictionary<string, DeclQualifiedName>? infixOperators)
    {
        var rootContext =
            new ApplicationContext(
                CurrentTopLevel: functionName,
                LocalBindings: ImmutableDictionary<string, ElmValue>.Empty);

        var application =
            new Application(
                FunctionName: functionName,
                Arguments: arguments,
                Context: rootContext);

        return
            RunTrampolineAsResult(
                initialExpression: null,
                initialEnv: rootContext,
                initialApplication: application,
                resolveApplication: resolveApplication,
                infixOperators: infixOperators);
    }

    /// <summary>
    /// Parses <paramref name="rootExpressionText"/> as an Elm expression and interprets it
    /// against the supplied <paramref name="declarations"/> via the combined
    /// <see cref="PineBuiltinResolver(Application)"/> /
    /// <see cref="UserDefinedResolver(Application, IReadOnlyDictionary{DeclQualifiedName, SyntaxModel.Declaration})"/> resolver.
    /// </summary>
    /// <returns>
    /// A <see cref="Result{ErrT, OkT}"/> containing the evaluated <see cref="ElmValue"/> on success,
    /// or an <see cref="ElmInterpretationError"/> describing either a parse failure (with an empty
    /// call stack) or a runtime interpretation failure on the error branch.
    /// </returns>
    public static Result<ElmInterpretationError, ElmValue> ParseAndInterpret(
        string rootExpressionText,
        IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration> declarations)
    {
        var parseResult = ParseRootExpression(rootExpressionText);

        if (parseResult.IsErrOrNull() is { } parseErr)
        {
            return new ElmInterpretationError(parseErr, []);
        }

        if (parseResult.IsOkOrNull() is not { } rootExpression)
        {
            throw new System.NotImplementedException(
                "Unexpected parse result type: " + parseResult.GetType().FullName);
        }

        return Interpret(rootExpression, declarations);
    }

    /// <summary>
    /// Builds an operator-symbol → function-name map from any
    /// <see cref="SyntaxModel.Declaration.InfixDeclaration"/> entries in
    /// <paramref name="declarations"/>. The function name is materialized as a
    /// <see cref="DeclQualifiedName"/> in the same namespace as the infix declaration so
    /// the resolver dispatches it like any other top-level call.
    /// </summary>
    private static IReadOnlyDictionary<string, DeclQualifiedName> BuildInfixOperatorMap(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration> declarations)
    {
        var map = new Dictionary<string, DeclQualifiedName>();

        foreach (var (declName, declaration) in declarations)
        {
            if (declaration is SyntaxModel.Declaration.InfixDeclaration infixDecl)
            {
                map[infixDecl.Infix.Operator.Value] =
                    new DeclQualifiedName(
                        Namespaces: declName.Namespaces,
                        DeclName: infixDecl.Infix.FunctionName.Value);
            }
        }

        return map;
    }

    /// <summary>
    /// Parses <paramref name="rootExpressionText"/> as an Elm expression and interprets it
    /// using the caller-supplied <paramref name="resolveApplication"/> callback.
    /// </summary>
    /// <returns>
    /// A <see cref="Result{ErrT, OkT}"/> containing the evaluated <see cref="ElmValue"/> on success,
    /// or an <see cref="ElmInterpretationError"/> describing either a parse failure (with an empty
    /// call stack) or a runtime interpretation failure on the error branch.
    /// </returns>
    public static Result<ElmInterpretationError, ElmValue> ParseAndInterpret(
        string rootExpressionText,
        System.Func<Application, ApplicationResolution> resolveApplication)
    {
        var parseResult = ParseRootExpression(rootExpressionText);

        if (parseResult.IsErrOrNull() is { } parseErr)
        {
            return new ElmInterpretationError(parseErr, []);
        }

        if (parseResult.IsOkOrNull() is not { } rootExpression)
        {
            throw new System.NotImplementedException(
                "Unexpected parse result type: " + parseResult.GetType().FullName);
        }

        return Interpret(rootExpression, resolveApplication);
    }

    /// <summary>
    /// As <see cref="Interpret(DeclQualifiedName, IReadOnlyList{ElmValue}, IReadOnlyDictionary{DeclQualifiedName, SyntaxModel.Declaration})"/>,
    /// but additionally returns an <see cref="ElmSyntaxInterpreterPerformanceCounters"/>
    /// snapshot describing how much work the interpreter performed (trampoline iterations,
    /// direct vs function-value applications, Pine_builtin invocations).
    /// </summary>
    /// <returns>
    /// A tuple of the usual interpretation <see cref="Result{ErrT, OkT}"/> and the
    /// counters snapshot. Counters are populated even when the interpretation result is
    /// an error, reflecting the work performed up to the point of failure.
    /// </returns>
    public static (Result<ElmInterpretationError, ElmValue> Result, ElmSyntaxInterpreterPerformanceCounters Counters)
        InterpretWithCounters(
            DeclQualifiedName functionName,
            IReadOnlyList<ElmValue> arguments,
            IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration> declarations)
    {
        var interpreter = new ElmSyntaxInterpreter();
        var invocationCounter = new InvocationCounter();

        var combined =
            interpreter.CombineResolvers(
                [
                app => interpreter.PineBuiltinResolverCounting(app, invocationCounter),
                app => interpreter.UserDefinedResolver(app, declarations),
                ]);

        var rootContext =
            new ApplicationContext(
                CurrentTopLevel: functionName,
                LocalBindings: ImmutableDictionary<string, ElmValue>.Empty);

        var application =
            new Application(
                FunctionName: functionName,
                Arguments: arguments,
                Context: rootContext);

        var result =
            RunTrampolineAsResult(
                initialExpression: null,
                initialEnv: rootContext,
                initialApplication: application,
                resolveApplication: combined,
                infixOperators: BuildInfixOperatorMap(declarations),
                invocationCounter: invocationCounter);

        return (result, invocationCounter.ToReadOnly());
    }

    /// <summary>
    /// As <see cref="ParseAndInterpret(string, IReadOnlyDictionary{DeclQualifiedName, SyntaxModel.Declaration})"/>,
    /// but additionally returns an <see cref="ElmSyntaxInterpreterPerformanceCounters"/>
    /// snapshot describing how much work the interpreter performed.
    /// </summary>
    /// <returns>
    /// A tuple of the usual interpretation <see cref="Result{ErrT, OkT}"/> and the
    /// counters snapshot. Parse failures yield zero counters because the trampoline never ran.
    /// </returns>
    public static (Result<ElmInterpretationError, ElmValue> Result, ElmSyntaxInterpreterPerformanceCounters Counters)
        ParseAndInterpretWithCounters(
            string rootExpressionText,
            IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration> declarations)
    {
        var parseResult = ParseRootExpression(rootExpressionText);

        if (parseResult.IsErrOrNull() is { } parseErr)
        {
            return (new ElmInterpretationError(parseErr, []), default);
        }

        if (parseResult.IsOkOrNull() is not { } rootExpression)
        {
            throw new System.NotImplementedException(
                "Unexpected parse result type: " + parseResult.GetType().FullName);
        }

        var interpreter = new ElmSyntaxInterpreter();
        var invocationCounter = new InvocationCounter();

        var combined =
            interpreter.CombineResolvers(
                [
                app => interpreter.PineBuiltinResolverCounting(app, invocationCounter),
                app => interpreter.UserDefinedResolver(app, declarations),
                ]);

        var rootContext =
            new ApplicationContext(
                CurrentTopLevel: new DeclQualifiedName([], ""),
                LocalBindings: ImmutableDictionary<string, ElmValue>.Empty);

        var result =
            RunTrampolineAsResult(
                initialExpression: rootExpression,
                initialEnv: rootContext,
                initialApplication: null,
                resolveApplication: combined,
                infixOperators: BuildInfixOperatorMap(declarations),
                invocationCounter: invocationCounter);

        return (result, invocationCounter.ToReadOnly());
    }

    /// <summary>
    /// Parses <paramref name="rootExpressionText"/> as an Elm expression by wrapping it in a
    /// synthetic module and extracting the body of the sole function declaration.
    /// </summary>
    private static Result<string, SyntaxModel.Expression> ParseRootExpression(
        string rootExpressionText)
    {
        const string rootName = "pine_root_expression";

        var indented =
            string.Join(
                "\n",
                rootExpressionText
                .Replace("\r\n", "\n")
                .Split('\n')
                .Select(line => "    " + line));

        var syntheticModuleText =
            "module PineRootExpression exposing (..)\n\n\n"
            + rootName + " =\n"
            + indented
            + "\n";

        var parseResult = ElmSyntaxParser.ParseModuleText(syntheticModuleText);

        if (parseResult.IsErrOrNull() is { } parseErr)
        {
            return "Failed to parse root expression: " + parseErr;
        }

        if (parseResult.IsOkOrNull() is not { } parsedFile)
        {
            throw new System.NotImplementedException(
                "Unexpected parse result type: " + parseResult.GetType().FullName);
        }

        foreach (var declarationNode in parsedFile.Declarations)
        {
            if (declarationNode.Value is SyntaxModel.Declaration.FunctionDeclaration functionDeclaration
                && functionDeclaration.Function.Declaration.Value.Name.Value == rootName)
            {
                return functionDeclaration.Function.Declaration.Value.Expression.Value;
            }
        }

        return "Parsed module did not contain the synthetic root declaration.";
    }

    // ------------------------------------------------------------------------
    // CEK-style trampoline
    //
    // The original interpreter walked the AST by ordinary C# recursion through
    // EvaluateExpression / ResolveFunctionOrValue / DriveResolution. That bounded
    // the interpreter by the .NET thread stack and gave callers no way to
    // surface an Elm-level stack trace for runtime errors.
    //
    // This implementation evaluates expressions via an explicit loop that
    // manipulates:
    //
    //   * a (expression-to-evaluate, environment) state OR a (return-value) state,
    //   * a Stack<Kont> of pending "what to do once the current subexpression
    //     yields a value" continuations,
    //   * a Stack<DeclQualifiedName> recording the currently active Elm call
    //     frames (used to produce stack traces in ElmInterpretationException).
    //
    // Each entry of the Kont ADT corresponds to one "hole" in the pre-refactor
    // EvaluateExpression switch. Entering a user-defined function body pushes
    // a Kont.CallFrame so the trace can be captured at any point of evaluation.
    // When a call is in tail position (the current top of the continuation
    // stack is itself a CallFrame), we pop that old frame before pushing the
    // new one so tail-recursive Elm functions run in O(1) explicit-stack space.
    // ------------------------------------------------------------------------

    private abstract record Kont
    {
        /// <summary>Apply integer negation to the returned value.</summary>
        public sealed record Negate : Kont;

        /// <summary>
        /// Building a list or tuple: we are collecting <see cref="Elements"/> into
        /// <see cref="Accumulated"/>. <see cref="NextIndex"/> is the slot about to be
        /// filled by the currently-returned value.
        /// </summary>
        public sealed record BuildList(
            IReadOnlyList<SyntaxModel.Node<SyntaxModel.Expression>> Elements,
            int NextIndex,
            ElmValue[] Accumulated,
            ApplicationContext Env) : Kont;

        /// <summary>After evaluating the condition of an if-expression, pick a branch.</summary>
        public sealed record IfBranch(
            SyntaxModel.Node<SyntaxModel.Expression> ThenBranch,
            SyntaxModel.Node<SyntaxModel.Expression> ElseBranch,
            ApplicationContext Env) : Kont;

        /// <summary>
        /// Building a record literal: collecting successive field values into
        /// <see cref="Accumulated"/>. The field name at <see cref="NextIndex"/>
        /// is the one the currently-returned value is for.
        /// </summary>
        public sealed record BuildRecord(
            IReadOnlyList<SyntaxModel.RecordExprField> Fields,
            int NextIndex,
            (string FieldName, ElmValue Value)[] Accumulated,
            ApplicationContext Env) : Kont;

        /// <summary>
        /// Building the argument list for an application of <see cref="FunctionOrValue"/>.
        /// <see cref="NextIndex"/> is the argument slot the currently-returned value fills.
        /// When all arguments are collected, the application is resolved.
        /// </summary>
        public sealed record BuildArgs(
            SyntaxModel.Expression.FunctionOrValue FunctionOrValue,
            IReadOnlyList<SyntaxModel.Node<SyntaxModel.Expression>> Arguments,
            int NextIndex,
            ElmValue[] Accumulated,
            ApplicationContext Env) : Kont;

        /// <summary>
        /// Building the argument list for an application whose function position is an arbitrary
        /// expression (not a bare <see cref="SyntaxModel.Expression.FunctionOrValue"/> name) — for
        /// example a parenthesised lambda, an <c>if</c>-expression that yields a function, or a
        /// record-field access. Behaves like <see cref="BuildArgs"/> for argument evaluation;
        /// once all arguments are collected, evaluation switches to <see cref="FunctionExpr"/>
        /// to obtain the callable value, and an <see cref="ApplyEvaluatedFunction"/> is pushed
        /// to perform the application once the function value is in hand.
        /// </summary>
        public sealed record BuildArgsForValue(
            SyntaxModel.Node<SyntaxModel.Expression> FunctionExpr,
            IReadOnlyList<SyntaxModel.Node<SyntaxModel.Expression>> Arguments,
            int NextIndex,
            ElmValue[] Accumulated,
            ApplicationContext Env) : Kont;

        /// <summary>
        /// After evaluating the function expression for a <see cref="BuildArgsForValue"/>
        /// application, apply the returned value (which must be an <see cref="ElmValue.ElmFunction"/>)
        /// to the previously-collected arguments.
        /// </summary>
        public sealed record ApplyEvaluatedFunction(
            IReadOnlyList<ElmValue> Arguments,
            ApplicationContext Env) : Kont;

        /// <summary>
        /// Inserted under a <see cref="CallFrame"/> when a function is over-applied: the call has
        /// been saturated at <see cref="CallFrame.Arguments"/> and once its body returns a value
        /// (which must itself be an <see cref="ElmValue.ElmFunction"/>) the <see cref="ExtraArgs"/>
        /// will be applied to it. Surfaces as the unifying mechanism for both
        /// <c>(f x) y z</c>-style chained applications and the case where a single application
        /// node's argument list spans the outer function's parameters and the inner closure's
        /// parameters.
        /// </summary>
        public sealed record AfterCall(
            IReadOnlyList<ElmValue> ExtraArgs,
            ApplicationContext Env) : Kont;

        /// <summary>
        /// After evaluating the expression for a <c>let</c>'s <c>LetFunction</c>
        /// declaration, bind the name to the returned value, then continue processing
        /// the remaining declarations and finally the body.
        /// </summary>
        public sealed record LetBindFunction(
            string BindingName,
            IReadOnlyList<SyntaxModel.Node<SyntaxModel.Expression.LetDeclaration>> Remaining,
            int NextIndex,
            Dictionary<string, ElmValue> Extended,
            SyntaxModel.Node<SyntaxModel.Expression> Body,
            ApplicationContext Outer) : Kont;

        /// <summary>
        /// After evaluating the expression of a <c>let</c>'s <c>LetDestructuring</c>
        /// declaration, bind the pattern to the returned value, then continue.
        /// </summary>
        public sealed record LetBindDestructure(
            SyntaxModel.Pattern Pattern,
            IReadOnlyList<SyntaxModel.Node<SyntaxModel.Expression.LetDeclaration>> Remaining,
            int NextIndex,
            Dictionary<string, ElmValue> Extended,
            SyntaxModel.Node<SyntaxModel.Expression> Body,
            ApplicationContext Outer) : Kont;

        /// <summary>
        /// Marker frame indicating that a call to a user-defined Elm function is
        /// currently in progress. Contributes one entry to the Elm call stack but
        /// performs no computation on return (the returned value simply bubbles up).
        /// <see cref="Arguments"/> holds the values the function was called with — used
        /// both to render runtime-error stack traces as Elm-style function applications
        /// and to detect infinite recursion (a repeated <c>(FunctionName, Arguments)</c>
        /// pair on the kont stack).
        /// </summary>
        public sealed record CallFrame(
            DeclQualifiedName FunctionName,
            IReadOnlyList<ElmValue> Arguments) : Kont;

        /// <summary>
        /// After evaluating the scrutinee of a <c>case … of</c> expression, try the
        /// arms in source order and evaluate the first one whose pattern matches the
        /// returned value. If no arm matches, a runtime error is raised.
        /// </summary>
        public sealed record MatchCase(
            IReadOnlyList<SyntaxModel.Case> Cases,
            ApplicationContext Env) : Kont;

        /// <summary>
        /// After evaluating the record-position expression of a record-access
        /// (<c>record.field</c>), look up <see cref="FieldName"/> on the returned
        /// value (which must be an <see cref="ElmValue.ElmRecord"/>).
        /// </summary>
        public sealed record AccessRecordField(
            string FieldName) : Kont;

        /// <summary>
        /// Building a record-update expression <c>{ record | f1 = e1, f2 = e2 }</c>.
        /// First the original record value is obtained (by evaluating its name as a
        /// <see cref="SyntaxModel.Expression.FunctionOrValue"/>); afterwards each
        /// field-update value expression is evaluated in source order. <see cref="OriginalRecord"/>
        /// is <c>null</c> while the original record is still being computed; once obtained it is
        /// captured here so the update field expressions can be collected. <see cref="NextIndex"/>
        /// is the field-update slot the currently-returned value fills (or <c>-1</c> while waiting
        /// for the original record).
        /// </summary>
        public sealed record BuildRecordUpdate(
            ElmValue.ElmRecord? OriginalRecord,
            IReadOnlyList<SyntaxModel.RecordExprField> Fields,
            int NextIndex,
            (string FieldName, ElmValue Value)[] Accumulated,
            ApplicationContext Env) : Kont;
    }

    /// <summary>
    /// Entry point of the trampoline. At most one of <paramref name="initialExpression"/>
    /// and <paramref name="initialApplication"/> is non-null:
    /// expression-evaluation starts from <paramref name="initialExpression"/>; direct-call
    /// entry points (<see cref="Interpret(DeclQualifiedName, IReadOnlyList{ElmValue}, System.Func{Application, ApplicationResolution})"/>)
    /// start by resolving <paramref name="initialApplication"/>.
    ///
    /// <see cref="RunTrampolineAsResult"/> wraps this as the boundary between the trampoline's
    /// exception-based internal error signalling and the public <see cref="Result{ErrT, OkT}"/>-based
    /// API: it catches <see cref="ElmInterpretationException"/> and converts it to
    /// <see cref="Result{ErrT, OkT}.Err"/>; any other exception (e.g.
    /// <see cref="System.NotImplementedException"/> for a genuine interpreter feature gap)
    /// propagates unchanged.
    /// </summary>
    private static Result<ElmInterpretationError, ElmValue> RunTrampolineAsResult(
        SyntaxModel.Expression? initialExpression,
        ApplicationContext initialEnv,
        Application? initialApplication,
        System.Func<Application, ApplicationResolution> resolveApplication,
        IReadOnlyDictionary<string, DeclQualifiedName>? infixOperators,
        InvocationCounter? invocationCounter = null)
    {
        try
        {
            return
                RunTrampoline(
                    initialExpression: initialExpression,
                    initialEnv: initialEnv,
                    initialApplication: initialApplication,
                    resolveApplication: resolveApplication,
                    infixOperators: infixOperators,
                    invocationCounter: invocationCounter);
        }
        catch (ElmInterpretationException interpretationException)
        {
            return interpretationException.Error;
        }
    }

    private static ElmValue RunTrampoline(
        SyntaxModel.Expression? initialExpression,
        ApplicationContext initialEnv,
        Application? initialApplication,
        System.Func<Application, ApplicationResolution> resolveApplication,
        IReadOnlyDictionary<string, DeclQualifiedName>? infixOperators,
        InvocationCounter? invocationCounter = null)
    {
        var kstack = new Stack<Kont>();
        invocationCounter ??= new InvocationCounter();

        // Either: (currentExpr, currentEnv) is the next thing to evaluate ("Eval" mode),
        // or currentValue holds the value about to be returned to the top kont ("Return" mode).
        SyntaxModel.Expression? currentExpr;
        ApplicationContext currentEnv;
        ElmValue? currentValue = null;

        if (initialApplication is not null)
        {
            // Direct-call entry: we start in an unusual state – no expression yet, we need to
            // resolve the top-level application first. Applying the initial application may
            // either immediately produce a value (Resolved) or switch us into evaluating a
            // function body (ContinueWithFunction, which pushes a CallFrame).
            switch (ApplyResolvedCall(
                application: initialApplication,
                resolveApplication: resolveApplication,
                kstack: kstack,
                invocationCounter: invocationCounter))
            {
                case ApplyCallOutcome.ResolvedValue resolvedValue:
                    return resolvedValue.Value;

                case ApplyCallOutcome.ContinueEvaluating continueEvaluating:
                    currentExpr = continueEvaluating.Expression;
                    currentEnv = continueEvaluating.Env;
                    break;

                default:
                    throw new System.InvalidOperationException(
                        "Unexpected ApplyCallOutcome shape.");
            }
        }
        else
        {
            if (initialExpression is null)
            {
                throw new System.ArgumentException(
                    "RunTrampoline requires either initialExpression or initialApplication.");
            }

            currentExpr = initialExpression;
            currentEnv = initialEnv;
        }

        while (true)
        {
            invocationCounter.InstructionLoopCount++;

            if (currentExpr is not null)
            {
                // ---- Eval mode ------------------------------------------------

                switch (currentExpr)
                {
                    case SyntaxModel.Expression.UnitExpr:
                        currentValue = ElmValue.ListInstance([]);
                        currentExpr = null;
                        break;

                    case SyntaxModel.Expression.Literal literal:
                        currentValue = ElmValue.StringInstance(literal.Value);
                        currentExpr = null;
                        break;

                    case SyntaxModel.Expression.CharLiteral charLiteral:
                        currentValue = ElmValue.CharInstance(charLiteral.Value);
                        currentExpr = null;
                        break;

                    case SyntaxModel.Expression.Integer integer:
                        currentValue = ElmValue.Integer(ParseIntegerLiteral(integer.LiteralText));
                        currentExpr = null;
                        break;

                    case SyntaxModel.Expression.ParenthesizedExpression parenthesized:
                        currentExpr = parenthesized.Expression.Value;
                        break;

                    case SyntaxModel.Expression.Negation negation:
                        kstack.Push(new Kont.Negate());
                        currentExpr = negation.Expression.Value;
                        break;

                    case SyntaxModel.Expression.ListExpr listExpr:
                        {
                            var nodes = listExpr.Elements.Nodes.ToList();

                            if (nodes.Count is 0)
                            {
                                currentValue = ElmValue.ListInstance([]);
                                currentExpr = null;
                                break;
                            }

                            var accumulated = new ElmValue[nodes.Count];

                            kstack.Push(
                                new Kont.BuildList(
                                    Elements: nodes,
                                    NextIndex: 0,
                                    Accumulated: accumulated,
                                    Env: currentEnv));

                            currentExpr = nodes[0].Value;
                            break;
                        }

                    case SyntaxModel.Expression.TupledExpression tupledExpression:
                        {
                            var nodes = tupledExpression.Elements.Nodes.ToList();

                            if (nodes.Count is 0)
                            {
                                currentValue = ElmValue.ListInstance([]);
                                currentExpr = null;
                                break;
                            }

                            var accumulated = new ElmValue[nodes.Count];

                            kstack.Push(
                                new Kont.BuildList(
                                    Elements: nodes,
                                    NextIndex: 0,
                                    Accumulated: accumulated,
                                    Env: currentEnv));

                            currentExpr = nodes[0].Value;
                            break;
                        }

                    case SyntaxModel.Expression.IfBlock ifBlock:
                        kstack.Push(
                            new Kont.IfBranch(
                                ThenBranch: ifBlock.ThenBlock,
                                ElseBranch: ifBlock.ElseBlock,
                                Env: currentEnv));

                        currentExpr = ifBlock.Condition.Value;
                        break;

                    case SyntaxModel.Expression.RecordExpr recordExpr:
                        {
                            var fields = recordExpr.Fields.Nodes.ToList();

                            if (fields.Count is 0)
                            {
                                currentValue = MakeElmRecord([]);
                                currentExpr = null;
                                break;
                            }

                            var accumulated = new (string, ElmValue)[fields.Count];

                            kstack.Push(
                                new Kont.BuildRecord(
                                    Fields: fields,
                                    NextIndex: 0,
                                    Accumulated: accumulated,
                                    Env: currentEnv));

                            currentExpr = fields[0].ValueExpr.Value;
                            break;
                        }

                    case SyntaxModel.Expression.FunctionOrValue functionOrValue:
                        {
                            // Bare name lookup. If it resolves to a local binding, return it directly;
                            // otherwise it's a nullary application which is handled by the same path
                            // as a full application with zero arguments.
                            if (functionOrValue.ModuleName.Count is 0
                                && currentEnv.LocalBindings.TryGetValue(functionOrValue.Name, out var localValue))
                            {
                                currentValue = localValue;
                                currentExpr = null;
                                break;
                            }

                            var outcome =
                                ApplyFunctionOrValue(
                                    functionOrValue: functionOrValue,
                                    arguments: [],
                                    env: currentEnv,
                                    resolveApplication: resolveApplication,
                                    kstack: kstack,
                                    invocationCounter: invocationCounter);

                            switch (outcome)
                            {
                                case ApplyCallOutcome.ResolvedValue resolvedValue:
                                    currentValue = resolvedValue.Value;
                                    currentExpr = null;
                                    break;

                                case ApplyCallOutcome.ContinueEvaluating continueEvaluating:
                                    currentExpr = continueEvaluating.Expression;
                                    currentEnv = continueEvaluating.Env;
                                    break;
                            }

                            break;
                        }

                    case SyntaxModel.Expression.Application application:
                        {
                            var functionExpression = UnwrapParentheses(application.Function.Value);

                            var args = application.Arguments;

                            if (args.Count is 0)
                            {
                                // Degenerate: an Application node with zero arguments. Reduce to
                                // evaluating the function expression in the current environment;
                                // if it happens to be a bare FunctionOrValue, the next iteration's
                                // FunctionOrValue branch handles local-binding lookup and resolver
                                // dispatch consistently.
                                currentExpr = functionExpression;
                                break;
                            }

                            var accumulated = new ElmValue[args.Count];

                            if (functionExpression is SyntaxModel.Expression.FunctionOrValue fnOrVal)
                            {
                                kstack.Push(
                                    new Kont.BuildArgs(
                                        FunctionOrValue: fnOrVal,
                                        Arguments: args,
                                        NextIndex: 0,
                                        Accumulated: accumulated,
                                        Env: currentEnv));
                            }
                            else
                            {
                                // Function position is an arbitrary expression (e.g. a lambda,
                                // an `if`, a record-field access). Evaluate the arguments first
                                // (matching the BuildArgs path's left-to-right argument order),
                                // then evaluate the function expression and apply its value.
                                kstack.Push(
                                    new Kont.BuildArgsForValue(
                                        FunctionExpr: application.Function,
                                        Arguments: args,
                                        NextIndex: 0,
                                        Accumulated: accumulated,
                                        Env: currentEnv));
                            }

                            currentExpr = args[0].Value;
                            break;
                        }

                    case SyntaxModel.Expression.LambdaExpression lambdaExpression:
                        {
                            // A lambda evaluates to a closure value capturing the current local
                            // bindings (so any free variables in the body resolve to their values
                            // at the moment the lambda was reached, not at the moment the closure
                            // is finally invoked) and the surrounding top-level declaration name
                            // (so post-invocation stack traces remain coherent).
                            var lambda = lambdaExpression.Lambda;

                            currentValue =
                                new ElmValue.ElmFunction(
                                    Source: new ElmValue.ElmFunction.SourceRef.Lambda(lambda),
                                    ParameterCount: lambda.Arguments.Count,
                                    ArgumentsAlreadyCollected: [],
                                    CapturedBindings: SnapshotBindings(currentEnv.LocalBindings),
                                    CapturedTopLevel: currentEnv.CurrentTopLevel);

                            currentExpr = null;
                            break;
                        }

                    case SyntaxModel.Expression.LetExpression letExpression:
                        {
                            var decls = letExpression.Value.Declarations;
                            var extended = new Dictionary<string, ElmValue>(currentEnv.LocalBindings);

                            if (decls.Count is 0)
                            {
                                currentEnv =
                                    new ApplicationContext(
                                        CurrentTopLevel: currentEnv.CurrentTopLevel,
                                        LocalBindings: extended);

                                currentExpr = letExpression.Value.Expression.Value;
                                break;
                            }

                            // Start evaluating the first let declaration's right-hand side.
                            (currentExpr, currentEnv) =
                                BeginNextLetDecl(
                                    decls: decls,
                                    nextIndex: 0,
                                    extended: extended,
                                    body: letExpression.Value.Expression,
                                    outerEnv: currentEnv,
                                    kstack: kstack);

                            break;
                        }

                    case SyntaxModel.Expression.CaseExpression caseExpression:
                        kstack.Push(
                            new Kont.MatchCase(
                                Cases: caseExpression.CaseBlock.Cases,
                                Env: currentEnv));

                        currentExpr = caseExpression.CaseBlock.Expression.Value;
                        break;

                    case SyntaxModel.Expression.RecordAccess recordAccess:
                        {
                            // Push a continuation that, once the record-position expression
                            // produces a value, looks up the requested field on it.
                            kstack.Push(
                                new Kont.AccessRecordField(
                                    FieldName: recordAccess.FieldName.Value));

                            currentExpr = recordAccess.Record.Value;
                            break;
                        }

                    case SyntaxModel.Expression.RecordAccessFunction recordAccessFunction:
                        {
                            // A record-access function literal `.field` evaluates to a
                            // single-parameter closure `\r -> r.field`. We synthesise that
                            // closure as a lambda whose body is a RecordAccess node, so the
                            // existing closure-application machinery handles invocation.
                            // The parser includes the leading dot in FunctionName; strip it
                            // to obtain the bare field name.
                            var fieldName = recordAccessFunction.FunctionName;
                            if (fieldName.Length > 0 && fieldName[0] == '.')
                                fieldName = fieldName[1..];

                            currentValue =
                                MakeRecordAccessClosure(
                                    fieldName: fieldName,
                                    capturedTopLevel: currentEnv.CurrentTopLevel);

                            currentExpr = null;
                            break;
                        }

                    case SyntaxModel.Expression.RecordUpdateExpression recordUpdate:
                        {
                            var fields = recordUpdate.Fields.Nodes.ToList();

                            if (fields.Count is 0)
                            {
                                throw MakeRuntimeError(
                                    "Record update with no field assignments.",
                                    kstack);
                            }

                            var accumulated = new (string, ElmValue)[fields.Count];

                            // First evaluate the record name as a bare FunctionOrValue so the
                            // existing local-binding / top-level lookup machinery applies.
                            var recordNameExpr =
                                new SyntaxModel.Expression.FunctionOrValue(
                                    ModuleName: [],
                                    Name: recordUpdate.RecordName.Value);

                            kstack.Push(
                                new Kont.BuildRecordUpdate(
                                    OriginalRecord: null,
                                    Fields: fields,
                                    NextIndex: -1,
                                    Accumulated: accumulated,
                                    Env: currentEnv));

                            currentExpr = recordNameExpr;
                            break;
                        }

                    case SyntaxModel.Expression.OperatorApplication operatorApplication:
                        {
                            // Translate the operator into the corresponding source-defined
                            // function call. Look up the operator symbol in the
                            // <c>infix … = funcName</c> map collected from the user's
                            // declarations — we deliberately do NOT consult any hard-coded
                            // table here, so the actual implementation referenced by the
                            // infix declaration is what runs.
                            var opSymbol = operatorApplication.Operator.Value;

                            if (infixOperators is null
                                || !infixOperators.TryGetValue(opSymbol, out var opFunctionName))
                            {
                                throw MakeRuntimeError(
                                    "No infix declaration found for operator '" + opSymbol + "'.",
                                    kstack);
                            }

                            var operatorRange = operatorApplication.Operator.Range;

                            var functionRefNode =
                                new SyntaxModel.Node<SyntaxModel.Expression>(
                                    Range: operatorRange,
                                    Value: new SyntaxModel.Expression.FunctionOrValue(
                                        ModuleName: opFunctionName.Namespaces,
                                        Name: opFunctionName.DeclName));

                            currentExpr =
                                new SyntaxModel.Expression.Application(
                                    Function: functionRefNode,
                                    Arguments: [operatorApplication.Left, operatorApplication.Right]);
                            break;
                        }

                    case SyntaxModel.Expression.PrefixOperator prefixOperator:
                        {
                            // The `(<op>)` prefix form denotes the function value referenced
                            // by the operator's infix declaration. Re-route it through the
                            // bare FunctionOrValue path which, for an unsaturated reference,
                            // returns an <see cref="ElmValue.ElmFunction"/> closure usable as
                            // a higher-order argument or partially applied at the call site.
                            var opSymbol = prefixOperator.Operator;

                            if (infixOperators is null
                                || !infixOperators.TryGetValue(opSymbol, out var opFunctionName))
                            {
                                throw MakeRuntimeError(
                                    "No infix declaration found for operator '" + opSymbol + "'.",
                                    kstack);
                            }

                            currentExpr =
                                new SyntaxModel.Expression.FunctionOrValue(
                                    ModuleName: opFunctionName.Namespaces,
                                    Name: opFunctionName.DeclName);
                            break;
                        }

                    default:
                        throw new System.NotImplementedException(
                            "Expression type not implemented: " + currentExpr.GetType().FullName);
                }
            }
            else
            {
                // ---- Return mode ---------------------------------------------

                if (kstack.Count is 0)
                {
                    if (currentValue is null)
                    {
                        throw new System.InvalidOperationException(
                            "Trampoline terminated without a value.");
                    }

                    return currentValue;
                }

                var top = kstack.Pop();
                var value = currentValue!;

                switch (top)
                {
                    case Kont.Negate:
                        {
                            if (value is ElmValue.ElmInteger elmInteger)
                            {
                                currentValue = ElmValue.Integer(-elmInteger.Value);
                                break;
                            }

                            throw MakeRuntimeError(
                                "Negation is only implemented for integer values, got "
                                + value.GetType().FullName,
                                kstack);
                        }

                    case Kont.BuildList buildList:
                        {
                            buildList.Accumulated[buildList.NextIndex] = value;
                            var next = buildList.NextIndex + 1;

                            if (next == buildList.Elements.Count)
                            {
                                currentValue = ElmValue.ListInstance(buildList.Accumulated);
                                break;
                            }

                            kstack.Push(buildList with { NextIndex = next });
                            currentExpr = buildList.Elements[next].Value;
                            currentEnv = buildList.Env;
                            currentValue = null;
                            break;
                        }

                    case Kont.IfBranch ifBranch:
                        {
                            var conditionIsTrue =
                                value is ElmValue.ElmTag tag && tag.TagName is "True";

                            currentExpr =
                                conditionIsTrue
                                ?
                                ifBranch.ThenBranch.Value
                                :
                                ifBranch.ElseBranch.Value;

                            currentEnv = ifBranch.Env;
                            currentValue = null;
                            break;
                        }

                    case Kont.BuildRecord buildRecord:
                        {
                            var fieldName = buildRecord.Fields[buildRecord.NextIndex].FieldName.Value;
                            buildRecord.Accumulated[buildRecord.NextIndex] = (fieldName, value);

                            var next = buildRecord.NextIndex + 1;

                            if (next == buildRecord.Fields.Count)
                            {
                                currentValue = MakeElmRecord(buildRecord.Accumulated);
                                break;
                            }

                            kstack.Push(buildRecord with { NextIndex = next });
                            currentExpr = buildRecord.Fields[next].ValueExpr.Value;
                            currentEnv = buildRecord.Env;
                            currentValue = null;
                            break;
                        }

                    case Kont.BuildArgs buildArgs:
                        {
                            buildArgs.Accumulated[buildArgs.NextIndex] = value;
                            var next = buildArgs.NextIndex + 1;

                            if (next < buildArgs.Arguments.Count)
                            {
                                kstack.Push(buildArgs with { NextIndex = next });
                                currentExpr = buildArgs.Arguments[next].Value;
                                currentEnv = buildArgs.Env;
                                currentValue = null;
                                break;
                            }

                            // All arguments evaluated: dispatch the call.
                            //
                            // Unqualified names are first tried as local bindings: if the binding
                            // holds an ElmFunction value (a closure created by partial application
                            // or a lambda), the call is dispatched against that closure rather
                            // than the resolver. This is what makes a let-bound partial
                            // application like `let addTen = add 10 in addTen 32` apply against
                            // the local closure instead of trying to look up `addTen` as a
                            // top-level declaration.
                            if (buildArgs.FunctionOrValue.ModuleName.Count is 0
                                && buildArgs.Env.LocalBindings.TryGetValue(
                                    buildArgs.FunctionOrValue.Name,
                                    out var localFnValue))
                            {
                                var localOutcome =
                                    ApplyFunctionValue(
                                        functionValue: localFnValue,
                                        functionRenderForError: buildArgs.FunctionOrValue.Name,
                                        newArguments: buildArgs.Accumulated,
                                        callerEnv: buildArgs.Env,
                                        kstack: kstack,
                                        invocationCounter: invocationCounter);

                                switch (localOutcome)
                                {
                                    case ApplyCallOutcome.ResolvedValue resolvedValue:
                                        currentValue = resolvedValue.Value;
                                        currentExpr = null;
                                        break;

                                    case ApplyCallOutcome.ContinueEvaluating continueEvaluating:
                                        currentExpr = continueEvaluating.Expression;
                                        currentEnv = continueEvaluating.Env;
                                        currentValue = null;
                                        break;
                                }

                                break;
                            }

                            var outcome =
                                ApplyFunctionOrValue(
                                    functionOrValue: buildArgs.FunctionOrValue,
                                    arguments: buildArgs.Accumulated,
                                    env: buildArgs.Env,
                                    resolveApplication: resolveApplication,
                                    kstack: kstack,
                                    invocationCounter: invocationCounter);

                            switch (outcome)
                            {
                                case ApplyCallOutcome.ResolvedValue resolvedValue:
                                    currentValue = resolvedValue.Value;
                                    currentExpr = null;
                                    break;

                                case ApplyCallOutcome.ContinueEvaluating continueEvaluating:
                                    currentExpr = continueEvaluating.Expression;
                                    currentEnv = continueEvaluating.Env;
                                    currentValue = null;
                                    break;
                            }

                            break;
                        }

                    case Kont.BuildArgsForValue buildArgsForValue:
                        {
                            buildArgsForValue.Accumulated[buildArgsForValue.NextIndex] = value;
                            var next = buildArgsForValue.NextIndex + 1;

                            if (next < buildArgsForValue.Arguments.Count)
                            {
                                kstack.Push(buildArgsForValue with { NextIndex = next });
                                currentExpr = buildArgsForValue.Arguments[next].Value;
                                currentEnv = buildArgsForValue.Env;
                                currentValue = null;
                                break;
                            }

                            // All arguments evaluated. Now evaluate the function expression and
                            // apply its value via the ApplyEvaluatedFunction continuation.
                            kstack.Push(
                                new Kont.ApplyEvaluatedFunction(
                                    Arguments: buildArgsForValue.Accumulated,
                                    Env: buildArgsForValue.Env));

                            currentExpr = buildArgsForValue.FunctionExpr.Value;
                            currentEnv = buildArgsForValue.Env;
                            currentValue = null;
                            break;
                        }

                    case Kont.ApplyEvaluatedFunction applyEvaluatedFunction:
                        {
                            var applyOutcome =
                                ApplyFunctionValue(
                                    functionValue: value,
                                    functionRenderForError: null,
                                    newArguments: applyEvaluatedFunction.Arguments,
                                    callerEnv: applyEvaluatedFunction.Env,
                                    kstack: kstack,
                                    invocationCounter: invocationCounter);

                            switch (applyOutcome)
                            {
                                case ApplyCallOutcome.ResolvedValue resolvedValue:
                                    currentValue = resolvedValue.Value;
                                    currentExpr = null;
                                    break;

                                case ApplyCallOutcome.ContinueEvaluating continueEvaluating:
                                    currentExpr = continueEvaluating.Expression;
                                    currentEnv = continueEvaluating.Env;
                                    currentValue = null;
                                    break;
                            }

                            break;
                        }

                    case Kont.AfterCall afterCall:
                        {
                            // The body of a saturated call has produced a value; if extra args
                            // remain (over-application), apply them to the returned value.
                            var afterOutcome =
                                ApplyFunctionValue(
                                    functionValue: value,
                                    functionRenderForError: null,
                                    newArguments: afterCall.ExtraArgs,
                                    callerEnv: afterCall.Env,
                                    kstack: kstack,
                                    invocationCounter: invocationCounter);

                            switch (afterOutcome)
                            {
                                case ApplyCallOutcome.ResolvedValue resolvedValue:
                                    currentValue = resolvedValue.Value;
                                    currentExpr = null;
                                    break;

                                case ApplyCallOutcome.ContinueEvaluating continueEvaluating:
                                    currentExpr = continueEvaluating.Expression;
                                    currentEnv = continueEvaluating.Env;
                                    currentValue = null;
                                    break;
                            }

                            break;
                        }

                    case Kont.LetBindFunction letBindFunction:
                        {
                            letBindFunction.Extended[letBindFunction.BindingName] = value;

                            (currentExpr, currentEnv) =
                                BeginNextLetDecl(
                                    decls: letBindFunction.Remaining,
                                    nextIndex: letBindFunction.NextIndex + 1,
                                    extended: letBindFunction.Extended,
                                    body: letBindFunction.Body,
                                    outerEnv: letBindFunction.Outer,
                                    kstack: kstack);

                            currentValue = null;
                            break;
                        }

                    case Kont.LetBindDestructure letBindDestructure:
                        {
                            try
                            {
                                BindPattern(
                                    letBindDestructure.Pattern,
                                    value,
                                    letBindDestructure.Extended);
                            }
                            catch (System.Exception ex) when (ex is not ElmInterpretationException)
                            {
                                throw MakeRuntimeError(ex.Message, kstack, ex);
                            }

                            (currentExpr, currentEnv) =
                                BeginNextLetDecl(
                                    decls: letBindDestructure.Remaining,
                                    nextIndex: letBindDestructure.NextIndex + 1,
                                    extended: letBindDestructure.Extended,
                                    body: letBindDestructure.Body,
                                    outerEnv: letBindDestructure.Outer,
                                    kstack: kstack);

                            currentValue = null;
                            break;
                        }

                    case Kont.CallFrame:

                        // The call has produced its return value. Nothing to do: simply bubble
                        // the value up to the next continuation on the stack.
                        break;

                    case Kont.MatchCase matchCase:
                        {
                            var matched = false;

                            foreach (var caseNode in matchCase.Cases)
                            {
                                var bindings =
                                    new Dictionary<string, ElmValue>(matchCase.Env.LocalBindings);

                                if (TryMatchPattern(caseNode.Pattern.Value, value, bindings))
                                {
                                    currentEnv =
                                        new ApplicationContext(
                                            CurrentTopLevel: matchCase.Env.CurrentTopLevel,
                                            LocalBindings: bindings);

                                    currentExpr = caseNode.Expression.Value;
                                    currentValue = null;
                                    matched = true;
                                    break;
                                }
                            }

                            if (!matched)
                            {
                                var renderedValue =
                                    ElmValue.RenderAsElmExpression(value).expressionString;

                                throw MakeRuntimeError(
                                    "Case expression did not match any arm.\nScrutinee value: "
                                    + renderedValue,
                                    kstack);
                            }

                            break;
                        }

                    case Kont.AccessRecordField accessRecordField:
                        {
                            if (value is not ElmValue.ElmRecord recordValue)
                            {
                                var renderedValue =
                                    ElmValue.RenderAsElmExpression(value).expressionString;

                                throw MakeRuntimeError(
                                    "Record-access expression expects a record value, got: "
                                    + renderedValue,
                                    kstack);
                            }

                            var fieldValue = recordValue[accessRecordField.FieldName];

                            if (fieldValue is null)
                            {
                                throw MakeRuntimeError(
                                    "Record does not contain a field named '"
                                    + accessRecordField.FieldName + "'.",
                                    kstack);
                            }

                            currentValue = fieldValue;
                            break;
                        }

                    case Kont.BuildRecordUpdate buildRecordUpdate:
                        {
                            if (buildRecordUpdate.NextIndex < 0)
                            {
                                // Just received the original record value.
                                if (value is not ElmValue.ElmRecord originalRecord)
                                {
                                    var renderedValue =
                                        ElmValue.RenderAsElmExpression(value).expressionString;

                                    throw MakeRuntimeError(
                                        "Record-update expression expects a record value, got: "
                                        + renderedValue,
                                        kstack);
                                }

                                kstack.Push(
                                    buildRecordUpdate with
                                    {
                                        OriginalRecord = originalRecord,
                                        NextIndex = 0,
                                    });

                                currentExpr = buildRecordUpdate.Fields[0].ValueExpr.Value;
                                currentEnv = buildRecordUpdate.Env;
                                currentValue = null;
                                break;
                            }

                            var fieldName =
                                buildRecordUpdate.Fields[buildRecordUpdate.NextIndex].FieldName.Value;
                            buildRecordUpdate.Accumulated[buildRecordUpdate.NextIndex] =
                                (fieldName, value);

                            var nextIndex = buildRecordUpdate.NextIndex + 1;

                            if (nextIndex < buildRecordUpdate.Fields.Count)
                            {
                                kstack.Push(buildRecordUpdate with { NextIndex = nextIndex });
                                currentExpr =
                                    buildRecordUpdate.Fields[nextIndex].ValueExpr.Value;
                                currentEnv = buildRecordUpdate.Env;
                                currentValue = null;
                                break;
                            }

                            // All update values evaluated: build the new record by
                            // overlaying the updates onto the original record's fields.
                            // Elm record-update preserves the original field order (which
                            // for Elm records produced by this interpreter is alphabetical)
                            // and requires every updated field name to exist on the record.
                            var original = buildRecordUpdate.OriginalRecord!;

                            var updates =
                                new Dictionary<string, ElmValue>(buildRecordUpdate.Accumulated.Length);

                            foreach (var (updateFieldName, updateValue) in buildRecordUpdate.Accumulated)
                            {
                                updates[updateFieldName] = updateValue;
                            }

                            // Verify every update target field exists on the original record.
                            foreach (var updateFieldName in updates.Keys)
                            {
                                if (original[updateFieldName] is null)
                                {
                                    throw MakeRuntimeError(
                                        "Record-update references field '"
                                        + updateFieldName
                                        + "' which is not present on the record.",
                                        kstack);
                                }
                            }

                            var newFields =
                                new (string FieldName, ElmValue Value)[original.Fields.Count];

                            for (var i = 0; i < original.Fields.Count; i++)
                            {
                                var origField = original.Fields[i];

                                newFields[i] =
                                    updates.TryGetValue(origField.FieldName, out var newValue)
                                    ? (origField.FieldName, newValue)
                                    : origField;
                            }

                            currentValue = new ElmValue.ElmRecord(newFields);
                            break;
                        }

                    default:
                        throw new System.InvalidOperationException(
                            "Unknown continuation kind: " + top.GetType().FullName);
                }
            }
        }
    }

    /// <summary>
    /// Shape returned by <see cref="ApplyFunctionOrValue"/> / <see cref="ApplyResolvedCall"/>:
    /// either the call has already produced a fully-resolved value, or the trampoline should
    /// continue by evaluating an expression (the callee's body) in a new environment. In the
    /// latter case, a <see cref="Kont.CallFrame"/> has already been pushed on the kont stack.
    /// </summary>
    private abstract record ApplyCallOutcome
    {
        public sealed record ResolvedValue(ElmValue Value) : ApplyCallOutcome;

        public sealed record ContinueEvaluating(
            SyntaxModel.Expression Expression,
            ApplicationContext Env) : ApplyCallOutcome;
    }

    /// <summary>
    /// Periodic infinite-recursion detection interval: the interpreter scans the kont stack
    /// for a duplicated <c>(FunctionName, Arguments)</c> pair once every this many user-defined
    /// function invocations. A pure language like Elm cannot escape such a repeated pair, so
    /// it is reported as infinite recursion. The interval trades detection latency against
    /// scan cost; 1000 keeps per-call overhead negligible while catching real cycles well
    /// before they exhaust available memory.
    /// </summary>
    private const int InfiniteRecursionCheckInterval = 1000;

    /// <summary>
    /// Mutable counter threaded through <see cref="ApplyResolvedCall"/> and
    /// <see cref="ApplyFunctionOrValue"/> so all function-body entries can be counted against
    /// a single origin and the <see cref="InfiniteRecursionCheckInterval"/> check can fire.
    /// Also accumulates the high-level performance counters surfaced via
    /// <see cref="ElmSyntaxInterpreterPerformanceCounters"/>.
    /// </summary>
    private sealed class InvocationCounter
    {
        /// <summary>
        /// Number of user-defined function bodies entered. Used by the periodic
        /// infinite-recursion check.
        /// </summary>
        public int Count;

        /// <summary>Iterations of the trampoline <c>while(true)</c> loop.</summary>
        public long InstructionLoopCount;

        /// <summary>
        /// Number of direct (name-based) function applications dispatched through
        /// <see cref="ApplyFunctionOrValue"/>.
        /// </summary>
        public long DirectFunctionApplicationCount;

        /// <summary>
        /// Number of applications of an evaluated function value (closure) dispatched through
        /// <see cref="ApplyFunctionValue"/>.
        /// </summary>
        public long FunctionValueApplicationCount;

        /// <summary>
        /// Number of applications resolved by <see cref="PineBuiltinResolver(Application)"/>
        /// (i.e. forwarded to <see cref="KernelFunction"/>).
        /// </summary>
        public long PineBuiltinInvocationCount;

        /// <summary>
        /// Snapshots the accumulated counters as a public, immutable
        /// <see cref="ElmSyntaxInterpreterPerformanceCounters"/> value.
        /// </summary>
        public ElmSyntaxInterpreterPerformanceCounters ToReadOnly() =>
            new(
                InstructionLoopCount: InstructionLoopCount,
                DirectFunctionApplicationCount: DirectFunctionApplicationCount,
                FunctionValueApplicationCount: FunctionValueApplicationCount,
                PineBuiltinInvocationCount: PineBuiltinInvocationCount);
    }

    private static ApplyCallOutcome ApplyFunctionOrValue(
        SyntaxModel.Expression.FunctionOrValue functionOrValue,
        IReadOnlyList<ElmValue> arguments,
        ApplicationContext env,
        System.Func<Application, ApplicationResolution> resolveApplication,
        Stack<Kont> kstack,
        InvocationCounter invocationCounter)
    {
        invocationCounter.DirectFunctionApplicationCount++;

        var application =
            new Application(
                FunctionName:
                new DeclQualifiedName(
                    Namespaces: [.. functionOrValue.ModuleName],
                    DeclName: functionOrValue.Name),
                Arguments: arguments,
                Context: env);

        return ApplyResolvedCall(application, resolveApplication, kstack, invocationCounter);
    }

    private static ApplyCallOutcome ApplyResolvedCall(
        Application application,
        System.Func<Application, ApplicationResolution> resolveApplication,
        Stack<Kont> kstack,
        InvocationCounter invocationCounter)
    {
        ApplicationResolution resolution;

        try
        {
            resolution = resolveApplication(application);
        }
        catch (ElmInterpretationException)
        {
            throw;
        }
        catch (System.Exception ex)
        {
            throw MakeRuntimeError(
                "Failed to resolve application of '"
                + application.FunctionName.FullName
                + "': " + ex.Message,
                kstack,
                ex);
        }

        switch (resolution)
        {
            case ApplicationResolution.Resolved resolved:
                return new ApplyCallOutcome.ResolvedValue(resolved.Value);

            case ApplicationResolution.ContinueWithFunction continueWithFunction:
                {
                    var functionImpl = continueWithFunction.Function;
                    var expectedArity = functionImpl.Arguments.Count;
                    var providedCount = application.Arguments.Count;

                    // Under-application: build a closure value capturing the arguments supplied
                    // so far. Top-level user-defined functions don't lexically close over the
                    // call site's local bindings — their bodies only reference module-scope
                    // names, the parameter patterns, and let-bindings local to the body — so
                    // the captured environment is empty.
                    if (providedCount < expectedArity)
                    {
                        return
                            new ApplyCallOutcome.ResolvedValue(
                                new ElmValue.ElmFunction(
                                    Source:
                                        new ElmValue.ElmFunction.SourceRef.Declared(
                                            Name: application.FunctionName,
                                            Implementation: functionImpl),
                                    ParameterCount: expectedArity,
                                    ArgumentsAlreadyCollected: [.. application.Arguments],
                                    CapturedBindings:
                                        ImmutableDictionary<string, ElmValue>.Empty,
                                    CapturedTopLevel: application.FunctionName));
                    }

                    // Over-application: split the argument list at the declared arity. The first
                    // `expectedArity` arguments saturate this call; the remaining arguments are
                    // pushed as an AfterCall continuation and applied to whatever value the body
                    // returns (which must itself be a function).
                    IReadOnlyList<ElmValue> saturatingArgs;
                    IReadOnlyList<ElmValue>? extraArgs;

                    if (providedCount > expectedArity)
                    {
                        var saturating = new ElmValue[expectedArity];
                        var extra = new ElmValue[providedCount - expectedArity];

                        for (var i = 0; i < expectedArity; i++)
                            saturating[i] = application.Arguments[i];

                        for (var i = 0; i < extra.Length; i++)
                            extra[i] = application.Arguments[expectedArity + i];

                        saturatingArgs = saturating;
                        extraArgs = extra;
                    }
                    else
                    {
                        saturatingArgs = application.Arguments;
                        extraArgs = null;
                    }

                    var bindings = new Dictionary<string, ElmValue>();

                    for (var i = 0; i < expectedArity; i++)
                    {
                        try
                        {
                            BindPattern(
                                functionImpl.Arguments[i].Value,
                                saturatingArgs[i],
                                bindings);
                        }
                        catch (System.Exception ex) when (ex is not ElmInterpretationException)
                        {
                            throw MakeRuntimeError(ex.Message, kstack, ex);
                        }
                    }

                    var innerContext =
                        new ApplicationContext(
                            CurrentTopLevel: application.FunctionName,
                            LocalBindings: bindings);

                    if (extraArgs is not null)
                    {
                        // Push the over-application continuation BEFORE the CallFrame so that
                        // when the body finishes, the value bubbles past the CallFrame and into
                        // AfterCall, which then re-enters ApplyFunctionValue with the remaining
                        // arguments.
                        kstack.Push(
                            new Kont.AfterCall(
                                ExtraArgs: extraArgs,
                                Env: application.Context));
                    }

                    // Every call — including self-recursion — pushes a fresh CallFrame onto
                    // the kont stack. This lets the infinite-recursion detector later see a
                    // cycle as a literal repeated (FunctionName, Arguments) pair in the
                    // stack, matching the wording of the spec. The trade-off is that purely
                    // tail-recursive Elm functions use explicit-stack memory proportional to
                    // their depth rather than O(1); for the test-only interpreter in this
                    // assembly that is an acceptable cost.
                    kstack.Push(
                        new Kont.CallFrame(
                            application.FunctionName,
                            saturatingArgs));

                    invocationCounter.Count++;

                    if (invocationCounter.Count % InfiniteRecursionCheckInterval == 0)
                    {
                        CheckForInfiniteRecursion(kstack);
                    }

                    return
                        new ApplyCallOutcome.ContinueEvaluating(
                            Expression: functionImpl.Expression.Value,
                            Env: innerContext);
                }

            default:
                throw new System.NotImplementedException(
                    "Unknown application resolution type: " + resolution.GetType().FullName);
        }
    }

    /// <summary>
    /// Applies <paramref name="functionValue"/> — required to be an
    /// <see cref="ElmValue.ElmFunction"/> closure — to <paramref name="newArguments"/>. Combines
    /// the closure's previously-collected arguments with the new ones and dispatches:
    /// <list type="bullet">
    /// <item><b>Under-application</b> (combined &lt; arity): produces a new closure carrying the
    /// extended argument list and the same captured environment.</item>
    /// <item><b>Saturation</b> (combined == arity): binds the parameter patterns, restores the
    /// captured environment plus the bound arguments, pushes a <see cref="Kont.CallFrame"/> for
    /// the call, and returns a <see cref="ApplyCallOutcome.ContinueEvaluating"/> for the body.</item>
    /// <item><b>Over-application</b> (combined &gt; arity): saturates with the first <c>arity</c>
    /// arguments and pushes a <see cref="Kont.AfterCall"/> with the remainder so that the body's
    /// returned value is in turn applied to those leftover arguments.</item>
    /// </list>
    /// <paramref name="functionRenderForError"/> supplies a name for the runtime error message
    /// raised when <paramref name="functionValue"/> is not callable; it is <c>null</c> for
    /// callable values that have no syntactic name (the result of a sub-expression).
    /// </summary>
    private static ApplyCallOutcome ApplyFunctionValue(
        ElmValue functionValue,
        string? functionRenderForError,
        IReadOnlyList<ElmValue> newArguments,
        ApplicationContext callerEnv,
        Stack<Kont> kstack,
        InvocationCounter invocationCounter)
    {
        invocationCounter.FunctionValueApplicationCount++;

        if (functionValue is not ElmValue.ElmFunction closure)
        {
            if (newArguments.Count is 0)
            {
                // Nothing to apply — pass the value through. This case shouldn't be reachable
                // from the public eval paths but the explicit branch keeps the helper total.
                return new ApplyCallOutcome.ResolvedValue(functionValue);
            }

            var rendered =
                ElmValue.RenderAsElmExpression(functionValue).expressionString;

            var nameSuffix =
                functionRenderForError is null
                ? ""
                : " bound to '" + functionRenderForError + "'";

            throw MakeRuntimeError(
                "Cannot apply " + newArguments.Count
                + " argument" + (newArguments.Count is 1 ? "" : "s")
                + " to non-function value" + nameSuffix + ": "
                + rendered,
                kstack);
        }

        if (newArguments.Count is 0)
        {
            // No extra arguments supplied: the closure as-is is the result.
            return new ApplyCallOutcome.ResolvedValue(closure);
        }

        // Combine previously-collected and newly-supplied arguments.
        var alreadyCount = closure.ArgumentsAlreadyCollected.Count;
        var combinedCount = alreadyCount + newArguments.Count;
        var combined = new ElmValue[combinedCount];

        for (var i = 0; i < alreadyCount; i++)
            combined[i] = closure.ArgumentsAlreadyCollected[i];

        for (var i = 0; i < newArguments.Count; i++)
            combined[alreadyCount + i] = newArguments[i];

        var arity = closure.ParameterCount;

        if (combinedCount < arity)
        {
            // Still under-applied: produce a new closure.
            return
                new ApplyCallOutcome.ResolvedValue(
                    closure with { ArgumentsAlreadyCollected = combined });
        }

        // Either saturated or over-applied: split.
        ElmValue[] saturatingArgs;
        ElmValue[]? extraArgs;

        if (combinedCount > arity)
        {
            saturatingArgs = new ElmValue[arity];
            extraArgs = new ElmValue[combinedCount - arity];

            for (var i = 0; i < arity; i++)
                saturatingArgs[i] = combined[i];

            for (var i = 0; i < extraArgs.Length; i++)
                extraArgs[i] = combined[arity + i];
        }
        else
        {
            saturatingArgs = combined;
            extraArgs = null;
        }

        // Bind parameter patterns according to the closure's source.
        var bodyBindings = new Dictionary<string, ElmValue>(closure.CapturedBindings);

        IReadOnlyList<SyntaxModel.Node<SyntaxModel.Pattern>> parameterPatterns;
        SyntaxModel.Expression bodyExpression;
        DeclQualifiedName callFrameName;

        switch (closure.Source)
        {
            case ElmValue.ElmFunction.SourceRef.Declared declared:
                parameterPatterns = declared.Implementation.Arguments;
                bodyExpression = declared.Implementation.Expression.Value;
                callFrameName = declared.Name;
                break;

            case ElmValue.ElmFunction.SourceRef.Lambda lambdaSource:
                parameterPatterns = lambdaSource.LambdaStruct.Arguments;
                bodyExpression = lambdaSource.LambdaStruct.Expression.Value;
                callFrameName = SyntheticLambdaName(lambdaSource.LambdaStruct);
                break;

            default:
                throw new System.NotImplementedException(
                    "Unknown ElmFunction.SourceRef shape: " + closure.Source.GetType().FullName);
        }

        for (var i = 0; i < arity; i++)
        {
            try
            {
                BindPattern(
                    parameterPatterns[i].Value,
                    saturatingArgs[i],
                    bodyBindings);
            }
            catch (System.Exception ex) when (ex is not ElmInterpretationException)
            {
                throw MakeRuntimeError(ex.Message, kstack, ex);
            }
        }

        var innerContext =
            new ApplicationContext(
                CurrentTopLevel: closure.CapturedTopLevel,
                LocalBindings: bodyBindings);

        if (extraArgs is not null)
        {
            kstack.Push(
                new Kont.AfterCall(
                    ExtraArgs: extraArgs,
                    Env: callerEnv));
        }

        kstack.Push(
            new Kont.CallFrame(
                callFrameName,
                saturatingArgs));

        invocationCounter.Count++;

        if (invocationCounter.Count % InfiniteRecursionCheckInterval == 0)
        {
            CheckForInfiniteRecursion(kstack);
        }

        return
            new ApplyCallOutcome.ContinueEvaluating(
                Expression: bodyExpression,
                Env: innerContext);
    }

    /// <summary>
    /// Builds a synthetic <see cref="DeclQualifiedName"/> for a lambda's <see cref="Kont.CallFrame"/>
    /// using the lambda's <see cref="SyntaxModel.LambdaStruct.BackslashLocation"/>. The name has no
    /// namespace; its <see cref="DeclQualifiedName.DeclName"/> is shaped like
    /// <c>&lt;lambda@row:col&gt;</c> so stack traces remain readable and the infinite-recursion
    /// detector can still find a cycle of repeated (name, args) pairs.
    /// </summary>
    private static DeclQualifiedName SyntheticLambdaName(SyntaxModel.LambdaStruct lambda)
    {
        var loc = lambda.BackslashLocation;

        return
            new DeclQualifiedName(
                Namespaces: [],
                DeclName: "<lambda@" + loc.Row + ":" + loc.Column + ">");
    }

    /// <summary>
    /// Snapshots the local-binding environment as an immutable dictionary suitable for capture
    /// inside an <see cref="ElmValue.ElmFunction"/> closure. Avoids accidental aliasing of the
    /// caller's mutable <c>Dictionary&lt;string, ElmValue&gt;</c> instances.
    /// </summary>
    private static IReadOnlyDictionary<string, ElmValue> SnapshotBindings(
        IReadOnlyDictionary<string, ElmValue> bindings)
    {
        if (bindings.Count is 0)
            return ImmutableDictionary<string, ElmValue>.Empty;

        if (bindings is ImmutableDictionary<string, ElmValue> alreadyImmutable)
            return alreadyImmutable;

        return bindings.ToImmutableDictionary();
    }

    /// <summary>
    /// Walks the kont stack looking for two <see cref="Kont.CallFrame"/> entries with the same
    /// <see cref="Kont.CallFrame.FunctionName"/> and structurally equal
    /// <see cref="Kont.CallFrame.Arguments"/>. If such a pair is found, infinite recursion is
    /// raised as an <see cref="ElmInterpretationException"/> whose
    /// <see cref="ElmInterpretationError.CallStack"/> is the stack truncated to the first
    /// cycle: the innermost (just-pushed) frame at index 0, followed by the frames between
    /// it and the older duplicate, followed by the older duplicate itself.
    /// </summary>
    private static void CheckForInfiniteRecursion(Stack<Kont> kstack)
    {
        Kont.CallFrame? topCallFrame = null;

        foreach (var frame in kstack)
        {
            if (frame is Kont.CallFrame callFrame)
            {
                topCallFrame = callFrame;
                break;
            }
        }

        if (topCallFrame is null)
            return;

        var truncatedStack = new List<ElmCallStackFrame>();
        var seenTop = false;

        foreach (var frame in kstack)
        {
            if (frame is not Kont.CallFrame callFrame)
                continue;

            truncatedStack.Add(
                new ElmCallStackFrame(callFrame.FunctionName, callFrame.Arguments));

            if (!seenTop)
            {
                seenTop = true;
                continue;
            }

            if (callFrame.FunctionName.Equals(topCallFrame.FunctionName)
                && ArgumentListsEqual(callFrame.Arguments, topCallFrame.Arguments))
            {
                throw new ElmInterpretationException(
                    new ElmInterpretationError(
                        "Infinite recursion detected: the call stack contains a repeated (function, arguments) pair.",
                        truncatedStack));
            }
        }
    }

    private static bool ArgumentListsEqual(
        IReadOnlyList<ElmValue> left,
        IReadOnlyList<ElmValue> right)
    {
        if (left.Count != right.Count)
            return false;

        for (var i = 0; i < left.Count; i++)
        {
            if (!left[i].Equals(right[i]))
                return false;
        }

        return true;
    }

    /// <summary>
    /// Advances the <c>let</c>-declaration processing state: if there is another declaration at
    /// <paramref name="nextIndex"/>, pushes the appropriate <see cref="Kont.LetBindFunction"/> /
    /// <see cref="Kont.LetBindDestructure"/> and returns an (expression, env) pair for its
    /// right-hand side. Otherwise returns the pair for the body, evaluated under the extended
    /// environment.
    /// </summary>
    private static (SyntaxModel.Expression Expr, ApplicationContext Env) BeginNextLetDecl(
        IReadOnlyList<SyntaxModel.Node<SyntaxModel.Expression.LetDeclaration>> decls,
        int nextIndex,
        Dictionary<string, ElmValue> extended,
        SyntaxModel.Node<SyntaxModel.Expression> body,
        ApplicationContext outerEnv,
        Stack<Kont> kstack)
    {
        while (nextIndex < decls.Count)
        {
            var declNode = decls[nextIndex];

            switch (declNode.Value)
            {
                case SyntaxModel.Expression.LetDeclaration.LetFunction letFunction:
                    {
                        var functionImpl = letFunction.Function.Declaration.Value;

                        if (functionImpl.Arguments.Count is not 0)
                        {
                            throw MakeRuntimeError(
                                "Let bindings with parameters are not implemented yet.",
                                kstack);
                        }

                        var innerEnv =
                            new ApplicationContext(
                                CurrentTopLevel: outerEnv.CurrentTopLevel,
                                LocalBindings: new Dictionary<string, ElmValue>(extended));

                        kstack.Push(
                            new Kont.LetBindFunction(
                                BindingName: functionImpl.Name.Value,
                                Remaining: decls,
                                NextIndex: nextIndex,
                                Extended: extended,
                                Body: body,
                                Outer: outerEnv));

                        return (functionImpl.Expression.Value, innerEnv);
                    }

                case SyntaxModel.Expression.LetDeclaration.LetDestructuring letDestructuring:
                    {
                        var innerEnv =
                            new ApplicationContext(
                                CurrentTopLevel: outerEnv.CurrentTopLevel,
                                LocalBindings: new Dictionary<string, ElmValue>(extended));

                        kstack.Push(
                            new Kont.LetBindDestructure(
                                Pattern: letDestructuring.Pattern.Value,
                                Remaining: decls,
                                NextIndex: nextIndex,
                                Extended: extended,
                                Body: body,
                                Outer: outerEnv));

                        return (letDestructuring.Expression.Value, innerEnv);
                    }

                default:
                    throw new System.NotImplementedException(
                        "Let declaration type not implemented: " + declNode.Value.GetType().FullName);
            }
        }

        // All declarations processed: evaluate the body under the extended environment.
        var bodyEnv =
            new ApplicationContext(
                CurrentTopLevel: outerEnv.CurrentTopLevel,
                LocalBindings: extended);

        return (body.Value, bodyEnv);
    }

    /// <summary>
    /// Collects the currently-active Elm call frames from the kont stack, innermost first.
    /// Used when raising <see cref="ElmInterpretationException"/> /
    /// <see cref="ElmInterpretationError"/>.
    /// </summary>
    private static IReadOnlyList<ElmCallStackFrame> SnapshotCallStack(Stack<Kont> kstack)
    {
        var trace = new List<ElmCallStackFrame>();

        foreach (var frame in kstack)
        {
            if (frame is Kont.CallFrame callFrame)
            {
                trace.Add(new ElmCallStackFrame(callFrame.FunctionName, callFrame.Arguments));
            }
        }

        return trace;
    }

    /// <summary>
    /// Constructs an <see cref="ElmInterpretationException"/> carrying an
    /// <see cref="ElmInterpretationError"/> with the Elm call stack captured from the current
    /// state of <paramref name="kstack"/>. The trampoline uses this to unwind through arbitrary
    /// C# frames (including user resolver callbacks); the exception is caught at the public
    /// boundary and surfaced as <see cref="Result{ErrT, OkT}.Err"/>.
    /// </summary>
    private static ElmInterpretationException MakeRuntimeError(
        string message,
        Stack<Kont> kstack,
        System.Exception? innerException = null)
    {
        var error = new ElmInterpretationError(message, SnapshotCallStack(kstack));
        return new ElmInterpretationException(error, innerException);
    }


    /// <summary>
    /// Attempts to match <paramref name="value"/> against <paramref name="pattern"/>. On a
    /// successful match, any variable bindings introduced by the pattern (including those of
    /// nested patterns and <see cref="SyntaxModel.Pattern.AsPattern"/> aliases) are written into
    /// <paramref name="bindings"/> and the method returns <c>true</c>. On a non-match — including
    /// shape, length, tag-name or literal-equality mismatches — returns <c>false</c> so the
    /// caller can try the next case arm.
    /// <para />
    /// Used by the <c>case … of</c> trampoline continuation to test arms in source order.
    /// Mirrors the runtime semantics of patterns lowered to Pine: each arm reduces to a
    /// boolean condition that is checked at runtime, with no static type guarantees about the
    /// scrutinee's shape.
    /// </summary>
    private static bool TryMatchPattern(
        SyntaxModel.Pattern pattern,
        ElmValue value,
        IDictionary<string, ElmValue> bindings)
    {
        switch (pattern)
        {
            case SyntaxModel.Pattern.AllPattern:
                return true;

            case SyntaxModel.Pattern.VarPattern varPattern:
                bindings[varPattern.Name] = value;
                return true;

            case SyntaxModel.Pattern.UnitPattern:

                // The unit value is represented as an empty Elm list.
                return value is ElmValue.ElmList unitList && unitList.Items.Count is 0;

            case SyntaxModel.Pattern.ParenthesizedPattern parenthesized:
                return TryMatchPattern(parenthesized.Pattern.Value, value, bindings);

            case SyntaxModel.Pattern.AsPattern asPattern:
                {
                    if (!TryMatchPattern(asPattern.Pattern.Value, value, bindings))
                        return false;

                    bindings[asPattern.Name.Value] = value;
                    return true;
                }

            case SyntaxModel.Pattern.CharPattern charPattern:
                return value is ElmValue.ElmChar elmChar && elmChar.Value == charPattern.Value;

            case SyntaxModel.Pattern.StringPattern stringPattern:
                return value is ElmValue.ElmString elmString && elmString.Value == stringPattern.Value;

            case SyntaxModel.Pattern.IntPattern intPattern:
                return value is ElmValue.ElmInteger elmInt && elmInt.Value == intPattern.Value;

            case SyntaxModel.Pattern.HexPattern hexPattern:
                return value is ElmValue.ElmInteger elmHex && elmHex.Value == hexPattern.Value;

            case SyntaxModel.Pattern.TuplePattern tuplePattern:
                {
                    if (value is not ElmValue.ElmList tupleList)
                        return false;

                    var elementPatterns = tuplePattern.Elements;

                    if (elementPatterns.Count != tupleList.Items.Count)
                        return false;

                    for (var i = 0; i < elementPatterns.Count; i++)
                    {
                        if (!TryMatchPattern(elementPatterns[i].Value, tupleList.Items[i], bindings))
                            return false;
                    }

                    return true;
                }

            case SyntaxModel.Pattern.RecordPattern recordPattern:
                {
                    if (value is not ElmValue.ElmRecord recordValue)
                        return false;

                    foreach (var fieldNameNode in recordPattern.Fields.Nodes)
                    {
                        var fieldName = fieldNameNode.Value;
                        var fieldValue = recordValue[fieldName];

                        if (fieldValue is null)
                            return false;

                        bindings[fieldName] = fieldValue;
                    }

                    return true;
                }

            case SyntaxModel.Pattern.UnConsPattern unConsPattern:
                {
                    if (value is not ElmValue.ElmList consList)
                        return false;

                    if (consList.Items.Count is 0)
                        return false;

                    var head = consList.Items[0];
                    var tailItems = new ElmValue[consList.Items.Count - 1];

                    for (var i = 1; i < consList.Items.Count; i++)
                        tailItems[i - 1] = consList.Items[i];

                    var tail = ElmValue.ListInstance(tailItems);

                    if (!TryMatchPattern(unConsPattern.Head.Value, head, bindings))
                        return false;

                    return TryMatchPattern(unConsPattern.Tail.Value, tail, bindings);
                }

            case SyntaxModel.Pattern.ListPattern listPattern:
                {
                    if (value is not ElmValue.ElmList listValue)
                        return false;

                    var elementPatterns = listPattern.Elements;

                    if (elementPatterns.Count != listValue.Items.Count)
                        return false;

                    for (var i = 0; i < elementPatterns.Count; i++)
                    {
                        if (!TryMatchPattern(elementPatterns[i].Value, listValue.Items[i], bindings))
                            return false;
                    }

                    return true;
                }

            case SyntaxModel.Pattern.NamedPattern namedPattern:
                {
                    // Special case: the production `Basics.elm` represents the primitive
                    // `String` and `Elm_Float` types as ordinary tagged values, but the
                    // interpreter uses dedicated <see cref="ElmValue.ElmString"/> and
                    // <see cref="ElmValue.ElmFloat"/> variants. Allow the corresponding
                    // named patterns to match those variants by mapping back to the
                    // tag-style representation.
                    if (TryMatchPrimitiveTagShape(namedPattern, value, bindings) is { } primitiveResult)
                        return primitiveResult;

                    if (value is not ElmValue.ElmTag tagValue)
                        return false;

                    if (tagValue.TagName != namedPattern.Name.Name)
                        return false;

                    if (namedPattern.Arguments.Count != tagValue.Arguments.Count)
                        return false;

                    for (var i = 0; i < namedPattern.Arguments.Count; i++)
                    {
                        if (!TryMatchPattern(namedPattern.Arguments[i].Value, tagValue.Arguments[i], bindings))
                            return false;
                    }

                    return true;
                }

            default:
                throw new System.NotImplementedException(
                    "Matching pattern of type " + pattern.GetType().FullName + " is not implemented.");
        }
    }

    private static void BindPattern(
        SyntaxModel.Pattern pattern,
        ElmValue value,
        IDictionary<string, ElmValue> bindings)
    {
        switch (pattern)
        {
            case SyntaxModel.Pattern.VarPattern varPattern:
                bindings[varPattern.Name] = value;
                break;

            case SyntaxModel.Pattern.AllPattern:
                break;

            case SyntaxModel.Pattern.UnitPattern:
                break;

            case SyntaxModel.Pattern.ParenthesizedPattern parenthesized:
                BindPattern(parenthesized.Pattern.Value, value, bindings);
                break;

            case SyntaxModel.Pattern.AsPattern asPattern:
                BindPattern(asPattern.Pattern.Value, value, bindings);
                bindings[asPattern.Name.Value] = value;
                break;

            case SyntaxModel.Pattern.TuplePattern tuplePattern:
                {
                    if (value is not ElmValue.ElmList tupleList)
                    {
                        throw new System.InvalidOperationException(
                            "Cannot bind tuple pattern to value of type "
                            + value.GetType().FullName + ".");
                    }

                    var elementPatterns = tuplePattern.Elements;

                    if (elementPatterns.Count != tupleList.Items.Count)
                    {
                        throw new System.InvalidOperationException(
                            "Tuple pattern has "
                            + elementPatterns.Count
                            + " elements, but value has "
                            + tupleList.Items.Count + ".");
                    }

                    for (var i = 0; i < elementPatterns.Count; i++)
                    {
                        BindPattern(elementPatterns[i].Value, tupleList.Items[i], bindings);
                    }

                    break;
                }

            case SyntaxModel.Pattern.RecordPattern recordPattern:
                {
                    if (value is not ElmValue.ElmRecord recordValue)
                    {
                        throw new System.InvalidOperationException(
                            "Cannot bind record pattern to value of type "
                            + value.GetType().FullName + ".");
                    }

                    foreach (var fieldNameNode in recordPattern.Fields.Nodes)
                    {
                        var fieldName = fieldNameNode.Value;
                        var fieldValue = recordValue[fieldName];

                        if (fieldValue is null)
                        {
                            throw new System.InvalidOperationException(
                                "Record pattern references field '"
                                + fieldName
                                + "' which is not present on the value.");
                        }

                        bindings[fieldName] = fieldValue;
                    }

                    break;
                }

            case SyntaxModel.Pattern.NamedPattern namedPattern:
                {
                    // See the parallel branch in <see cref="TryMatchPattern"/> for context.
                    if (TryBindPrimitiveTagShape(namedPattern, value, bindings))
                        break;

                    if (value is not ElmValue.ElmTag tagValue)
                    {
                        throw new System.InvalidOperationException(
                            "Cannot bind named pattern '"
                            + namedPattern.Name.Name
                            + "' to value of type "
                            + value.GetType().FullName + ".");
                    }

                    if (tagValue.TagName != namedPattern.Name.Name)
                    {
                        throw new System.InvalidOperationException(
                            "Named pattern expects tag '"
                            + namedPattern.Name.Name
                            + "', but value has tag '"
                            + tagValue.TagName + "'.");
                    }

                    if (namedPattern.Arguments.Count != tagValue.Arguments.Count)
                    {
                        throw new System.InvalidOperationException(
                            "Named pattern '"
                            + namedPattern.Name.Name
                            + "' has "
                            + namedPattern.Arguments.Count
                            + " arguments, but tag value has "
                            + tagValue.Arguments.Count + ".");
                    }

                    for (var i = 0; i < namedPattern.Arguments.Count; i++)
                    {
                        BindPattern(namedPattern.Arguments[i].Value, tagValue.Arguments[i], bindings);
                    }

                    break;
                }

            default:
                throw new System.NotImplementedException(
                    "Binding pattern of type " + pattern.GetType().FullName + " is not implemented.");
        }
    }

    /// <summary>
    /// Implements the named-pattern matching shim for the primitive-but-tagged shapes
    /// (<c>String</c>, <c>Elm_Float</c>) that the bundled <c>Basics.elm</c> exposes as
    /// ordinary user-level constructors but which the interpreter materializes as the
    /// dedicated <see cref="ElmValue.ElmString"/> / <see cref="ElmValue.ElmFloat"/>
    /// variants.
    /// </summary>
    /// <returns>
    /// <c>true</c> on a successful match, <c>false</c> on a definite mismatch, or
    /// <c>null</c> when the pattern's tag name is not one of the recognised shapes
    /// (in which case the caller should continue with normal tag-based matching).
    /// </returns>
    private static bool? TryMatchPrimitiveTagShape(
        SyntaxModel.Pattern.NamedPattern namedPattern,
        ElmValue value,
        IDictionary<string, ElmValue> bindings)
    {
        switch (namedPattern.Name.Name)
        {
            case "String":
                {
                    if (value is not ElmValue.ElmString elmString)
                        return null;

                    if (namedPattern.Arguments.Count is not 1)
                        return false;

                    // The Pine representation of a string is a single blob holding the
                    // UTF-32 code points; expose that blob to the inner pattern so kernel
                    // calls like `Pine_kernel.skip [ offset, stringA ]` work as expected.
                    var blobBytes = CommonEncodings.StringEncoding.BlobValueFromString(elmString.Value).Bytes;
                    var inner = new ElmValue.ElmPineBlob(blobBytes);

                    return TryMatchPattern(namedPattern.Arguments[0].Value, inner, bindings);
                }

            case "Elm_Float":
                {
                    if (value is not ElmValue.ElmFloat elmFloat)
                        return null;

                    if (namedPattern.Arguments.Count is not 2)
                        return false;

                    if (!TryMatchPattern(
                            namedPattern.Arguments[0].Value,
                            ElmValue.Integer(elmFloat.Numerator),
                            bindings))
                    {
                        return false;
                    }

                    return TryMatchPattern(
                        namedPattern.Arguments[1].Value,
                        ElmValue.Integer(elmFloat.Denominator),
                        bindings);
                }

            default:
                return null;
        }
    }

    /// <summary>
    /// Companion of <see cref="TryMatchPrimitiveTagShape"/> for the irrefutable
    /// <see cref="BindPattern"/> path. Returns <c>true</c> when the pattern was
    /// recognised and bound, in which case the caller must skip its own
    /// tag-based binding logic.
    /// </summary>
    private static bool TryBindPrimitiveTagShape(
        SyntaxModel.Pattern.NamedPattern namedPattern,
        ElmValue value,
        IDictionary<string, ElmValue> bindings)
    {
        switch (namedPattern.Name.Name)
        {
            case "String" when value is ElmValue.ElmString elmString:
                {
                    if (namedPattern.Arguments.Count is not 1)
                    {
                        throw new System.InvalidOperationException(
                            "Named pattern 'String' against ElmString expects exactly one argument, but has "
                            + namedPattern.Arguments.Count + ".");
                    }

                    var blobBytes = CommonEncodings.StringEncoding.BlobValueFromString(elmString.Value).Bytes;

                    BindPattern(
                        namedPattern.Arguments[0].Value,
                        new ElmValue.ElmPineBlob(blobBytes),
                        bindings);

                    return true;
                }

            case "Elm_Float" when value is ElmValue.ElmFloat elmFloat:
                {
                    if (namedPattern.Arguments.Count is not 2)
                    {
                        throw new System.InvalidOperationException(
                            "Named pattern 'Elm_Float' against ElmFloat expects exactly two arguments, but has "
                            + namedPattern.Arguments.Count + ".");
                    }

                    BindPattern(
                        namedPattern.Arguments[0].Value,
                        ElmValue.Integer(elmFloat.Numerator),
                        bindings);

                    BindPattern(
                        namedPattern.Arguments[1].Value,
                        ElmValue.Integer(elmFloat.Denominator),
                        bindings);

                    return true;
                }

            default:
                return false;
        }
    }

    private static SyntaxModel.Expression UnwrapParentheses(SyntaxModel.Expression expression)
    {
        while (expression is SyntaxModel.Expression.ParenthesizedExpression parenthesized)
        {
            expression = parenthesized.Expression.Value;
        }

        return expression;
    }

    private static BigInteger ParseIntegerLiteral(string literalText)
    {
        var trimmed = literalText.Trim();

        var negative = trimmed.StartsWith('-');

        var absolute = negative ? trimmed[1..] : trimmed;

        BigInteger magnitude;

        if (absolute.StartsWith("0x", System.StringComparison.OrdinalIgnoreCase))
        {
            // Prepend '0' to the hex digits so BigInteger does not interpret a leading high-bit as sign.
            var hexDigits = "0" + absolute[2..];

            magnitude =
                BigInteger.Parse(
                    hexDigits,
                    System.Globalization.NumberStyles.HexNumber,
                    System.Globalization.CultureInfo.InvariantCulture);
        }
        else
        {
            magnitude =
                BigInteger.Parse(
                    absolute,
                    System.Globalization.NumberStyles.Integer,
                    System.Globalization.CultureInfo.InvariantCulture);
        }

        return negative ? -magnitude : magnitude;
    }

    private static ElmValue MakeElmRecord(IReadOnlyList<(string FieldName, ElmValue Value)> fields)
    {
        var sorted =
            fields
            .OrderBy(field => field.FieldName, System.StringComparer.Ordinal)
            .ToList();

        return new ElmValue.ElmRecord(sorted);
    }

    private static readonly FrozenSet<string> s_pineBuiltinModuleNamesDefault =
        FrozenSet.Create(["Pine_builtin", "Pine_kernel"]);

    /// <summary>
    /// Default <c>Pine_builtin</c> / <c>Pine_kernel</c> resolver: forwards applications whose
    /// module name is <c>Pine_builtin</c> or <c>Pine_kernel</c> to <see cref="KernelFunction.ApplyKernelFunctionGeneric(string, PineValue)"/>.
    /// </summary>
    public ApplicationResolution? PineBuiltinResolver(
        Application application) =>
        PineBuiltinResolver(application, s_pineBuiltinModuleNamesDefault);

    /// <summary>
    /// Resolver that forwards applications whose module name matches one of the supplied
    /// <paramref name="pineBuiltinPseudoModuleNames"/> to <see cref="KernelFunction.ApplyKernelFunctionGeneric(string, PineValue)"/>.
    /// Each Elm argument is encoded as a <see cref="PineValue"/> prior to the kernel invocation and the
    /// kernel's return value is decoded back to an <see cref="ElmValue"/>.
    /// </summary>
    public ApplicationResolution? PineBuiltinResolver(
        Application application,
        IReadOnlySet<string> pineBuiltinPseudoModuleNames)
    {
        if (application.FunctionName.Namespaces.Count is not 1)
            return null;

        var moduleName = application.FunctionName.Namespaces[0];

        if (!pineBuiltinPseudoModuleNames.Contains(moduleName))
            return null;

        var functionName = application.FunctionName.DeclName;

        // Elm kernel functions receive a single argument; when multiple arguments are supplied in Elm,
        // they are packed into a list and passed as that single argument.
        PineValue kernelInput;

        if (application.Arguments.Count is 1)
        {
            kernelInput = ElmValueEncoding.ElmValueAsPineValue(application.Arguments[0]);
        }
        else
        {
            var argumentValues = new PineValue[application.Arguments.Count];

            for (var i = 0; i < application.Arguments.Count; i++)
            {
                argumentValues[i] = ElmValueEncoding.ElmValueAsPineValue(application.Arguments[i]);
            }

            kernelInput = PineValue.List(argumentValues);
        }

        var resultPine = KernelFunction.ApplyKernelFunctionGeneric(functionName, kernelInput);

        var resultElm =
            ElmValueEncoding.PineValueAsElmValue(resultPine, null, null)
            .Extract(
                err =>
                throw new System.InvalidOperationException(
                    "Failed decoding kernel function '" + functionName + "' result as Elm value: " + err));

        return new ApplicationResolution.Resolved(resultElm);
    }

    /// <summary>
    /// As <see cref="PineBuiltinResolver(Application)"/>, but additionally increments
    /// <see cref="InvocationCounter.PineBuiltinInvocationCount"/> on every successful
    /// resolution. Used by the *<c>WithCounters</c> public entry points to surface the
    /// number of <c>Pine_builtin</c> / <c>Pine_kernel</c> invocations.
    /// </summary>
    private ApplicationResolution? PineBuiltinResolverCounting(
        Application application,
        InvocationCounter invocationCounter)
    {
        var resolution = PineBuiltinResolver(application);

        if (resolution is not null)
        {
            invocationCounter.PineBuiltinInvocationCount++;
        }

        return resolution;
    }

    /// <summary>
    /// Resolver that matches an application against the supplied user-level <paramref name="declarations"/>:
    /// top-level function declarations yield <see cref="ApplicationResolution.ContinueWithFunction"/>,
    /// record type alias constructors and choice type tag constructors with full application yield
    /// <see cref="ApplicationResolution.Resolved"/>. Returns <c>null</c> when no declaration matches.
    /// </summary>
    public ApplicationResolution? UserDefinedResolver(
        Application application,
        IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration> declarations)
    {
        var requestedName = application.FunctionName.DeclName;
        var requestedNamespaces = application.FunctionName.Namespaces;

        foreach (var (declName, declaration) in declarations)
        {
            // An unqualified reference (empty Namespaces) matches any declaration with the same DeclName;
            // a qualified reference must match the declaration's namespaces exactly.
            if (requestedNamespaces.Count is not 0
                && !NamespacesEqual(requestedNamespaces, declName.Namespaces))
            {
                continue;
            }

            switch (declaration)
            {
                case SyntaxModel.Declaration.FunctionDeclaration functionDeclaration
                when functionDeclaration.Function.Declaration.Value.Name.Value == requestedName:

                    return
                        new ApplicationResolution.ContinueWithFunction(
                            functionDeclaration.Function.Declaration.Value);

                case SyntaxModel.Declaration.AliasDeclaration aliasDeclaration
                when aliasDeclaration.TypeAlias.Name.Value == requestedName:

                    {
                        if (aliasDeclaration.TypeAlias.TypeAnnotation.Value
                            is SyntaxModel.TypeAnnotation.Record recordAnnotation)
                        {
                            var fieldNames =
                                recordAnnotation.RecordDefinition.Fields.Nodes
                                .Select(field => field.Value.FieldName.Value)
                                .ToList();

                            if (fieldNames.Count == application.Arguments.Count)
                            {
                                var fields = new List<(string FieldName, ElmValue Value)>(fieldNames.Count);

                                for (var i = 0; i < fieldNames.Count; i++)
                                {
                                    fields.Add((fieldNames[i], application.Arguments[i]));
                                }

                                return new ApplicationResolution.Resolved(MakeElmRecord(fields));
                            }

                            // Partial application of a record-type-alias constructor: synthesise a
                            // closure that, when fully applied, re-enters the resolver with all
                            // arguments and produces the record.
                            if (application.Arguments.Count < fieldNames.Count)
                            {
                                return
                                    new ApplicationResolution.Resolved(
                                        MakeConstructorClosure(
                                            qualifiedName: declName,
                                            arity: fieldNames.Count,
                                            alreadyCollected: application.Arguments));
                            }
                        }

                        break;
                    }

                case SyntaxModel.Declaration.ChoiceTypeDeclaration choiceTypeDeclaration:
                    {
                        foreach (var (_, constructorNode) in choiceTypeDeclaration.TypeDeclaration.Constructors)
                        {
                            if (constructorNode.Value.Name.Value != requestedName)
                                continue;

                            var ctorArity = constructorNode.Value.Arguments.Count;

                            if (ctorArity == application.Arguments.Count)
                            {
                                return
                                    new ApplicationResolution.Resolved(
                                        ElmValue.TagInstanceAsValue(requestedName, application.Arguments));
                            }

                            // Partial application of a choice-type tag constructor.
                            if (application.Arguments.Count < ctorArity)
                            {
                                return
                                    new ApplicationResolution.Resolved(
                                        MakeConstructorClosure(
                                            qualifiedName: declName with { DeclName = requestedName },
                                            arity: ctorArity,
                                            alreadyCollected: application.Arguments));
                            }
                        }

                        break;
                    }
            }
        }

        return null;
    }

    /// <summary>
    /// Builds an <see cref="ElmValue.ElmFunction"/> closure for a partially-applied
    /// type-alias-record or choice-type-tag constructor. The synthesised lambda's body
    /// re-references the constructor by name with one positional argument per parameter, so
    /// that when the closure is finally saturated the resolver re-enters with all arguments
    /// and produces the constructed value.
    /// </summary>
    private static ElmValue.ElmFunction MakeConstructorClosure(
        DeclQualifiedName qualifiedName,
        int arity,
        IReadOnlyList<ElmValue> alreadyCollected)
    {
        var defaultLocation = new SyntaxModel.Location(0, 0);
        var defaultRange = new SyntaxModel.Range(defaultLocation, defaultLocation);

        var parameterPatterns = new SyntaxModel.Node<SyntaxModel.Pattern>[arity];
        var argumentRefs = new SyntaxModel.Node<SyntaxModel.Expression>[arity];

        for (var i = 0; i < arity; i++)
        {
            var paramName = "__ctor_arg_" + i;

            parameterPatterns[i] =
                new SyntaxModel.Node<SyntaxModel.Pattern>(
                    defaultRange,
                    new SyntaxModel.Pattern.VarPattern(paramName));

            argumentRefs[i] =
                new SyntaxModel.Node<SyntaxModel.Expression>(
                    defaultRange,
                    new SyntaxModel.Expression.FunctionOrValue(
                        ModuleName: [],
                        Name: paramName));
        }

        var body =
            new SyntaxModel.Expression.Application(
                Function:
                    new SyntaxModel.Node<SyntaxModel.Expression>(
                        defaultRange,
                        new SyntaxModel.Expression.FunctionOrValue(
                            ModuleName: qualifiedName.Namespaces,
                            Name: qualifiedName.DeclName)),
                Arguments: argumentRefs);

        var lambda =
            new SyntaxModel.LambdaStruct(
                BackslashLocation: defaultLocation,
                Arguments: parameterPatterns,
                ArrowLocation: defaultLocation,
                Expression: new SyntaxModel.Node<SyntaxModel.Expression>(defaultRange, body));

        return
            new ElmValue.ElmFunction(
                Source: new ElmValue.ElmFunction.SourceRef.Lambda(lambda),
                ParameterCount: arity,
                ArgumentsAlreadyCollected: [.. alreadyCollected],
                CapturedBindings: ImmutableDictionary<string, ElmValue>.Empty,
                CapturedTopLevel: qualifiedName);
    }

    /// <summary>
    /// Builds an <see cref="ElmValue.ElmFunction"/> closure for a record-access function literal
    /// (<c>.field</c>): the synthesised lambda takes a single record argument and returns the
    /// requested field's value via a <see cref="SyntaxModel.Expression.RecordAccess"/> node.
    /// </summary>
    private static ElmValue.ElmFunction MakeRecordAccessClosure(
        string fieldName,
        DeclQualifiedName capturedTopLevel)
    {
        var defaultLocation = new SyntaxModel.Location(0, 0);
        var defaultRange = new SyntaxModel.Range(defaultLocation, defaultLocation);

        const string paramName = "__record_access_arg";

        var parameterPatterns = new SyntaxModel.Node<SyntaxModel.Pattern>[]
        {
            new(defaultRange, new SyntaxModel.Pattern.VarPattern(paramName)),
        };

        var recordRef =
            new SyntaxModel.Node<SyntaxModel.Expression>(
                defaultRange,
                new SyntaxModel.Expression.FunctionOrValue(
                    ModuleName: [],
                    Name: paramName));

        var body =
            new SyntaxModel.Expression.RecordAccess(
                Record: recordRef,
                FieldName: new SyntaxModel.Node<string>(defaultRange, fieldName));

        var lambda =
            new SyntaxModel.LambdaStruct(
                BackslashLocation: defaultLocation,
                Arguments: parameterPatterns,
                ArrowLocation: defaultLocation,
                Expression: new SyntaxModel.Node<SyntaxModel.Expression>(defaultRange, body));

        return
            new ElmValue.ElmFunction(
                Source: new ElmValue.ElmFunction.SourceRef.Lambda(lambda),
                ParameterCount: 1,
                ArgumentsAlreadyCollected: [],
                CapturedBindings: ImmutableDictionary<string, ElmValue>.Empty,
                CapturedTopLevel: capturedTopLevel);
    }

    private static bool NamespacesEqual(IReadOnlyList<string> left, IReadOnlyList<string> right)
    {
        if (left.Count != right.Count)
            return false;

        for (var i = 0; i < left.Count; i++)
        {
            if (left[i] != right[i])
                return false;
        }

        return true;
    }

    /// <summary>
    /// Combines multiple fallible resolvers into a single total resolver: each candidate is tried in order;
    /// the first non-null result wins. Throws if no candidate matches.
    /// </summary>
    public System.Func<Application, ApplicationResolution> CombineResolvers(
        IReadOnlyList<System.Func<Application, ApplicationResolution?>> resolvers)
    {
        return
            application =>
            {
                foreach (var resolver in resolvers)
                {
                    var resolution = resolver(application);

                    if (resolution is not null)
                        return resolution;
                }

                throw new System.InvalidOperationException(
                    "No resolver matched application of '" + application.FunctionName.FullName + "'.");
            };
    }
}
