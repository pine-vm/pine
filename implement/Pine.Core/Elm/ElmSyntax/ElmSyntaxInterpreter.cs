using Microsoft.CodeAnalysis;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Internal;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// One frame of an Elm call stack: the function declaration that was active and the values
/// it was called with. Used inside <see cref="ElmInterpretationError"/> to describe where a
/// runtime error was raised and, when the interpreter detects infinite recursion, to describe
/// the detected cycle.
/// </summary>
public sealed record ElmCallStackFrame(
    DeclQualifiedName FunctionName,
    IReadOnlyList<PineValueInProcess> Arguments)
{
    /// <summary>
    /// Default upper bound (in characters) applied to each individual argument rendering
    /// when a frame is rendered as part of a stack trace. Argument renderings longer than
    /// this are truncated (see <see cref="Render(int)"/>) so that a single large value (for
    /// example a whole source file passed as a <c>String</c>) does not flood the trace, while
    /// still surfacing enough of the value — and its total length — to be useful for debugging.
    /// </summary>
    public const int DefaultArgumentRenderLengthLimit = 400;

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
            if (!ElmSyntaxInterpreter.ValuesEqualInProcess(Arguments[i], other.Arguments[i]))
                return false;
        }

        return true;
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        // The in-process arguments have no structural hash that is consistent with
        // ValuesEqualInProcess (PineValueInProcess uses reference identity), so the argument
        // count is the strongest structural component that keeps equal frames in the same bucket.
        var hash = new System.HashCode();
        hash.Add(FunctionName);
        hash.Add(Arguments.Count);

        return hash.ToHashCode();
    }

    /// <summary>
    /// Renders the frame as an Elm-style function application: the fully-qualified function
    /// name followed by each argument's Elm-expression rendering, parenthesising arguments
    /// whose own rendering would be ambiguous in an application context. Each argument's
    /// rendering is truncated to <see cref="DefaultArgumentRenderLengthLimit"/> characters; use
    /// <see cref="Render(int)"/> to configure a different limit.
    /// </summary>
    public override string ToString()
    {
        return Render(DefaultArgumentRenderLengthLimit);
    }

    /// <summary>
    /// Renders the frame as an Elm-style function application (see <see cref="ToString"/>),
    /// truncating each individual argument rendering to at most
    /// <paramref name="argumentRenderLengthLimit"/> characters. A negative limit disables
    /// truncation and renders every argument in full. When an argument rendering is truncated,
    /// a suffix recording the value's full character length is appended so that the information
    /// most useful for debugging (the value's prefix and its overall size) is retained.
    /// </summary>
    /// <param name="argumentRenderLengthLimit">
    /// Maximum number of characters to keep from each argument's rendering. Negative to keep all.
    /// </param>
    public string Render(int argumentRenderLengthLimit)
    {
        if (Arguments.Count is 0)
            return FunctionName.FullName;

        var sb = new System.Text.StringBuilder(FunctionName.FullName);

        foreach (var argument in Arguments)
        {
            var (rendered, needsParens) = ElmSyntaxInterpreter.RenderArgumentForError(argument);

            var truncated = TruncateArgumentRendering(rendered, argumentRenderLengthLimit);

            sb.Append(' ');

            if (needsParens)
                sb.Append('(').Append(truncated).Append(')');

            else
                sb.Append(truncated);
        }

        return sb.ToString();
    }

    /// <summary>
    /// Truncates <paramref name="rendered"/> to at most <paramref name="limit"/> characters,
    /// appending a marker that records the value's full length. Returns the input unchanged when
    /// <paramref name="limit"/> is negative or the rendering already fits. The cut is adjusted so
    /// that it never splits a UTF-16 surrogate pair.
    /// </summary>
    internal static string TruncateArgumentRendering(string rendered, int limit)
    {
        if (limit < 0 || rendered.Length <= limit)
            return rendered;

        var cut = limit;

        // Avoid splitting a surrogate pair at the cut boundary.
        if (cut > 0 && char.IsHighSurrogate(rendered[cut - 1]))
            cut--;

        return
            rendered[..cut]
            + "…⟨truncated, " + rendered.Length.ToString() + " chars total⟩";
    }
}

/// <summary>
/// Structured representation of a runtime error raised while interpreting Elm code.
/// Returned on the error branch of every <see cref="ElmSyntaxInterpreter.InterpretAsElmValue(SyntaxModel.Expression, IReadOnlyDictionary{DeclQualifiedName, SyntaxModel.Declaration})"/>
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
    /// line per frame. Each frame's argument renderings are truncated to
    /// <see cref="ElmCallStackFrame.DefaultArgumentRenderLengthLimit"/> characters; use
    /// <see cref="Render(int)"/> to configure a different limit.
    /// </summary>
    public override string ToString()
    {
        return Render(ElmCallStackFrame.DefaultArgumentRenderLengthLimit);
    }

    /// <summary>
    /// Renders the error as a human-readable string (see <see cref="ToString"/>), truncating each
    /// stack frame's individual argument renderings to at most
    /// <paramref name="argumentRenderLengthLimit"/> characters. A negative limit disables
    /// truncation and renders every argument in full.
    /// </summary>
    /// <param name="argumentRenderLengthLimit">
    /// Maximum number of characters to keep from each argument's rendering. Negative to keep all.
    /// </param>
    public string Render(int argumentRenderLengthLimit)
    {
        if (CallStack.Count is 0)
            return Message;

        var sb = new System.Text.StringBuilder(Message);
        sb.Append("\nElm call stack (innermost first):");

        for (var i = 0; i < CallStack.Count; i++)
        {
            sb.Append("\n  at ").Append(CallStack[i].Render(argumentRenderLengthLimit));
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
internal sealed class ElmInterpretationException(
    ElmInterpretationError error,
    System.Exception? innerException = null) : System.Exception(error.ToString(), innerException)
{
    public ElmInterpretationError Error { get; } = error;
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
        ImmutableList<PineValueInProcess> Arguments,
        ApplicationContext Context);

    /// <summary>
    /// Contextual information accompanying an <see cref="Application"/>: the top-level declaration
    /// currently being evaluated and the local bindings visible at the call site.
    /// </summary>
    public record ApplicationContext(
        DeclQualifiedName CurrentTopLevel,
        IReadOnlyDictionary<string, PineValueInProcess> LocalBindings);

    /// <summary>
    /// Outcome of attempting to resolve an <see cref="Application"/>: either a final value
    /// has been produced, or the interpreter should continue by evaluating a function body
    /// with parameters bound to argument values.
    /// </summary>
    public abstract record ApplicationResolution
    {
        /// <summary>A fully-resolved Elm value.</summary>
        public sealed record Resolved(PineValueInProcess Value)
            : ApplicationResolution;

        /// <summary>
        /// The application resolved to a user-defined function. The interpreter will bind
        /// the function's argument patterns to the call's arguments and then evaluate the body.
        /// <para>
        /// <paramref name="ResolvedName"/> optionally carries the full module-qualified name of the
        /// declaration the application resolved to. When present, the interpreter uses it as the
        /// "current top-level" while evaluating the body, so that <em>unqualified</em> references
        /// inside the body (e.g. constructors or sibling helpers of the same module) resolve against
        /// the resolved declaration's own module rather than the — possibly unqualified — name that
        /// appeared at the call site. When <c>null</c>, the call site's
        /// <see cref="Application.FunctionName"/> is used (legacy behaviour).
        /// </para>
        /// </summary>
        public sealed record ContinueWithFunction(
            ElmSyntaxAbstract.FunctionImplementation Function,
            DeclQualifiedName? ResolvedName = null)
            : ApplicationResolution;
    }

    /// <summary>
    /// Interprets <paramref name="rootExpressionText"/> using a resolver that combines
    /// <see cref="PineBuiltinResolver(Application)"/> with <see cref="UserDefinedResolver(Application, IReadOnlyDictionary{DeclQualifiedName, ElmSyntaxAbstract.Declaration})"/>
    /// backed by the supplied <paramref name="prepared"/>.
    /// </summary>
    public static Result<ElmInterpretationError, ElmValue> InterpretAsElmValue(
        string rootExpressionText,
        Prepared prepared)
    {
        var parseResult =
            ParseAndCanonicalizeExpressionWithDefaultImports(rootExpressionText);

        if (parseResult.IsErrOrNull() is { } parseErr)
        {
            return new ElmInterpretationError($"Parse error: {parseErr}", []);
        }

        if (parseResult.IsOkOrNull() is not { } rootExpression)
        {
            throw new System.NotImplementedException(
                $"Unexpected parse result type: {parseResult.GetType().FullName}");
        }

        var combined = BuildResolvers(prepared.Declarations);

        var valueInProcess =
            Interpret(rootExpression, combined, BuildInfixOperatorMap(prepared.Declarations));

        return valueInProcess.Map(ToElm);
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
    public static Result<ElmInterpretationError, ElmValue> InterpretAsElmValue(
        SyntaxModel.Expression rootExpression,
        IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration> declarations)
    {
        var combined = BuildResolvers(declarations);

        var valueInProcess =
            Interpret(rootExpression, combined, BuildInfixOperatorMap(declarations));

        return valueInProcess.Map(ToElm);
    }

    /// <summary>
    /// Interprets <paramref name="rootExpression"/> using a resolver that combines
    /// <see cref="PineBuiltinResolver(Application)"/> with <see cref="UserDefinedResolver(Application, IReadOnlyDictionary{DeclQualifiedName, SyntaxModel.Declaration})"/>
    /// backed by the supplied <paramref name="declarations"/>.
    /// </summary>
    /// <returns>
    /// A <see cref="Result{ErrT, OkT}"/> containing the evaluated <see cref="PineValueInProcess"/> on success,
    /// or an <see cref="ElmInterpretationError"/> describing the runtime failure (with its Elm
    /// call stack) on the error branch.
    /// </returns>
    public static Result<ElmInterpretationError, PineValueInProcess> Interpret(
        SyntaxModel.Expression rootExpression,
        IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration> declarations)
    {
        var combined = BuildResolvers(declarations);

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
    public static Result<ElmInterpretationError, PineValueInProcess> Interpret(
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
    public static Result<ElmInterpretationError, PineValueInProcess> Interpret(
        SyntaxModel.Expression rootExpression,
        System.Func<Application, ApplicationResolution> resolveApplication,
        IReadOnlyDictionary<string, DeclQualifiedName>? infixOperators)
    {
        var context =
            new ApplicationContext(
                CurrentTopLevel: DeclQualifiedName.Create([], ""),
                LocalBindings: ImmutableDictionary<string, PineValueInProcess>.Empty);

        return
            RunTrampoline(
                initialExpression: ElmSyntaxAbstract.ConvertFromConcrete.FromExpression(rootExpression),
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
    public static Result<ElmInterpretationError, ElmValue> InterpretAsElmValue(
        DeclQualifiedName functionName,
        IReadOnlyList<ElmValue> arguments,
        IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration> declarations)
    {
        var combined = BuildResolvers(declarations);

        var argumentsInProcess =
            arguments.Select(ToProcess)
            .ToImmutableList();

        var valueInProcessResult =
            Interpret(functionName, argumentsInProcess, combined, BuildInfixOperatorMap(declarations));

        return valueInProcessResult.Map(ToElm);
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
    public static Result<ElmInterpretationError, PineValueInProcess> Interpret(
        DeclQualifiedName functionName,
        IReadOnlyList<PineValueInProcess> arguments,
        IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration> declarations)
    {
        var combined = BuildResolvers(declarations);

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
    public static Result<ElmInterpretationError, PineValueInProcess> Interpret(
        DeclQualifiedName functionName,
        IReadOnlyList<PineValueInProcess> arguments,
        System.Func<Application, ApplicationResolution> resolveApplication) =>
        Interpret(functionName, arguments, resolveApplication, infixOperators: null);

    /// <summary>
    /// As <see cref="Interpret(DeclQualifiedName, IReadOnlyList{PineValueInProcess}, System.Func{Application, ApplicationResolution})"/>,
    /// but additionally consults <paramref name="infixOperators"/> for source-defined infix
    /// operator dispatch.
    /// </summary>
    public static Result<ElmInterpretationError, PineValueInProcess> Interpret(
        DeclQualifiedName functionName,
        IReadOnlyList<PineValueInProcess> arguments,
        System.Func<Application, ApplicationResolution> resolveApplication,
        IReadOnlyDictionary<string, DeclQualifiedName>? infixOperators)
    {
        var rootContext =
            new ApplicationContext(
                CurrentTopLevel: functionName,
                LocalBindings: ImmutableDictionary<string, PineValueInProcess>.Empty);

        var application =
            new Application(
                FunctionName: functionName,
                Arguments: [.. arguments],
                Context: rootContext);

        return
            RunTrampoline(
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
    public static Result<ElmInterpretationError, ElmValue> ParseAndInterpretAsElmValue(
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

        return InterpretAsElmValue(rootExpression, declarations);
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
    public static Result<ElmInterpretationError, PineValueInProcess> ParseAndInterpret(
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
        return BuildInfixOperatorMap(GetOrConvertDeclarations(declarations));
    }

    /// <summary>
    /// Builds an operator-symbol → function-name map from any
    /// <see cref="SyntaxModel.Declaration.InfixDeclaration"/> entries in
    /// <paramref name="declarations"/>. The function name is materialized as a
    /// <see cref="DeclQualifiedName"/> in the same namespace as the infix declaration so
    /// the resolver dispatches it like any other top-level call.
    /// </summary>
    private static IReadOnlyDictionary<string, DeclQualifiedName> BuildInfixOperatorMap(
        IReadOnlyDictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration> declarations)
    {
        var map = new Dictionary<string, DeclQualifiedName>();

        foreach (var (declName, declaration) in declarations)
        {
            if (declaration is ElmSyntaxAbstract.Declaration.InfixDeclaration infixDecl)
            {
                map[infixDecl.Infix.Operator] =
                    DeclQualifiedName.Create(
                        namespaces: declName.Namespaces,
                        declName: infixDecl.Infix.FunctionName);
            }
        }

        return map;
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
    public static (Result<ElmInterpretationError, PineValueInProcess> Result, ElmSyntaxInterpreterPerformanceCounters Counters)
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

        var invocationCounter = new InvocationCounter();

        var combined =
            CombineResolvers(
                [
                app => PineBuiltinResolverCounting(app, invocationCounter),
                app => UserDefinedResolver(app, declarations),
                ]);

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
                infixOperators: BuildInfixOperatorMap(declarations),
                invocationLogger: invocationCounter);

        return (result, invocationCounter.ToReadOnly());
    }

    /// <summary>
    /// As <see cref="ParseAndInterpretWithCounters(string, IReadOnlyDictionary{DeclQualifiedName, SyntaxModel.Declaration})"/>,
    /// but additionally invokes <paramref name="onApplication"/> on every direct or
    /// function-value function application observed by the interpreter. The default
    /// counting <see cref="IInvocationLogger"/> implementation is constructed with this
    /// delegate so all counter updates AND the per-application callback are forwarded
    /// from a single sink — the returned counters and the entries observed by
    /// <paramref name="onApplication"/> are therefore guaranteed to agree.
    /// </summary>
    /// <param name="rootExpressionText">The Elm expression text to parse and evaluate.</param>
    /// <param name="declarations">User-defined declarations available to the root expression.</param>
    /// <param name="onApplication">
    /// Callback invoked once for each <see cref="ApplicationLogEntry"/> the interpreter
    /// dispatches. Tests typically supply <c>list.Add</c> here to capture a trace they
    /// can later search or render.
    /// </param>
    public static (Result<ElmInterpretationError, PineValueInProcess> Result, ElmSyntaxInterpreterPerformanceCounters Counters)
        ParseAndInterpretWithCounters(
        string rootExpressionText,
        IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration> declarations,
        System.Action<ApplicationLogEntry> onApplication)
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

        var invocationCounter = new InvocationCounter(onApplication);

        var combined =
            CombineResolvers(
                [
                app => PineBuiltinResolverCounting(app, invocationCounter),
                app => UserDefinedResolver(app, declarations),
                ]);

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
                infixOperators: BuildInfixOperatorMap(declarations),
                invocationLogger: invocationCounter);

        return (result, invocationCounter.ToReadOnly());
    }

    /// <summary>
    /// Parses <paramref name="rootExpressionText"/> as an Elm expression directly via
    /// <see cref="ElmSyntaxParser.ParseExpression(string)"/>. Exposed internally so collaborating
    /// helpers (for example <c>CompareInterpreterWithIntermediateVM</c> in the test assembly) can
    /// decompose the parsed form to extract a top-level function call's arguments.
    /// </summary>
    internal static Result<string, SyntaxModel.Expression> ParseRootExpressionForDecomposition(
        string rootExpressionText) =>
        ParseRootExpression(rootExpressionText);

    /// <summary>
    /// Parses <paramref name="rootExpressionText"/> as an Elm expression directly via
    /// <see cref="ElmSyntaxParser.ParseExpression(string)"/>.
    /// </summary>
    private static Result<string, SyntaxModel.Expression> ParseRootExpression(
        string rootExpressionText)
    {
        var parseResult = ElmSyntaxParser.ParseExpression(rootExpressionText);

        if (parseResult.IsErrOrNull() is { } parseErr)
        {
            return "Failed to parse root expression: " + parseErr;
        }

        if (parseResult.IsOkOrNull() is not { } parsedExpression)
        {
            throw new System.NotImplementedException(
                "Unexpected parse result type: " + parseResult.GetType().FullName);
        }

        return parsedExpression;
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
            IReadOnlyList<ElmSyntaxAbstract.Expression> Elements,
            int NextIndex,
            PineValueInProcess[] Accumulated,
            ApplicationContext Env) : Kont;

        /// <summary>After evaluating the condition of an if-expression, pick a branch.</summary>
        public sealed record IfBranch(
            ElmSyntaxAbstract.Expression ThenBranch,
            ElmSyntaxAbstract.Expression ElseBranch,
            ApplicationContext Env) : Kont;

        /// <summary>
        /// Building a record literal: collecting successive field values into
        /// <see cref="Accumulated"/>. The field name at <see cref="NextIndex"/>
        /// is the one the currently-returned value is for.
        /// </summary>
        public sealed record BuildRecord(
            IReadOnlyList<ElmSyntaxAbstract.RecordSetter> Fields,
            int NextIndex,
            (string FieldName, PineValue FieldNameValue, PineValueInProcess Value)[] Accumulated,
            ApplicationContext Env) : Kont;

        /// <summary>
        /// Building the argument list for an application of <see cref="FunctionOrValue"/>.
        /// <see cref="NextIndex"/> is the argument slot the currently-returned value fills.
        /// When all arguments are collected, the application is resolved.
        /// </summary>
        public sealed record BuildArgs(
            ElmSyntaxAbstract.Expression.FunctionOrValue FunctionOrValue,
            IReadOnlyList<ElmSyntaxAbstract.Expression> Arguments,
            int NextIndex,
            PineValueInProcess[] Accumulated,
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
            ElmSyntaxAbstract.Expression FunctionExpr,
            IReadOnlyList<ElmSyntaxAbstract.Expression> Arguments,
            int NextIndex,
            PineValueInProcess[] Accumulated,
            ApplicationContext Env) : Kont;

        /// <summary>
        /// After evaluating the function expression for a <see cref="BuildArgsForValue"/>
        /// application, apply the returned value (which must be an <see cref="ElmClosureInProcess"/>)
        /// to the previously-collected arguments.
        /// </summary>
        public sealed record ApplyEvaluatedFunction(
            IReadOnlyList<PineValueInProcess> Arguments,
            ApplicationContext Env) : Kont;

        /// <summary>
        /// Inserted under a <see cref="CallFrame"/> when a function is over-applied: the call has
        /// been saturated at <see cref="CallFrame.Arguments"/> and once its body returns a value
        /// (which must itself be an <see cref="ElmClosureInProcess"/>) the <see cref="ExtraArgs"/>
        /// will be applied to it. Surfaces as the unifying mechanism for both
        /// <c>(f x) y z</c>-style chained applications and the case where a single application
        /// node's argument list spans the outer function's parameters and the inner closure's
        /// parameters.
        /// </summary>
        public sealed record AfterCall(
            IReadOnlyList<PineValueInProcess> ExtraArgs,
            ApplicationContext Env) : Kont;

        /// <summary>
        /// After evaluating the expression for a <c>let</c>'s <c>LetFunction</c>
        /// declaration, bind the name to the returned value, then continue processing
        /// the remaining declarations and finally the body.
        /// </summary>
        public sealed record LetBindFunction(
            string BindingName,
            IReadOnlyList<ElmSyntaxAbstract.LetDeclaration> Remaining,
            int NextIndex,
            Dictionary<string, PineValueInProcess> Extended,
            ElmSyntaxAbstract.Expression Body,
            ApplicationContext Outer) : Kont;

        /// <summary>
        /// After evaluating the expression of a <c>let</c>'s <c>LetDestructuring</c>
        /// declaration, bind the pattern to the returned value, then continue.
        /// </summary>
        public sealed record LetBindDestructure(
            ElmSyntaxAbstract.Pattern Pattern,
            IReadOnlyList<ElmSyntaxAbstract.LetDeclaration> Remaining,
            int NextIndex,
            Dictionary<string, PineValueInProcess> Extended,
            ElmSyntaxAbstract.Expression Body,
            ApplicationContext Outer) : Kont;

        /// <summary>
        /// Marker frame indicating that a call to a user-defined Elm function is
        /// currently in progress. Contributes one entry to the Elm call stack but
        /// performs no computation on return (the returned value simply bubbles up).
        /// <see cref="Arguments"/> holds the values the function was called with — used
        /// both to render runtime-error stack traces as Elm-style function applications
        /// and to detect infinite recursion (a repeated <c>(SourceIdentity, Arguments)</c>
        /// pair on the kont stack).
        /// <para>
        /// <see cref="SourceIdentity"/> is an opaque, reference-comparable handle on the
        /// AST node the call originated from — a <see cref="SyntaxModel.FunctionImplementation"/>
        /// for top-level / let-bound declarations and a <see cref="SyntaxModel.LambdaStruct"/>
        /// for anonymous lambdas. The infinite-recursion detector compares frames by
        /// <see cref="SourceIdentity"/> (using <see cref="object.ReferenceEquals"/>), not by
        /// <see cref="FunctionName"/>: two anonymous lambdas constructed at the same source
        /// position (e.g. <c>ParserFast</c> combinator chains where every lambda's synthetic
        /// name collapses to <c>&lt;lambda@1:1&gt;</c>) are then correctly treated as
        /// distinct frames as long as they are distinct AST nodes.
        /// </para>
        /// </summary>
        public sealed record CallFrame(
            DeclQualifiedName FunctionName,
            object SourceIdentity,
            IReadOnlyList<PineValueInProcess> Arguments) : Kont;

        /// <summary>
        /// After evaluating the scrutinee of a <c>case … of</c> expression, try the
        /// arms in source order and evaluate the first one whose pattern matches the
        /// returned value. If no arm matches, a runtime error is raised.
        /// </summary>
        public sealed record MatchCase(
            IReadOnlyList<ElmSyntaxAbstract.Case> Cases,
            ApplicationContext Env) : Kont;

        /// <summary>
        /// After evaluating the record-position expression of a record-access
        /// (<c>record.field</c>), look up <see cref="FieldName"/> on the returned
        /// value (which must be an <see cref="ElmValue.ElmRecord"/>).
        /// </summary>
        public sealed record AccessRecordField(
            string FieldName,
            PineValue FieldNameValue) : Kont;

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
            PineValueInProcess? OriginalRecord,
            IReadOnlyList<ElmSyntaxAbstract.RecordSetter> Fields,
            int NextIndex,
            (string FieldName, PineValue FieldNameValue, PineValueInProcess FieldValue)[] Accumulated,
            ApplicationContext Env) : Kont;
    }

    /// <summary>
    /// Entry point of the trampoline. At most one of <paramref name="initialExpression"/>
    /// and <paramref name="initialApplication"/> is non-null:
    /// expression-evaluation starts from <paramref name="initialExpression"/>; direct-call
    /// entry points (<see cref="Interpret(DeclQualifiedName, IReadOnlyList{PineValueInProcess}, System.Func{Application, ApplicationResolution})"/>)
    /// start by resolving <paramref name="initialApplication"/>.
    /// </summary>
    private static Result<ElmInterpretationError, PineValueInProcess> RunTrampoline(
        ElmSyntaxAbstract.Expression? initialExpression,
        ApplicationContext initialEnv,
        Application? initialApplication,
        System.Func<Application, ApplicationResolution> resolveApplication,
        IReadOnlyDictionary<string, DeclQualifiedName>? infixOperators,
        IInvocationLogger? invocationLogger = null)
    {
        var kstack = new Stack<Kont>();
        invocationLogger ??= new InvocationCounter();

        // Either: (currentExpr, currentEnv) is the next thing to evaluate ("Eval" mode),
        // or currentValue holds the value about to be returned to the top kont ("Return" mode).
        ElmSyntaxAbstract.Expression? currentExpr;
        ApplicationContext currentEnv;
        PineValueInProcess? currentValue = null;

        if (initialApplication is not null)
        {
            // Direct-call entry: we start in an unusual state – no expression yet, we need to
            // resolve the top-level application first. Applying the initial application may
            // either immediately produce a value (Resolved) or switch us into evaluating a
            // function body (ContinueWithFunction, which pushes a CallFrame).

            var applyCallResult =
                ApplyResolvedCall(
                    application: initialApplication,
                    resolveApplication: resolveApplication,
                    kstack: kstack,
                    invocationLogger: invocationLogger);

            if (applyCallResult.IsOkOrNull() is { } applyOk)
            {
                switch (applyOk)
                {
                    case ApplyCallOutcome.ResolvedValue resolvedValue:
                        return resolvedValue.Value;

                    case ApplyCallOutcome.ContinueEvaluating continueEvaluating:
                        currentExpr = continueEvaluating.Expression;
                        currentEnv = continueEvaluating.Env;
                        break;

                    default:
                        throw new System.NotImplementedException(
                            "Unexpected ApplyCallOutcome shape.");
                }
            }
            else
            {
                if (applyCallResult.IsErrOrNull() is { } applyErr)
                {
                    return applyErr;
                }

                throw new System.NotImplementedException(
                    "ApplyResolvedCall did not return a value or an error: " + applyCallResult.GetType().FullName);
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
            invocationLogger.OnInstructionLoop();

            if (currentExpr is not null)
            {
                // ---- Eval mode ------------------------------------------------

                switch (currentExpr)
                {
                    case ElmSyntaxAbstract.Expression.UnitExpr:
                        currentValue = PineValueInProcess.EmptyList;
                        currentExpr = null;
                        break;

                    case ElmSyntaxAbstract.Expression.StringLiteral literal:
                        currentValue = PineValueInProcess.Create(literal.ValueAsPineValue);
                        currentExpr = null;
                        break;

                    case ElmSyntaxAbstract.Expression.CharLiteral charLiteral:
                        currentValue = PineValueInProcess.Create(charLiteral.ValueAsPineValue);
                        currentExpr = null;
                        break;

                    case ElmSyntaxAbstract.Expression.Integer integer:
                        currentValue = PineValueInProcess.Create(integer.ValueAsPineValue);
                        currentExpr = null;
                        break;

                    case ElmSyntaxAbstract.Expression.Negation negation:
                        kstack.Push(new Kont.Negate());
                        currentExpr = negation.Expression;
                        break;

                    case ElmSyntaxAbstract.Expression.ListExpr listExpr:
                        {
                            var nodes = listExpr.Elements;

                            if (nodes.Count is 0)
                            {
                                currentValue = PineValueInProcess.EmptyList;
                                currentExpr = null;
                                break;
                            }

                            var accumulated = new PineValueInProcess[nodes.Count];

                            kstack.Push(
                                new Kont.BuildList(
                                    Elements: nodes,
                                    NextIndex: 0,
                                    Accumulated: accumulated,
                                    Env: currentEnv));

                            currentExpr = nodes[0];
                            break;
                        }

                    case ElmSyntaxAbstract.Expression.TupledExpression tupledExpression:
                        {
                            var nodes = tupledExpression.Elements;

                            if (nodes.Count is 0)
                            {
                                currentValue = PineValueInProcess.EmptyList;
                                currentExpr = null;
                                break;
                            }

                            var accumulated = new PineValueInProcess[nodes.Count];

                            kstack.Push(
                                new Kont.BuildList(
                                    Elements: nodes,
                                    NextIndex: 0,
                                    Accumulated: accumulated,
                                    Env: currentEnv));

                            currentExpr = nodes[0];
                            break;
                        }

                    case ElmSyntaxAbstract.Expression.IfBlock ifBlock:
                        kstack.Push(
                            new Kont.IfBranch(
                                ThenBranch: ifBlock.ThenBlock,
                                ElseBranch: ifBlock.ElseBlock,
                                Env: currentEnv));

                        currentExpr = ifBlock.Condition;
                        break;

                    case ElmSyntaxAbstract.Expression.RecordExpr recordExpr:
                        {
                            var fields = recordExpr.Fields;

                            if (fields.Count is 0)
                            {
                                currentValue = BuildRecordValue([]);
                                currentExpr = null;
                                break;
                            }

                            var accumulated =
                                new (string FieldName, PineValue FieldNameValue, PineValueInProcess FieldValue)[fields.Count];

                            kstack.Push(
                                new Kont.BuildRecord(
                                    Fields: fields,
                                    NextIndex: 0,
                                    Accumulated: accumulated,
                                    Env: currentEnv));

                            currentExpr = fields[0].Value;
                            break;
                        }

                    case ElmSyntaxAbstract.Expression.FunctionOrValue functionOrValue:
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

                            var applyResult =
                                ApplyFunctionOrValue(
                                    functionOrValue: functionOrValue,
                                    arguments: [],
                                    env: currentEnv,
                                    resolveApplication: resolveApplication,
                                    kstack: kstack,
                                    invocationLogger: invocationLogger);

                            if (applyResult.IsOkOrNull() is { } outcome)
                            {
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

                                    default:
                                        throw new System.NotImplementedException(
                                            $"Unknown ApplyCallOutcome: {outcome}");
                                }

                                break;
                            }
                            else
                            {
                                if (applyResult.IsErrOrNull() is { } error)
                                {
                                    return error;
                                }

                                throw new System.NotImplementedException(
                                    "Unexpected type of application result: " + applyResult.GetType().FullName);
                            }
                        }

                    case ElmSyntaxAbstract.Expression.Application application:
                        {
                            var functionExpression = application.Function;

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

                            var accumulated = new PineValueInProcess[args.Count];

                            if (functionExpression is ElmSyntaxAbstract.Expression.FunctionOrValue fnOrVal)
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
                                        FunctionExpr: functionExpression,
                                        Arguments: args,
                                        NextIndex: 0,
                                        Accumulated: accumulated,
                                        Env: currentEnv));
                            }

                            currentExpr = args[0];
                            break;
                        }

                    case ElmSyntaxAbstract.Expression.LambdaExpression lambdaExpression:
                        {
                            // A lambda evaluates to a closure value capturing the current local
                            // bindings (so any free variables in the body resolve to their values
                            // at the moment the lambda was reached, not at the moment the closure
                            // is finally invoked) and the surrounding top-level declaration name
                            // (so post-invocation stack traces remain coherent).
                            currentValue =
                                new ElmClosureInProcess(
                                    source: new ElmClosureInProcess.SourceRef.Lambda(lambdaExpression),
                                    parameterCount: lambdaExpression.Arguments.Count,
                                    argumentsAlreadyCollected: [],
                                    capturedBindings: SnapshotBindings(currentEnv.LocalBindings),
                                    capturedTopLevel: currentEnv.CurrentTopLevel);

                            currentExpr = null;
                            break;
                        }

                    case ElmSyntaxAbstract.Expression.LetExpression letExpression:
                        {
                            var decls = letExpression.Declarations;
                            var extended = new Dictionary<string, PineValueInProcess>(currentEnv.LocalBindings);

                            if (decls.Count is 0)
                            {
                                currentEnv =
                                    new ApplicationContext(
                                        CurrentTopLevel: currentEnv.CurrentTopLevel,
                                        LocalBindings: extended);

                                currentExpr = letExpression.Expression;
                                break;
                            }

                            // Implements Elm's letrec semantics: every binding in the let
                            // group is mutually visible, regardless of source order. Pre-build
                            // closures for parameterised LetFunction bindings (their bodies
                            // are not yet evaluated, so they can freely reference any sibling
                            // through the shared mutable `extended` dictionary captured as
                            // their environment), then evaluate non-function bindings in
                            // dependency order.

                            var prepareResult =
                                PrepareLetGroupAndSortNonFunctionDecls(
                                    decls: decls,
                                    extended: extended,
                                    outerTopLevel: currentEnv.CurrentTopLevel,
                                    kstack: kstack);

                            if (prepareResult.IsOkOrNull() is { } sortedNonFunctionDecls)
                            {
                                if (sortedNonFunctionDecls.Count is 0)
                                {
                                    currentEnv =
                                        new ApplicationContext(
                                            CurrentTopLevel: currentEnv.CurrentTopLevel,
                                            LocalBindings: extended);

                                    currentExpr = letExpression.Expression;
                                    break;
                                }

                                // Start evaluating the first non-function let binding's RHS.
                                var beginNextResult =
                                    BeginNextLetDecl(
                                        decls: sortedNonFunctionDecls,
                                        nextIndex: 0,
                                        extended: extended,
                                        body: letExpression.Expression,
                                        outerEnv: currentEnv,
                                        kstack: kstack);

                                if (beginNextResult.IsOkOrNullable() is { } beginNextOk)
                                {
                                    (currentExpr, currentEnv) = beginNextOk;

                                    break;
                                }
                                else
                                {
                                    if (beginNextResult.IsErrOrNull() is { } beginNextErr)
                                    {
                                        return beginNextErr;
                                    }

                                    throw new System.NotImplementedException(
                                        "Unexpected type of result: " + beginNextResult.GetType().FullName);
                                }
                            }
                            else
                            {
                                if (prepareResult.IsErrOrNull() is { } prepareErr)
                                {
                                    return prepareErr;
                                }

                                throw new System.NotImplementedException(
                                    "Unexpected type of result: " + prepareResult.GetType().FullName);
                            }
                        }

                    case ElmSyntaxAbstract.Expression.CaseExpression caseExpression:
                        kstack.Push(
                            new Kont.MatchCase(
                                Cases: caseExpression.Cases,
                                Env: currentEnv));

                        currentExpr = caseExpression.Expression;
                        break;

                    case ElmSyntaxAbstract.Expression.RecordAccess recordAccess:
                        {
                            // Push a continuation that, once the record-position expression
                            // produces a value, looks up the requested field on it.
                            kstack.Push(
                                new Kont.AccessRecordField(
                                    FieldName: recordAccess.FieldName,
                                    FieldNameValue: recordAccess.FieldNameValue));

                            currentExpr = recordAccess.Record;
                            break;
                        }

                    case ElmSyntaxAbstract.Expression.RecordAccessFunction recordAccessFunction:
                        {
                            currentValue =
                                ElmRecordAccessChainInProcess.CreateFromFieldNames([recordAccessFunction.FieldName]);

                            currentExpr = null;
                            break;
                        }

                    case ElmSyntaxAbstract.Expression.RecordUpdateExpression recordUpdate:
                        {
                            var fields = recordUpdate.Fields;

                            if (fields.Count is 0)
                            {
                                return
                                    MakeRuntimeError(
                                        "Record update with no field assignments.",
                                        kstack);
                            }

                            var accumulated = new (string, PineValue, PineValueInProcess)[fields.Count];

                            // First evaluate the record name as a bare FunctionOrValue so the
                            // existing local-binding / top-level lookup machinery applies.
                            var recordNameExpr =
                                new ElmSyntaxAbstract.Expression.FunctionOrValue(
                                    ModuleName: [],
                                    Name: recordUpdate.RecordName);

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

                    case ElmSyntaxAbstract.Expression.OperatorApplication operatorApplication:
                        {
                            // Translate the operator into the corresponding source-defined
                            // function call. Look up the operator symbol in the
                            // <c>infix … = funcName</c> map collected from the user's
                            // declarations — we deliberately do NOT consult any hard-coded
                            // table here, so the actual implementation referenced by the
                            // infix declaration is what runs.
                            var opSymbol = operatorApplication.Operator;

                            if (infixOperators is null
                                || !infixOperators.TryGetValue(opSymbol, out var opFunctionName))
                            {
                                return
                                    MakeRuntimeError(
                                        "No infix declaration found for operator '" + opSymbol + "'.",
                                        kstack);
                            }

                            var functionRef =
                                new ElmSyntaxAbstract.Expression.FunctionOrValue(
                                    ModuleName: opFunctionName.Namespaces,
                                    Name: opFunctionName.DeclName);

                            currentExpr =
                                new ElmSyntaxAbstract.Expression.Application(
                                    Function: functionRef,
                                    Arguments: [operatorApplication.Left, operatorApplication.Right]);

                            break;
                        }

                    case ElmSyntaxAbstract.Expression.PrefixOperator prefixOperator:
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
                                return
                                    MakeRuntimeError(
                                        "No infix declaration found for operator '" + opSymbol + "'.",
                                        kstack);
                            }

                            currentExpr =
                                new ElmSyntaxAbstract.Expression.FunctionOrValue(
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
                            if (!ContainsOpaque(value) && value.AsInteger() is { } intValue)
                            {
                                currentValue = MakeInteger(-intValue);
                                break;
                            }

                            return
                                MakeRuntimeError(
                                    "Negation is only implemented for integer values, got "
                                    + RenderArgumentForError(value).rendered,
                                    kstack);
                        }

                    case Kont.BuildList buildList:
                        {
                            buildList.Accumulated[buildList.NextIndex] = value;
                            var next = buildList.NextIndex + 1;

                            if (next == buildList.Elements.Count)
                            {
                                currentValue = PineValueInProcess.CreateList(buildList.Accumulated);
                                break;
                            }

                            kstack.Push(buildList with { NextIndex = next });
                            currentExpr = buildList.Elements[next];
                            currentEnv = buildList.Env;
                            currentValue = null;
                            break;
                        }

                    case Kont.IfBranch ifBranch:
                        {
                            var conditionIsTrue = IsElmTrue(value);

                            currentExpr =
                                conditionIsTrue
                                ?
                                ifBranch.ThenBranch
                                :
                                ifBranch.ElseBranch;

                            currentEnv = ifBranch.Env;
                            currentValue = null;
                            break;
                        }

                    case Kont.BuildRecord buildRecord:
                        {
                            /*
                             * The record expressions in the Elm syntax model must already come sorted,
                             * so we dont sort the fields here.
                             * */

                            var fieldName = buildRecord.Fields[buildRecord.NextIndex].FieldName;
                            var fieldNameValue = buildRecord.Fields[buildRecord.NextIndex].FieldNameValue;

                            buildRecord.Accumulated[buildRecord.NextIndex] = (fieldName, fieldNameValue, value);

                            var next = buildRecord.NextIndex + 1;

                            if (next == buildRecord.Fields.Count)
                            {
                                currentValue = BuildRecordValue(buildRecord.Accumulated);
                                break;
                            }

                            kstack.Push(buildRecord with { NextIndex = next });
                            currentExpr = buildRecord.Fields[next].Value;
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
                                currentExpr = buildArgs.Arguments[next];
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
                                var localResult =
                                    ApplyFunctionValue(
                                        functionValue: localFnValue,
                                        functionRenderForError: buildArgs.FunctionOrValue.Name,
                                        newArguments: buildArgs.Accumulated,
                                        callerEnv: buildArgs.Env,
                                        kstack: kstack,
                                        invocationLogger: invocationLogger);

                                if (localResult.IsOkOrNull() is { } localOutcome)
                                {
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

                                        default:
                                            throw new System.NotImplementedException(
                                                $"Unknown ApplyCallOutcome: {localOutcome.GetType().FullName}");
                                    }
                                }

                                break;
                            }

                            var applyResult =
                                ApplyFunctionOrValue(
                                    functionOrValue: buildArgs.FunctionOrValue,
                                    arguments: [.. buildArgs.Accumulated],
                                    env: buildArgs.Env,
                                    resolveApplication: resolveApplication,
                                    kstack: kstack,
                                    invocationLogger: invocationLogger);

                            if (applyResult.IsOkOrNull() is { } outcome)
                            {
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

                                    default:
                                        throw new System.NotImplementedException(
                                            $"Unknown ApplyCallOutcome: {outcome.GetType().FullName}");
                                }

                                break;
                            }
                            else
                            {
                                if (applyResult.IsErrOrNull() is { } applyErr)
                                    return applyErr;

                                throw new System.NotImplementedException(
                                    $"ApplyFunctionOrValue returned a result that is neither Ok nor Err: {applyResult.GetType().FullName}");
                            }
                        }

                    case Kont.BuildArgsForValue buildArgsForValue:
                        {
                            buildArgsForValue.Accumulated[buildArgsForValue.NextIndex] = value;
                            var next = buildArgsForValue.NextIndex + 1;

                            if (next < buildArgsForValue.Arguments.Count)
                            {
                                kstack.Push(buildArgsForValue with { NextIndex = next });
                                currentExpr = buildArgsForValue.Arguments[next];
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

                            currentExpr = buildArgsForValue.FunctionExpr;
                            currentEnv = buildArgsForValue.Env;
                            currentValue = null;
                            break;
                        }

                    case Kont.ApplyEvaluatedFunction applyEvaluatedFunction:
                        {
                            var applyResult =
                                ApplyFunctionValue(
                                    functionValue: value,
                                    functionRenderForError: null,
                                    newArguments: applyEvaluatedFunction.Arguments,
                                    callerEnv: applyEvaluatedFunction.Env,
                                    kstack: kstack,
                                    invocationLogger: invocationLogger);

                            if (applyResult.IsOkOrNull() is { } applyOutcome)
                            {
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

                            if (applyResult.IsErrOrNull() is { } applyErr)
                                return applyErr;

                            throw new System.NotImplementedException(
                                $"ApplyFunctionValue returned a result that is neither Ok nor Err: {applyResult.GetType().FullName}");
                        }

                    case Kont.AfterCall afterCall:
                        {
                            // The body of a saturated call has produced a value; if extra args
                            // remain (over-application), apply them to the returned value.
                            var afterCallApplyResult =
                                ApplyFunctionValue(
                                    functionValue: value,
                                    functionRenderForError: null,
                                    newArguments: afterCall.ExtraArgs,
                                    callerEnv: afterCall.Env,
                                    kstack: kstack,
                                    invocationLogger: invocationLogger);

                            if (afterCallApplyResult.IsOkOrNull() is { } afterCallApplyOutcome)
                            {
                                switch (afterCallApplyOutcome)
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

                            if (afterCallApplyResult.IsErrOrNull() is { } afterCallApplyErr)
                                return afterCallApplyErr;

                            throw new System.NotImplementedException(
                                $"ApplyFunctionValue (for AfterCall) returned a result that is neither Ok nor Err: {afterCallApplyResult.GetType().FullName}");
                        }

                    case Kont.LetBindFunction letBindFunction:
                        {
                            letBindFunction.Extended[letBindFunction.BindingName] = value;

                            var beginNextResult =
                                BeginNextLetDecl(
                                    decls: letBindFunction.Remaining,
                                    nextIndex: letBindFunction.NextIndex + 1,
                                    extended: letBindFunction.Extended,
                                    body: letBindFunction.Body,
                                    outerEnv: letBindFunction.Outer,
                                    kstack: kstack);

                            if (beginNextResult.IsOkOrNullable() is { } beginNextOutcome)
                            {
                                (currentExpr, currentEnv) = beginNextOutcome;

                                currentValue = null;
                                break;
                            }

                            if (beginNextResult.IsErrOrNull() is { } beginNextErr)
                                return beginNextErr;

                            throw new System.NotImplementedException(
                                $"BeginNextLetDecl returned a result that is neither Ok nor Err: {beginNextResult.GetType().FullName}");
                        }

                    case Kont.LetBindDestructure letBindDestructure:
                        {
                            try
                            {
                                var patternBindings = new Dictionary<string, PineValueInProcess>();

                                BindPattern(
                                    letBindDestructure.Pattern,
                                    value,
                                    patternBindings);

                                foreach (var (boundName, boundValue) in patternBindings)
                                {
                                    letBindDestructure.Extended[boundName] = boundValue;
                                }
                            }
                            catch (System.Exception ex)
                            {
                                return MakeRuntimeError(ex.Message, kstack);
                            }

                            var beginNextResult =
                                BeginNextLetDecl(
                                    decls: letBindDestructure.Remaining,
                                    nextIndex: letBindDestructure.NextIndex + 1,
                                    extended: letBindDestructure.Extended,
                                    body: letBindDestructure.Body,
                                    outerEnv: letBindDestructure.Outer,
                                    kstack: kstack);

                            if (beginNextResult.IsOkOrNullable() is { } beginNextOutcome)
                            {
                                (currentExpr, currentEnv) = beginNextOutcome;
                            }
                            else
                            {
                                if (beginNextResult.IsErrOrNull() is { } beginNextErr)
                                    return beginNextErr;

                                throw new System.NotImplementedException(
                                    $"BeginNextLetDecl returned a result that is neither Ok nor Err: {beginNextResult.GetType().FullName}");
                            }

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
                                var newBindings = new Dictionary<string, PineValueInProcess>();

                                if (TryMatchPattern(caseNode.Pattern, value, newBindings))
                                {
                                    var extendedEnv =
                                        new Dictionary<string, PineValueInProcess>(
                                            matchCase.Env.LocalBindings);

                                    foreach (var (boundName, boundValue) in newBindings)
                                    {
                                        extendedEnv[boundName] = boundValue;
                                    }

                                    currentEnv =
                                        new ApplicationContext(
                                            CurrentTopLevel: matchCase.Env.CurrentTopLevel,
                                            LocalBindings: extendedEnv);

                                    currentExpr = caseNode.Expression;
                                    currentValue = null;
                                    matched = true;
                                    break;
                                }
                            }

                            if (!matched)
                            {
                                var renderedValue =
                                    RenderArgumentForError(value).rendered;

                                return
                                    MakeRuntimeError(
                                        "Case expression did not match any arm.\nScrutinee value: "
                                        + renderedValue,
                                        kstack);
                            }

                            break;
                        }

                    case Kont.AccessRecordField accessRecordField:
                        {
                            try
                            {
                                var patternBindings = new Dictionary<string, PineValueInProcess>();

                                var fieldValue =
                                    RecordAccess(
                                        value,
                                        fieldName: accessRecordField.FieldName,
                                        fieldNameValue: accessRecordField.FieldNameValue);

                                currentValue = fieldValue;
                            }
                            catch (System.Exception ex)
                            {
                                return MakeRuntimeError(ex.Message, kstack);
                            }
                        }

                        break;

                    case Kont.BuildRecordUpdate buildRecordUpdate:
                        {
                            if (buildRecordUpdate.NextIndex < 0)
                            {
                                // Just received the original record value.

                                if (!PineValueInProcess.AreEqual(value.GetElementAt(0), ElmValue.ElmRecordTypeTagNameAsValue))
                                {
                                    var renderedValue =
                                        RenderArgumentForError(value).rendered;

                                    return
                                        MakeRuntimeError(
                                            "Expected a record value to update, but got something else.\nValue: "
                                            + renderedValue,
                                            kstack);
                                }

                                kstack.Push(
                                    buildRecordUpdate with
                                    {
                                        OriginalRecord = value,
                                        NextIndex = 0,
                                    });

                                currentExpr = buildRecordUpdate.Fields[0].Value;
                                currentEnv = buildRecordUpdate.Env;
                                currentValue = null;
                                break;
                            }

                            var fieldName =
                                buildRecordUpdate.Fields[buildRecordUpdate.NextIndex].FieldName;

                            var fieldNameValue =
                                buildRecordUpdate.Fields[buildRecordUpdate.NextIndex].FieldNameValue;

                            buildRecordUpdate.Accumulated[buildRecordUpdate.NextIndex] =
                                (fieldName, fieldNameValue, value);

                            var nextIndex = buildRecordUpdate.NextIndex + 1;

                            if (nextIndex < buildRecordUpdate.Fields.Count)
                            {
                                kstack.Push(buildRecordUpdate with { NextIndex = nextIndex });

                                currentExpr =
                                    buildRecordUpdate.Fields[nextIndex].Value;

                                currentEnv = buildRecordUpdate.Env;
                                currentValue = null;
                                break;
                            }

                            // All update values evaluated: build the new record by
                            // overlaying the updates onto the original record's fields.
                            // Elm record-update preserves the original field order (which
                            // for Elm records produced by this interpreter is alphabetical)
                            // and requires every updated field name to exist on the record.

                            if (buildRecordUpdate.OriginalRecord is not { } originalRecord)
                            {
                                return
                                    MakeRuntimeError(
                                        "Internal error: original record value was not set in BuildRecordUpdate continuation.",
                                        kstack);
                            }

                            var newListItems = new PineValueInProcess[originalRecord.GetLength()];

                            var fieldCount = (newListItems.Length - 1) / 2;

                            for (var i = 0; i < newListItems.Length; i++)
                            {
                                newListItems[i] = originalRecord.GetElementAt(i);
                            }

                            for (var i = 0; i < buildRecordUpdate.Accumulated.Length; i++)
                            {
                                var (updateFieldName, updateFieldNameValue, updateValue) =
                                    buildRecordUpdate.Accumulated[i];

                                int? fieldIndex = null;

                                for (var j = 0; j < fieldCount; j++)
                                {
                                    if (PineValueInProcess.AreEqual(
                                        originalRecord.GetElementAt(1 + 2 * j),
                                        updateFieldNameValue))
                                    {
                                        fieldIndex = j;
                                        break;
                                    }
                                }

                                if (fieldIndex is null)
                                {
                                    var renderedRecord =
                                        RenderArgumentForError(originalRecord).rendered;

                                    return
                                        MakeRuntimeError(
                                            "Cannot update field \"" + updateFieldName +
                                            "\" because it does not exist on the record.\nRecord value: "
                                            + renderedRecord,
                                            kstack);
                                }

                                newListItems[1 + 2 * fieldIndex.Value + 1] = updateValue;
                            }

                            currentValue = PineValueInProcess.CreateList(newListItems);
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
        public sealed record ResolvedValue(PineValueInProcess Value)
            : ApplyCallOutcome;

        public sealed record ContinueEvaluating(
            ElmSyntaxAbstract.Expression Expression,
            ApplicationContext Env)
            : ApplyCallOutcome;
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
    /// Default <see cref="IInvocationLogger"/> implementation. Threaded through
    /// <see cref="ApplyResolvedCall"/> and <see cref="ApplyFunctionOrValue"/> so all function-body entries can
    /// be counted against a single origin and the <see cref="InfiniteRecursionCheckInterval"/> check can fire.
    /// Also accumulates the high-level performance counters surfaced via
    /// <see cref="ElmSyntaxInterpreterPerformanceCounters"/>, optionally forwarding each
    /// <see cref="ApplicationLogEntry"/> event to a caller-supplied delegate so callers
    /// can build a richer trace without re-implementing the counting logic.
    /// </summary>
    private sealed class InvocationCounter(System.Action<ApplicationLogEntry>? onApplication) : IInvocationLogger
    {
        /// <summary>
        /// Optional delegate invoked on every direct or function-value application. Receives
        /// the same <see cref="ApplicationLogEntry"/> that a <see cref="RecordingInvocationLogger"/>
        /// would append to its log. Counter updates happen first; this delegate is fired
        /// afterwards so an exception from it leaves the counters in a consistent state.
        /// </summary>
        private readonly System.Action<ApplicationLogEntry>? _onApplication = onApplication;

        public InvocationCounter()
            : this(onApplication: null)
        {
        }

        /// <summary>
        /// Number of user-defined function bodies entered. Used by the periodic
        /// infinite-recursion check.
        /// </summary>
        public int _count;

        /// <summary>Iterations of the trampoline <c>while(true)</c> loop.</summary>
        public long _instructionLoopCount;

        /// <summary>
        /// Number of direct (name-based) function applications dispatched through
        /// <see cref="ApplyFunctionOrValue"/>.
        /// </summary>
        public long _directFunctionApplicationCount;

        /// <summary>
        /// Number of applications of an evaluated function value (closure) dispatched through
        /// <see cref="ApplyFunctionValue"/>.
        /// </summary>
        public long _functionValueApplicationCount;

        /// <summary>
        /// Number of applications resolved by <see cref="PineBuiltinResolver(Application)"/>
        /// (i.e. forwarded to <see cref="KernelFunction"/>).
        /// </summary>
        public long _pineBuiltinInvocationCount;

        /// <summary>
        /// Snapshots the accumulated counters as a public, immutable
        /// <see cref="ElmSyntaxInterpreterPerformanceCounters"/> value.
        /// </summary>
        public ElmSyntaxInterpreterPerformanceCounters ToReadOnly() =>
            new(
                InstructionLoopCount: _instructionLoopCount,
                DirectFunctionApplicationCount: _directFunctionApplicationCount,
                FunctionValueApplicationCount: _functionValueApplicationCount,
                PineBuiltinInvocationCount: _pineBuiltinInvocationCount);

        public void OnInstructionLoop() => _instructionLoopCount++;

        public void OnDirectFunctionApplication(Application application)
        {
            _directFunctionApplicationCount++;
            _onApplication?.Invoke(new ApplicationLogEntry.Direct(application));
        }

        public void OnFunctionValueApplication(
            PineValueInProcess functionValue,
            IReadOnlyList<PineValueInProcess> newArguments)
        {
            _functionValueApplicationCount++;
            _onApplication?.Invoke(new ApplicationLogEntry.FunctionValue(functionValue, newArguments));
        }

        public void OnPineBuiltinInvocation(Application application) =>
            _pineBuiltinInvocationCount++;

        public int IncrementUserCallDepth() => ++_count;
    }

    private static Result<ElmInterpretationError, ApplyCallOutcome> ApplyFunctionOrValue(
        ElmSyntaxAbstract.Expression.FunctionOrValue functionOrValue,
        ImmutableList<PineValueInProcess> arguments,
        ApplicationContext env,
        System.Func<Application, ApplicationResolution> resolveApplication,
        Stack<Kont> kstack,
        IInvocationLogger invocationLogger)
    {
        var application =
            new Application(
                FunctionName:
                DeclQualifiedName.Create(
                    namespaces: [.. functionOrValue.ModuleName],
                    declName: functionOrValue.Name),
                Arguments: arguments,
                Context: env);

        invocationLogger.OnDirectFunctionApplication(application);

        return ApplyResolvedCall(application, resolveApplication, kstack, invocationLogger);
    }

    private static Result<ElmInterpretationError, ApplyCallOutcome> ApplyResolvedCall(
        Application application,
        System.Func<Application, ApplicationResolution> resolveApplication,
        Stack<Kont> kstack,
        IInvocationLogger invocationLogger)
    {
        ApplicationResolution resolution;

        try
        {
            resolution = resolveApplication(application);
        }
        catch (System.Exception ex)
        {
            return
                MakeRuntimeError(
                    "Failed to resolve application of '"
                    + application.FunctionName.FullName
                    + "': " + ex.Message,
                    kstack);
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

                    // The top-level scope to evaluate the body under. Prefer the resolved
                    // declaration's own module-qualified name so that unqualified references inside
                    // the body resolve against that module; fall back to the call-site name.
                    var bodyTopLevel =
                        continueWithFunction.ResolvedName ?? application.FunctionName;

                    // Under-application: build a closure value capturing the arguments supplied
                    // so far. Top-level user-defined functions don't lexically close over the
                    // call site's local bindings — their bodies only reference module-scope
                    // names, the parameter patterns, and let-bindings local to the body — so
                    // the captured environment is empty.
                    if (providedCount < expectedArity)
                    {
                        var sourceRef =
                            new ElmClosureInProcess.SourceRef.Declared(
                                Name: application.FunctionName,
                                Implementation: functionImpl);

                        return
                            new ApplyCallOutcome.ResolvedValue(
                                new ElmClosureInProcess(
                                    sourceRef,
                                    parameterCount: expectedArity,
                                    argumentsAlreadyCollected: [.. application.Arguments],
                                    capturedBindings: ImmutableDictionary<string, PineValueInProcess>.Empty,
                                    capturedTopLevel: bodyTopLevel));
                    }

                    // Over-application: split the argument list at the declared arity. The first
                    // `expectedArity` arguments saturate this call; the remaining arguments are
                    // pushed as an AfterCall continuation and applied to whatever value the body
                    // returns (which must itself be a function).
                    IReadOnlyList<PineValueInProcess> saturatingArgs;
                    IReadOnlyList<PineValueInProcess>? extraArgs;

                    if (providedCount > expectedArity)
                    {
                        var saturating = new PineValueInProcess[expectedArity];
                        var extra = new PineValueInProcess[providedCount - expectedArity];

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

                    var bindings = new Dictionary<string, PineValueInProcess>();

                    for (var i = 0; i < expectedArity; i++)
                    {
                        try
                        {
                            BindPattern(
                                functionImpl.Arguments[i],
                                saturatingArgs[i],
                                bindings);
                        }
                        catch (System.Exception ex)
                        {
                            return MakeRuntimeError(ex.Message, kstack);
                        }
                    }

                    var innerBindings =
                        new Dictionary<string, PineValueInProcess>(bindings.Count);

                    foreach (var (boundName, boundValue) in bindings)
                    {
                        innerBindings[boundName] = boundValue;
                    }

                    var innerContext =
                        new ApplicationContext(
                            CurrentTopLevel: bodyTopLevel,
                            LocalBindings: innerBindings);

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
                    // cycle as a literal repeated (SourceIdentity, Arguments) pair in the
                    // stack, matching the wording of the spec. The trade-off is that purely
                    // tail-recursive Elm functions use explicit-stack memory proportional to
                    // their depth rather than O(1); for the test-only interpreter in this
                    // assembly that is an acceptable cost.
                    kstack.Push(
                        new Kont.CallFrame(
                            application.FunctionName,
                            functionImpl,
                            saturatingArgs));

                    if (invocationLogger.IncrementUserCallDepth() % InfiniteRecursionCheckInterval is 0)
                    {
                        if (CheckForInfiniteRecursion(kstack) is { } error)
                            return error;
                    }

                    return
                        new ApplyCallOutcome.ContinueEvaluating(
                            Expression: functionImpl.Expression,
                            Env: innerContext);
                }

            default:
                throw new System.NotImplementedException(
                    "Unknown application resolution type: " + resolution.GetType().FullName);
        }
    }

    /// <summary>
    /// Applies <paramref name="functionValue"/> — required to be an
    /// <see cref="ElmClosureInProcess"/> closure — to <paramref name="newArguments"/>. Combines
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
    private static Result<ElmInterpretationError, ApplyCallOutcome>
        ApplyFunctionValue(
        PineValueInProcess functionValue,
        string? functionRenderForError,
        IReadOnlyList<PineValueInProcess> newArguments,
        ApplicationContext callerEnv,
        Stack<Kont> kstack,
        IInvocationLogger invocationLogger)
    {
        if (newArguments.Count is 0)
            return new ApplyCallOutcome.ResolvedValue(functionValue);

        invocationLogger.OnFunctionValueApplication(functionValue, newArguments);

        if (functionValue is ElmRecordAccessChainInProcess recordAccessChain)
        {
            if (newArguments.Count is not 1)
            {
                return
                    MakeRuntimeError(
                        $"Cannot apply {RenderArgumentForError(recordAccessChain).rendered} to {newArguments.Count} argument(s) (expected 1)",
                        kstack);
            }

            var currentValue = newArguments[0];

            for (var i = 0; i < recordAccessChain.FieldNames.Length; i++)
            {
                try
                {
                    var fieldValue =
                        RecordAccess(
                            currentValue,
                            recordAccessChain.FieldNames[i].FieldName,
                            recordAccessChain.FieldNames[i].FieldNameValue);

                    currentValue = fieldValue;
                }
                catch (System.Exception ex)
                {
                    return MakeRuntimeError(ex.Message, kstack);
                }
            }

            return new ApplyCallOutcome.ResolvedValue(currentValue);
        }

        if (functionValue is ElmChoiceTagConstructorInProcess choiceTagConstructor)
        {
            var remainingArgs = choiceTagConstructor.TotalArity - choiceTagConstructor.Arguments.Count;

            if (newArguments.Count > remainingArgs)
            {
                return
                    MakeRuntimeError(
                        $"Too many arguments provided to choice tag constructor {choiceTagConstructor.TagName}: " +
                        $"expected {remainingArgs} but got {newArguments.Count}",
                        kstack);
            }

            var combinedArgs = choiceTagConstructor.Arguments.AddRange(newArguments);

            remainingArgs = choiceTagConstructor.TotalArity - combinedArgs.Count;

            if (remainingArgs is 0)
            {
                var finalValue =
                    BuildTaggedValue(
                        PineValueInProcess.Create(StringEncoding.ValueFromString(choiceTagConstructor.TagName)),
                        combinedArgs);

                return new ApplyCallOutcome.ResolvedValue(finalValue);
            }

            if (remainingArgs < 0)
            {
                return
                    MakeRuntimeError(
                        $"Too many arguments provided to choice tag constructor {choiceTagConstructor.TagName}: " +
                        $"expected {remainingArgs} but got {newArguments.Count}",
                        kstack);
            }

            return
                new ApplyCallOutcome.ResolvedValue(
                    choiceTagConstructor.WithArgumentsApplied(newArguments));
        }

        if (functionValue is ElmRecordTypeConstructorInProcess recordTypeConstructor)
        {
            var remainingArgs = recordTypeConstructor.FieldNames.Length - recordTypeConstructor.Arguments.Count;

            if (newArguments.Count > remainingArgs)
            {
                return
                    MakeRuntimeError(
                        $"Too many arguments provided to record type constructor {recordTypeConstructor.TypeName}: " +
                        $"expected {remainingArgs} but got {newArguments.Count}",
                        kstack);
            }

            var combinedArgs = recordTypeConstructor.Arguments.AddRange(newArguments);

            remainingArgs = recordTypeConstructor.FieldNames.Length - combinedArgs.Count;

            if (remainingArgs is 0)
            {
                var fields =
                    recordTypeConstructor.FieldNames
                    .Select((name, i) => (name.FieldName, name.FieldNameValue, combinedArgs[i]))
                    .OrderBy(f => f.FieldName, System.StringComparer.Ordinal);

                var finalValue =
                    BuildRecordValue([.. fields]);

                return new ApplyCallOutcome.ResolvedValue(finalValue);
            }

            if (remainingArgs < 0)
            {
                return
                    MakeRuntimeError(
                        $"Too many arguments provided to record type constructor {recordTypeConstructor.TypeName}: " +
                        $"expected {remainingArgs} but got {newArguments.Count}",
                        kstack);
            }

            return
                new ApplyCallOutcome.ResolvedValue(
                    recordTypeConstructor.WithArgumentsApplied(newArguments));
        }

        if (functionValue is not ElmClosureInProcess closure)
        {
            if (newArguments.Count is 0)
            {
                // Nothing to apply — pass the value through. This case shouldn't be reachable
                // from the public eval paths but the explicit branch keeps the helper total.
                return new ApplyCallOutcome.ResolvedValue(functionValue);
            }

            var rendered =
                RenderArgumentForError(functionValue).rendered;

            var nameSuffix =
                functionRenderForError is null
                ?
                ""
                :
                " bound to '" + functionRenderForError + "'";

            return
                MakeRuntimeError(
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
        var combined = new PineValueInProcess[combinedCount];

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
                    closure.With(combined));
        }

        // Either saturated or over-applied: split.
        PineValueInProcess[] saturatingArgs;
        PineValueInProcess[]? extraArgs;

        if (combinedCount > arity)
        {
            saturatingArgs = new PineValueInProcess[arity];
            extraArgs = new PineValueInProcess[combinedCount - arity];

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
        var bodyBindings = new Dictionary<string, PineValueInProcess>(closure.CapturedBindings);

        IReadOnlyList<ElmSyntaxAbstract.Pattern> parameterPatterns;
        ElmSyntaxAbstract.Expression bodyExpression;
        DeclQualifiedName callFrameName;
        object callFrameSourceIdentity;

        switch (closure.Source)
        {
            case ElmClosureInProcess.SourceRef.Declared declared:
                parameterPatterns = declared.Implementation.Arguments;
                bodyExpression = declared.Implementation.Expression;
                callFrameName = declared.Name;
                callFrameSourceIdentity = declared.Implementation;
                break;

            case ElmClosureInProcess.SourceRef.Lambda lambdaSource:
                parameterPatterns = lambdaSource.LambdaExpression.Arguments;
                bodyExpression = lambdaSource.LambdaExpression.Expression;
                callFrameName = SyntheticLambdaName(lambdaSource.LambdaExpression, closure.CapturedTopLevel);
                callFrameSourceIdentity = lambdaSource.LambdaExpression;
                break;

            default:
                throw new System.NotImplementedException(
                    "Unknown ElmClosureInProcess.SourceRef shape: " + closure.Source.GetType().FullName);
        }

        for (var i = 0; i < arity; i++)
        {
            try
            {
                var patternBindings = new Dictionary<string, PineValueInProcess>();

                BindPattern(
                    parameterPatterns[i],
                    saturatingArgs[i],
                    patternBindings);

                foreach (var (boundName, boundValue) in patternBindings)
                {
                    bodyBindings[boundName] = boundValue;
                }
            }
            catch (System.Exception ex)
            {
                return MakeRuntimeError(ex.Message, kstack);
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
                callFrameSourceIdentity,
                saturatingArgs));

        if (invocationLogger.IncrementUserCallDepth() % InfiniteRecursionCheckInterval is 0)
        {
            if (CheckForInfiniteRecursion(kstack) is { } error)
                return error;
        }

        return
            new ApplyCallOutcome.ContinueEvaluating(
                Expression: bodyExpression,
                Env: innerContext);
    }

    /// <summary>
    /// Builds a synthetic <see cref="DeclQualifiedName"/> for a lambda's <see cref="Kont.CallFrame"/>.
    /// The abstract syntax model carries no source location, so the lambda is named
    /// <c>&lt;lambda&gt;</c> qualified by <paramref name="containingDeclaration"/> — the top-level
    /// declaration the lambda was reached from — so that stack traces make clear which declaration
    /// the anonymous function originated in (e.g. <c>LanguageService.listDeclarationsInDeclaration.&lt;lambda&gt;</c>).
    /// The infinite-recursion detector relies on reference identity of the lambda AST node rather
    /// than this name, so the synthetic name does not cause false positives.
    /// </summary>
    private static DeclQualifiedName SyntheticLambdaName(
        ElmSyntaxAbstract.Expression.LambdaExpression lambda,
        DeclQualifiedName containingDeclaration)
    {
        _ = lambda;

        return containingDeclaration.ContainedDeclName("<lambda>");
    }

    /// <summary>
    /// Snapshots the local-binding environment as an immutable dictionary suitable for capture
    /// inside an <see cref="ElmClosureInProcess"/> closure. Avoids accidental aliasing of the
    /// caller's mutable <c>Dictionary&lt;string, ElmValue&gt;</c> instances.
    /// </summary>
    private static IReadOnlyDictionary<string, PineValueInProcess> SnapshotBindings(
        IReadOnlyDictionary<string, PineValueInProcess> bindings)
    {
        if (bindings.Count is 0)
            return ImmutableDictionary<string, PineValueInProcess>.Empty;

        if (bindings is ImmutableDictionary<string, PineValueInProcess> alreadyImmutable)
            return alreadyImmutable;

        return bindings.ToImmutableDictionary();
    }

    /// <summary>
    /// Walks the kont stack looking for two <see cref="Kont.CallFrame"/> entries with the same
    /// <see cref="Kont.CallFrame.SourceIdentity"/> (compared by reference) and structurally
    /// equal <see cref="Kont.CallFrame.Arguments"/>. If such a pair is found, infinite
    /// recursion is raised as an <see cref="ElmInterpretationException"/> whose
    /// <see cref="ElmInterpretationError.CallStack"/> is the stack truncated to the first
    /// cycle: the innermost (just-pushed) frame at index 0, followed by the frames between
    /// it and the older duplicate, followed by the older duplicate itself.
    /// <para>
    /// Comparing by reference identity of the source AST node — not by the rendered
    /// <see cref="Kont.CallFrame.FunctionName"/> — avoids false positives when two distinct
    /// anonymous lambdas happen to share a synthetic name (e.g. <c>ParserFast</c> combinator
    /// chains where every lambda's <c>BackslashLocation</c> collapses to row 1, column 1
    /// because the generated source has no line information). A genuine self-recursive
    /// call always re-uses the same AST node and is detected as before.
    /// </para>
    /// </summary>
    private static ElmInterpretationError? CheckForInfiniteRecursion(Stack<Kont> kstack)
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
            return null;

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

            if (ReferenceEquals(callFrame.SourceIdentity, topCallFrame.SourceIdentity) &&
                ArgumentListsEqual(callFrame.Arguments, topCallFrame.Arguments))
            {
                return
                    new ElmInterpretationError(
                        "Infinite recursion detected: the call stack contains a repeated (function, arguments) pair.",
                        truncatedStack);
            }
        }

        return null;
    }

    private static bool ArgumentListsEqual(
        IReadOnlyList<PineValueInProcess> left,
        IReadOnlyList<PineValueInProcess> right)
    {
        if (left.Count != right.Count)
            return false;

        for (var i = 0; i < left.Count; i++)
        {
            if (!ValuesEqualInProcess(left[i], right[i]))
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
    /// <remarks>
    /// <paramref name="decls"/> is the topologically-sorted list of <b>non-function</b> let
    /// bindings (zero-arg <c>LetFunction</c> + <c>LetDestructuring</c>) produced by
    /// <see cref="PrepareLetGroupAndSortNonFunctionDecls"/>. Parameterised
    /// <c>LetFunction</c> bindings have already been materialised as closures in
    /// <paramref name="extended"/> by that helper and are not present in <paramref name="decls"/>.
    /// </remarks>
    private static Result<ElmInterpretationError, (ElmSyntaxAbstract.Expression Expr, ApplicationContext Env)> BeginNextLetDecl(
        IReadOnlyList<ElmSyntaxAbstract.LetDeclaration> decls,
        int nextIndex,
        Dictionary<string, PineValueInProcess> extended,
        ElmSyntaxAbstract.Expression body,
        ApplicationContext outerEnv,
        Stack<Kont> kstack)
    {
        while (nextIndex < decls.Count)
        {
            var declNode = decls[nextIndex];

            switch (declNode)
            {
                case ElmSyntaxAbstract.LetDeclaration.LetFunction letFunction:
                    {
                        var functionImpl = letFunction.Function.Declaration;

                        // Parameterised LetFunction bindings should have been pre-built as
                        // closures in `extended` by PrepareLetGroupAndSortNonFunctionDecls.
                        // Reaching this branch with a non-empty argument list indicates a
                        // bug in the let-group setup.
                        if (functionImpl.Arguments.Count is not 0)
                        {
                            return
                                MakeRuntimeError(
                                    "Internal interpreter error: parameterised LetFunction reached "
                                    + "non-function evaluation path.",
                                    kstack);
                        }

                        var innerEnv =
                            new ApplicationContext(
                                CurrentTopLevel: outerEnv.CurrentTopLevel,
                                LocalBindings: extended);

                        kstack.Push(
                            new Kont.LetBindFunction(
                                BindingName: functionImpl.Name,
                                Remaining: decls,
                                NextIndex: nextIndex,
                                Extended: extended,
                                Body: body,
                                Outer: outerEnv));

                        return (functionImpl.Expression, innerEnv);
                    }

                case ElmSyntaxAbstract.LetDeclaration.LetDestructuring letDestructuring:
                    {
                        var innerEnv =
                            new ApplicationContext(
                                CurrentTopLevel: outerEnv.CurrentTopLevel,
                                LocalBindings: extended);

                        kstack.Push(
                            new Kont.LetBindDestructure(
                                Pattern: letDestructuring.Pattern,
                                Remaining: decls,
                                NextIndex: nextIndex,
                                Extended: extended,
                                Body: body,
                                Outer: outerEnv));

                        return (letDestructuring.Expression, innerEnv);
                    }

                default:
                    throw new System.NotImplementedException(
                        "Let declaration type not implemented: " + declNode.GetType().FullName);
            }
        }

        // All declarations processed: evaluate the body under the extended environment.
        var bodyEnv =
            new ApplicationContext(
                CurrentTopLevel: outerEnv.CurrentTopLevel,
                LocalBindings: extended);

        return (body, bodyEnv);
    }

    /// <summary>
    /// Prepares a <c>let</c> binding group for evaluation under Elm's letrec semantics:
    /// <list type="number">
    ///   <item>
    ///   For every parameterised <see cref="SyntaxModel.Expression.LetDeclaration.LetFunction"/>,
    ///   immediately materialises an <see cref="ElmClosureInProcess"/> closure into
    ///   <paramref name="extended"/>. The closure captures the same mutable
    ///   <paramref name="extended"/> dictionary used by the rest of the let group; closures
    ///   that are invoked after evaluation of the let group has completed therefore see all
    ///   sibling bindings, supporting full mutual recursion between let-bound functions and
    ///   forward references from any sibling into a function binding.
    ///   </item>
    ///   <item>
    ///   Computes the dependency graph among the remaining non-function bindings (zero-arg
    ///   <see cref="SyntaxModel.Expression.LetDeclaration.LetFunction"/> and every
    ///   <see cref="SyntaxModel.Expression.LetDeclaration.LetDestructuring"/>) and returns
    ///   them in topological order. A cycle in those non-function dependencies surfaces as
    ///   a runtime error (<c>"Cyclic let binding"</c>) — this matches Elm's behaviour where
    ///   only function bindings can participate in mutual recursion.
    ///   </item>
    /// </list>
    /// </summary>
    private static Result<ElmInterpretationError, IReadOnlyList<ElmSyntaxAbstract.LetDeclaration>>
        PrepareLetGroupAndSortNonFunctionDecls(
        IReadOnlyList<ElmSyntaxAbstract.LetDeclaration> decls,
        Dictionary<string, PineValueInProcess> extended,
        DeclQualifiedName outerTopLevel,
        Stack<Kont> kstack)
    {
        // Collect, per declaration, the set of names it introduces.
        var introducedByDecl = new HashSet<string>[decls.Count];
        var letGroupNames = new HashSet<string>();

        for (var i = 0; i < decls.Count; i++)
        {
            var introduced = new HashSet<string>();

            switch (decls[i])
            {
                case ElmSyntaxAbstract.LetDeclaration.LetFunction letFunction:
                    introduced.Add(letFunction.Function.Declaration.Name);
                    break;

                case ElmSyntaxAbstract.LetDeclaration.LetDestructuring letDestructuring:
                    CollectPatternNames(letDestructuring.Pattern, introduced);
                    break;
            }

            introducedByDecl[i] = introduced;

            foreach (var name in introduced)
            {
                letGroupNames.Add(name);
            }
        }

        // Pre-build closures for every parameterised LetFunction. These do not participate
        // in the dependency analysis below; their bodies are not evaluated here.
        var nonFunctionDecls =
            new List<ElmSyntaxAbstract.LetDeclaration>(decls.Count);

        var nonFunctionIntroduced = new List<HashSet<string>>(decls.Count);

        for (var i = 0; i < decls.Count; i++)
        {
            if (decls[i] is ElmSyntaxAbstract.LetDeclaration.LetFunction letFunction
                && letFunction.Function.Declaration.Arguments.Count is not 0)
            {
                var functionImpl = letFunction.Function.Declaration;

                var bindingName = functionImpl.Name;

                // Synthesise a DeclQualifiedName for the closure so over-application,
                // stack traces, and the infinite-recursion detector all surface a useful
                // identifier. The name is namespaced under the surrounding top-level
                // declaration so two let-bound functions of the same simple name from
                // different surrounding functions don't accidentally compare equal.
                var qualifiedName =
                    DeclQualifiedName.Create(
                        namespaces: [.. outerTopLevel.Namespaces, outerTopLevel.DeclName, "<let>"],
                        declName: bindingName);

                extended[bindingName] =
                    new ElmClosureInProcess(
                        source:
                        new ElmClosureInProcess.SourceRef.Declared(
                            Name: qualifiedName,
                            Implementation: functionImpl),
                        parameterCount: functionImpl.Arguments.Count,
                        argumentsAlreadyCollected: [],
                        // Capture `extended` itself (not a snapshot): future additions to the
                        // dictionary as the let group is populated will be visible to the
                        // closure when it is later invoked.
                        capturedBindings: extended,
                        capturedTopLevel: outerTopLevel);

                continue;
            }

            nonFunctionDecls.Add(decls[i]);
            nonFunctionIntroduced.Add(introducedByDecl[i]);
        }

        if (nonFunctionDecls.Count is 0)
            return nonFunctionDecls;

        // Build the dependency graph among non-function bindings: index i depends on
        // index j when binding i's RHS contains a free reference to a name introduced
        // by binding j. References to names already in `extended` (function closures
        // and outer bindings) are not considered dependencies — those are resolvable
        // immediately at evaluation time.
        var nonFunctionNameToIndex = new Dictionary<string, int>();

        for (var i = 0; i < nonFunctionDecls.Count; i++)
        {
            foreach (var name in nonFunctionIntroduced[i])
            {
                nonFunctionNameToIndex[name] = i;
            }
        }

        var dependencies = new HashSet<int>[nonFunctionDecls.Count];

        for (var i = 0; i < nonFunctionDecls.Count; i++)
        {
            var freeNames = new HashSet<string>();

            var rhs =
                nonFunctionDecls[i] switch
                {
                    ElmSyntaxAbstract.LetDeclaration.LetFunction lf =>
                    lf.Function.Declaration.Expression,

                    ElmSyntaxAbstract.LetDeclaration.LetDestructuring ld =>
                    ld.Expression,

                    _ =>
                    throw new System.InvalidOperationException(
                        "Unexpected let declaration shape in non-function decl list: "
                        + nonFunctionDecls[i].GetType().FullName),
                };

            CollectFreeNames(rhs, shadowed: [], free: freeNames);

            var deps = new HashSet<int>();

            foreach (var name in freeNames)
            {
                if (nonFunctionNameToIndex.TryGetValue(name, out var depIndex)
                    && depIndex != i)
                {
                    deps.Add(depIndex);
                }
            }

            dependencies[i] = deps;
        }

        // Topological sort via DFS, with cycle detection. Tracks visit state per node.
        const byte StateUnvisited = 0;
        const byte StateInProgress = 1;
        const byte StateDone = 2;

        var state = new byte[nonFunctionDecls.Count];

        var sorted =
            new List<ElmSyntaxAbstract.LetDeclaration>(nonFunctionDecls.Count);

        // Iterative DFS to avoid blowing the .NET call stack for large let groups.
        var dfsStack = new Stack<(int node, IEnumerator<int> deps)>();

        for (var start = 0; start < nonFunctionDecls.Count; start++)
        {
            if (state[start] is not StateUnvisited)
                continue;

            state[start] = StateInProgress;
            dfsStack.Push((start, dependencies[start].GetEnumerator()));

            try
            {
                while (dfsStack.Count > 0)
                {
                    var (node, depsEnum) = dfsStack.Peek();

                    if (depsEnum.MoveNext())
                    {
                        var dep = depsEnum.Current;

                        if (state[dep] is StateInProgress)
                        {
                            // Reconstruct the cycle path by walking back through the DFS
                            // stack from the current node down to the back-edge target,
                            // then closing the loop. This surfaces the full cycle for
                            // diagnostics rather than only the two adjacent nodes.
                            var stackFrames = dfsStack.ToArray(); // top-most first
                            var pathNames = new List<string>();
                            var includeFromHere = false;

                            for (var i = 0; i < stackFrames.Length; i++)
                            {
                                var frameNode = stackFrames[i].node;

                                if (frameNode == dep)
                                    includeFromHere = true;

                                if (includeFromHere)
                                {
                                    pathNames.Add(
                                        string.Join("/", nonFunctionIntroduced[frameNode]));
                                }
                            }

                            // Reverse so the path reads "dep -> ... -> node -> dep".
                            pathNames.Reverse();
                            pathNames.Add(string.Join("/", nonFunctionIntroduced[dep]));

                            return
                                MakeRuntimeError(
                                    "Cyclic let binding detected among non-function bindings: "
                                    + string.Join(" -> ", pathNames),
                                    kstack);
                        }

                        if (state[dep] is StateUnvisited)
                        {
                            state[dep] = StateInProgress;
                            dfsStack.Push((dep, dependencies[dep].GetEnumerator()));
                        }
                    }
                    else
                    {
                        state[node] = StateDone;
                        sorted.Add(nonFunctionDecls[node]);
                        dfsStack.Pop();
                        depsEnum.Dispose();
                    }
                }
            }
            catch
            {
                // Dispose any enumerators still on the stack to avoid leaking IDisposable
                // references on the abnormal-exit path (cycle detection raises here).
                while (dfsStack.Count > 0)
                {
                    dfsStack.Pop().deps.Dispose();
                }

                throw;
            }
        }

        return sorted;
    }

    /// <summary>
    /// Collects the names introduced by an Elm pattern into <paramref name="sink"/>.
    /// </summary>
    private static void CollectPatternNames(
        ElmSyntaxAbstract.Pattern pattern,
        HashSet<string> sink)
    {
        switch (pattern)
        {
            case ElmSyntaxAbstract.Pattern.VarPattern varPattern:
                sink.Add(varPattern.Name);
                break;

            case ElmSyntaxAbstract.Pattern.AllPattern:
            case ElmSyntaxAbstract.Pattern.UnitPattern:
            case ElmSyntaxAbstract.Pattern.CharPattern:
            case ElmSyntaxAbstract.Pattern.StringPattern:
            case ElmSyntaxAbstract.Pattern.IntPattern:
            case ElmSyntaxAbstract.Pattern.FloatPattern:
                break;

            case ElmSyntaxAbstract.Pattern.AsPattern asPattern:
                CollectPatternNames(asPattern.Pattern, sink);
                sink.Add(asPattern.Name);
                break;

            case ElmSyntaxAbstract.Pattern.TuplePattern tuplePattern:
                foreach (var element in tuplePattern.Elements)
                    CollectPatternNames(element, sink);

                break;

            case ElmSyntaxAbstract.Pattern.RecordPattern recordPattern:
                foreach (var field in recordPattern.Fields)
                    sink.Add(field.FieldName);

                break;

            case ElmSyntaxAbstract.Pattern.UnConsPattern unCons:
                CollectPatternNames(unCons.Head, sink);
                CollectPatternNames(unCons.Tail, sink);
                break;

            case ElmSyntaxAbstract.Pattern.ListPattern listPattern:
                foreach (var element in listPattern.Elements)
                    CollectPatternNames(element, sink);

                break;

            case ElmSyntaxAbstract.Pattern.NamedPattern namedPattern:
                foreach (var argument in namedPattern.Arguments)
                    CollectPatternNames(argument, sink);

                break;

            default:
                throw new System.NotImplementedException(
                    "Pattern type not implemented in CollectPatternNames: "
                    + pattern.GetType().FullName);
        }
    }

    /// <summary>
    /// Collects bare-name references (i.e. <see cref="SyntaxModel.Expression.FunctionOrValue"/>
    /// nodes with empty module name) appearing free in <paramref name="expression"/> into
    /// <paramref name="free"/>. Names introduced by lambdas, case alternatives, nested let
    /// blocks, or function-parameter patterns shadow outer references and are tracked through
    /// <paramref name="shadowed"/>.
    /// </summary>
    /// <remarks>
    /// Used by <see cref="PrepareLetGroupAndSortNonFunctionDecls"/> to compute the
    /// dependency graph among non-function let bindings. The intent is conservative
    /// over-approximation: it is fine to report a name that is not actually used at
    /// runtime as a dependency, because that just constrains the topological order
    /// — but it must never miss a name that <em>is</em> referenced.
    /// </remarks>
    private static void CollectFreeNames(
        ElmSyntaxAbstract.Expression expression,
        HashSet<string> shadowed,
        HashSet<string> free)
    {
        switch (expression)
        {
            case ElmSyntaxAbstract.Expression.UnitExpr:
            case ElmSyntaxAbstract.Expression.StringLiteral:
            case ElmSyntaxAbstract.Expression.CharLiteral:
            case ElmSyntaxAbstract.Expression.Integer:
            case ElmSyntaxAbstract.Expression.FloatLiteral:
            case ElmSyntaxAbstract.Expression.PrefixOperator:
            case ElmSyntaxAbstract.Expression.RecordAccessFunction:
            case ElmSyntaxAbstract.Expression.GLSLExpression:
                break;

            case ElmSyntaxAbstract.Expression.Negation negation:
                CollectFreeNames(negation.Expression, shadowed, free);
                break;

            case ElmSyntaxAbstract.Expression.ListExpr listExpr:
                foreach (var element in listExpr.Elements)
                    CollectFreeNames(element, shadowed, free);

                break;

            case ElmSyntaxAbstract.Expression.TupledExpression tupledExpression:
                foreach (var element in tupledExpression.Elements)
                    CollectFreeNames(element, shadowed, free);

                break;

            case ElmSyntaxAbstract.Expression.IfBlock ifBlock:
                CollectFreeNames(ifBlock.Condition, shadowed, free);
                CollectFreeNames(ifBlock.ThenBlock, shadowed, free);
                CollectFreeNames(ifBlock.ElseBlock, shadowed, free);
                break;

            case ElmSyntaxAbstract.Expression.RecordExpr recordExpr:
                foreach (var field in recordExpr.Fields)
                    CollectFreeNames(field.Value, shadowed, free);

                break;

            case ElmSyntaxAbstract.Expression.FunctionOrValue functionOrValue:
                if (functionOrValue.ModuleName.Count is 0
                    && !shadowed.Contains(functionOrValue.Name))
                {
                    free.Add(functionOrValue.Name);
                }

                break;

            case ElmSyntaxAbstract.Expression.Application application:
                CollectFreeNames(application.Function, shadowed, free);

                foreach (var argument in application.Arguments)
                    CollectFreeNames(argument, shadowed, free);

                break;

            case ElmSyntaxAbstract.Expression.OperatorApplication operatorApplication:

                // The operator itself resolves through the infix-operator map to a
                // top-level declaration, never to a let-group binding, so it is not
                // a candidate for the local-shadowing analysis.
                CollectFreeNames(operatorApplication.Left, shadowed, free);
                CollectFreeNames(operatorApplication.Right, shadowed, free);
                break;

            case ElmSyntaxAbstract.Expression.LambdaExpression lambdaExpression:
                {
                    var inner = new HashSet<string>(shadowed);

                    foreach (var arg in lambdaExpression.Arguments)
                        CollectPatternNames(arg, inner);

                    CollectFreeNames(lambdaExpression.Expression, inner, free);
                    break;
                }

            case ElmSyntaxAbstract.Expression.CaseExpression caseExpression:
                {
                    CollectFreeNames(caseExpression.Expression, shadowed, free);

                    foreach (var caseNode in caseExpression.Cases)
                    {
                        var inner = new HashSet<string>(shadowed);
                        CollectPatternNames(caseNode.Pattern, inner);
                        CollectFreeNames(caseNode.Expression, inner, free);
                    }

                    break;
                }

            case ElmSyntaxAbstract.Expression.LetExpression letExpression:
                {
                    // Names introduced by the nested let block shadow the outer scope
                    // for the purposes of free-name analysis (whether they are
                    // function bindings or value bindings).
                    var inner = new HashSet<string>(shadowed);

                    foreach (var declNode in letExpression.Declarations)
                    {
                        switch (declNode)
                        {
                            case ElmSyntaxAbstract.LetDeclaration.LetFunction nestedLet:
                                inner.Add(nestedLet.Function.Declaration.Name);
                                break;

                            case ElmSyntaxAbstract.LetDeclaration.LetDestructuring nestedDestr:
                                CollectPatternNames(nestedDestr.Pattern, inner);
                                break;
                        }
                    }

                    foreach (var declNode in letExpression.Declarations)
                    {
                        switch (declNode)
                        {
                            case ElmSyntaxAbstract.LetDeclaration.LetFunction nestedLet:
                                {
                                    // The function's own arguments shadow further inside its body.
                                    var bodyShadowed = new HashSet<string>(inner);

                                    foreach (var arg in nestedLet.Function.Declaration.Arguments)
                                        CollectPatternNames(arg, bodyShadowed);

                                    CollectFreeNames(
                                        nestedLet.Function.Declaration.Expression,
                                        bodyShadowed,
                                        free);

                                    break;
                                }

                            case ElmSyntaxAbstract.LetDeclaration.LetDestructuring nestedDestr:
                                CollectFreeNames(nestedDestr.Expression, inner, free);
                                break;
                        }
                    }

                    CollectFreeNames(letExpression.Expression, inner, free);
                    break;
                }

            case ElmSyntaxAbstract.Expression.RecordAccess recordAccess:
                CollectFreeNames(recordAccess.Record, shadowed, free);
                break;

            case ElmSyntaxAbstract.Expression.RecordUpdateExpression recordUpdate:
                if (!shadowed.Contains(recordUpdate.RecordName))
                    free.Add(recordUpdate.RecordName);

                foreach (var field in recordUpdate.Fields)
                    CollectFreeNames(field.Value, shadowed, free);

                break;

            default:

                // Unknown expression shape: be conservative and don't crash. Any names
                // missed here may cause spurious "Cyclic let binding" or unresolved
                // references — both of which are loud failure modes preferable to
                // silent miscompilation, and would be straightforward to fix by
                // extending this switch.
                throw new System.NotImplementedException(
                    "Expression type not implemented in CollectFreeNames: "
                    + expression.GetType().FullName);
        }
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
    /// Constructs an <see cref="ElmInterpretationError"/> with the Elm call stack captured from the current
    /// state of <paramref name="kstack"/>. The trampoline uses this to unwind through arbitrary
    /// C# frames (including user resolver callbacks); the exception is caught at the public
    /// boundary and surfaced as <see cref="Result{ErrT, OkT}.Err"/>.
    /// </summary>
    private static ElmInterpretationError MakeRuntimeError(
        string message,
        Stack<Kont> kstack)
    {
        return new ElmInterpretationError(message, SnapshotCallStack(kstack));
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
        ElmSyntaxAbstract.Pattern pattern,
        PineValueInProcess value,
        IDictionary<string, PineValueInProcess> bindings)
    {
        switch (pattern)
        {
            case ElmSyntaxAbstract.Pattern.AllPattern:
                return true;

            case ElmSyntaxAbstract.Pattern.VarPattern varPattern:
                bindings[varPattern.Name] = value;
                return true;

            case ElmSyntaxAbstract.Pattern.UnitPattern:

                // The unit value is represented as an empty Elm list.
                return PineValueInProcess.AreEqual(value, PineValueInProcess.EmptyList);

            case ElmSyntaxAbstract.Pattern.AsPattern asPattern:
                {
                    if (!TryMatchPattern(asPattern.Pattern, value, bindings))
                        return false;

                    bindings[asPattern.Name] = value;
                    return true;
                }

            case ElmSyntaxAbstract.Pattern.CharPattern charPattern:
                return PineValueInProcess.AreEqual(value, charPattern.ValueAsPineValue);

            case ElmSyntaxAbstract.Pattern.StringPattern stringPattern:
                return PineValueInProcess.AreEqual(value, stringPattern.ValueAsPineValue);

            case ElmSyntaxAbstract.Pattern.IntPattern intPattern:
                return PineValueInProcess.AreEqual(value, intPattern.ValueAsPineValue);

            case ElmSyntaxAbstract.Pattern.TuplePattern tuplePattern:
                {
                    if (!value.IsList())
                        return false;

                    var elementPatterns = tuplePattern.Elements;

                    for (var i = 0; i < elementPatterns.Count; i++)
                    {
                        if (!TryMatchPattern(elementPatterns[i], value.GetElementAt(i), bindings))
                            return false;
                    }

                    return true;
                }

            case ElmSyntaxAbstract.Pattern.RecordPattern recordPattern:
                {
                    var firstItem = value.GetElementAt(0);

                    if (!PineValueInProcess.AreEqual(firstItem, s_elmRecordTypeTagNameAsValueInProcess))
                        return false;

                    foreach (var field in recordPattern.Fields)
                    {
                        var fieldValue = RecordAccess(value, field.FieldName, field.FieldNameValue);

                        if (fieldValue is null)
                            return false;

                        bindings[field.FieldName] = fieldValue;
                    }

                    return true;
                }

            case ElmSyntaxAbstract.Pattern.UnConsPattern unConsPattern:
                {
                    if (!value.IsList())
                        return false;

                    if (value.GetLength() is 0)
                        return false;

                    var head = value.GetElementAt(0);
                    var tailItems = new PineValueInProcess[value.GetLength() - 1];

                    for (var i = 1; i < value.GetLength(); i++)
                        tailItems[i - 1] = value.GetElementAt(i);

                    var tail = PineValueInProcess.CreateList(tailItems);

                    if (!TryMatchPattern(unConsPattern.Head, head, bindings))
                        return false;

                    return TryMatchPattern(unConsPattern.Tail, tail, bindings);
                }

            case ElmSyntaxAbstract.Pattern.ListPattern listPattern:
                {
                    if (!value.IsList())
                        return false;

                    var itemPatterns = listPattern.Elements;

                    if (value.GetLength() != itemPatterns.Count)
                        return false;

                    for (var i = 0; i < itemPatterns.Count; i++)
                    {
                        if (!TryMatchPattern(itemPatterns[i], value.GetElementAt(i), bindings))
                            return false;
                    }

                    return true;
                }

            case ElmSyntaxAbstract.Pattern.NamedPattern namedPattern:
                {
                    if (namedPattern.Name.Name is "True")
                        return PineValueInProcess.AreEqual(value, PineValueInProcess.KernelTrueValue);

                    if (namedPattern.Name.Name is "False")
                        return PineValueInProcess.AreEqual(value, PineValueInProcess.KernelFalseValue);

                    var tagEncoded = StringEncoding.ValueFromString(namedPattern.Name.Name);

                    var firstItem = value.GetElementAt(0);

                    if (!PineValueInProcess.AreEqual(firstItem, tagEncoded))
                    {
                        return false;
                    }

                    var tagArguments = value.GetElementAt(1);

                    var tagArgumentsLength = tagArguments.GetLength();

                    if (namedPattern.Arguments.Count != tagArgumentsLength)
                    {
                        throw new System.InvalidOperationException(
                            "Named pattern '"
                            + namedPattern.Name.Name
                            + "' has "
                            + namedPattern.Arguments.Count
                            + " arguments, but tag value has "
                            + tagArgumentsLength + ".");
                    }

                    for (var i = 0; i < namedPattern.Arguments.Count; i++)
                    {
                        var argumentValue = tagArguments.GetElementAt(i);

                        if (!TryMatchPattern(namedPattern.Arguments[i], argumentValue, bindings))
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
        ElmSyntaxAbstract.Pattern pattern,
        PineValueInProcess value,
        IDictionary<string, PineValueInProcess> bindings)
    {
        switch (pattern)
        {
            case ElmSyntaxAbstract.Pattern.VarPattern varPattern:
                bindings[varPattern.Name] = value;
                break;

            case ElmSyntaxAbstract.Pattern.AllPattern:
                break;

            case ElmSyntaxAbstract.Pattern.UnitPattern:
                break;

            case ElmSyntaxAbstract.Pattern.AsPattern asPattern:
                BindPattern(asPattern.Pattern, value, bindings);
                bindings[asPattern.Name] = value;
                break;

            case ElmSyntaxAbstract.Pattern.TuplePattern tuplePattern:
                {
                    var itemPatterns = tuplePattern.Elements;

                    for (var i = 0; i < itemPatterns.Count; i++)
                    {
                        var itemValue = value.GetElementAt(i);

                        BindPattern(itemPatterns[i], itemValue, bindings);
                    }

                    break;
                }

            case ElmSyntaxAbstract.Pattern.ListPattern listPattern:
                {
                    var itemPatterns = listPattern.Elements;

                    for (var i = 0; i < itemPatterns.Count; i++)
                    {
                        var itemValue = value.GetElementAt(i);
                        BindPattern(itemPatterns[i], itemValue, bindings);
                    }

                    break;
                }

            case ElmSyntaxAbstract.Pattern.UnConsPattern unConsPattern:
                {
                    var headValue = value.GetElementAt(0);

                    var tailItems = new PineValueInProcess[value.GetLength() - 1];

                    for (var i = 1; i < value.GetLength(); i++)
                        tailItems[i - 1] = value.GetElementAt(i);

                    var tailValue = PineValueInProcess.CreateList(tailItems);

                    BindPattern(unConsPattern.Head, headValue, bindings);
                    BindPattern(unConsPattern.Tail, tailValue, bindings);

                    break;
                }

            case ElmSyntaxAbstract.Pattern.RecordPattern recordPattern:
                {
                    foreach (var field in recordPattern.Fields)
                    {
                        var fieldValue =
                            RecordAccess(
                                value,
                                fieldName: field.FieldName,
                                fieldNameValue: field.FieldNameValue);

                        if (fieldValue is null)
                        {
                            throw new System.InvalidOperationException(
                                "Record pattern references field '"
                                + field.FieldName
                                + "' which is not present on the value.");
                        }

                        bindings[field.FieldName] = fieldValue;
                    }

                    break;
                }

            case ElmSyntaxAbstract.Pattern.NamedPattern namedPattern:
                {
                    if (namedPattern.Name.Name is "True")
                        return;

                    if (namedPattern.Name.Name is "False")
                        return;

                    var tagEncoded = StringEncoding.ValueFromString(namedPattern.Name.Name);

                    var firstItem = value.GetElementAt(0);

                    if (!PineValueInProcess.AreEqual(firstItem, tagEncoded))
                    {
                        throw new System.InvalidOperationException(
                            "Cannot bind named pattern '"
                            + namedPattern.Name.Name
                            + "' to value of type "
                            + value.GetType().FullName + ".");
                    }

                    var tagArguments = value.GetElementAt(1);

                    var tagArgumentsLength = tagArguments.GetLength();

                    if (namedPattern.Arguments.Count != tagArgumentsLength)
                    {
                        throw new System.InvalidOperationException(
                            "Named pattern '"
                            + namedPattern.Name.Name
                            + "' has "
                            + namedPattern.Arguments.Count
                            + " arguments, but tag value has "
                            + tagArgumentsLength + ".");
                    }

                    for (var i = 0; i < namedPattern.Arguments.Count; i++)
                    {
                        BindPattern(namedPattern.Arguments[i], tagArguments.GetElementAt(i), bindings);
                    }

                    break;
                }

            default:
                throw new System.NotImplementedException(
                    "Binding pattern of type " + pattern.GetType().FullName + " is not implemented.");
        }
    }

    private static readonly FrozenSet<string> s_pineBuiltinModuleNamesDefault =
        FrozenSet.Create(["Pine_builtin", "Pine_kernel"]);

    /// <summary>
    /// Default <c>Pine_builtin</c> / <c>Pine_kernel</c> resolver: forwards applications whose
    /// module name is <c>Pine_builtin</c> or <c>Pine_kernel</c> to <see cref="KernelFunction.ApplyKernelFunctionGeneric(string, PineValue)"/>.
    /// </summary>
    public static ApplicationResolution? PineBuiltinResolver(
        Application application) =>
        PineBuiltinResolver(application, s_pineBuiltinModuleNamesDefault);

    public static System.Func<Application, ApplicationResolution?> ApplicationResolver(
        IReadOnlyDictionary<DeclQualifiedName, System.Func<ImmutableList<PineValueInProcess>, PineValueInProcess?>> functionResolvers)
    {
        ApplicationResolution? TryResolve(Application application)
        {
            if (functionResolvers.TryGetValue(application.FunctionName, out var resolver))
            {
                // A resolver returns null to signal that it does not handle this particular
                // application (for example a builtin that is under- or over-applied, so the
                // application must fall through to the regular currying / user-defined path).
                if (resolver(application.Arguments) is { } returnValue)
                {
                    return new ApplicationResolution.Resolved(returnValue);
                }
            }

            return null;
        }

        return TryResolve;
    }

    /// <summary>
    /// Resolver that forwards applications whose module name matches one of the supplied
    /// <paramref name="pineBuiltinPseudoModuleNames"/> to <see cref="KernelFunction.ApplyKernelFunctionGeneric(string, PineValue)"/>.
    /// Each Elm argument is encoded as a <see cref="PineValue"/> prior to the kernel invocation and the
    /// kernel's return value is decoded back to an <see cref="ElmValue"/>.
    /// </summary>
    public static ApplicationResolution? PineBuiltinResolver(
        Application application,
        IReadOnlySet<string> pineBuiltinPseudoModuleNames)
    {
        if (application.FunctionName.Namespaces.Count is not 1)
            return null;

        var moduleName = application.FunctionName.Namespaces[0];

        if (!pineBuiltinPseudoModuleNames.Contains(moduleName))
            return null;

        var functionName = application.FunctionName.DeclName;

        // Pine builtin/kernel functions receive a single argument;
        PineValueInProcess kernelInput;

        if (application.Arguments.Count is 1)
        {
            kernelInput = application.Arguments[0];
        }
        else
        {
            // Multiple arguments are illegal.

            throw new System.ArgumentException(
                "Unexpected number of arguments for Pine builtin/kernel function application. Function '" +
                application.FunctionName.FullName + "' received " + application.Arguments.Count + " arguments.");
        }

        var resultValue = ApplyPineBuiltinFunction(functionName, kernelInput);

        return new ApplicationResolution.Resolved(resultValue);
    }

    private static PineValueInProcess ApplyPineBuiltinFunction(
        string functionName,
        PineValueInProcess input)
    {
        if (functionName is nameof(KernelFunction.concat))
        {
            return PineValueInProcess.Concat(input);
        }

        if (functionName is nameof(KernelFunction.skip))
        {
            if (input.GetLength() is not 2)
            {
                return PineValueInProcess.EmptyList;
            }

            var countInt = input.GetElementAt(0).AsInteger();

            if (!countInt.HasValue)
            {
                return PineValueInProcess.EmptyList;
            }

            var count = countInt.Value;

            var source = input.GetElementAt(1);

            if (count < 0)
            {
                return source;
            }

            if (int.MaxValue < countInt.Value)
            {
                return
                    source.IsBlob()
                    ?
                    PineValueInProcess.EmptyBlob
                    :
                    PineValueInProcess.EmptyList;
            }

            return PineValueInProcess.Skip((int)count, source);
        }

        if (functionName is nameof(KernelFunction.take))
        {
            if (input.GetLength() is not 2)
            {
                return PineValueInProcess.EmptyList;
            }

            var countInt = input.GetElementAt(0).AsInteger();

            if (!countInt.HasValue)
            {
                return PineValueInProcess.EmptyList;
            }

            var count = countInt.Value;

            var source = input.GetElementAt(1);

            if (count <= 0)
            {
                return
                    source.IsBlob()
                    ?
                    PineValueInProcess.EmptyBlob
                    :
                    PineValueInProcess.EmptyList;
            }

            if (int.MaxValue < countInt.Value)
            {
                return source;
            }

            return PineValueInProcess.Take((int)count, source);
        }

        var resultPine = KernelFunction.ApplyKernelFunctionGeneric(functionName, input.Evaluate());

        return PineValueInProcess.Create(resultPine);
    }

    /// <summary>
    /// As <see cref="PineBuiltinResolver(Application)"/>, but additionally notifies the
    /// supplied <paramref name="invocationLogger"/> via
    /// <see cref="IInvocationLogger.OnPineBuiltinInvocation(Application)"/> on every successful
    /// resolution. Used by the *<c>WithCounters</c> public entry points to surface the
    /// number of <c>Pine_builtin</c> / <c>Pine_kernel</c> invocations.
    /// </summary>
    private static ApplicationResolution? PineBuiltinResolverCounting(
        Application application,
        IInvocationLogger invocationLogger)
    {
        var resolution = PineBuiltinResolver(application);

        if (resolution is not null)
        {
            invocationLogger.OnPineBuiltinInvocation(application);
        }

        return resolution;
    }

    /// <summary>
    /// Resolver that matches an application against the supplied user-level <paramref name="declarations"/>:
    /// top-level function declarations yield <see cref="ApplicationResolution.ContinueWithFunction"/>,
    /// record type alias constructors and choice type tag constructors with full application yield
    /// <see cref="ApplicationResolution.Resolved"/>. Returns <c>null</c> when no declaration matches.
    /// <para>
    /// An <em>unqualified</em> reference (empty <see cref="DeclQualifiedName.Namespaces"/>) is resolved
    /// with a preference for the caller's own module: the declaration whose namespaces equal the
    /// calling context's module (<see cref="Application.Context"/>'s <c>CurrentTopLevel.Namespaces</c>)
    /// is tried first, and only if no same-module declaration matches does resolution fall back to a
    /// module-agnostic search over every declaration. This matters for lambda-lifted helpers, which
    /// the compiler emits as unqualified call references (for example <c>map2__lifted__lambda1</c>)
    /// from their home module's body: several modules can define a helper with the same name but
    /// different behaviour (e.g. <c>ParserFast</c> and <c>Parser.Advanced</c> each define
    /// <c>map2__lifted__lambda1</c> whose body matches a <c>Good</c> constructor of different arity).
    /// Without the same-module preference a call would resolve to whichever module's helper appears
    /// first in dictionary iteration order, producing spurious "Case expression did not match any
    /// arm" failures when a value built by one module's constructor reaches the other module's helper.
    /// </para>
    /// </summary>
    public static ApplicationResolution? UserDefinedResolver(
        Application application,
        IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration> declarations) =>
        UserDefinedResolver(application, GetOrConvertDeclarations(declarations));

    /// <summary>
    /// Caches the conversion of a concrete declaration dictionary to its abstract
    /// (<see cref="ElmSyntaxAbstract.Declaration"/>) representation. The cache is keyed on the
    /// concrete dictionary instance so that repeated resolver invocations during a single
    /// interpretation reuse the <em>same</em> abstract <see cref="ElmSyntaxAbstract.FunctionImplementation"/>
    /// instances. This is required for the interpreter's recursion detection, which compares call
    /// frame source identities by reference.
    /// </summary>
    private static readonly System.Runtime.CompilerServices.ConditionalWeakTable<
        IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration>,
        IReadOnlyDictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration>>
        s_abstractDeclarationsCache =
        [];

    private static IReadOnlyDictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration> GetOrConvertDeclarations(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration> declarations) =>
        s_abstractDeclarationsCache.GetValue(
            declarations,
            static concrete =>
            {
                var converted =
                    new Dictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration>(concrete.Count);

                foreach (var (name, declaration) in concrete)
                {
                    converted[name] = ElmSyntaxAbstract.ConvertFromConcrete.FromDeclaration(declaration);
                }

                return converted;
            });

    /// <inheritdoc cref="UserDefinedResolver(Application, IReadOnlyDictionary{DeclQualifiedName, SyntaxModel.Declaration})"/>
    public static ApplicationResolution? UserDefinedResolver(
        Application application,
        IReadOnlyDictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration> declarations)
    {
        var requestedNamespaces = application.FunctionName.Namespaces;

        // For an unqualified reference, first restrict resolution to the caller's own module so
        // that a lambda-lifted (or otherwise same-named) helper resolves to the home-module
        // declaration rather than a colliding one in another module. Fall back to the
        // module-agnostic search only when the same-module pass finds nothing.
        if (requestedNamespaces.Count is 0)
        {
            var callerNamespaces = application.Context.CurrentTopLevel.Namespaces;

            if (callerNamespaces.Count is not 0)
            {
                var sameModuleResolution =
                    ResolveAgainstDeclarations(application, declarations, callerNamespaces);

                if (sameModuleResolution is not null)
                    return sameModuleResolution;
            }
        }

        return ResolveAgainstDeclarations(application, declarations, requiredNamespaces: null);
    }

    /// <summary>
    /// Core matching loop for <see cref="UserDefinedResolver(Application, IReadOnlyDictionary{DeclQualifiedName, ElmSyntaxAbstract.Declaration})"/>. When
    /// <paramref name="requiredNamespaces"/> is non-null, only declarations whose namespaces equal
    /// it are considered (used both for qualified references and for the same-module preference pass
    /// of unqualified references). When it is null, every declaration is considered (module-agnostic
    /// fallback).
    /// </summary>
    private static ApplicationResolution? ResolveAgainstDeclarations(
        Application application,
        IReadOnlyDictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration> declarations,
        IReadOnlyList<string>? requiredNamespaces)
    {
        var requestedName = application.FunctionName.DeclName;
        var requestedNamespaces = application.FunctionName.Namespaces;

        // A qualified reference must always match the declaration's namespaces exactly.
        var effectiveRequiredNamespaces =
            requestedNamespaces.Count is not 0 ? requestedNamespaces : requiredNamespaces;

        foreach (var (declName, declaration) in declarations)
        {
            if (effectiveRequiredNamespaces is not null
                && !NamespacesEqual(effectiveRequiredNamespaces, declName.Namespaces))
            {
                continue;
            }

            switch (declaration)
            {
                case ElmSyntaxAbstract.Declaration.FunctionDeclaration functionDeclaration
                when functionDeclaration.Function.Declaration.Name == requestedName:

                    return
                        new ApplicationResolution.ContinueWithFunction(
                            functionDeclaration.Function.Declaration,
                            ResolvedName: declName);

                case ElmSyntaxAbstract.Declaration.AliasDeclaration aliasDeclaration
                when aliasDeclaration.TypeAlias.Name == requestedName:

                    {
                        if (aliasDeclaration.TypeAlias.TypeAnnotation
                            is ElmSyntaxAbstract.TypeAnnotation.Record recordAnnotation)
                        {
                            var fieldNames =
                                recordAnnotation.RecordDefinition.Fields
                                .Select(field => (field.FieldName, field.FieldNameValue))
                                .ToList();

                            if (fieldNames.Count == application.Arguments.Count)
                            {
                                var fields =
                                    new List<(string FieldName, PineValue FieldNameValue, PineValueInProcess FieldValue)>(fieldNames.Count);

                                for (var i = 0; i < fieldNames.Count; i++)
                                {
                                    fields.Add(
                                        (fieldNames[i].FieldName, fieldNames[i].FieldNameValue, application.Arguments[i]));
                                }

                                return new ApplicationResolution.Resolved(BuildRecordValue([.. fields.OrderBy(f => f.FieldName)]));
                            }

                            // Partial application of a record-type-alias constructor: synthesise a
                            // closure that, when fully applied, re-enters the resolver with all
                            // arguments and produces the record.
                            if (application.Arguments.Count < fieldNames.Count)
                            {
                                return
                                    new ApplicationResolution.Resolved(
                                        new ElmRecordTypeConstructorInProcess(
                                            typeName: declName,
                                            fieldNames: [.. fieldNames],
                                            arguments: [.. application.Arguments]));

                            }
                        }

                        break;
                    }

                case ElmSyntaxAbstract.Declaration.ChoiceTypeDeclaration choiceTypeDeclaration:
                    {
                        foreach (var constructor in choiceTypeDeclaration.TypeDeclaration.Constructors)
                        {
                            if (constructor.Name != requestedName)
                                continue;

                            var ctorArity = constructor.Arguments.Count;

                            if (ctorArity == application.Arguments.Count)
                            {
                                if (ctorArity is 0 && constructor.Name is "True")
                                {
                                    return
                                        new ApplicationResolution.Resolved(
                                            PineValueInProcess.KernelTrueValue);
                                }

                                if (ctorArity is 0 && constructor.Name is "False")
                                {
                                    return
                                        new ApplicationResolution.Resolved(
                                            PineValueInProcess.KernelFalseValue);
                                }

                                return
                                    new ApplicationResolution.Resolved(
                                        BuildTaggedValue(PineValueInProcess.Create(constructor.NameValue), application.Arguments));
                            }

                            // Partial application of a choice-type tag constructor.
                            if (application.Arguments.Count < ctorArity)
                            {
                                return
                                    new ApplicationResolution.Resolved(
                                        new ElmChoiceTagConstructorInProcess(
                                            typeName: declName,
                                            tagName: constructor.Name,
                                            totalArity: ctorArity,
                                            arguments: application.Arguments));
                            }
                        }

                        break;
                    }
            }
        }

        return null;
    }

    private static PineValueInProcess BuildTaggedValue(
        PineValueInProcess tagValue,
        IReadOnlyList<PineValueInProcess> arguments)
    {
        var taggedValue = PineValueInProcess.CreateTagged(tagValue, arguments);

        return taggedValue;
    }

    /// <summary>
    /// Builds an in-process Elm record value from already-evaluated field values.
    /// </summary>
    private static PineValueInProcess BuildRecordValue(
        IReadOnlyList<(string FieldName, PineValue FieldNameValue, PineValueInProcess FieldValue)> fields)
    {
        var listItems = new PineValueInProcess[fields.Count * 2 + 1];

        listItems[0] = s_elmRecordTypeTagNameAsValueInProcess;

        for (var i = 0; i < fields.Count; i++)
        {
            var fieldNameValue = fields[i].FieldNameValue;

            listItems[i * 2 + 1] = PineValueInProcess.Create(fieldNameValue);
            listItems[i * 2 + 2] = fields[i].FieldValue;
        }

        return PineValueInProcess.CreateList(listItems);
    }

    private static PineValueInProcess RecordAccess(
        PineValueInProcess recordValue,
        string fieldName,
        PineValue fieldNameValue)
    {
        // List item at index 0 must be the record-type tag.

        var firstListItem = recordValue.GetElementAt(0);

        if (!PineValueInProcess.AreEqual(firstListItem, ElmValue.ElmRecordTypeTagNameAsValue))
        {
            var renderedValue =
                ElmValue.RenderAsElmExpression(ToElm(recordValue)).expressionString;

            throw new System.Exception(
                "Attempted to access a field on a non-record value.\nValue: "
                + renderedValue);
        }

        // Scan field names until we find a match for the field name.

        for (var fieldIndex = 0; true; fieldIndex++)
        {
            var fieldStartIndex = 1 + fieldIndex * 2;

            var currentFieldNameValue = recordValue.GetElementAt(fieldStartIndex);

            if (!currentFieldNameValue.IsBlob())
            {
                // We've reached the end of the record fields without finding a match.
                break;
            }

            if (PineValueInProcess.AreEqual(currentFieldNameValue, fieldNameValue))
            {
                // We found the field name. The field value is the next item in the list.
                return recordValue.GetElementAt(fieldStartIndex + 1);
            }
        }

        throw new System.Exception(
            "Record does not contain a field named '"
            + fieldName + "'.");
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
    /// Default resolver for applications of named functions.
    /// </summary>
    public static System.Func<Application, ApplicationResolution> BuildResolvers(
        IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration> declarations,
        IReadOnlyDictionary<DeclQualifiedName, System.Func<ImmutableList<PineValueInProcess>, PineValueInProcess?>>? customFunctionResolvers = null)
    {
        return
            CombineResolvers(
                [
                ApplicationResolver(customFunctionResolvers ?? ImmutableDictionary<DeclQualifiedName, System.Func<ImmutableList<PineValueInProcess>, PineValueInProcess?>>.Empty),
                PineBuiltinResolver,
                app => UserDefinedResolver(app, declarations)
                ]);
    }

    /// <summary>
    /// Default resolver for applications of named functions.
    /// </summary>
    public static System.Func<Application, ApplicationResolution> BuildResolvers(
        IReadOnlyDictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration> declarations)
    {
        return BuildResolvers(declarations, s_builtinFunctionResolvers);
    }

    /// <summary>
    /// Default resolver for applications of named functions.
    /// </summary>
    public static System.Func<Application, ApplicationResolution> BuildResolvers(
        IReadOnlyDictionary<DeclQualifiedName, ElmSyntaxAbstract.Declaration> declarations,
        IReadOnlyDictionary<DeclQualifiedName, System.Func<ImmutableList<PineValueInProcess>, PineValueInProcess?>>? customFunctionResolvers)
    {
        return
            CombineResolvers(
                [
                ApplicationResolver(customFunctionResolvers ?? ImmutableDictionary<DeclQualifiedName, System.Func<ImmutableList<PineValueInProcess>, PineValueInProcess?>>.Empty),
                PineBuiltinResolver,
                app => UserDefinedResolver(app, declarations)
                ]);
    }

    /// <summary>
    /// Combines multiple fallible resolvers into a single total resolver: each candidate is tried in order;
    /// the first non-null result wins. Throws if no candidate matches.
    /// </summary>
    public static System.Func<Application, ApplicationResolution> CombineResolvers(
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
