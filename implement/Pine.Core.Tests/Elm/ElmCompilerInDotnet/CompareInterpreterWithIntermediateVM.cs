using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Stil4mToFull = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.ToFullSyntaxModel;
using SyntaxModel = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

/// <summary>
/// Test framework that evaluates the same Elm root expression both via the intermediate VM
/// (the same path as <c>ElmParserExpressionTests.Expression_char_literal</c> and friends)
/// and via the <see cref="ElmSyntaxInterpreter"/>, asserting that the two results agree
/// and returning performance metrics from each path together with a log of every function
/// application observed by the syntax interpreter.
/// <para>
/// Both paths involve some expensive preparation (compiling the Elm modules, canonicalizing
/// declarations into the form the interpreter understands, etc.). This class is constructed
/// once via <see cref="Prepare(FileTree, IReadOnlyList{IReadOnlyList{string}}, IReadOnlyList{DeclQualifiedName}, int)"/>
/// or <see cref="Prepare(IReadOnlyList{string}, IReadOnlyList{DeclQualifiedName}, int)"/>
/// and then used to evaluate many root expressions, amortizing that preparation across
/// the test methods.
/// </para>
/// <para>
/// The <see cref="MaxOptimizationRounds"/> setting affects both paths: it is forwarded to
/// <see cref="ElmCompiler.CompileInteractiveEnvironment(FileTree, IReadOnlyList{IReadOnlyList{string}}, bool, int)"/>
/// (so the VM path runs against the bytecode emitted from that round count) and
/// <see cref="CompilationPipelineStageResults.ModulesForCompilation"/> from the same
/// invocation is fed into the syntax interpreter (so both paths see the same post-optimization
/// declarations).
/// </para>
/// </summary>
public sealed class CompareInterpreterWithIntermediateVM
{
    /// <summary>
    /// The optimization-round count used during preparation. Captured here so callers
    /// can include it in diagnostics or assertion messages.
    /// </summary>
    public int MaxOptimizationRounds { get; }

    private readonly Core.Interpreter.IntermediateVM.PineVM _vm;

    private readonly IReadOnlyDictionary<DeclQualifiedName, PineValue> _entryFunctionValuesByQualifiedName;

    private readonly IReadOnlyDictionary<string, DeclQualifiedName> _entryQualifiedNameBySimpleName;

    private readonly IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration> _interpreterDeclarations;

    private CompareInterpreterWithIntermediateVM(
        int maxOptimizationRounds,
        Core.Interpreter.IntermediateVM.PineVM vm,
        IReadOnlyDictionary<DeclQualifiedName, PineValue> entryFunctionValuesByQualifiedName,
        IReadOnlyDictionary<string, DeclQualifiedName> entryQualifiedNameBySimpleName,
        IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration> interpreterDeclarations)
    {
        MaxOptimizationRounds = maxOptimizationRounds;
        _vm = vm;
        _entryFunctionValuesByQualifiedName = entryFunctionValuesByQualifiedName;
        _entryQualifiedNameBySimpleName = entryQualifiedNameBySimpleName;
        _interpreterDeclarations = interpreterDeclarations;
    }

    /// <summary>
    /// Compiles the Elm modules in <paramref name="appCodeTree"/> and prepares both the
    /// intermediate-VM path (compiled function values) and the
    /// <see cref="ElmSyntaxInterpreter"/> path (declaration dictionary keyed by
    /// fully-qualified name) for the given <paramref name="entryPoints"/>. Both paths are
    /// produced from a single
    /// <see cref="ElmCompiler.CompileInteractiveEnvironment(FileTree, IReadOnlyList{IReadOnlyList{string}}, bool, int)"/>
    /// invocation so they observe the same <paramref name="maxOptimizationRounds"/>.
    /// </summary>
    /// <param name="appCodeTree">
    /// The full Elm source tree (kernel modules, libraries, user modules) the compiler should
    /// treat as the application.
    /// </param>
    /// <param name="rootFilePaths">Root file paths forwarded to the compiler.</param>
    /// <param name="entryPoints">
    /// Fully-qualified names of the top-level declarations that root expressions are allowed to
    /// invoke. A root expression like <c>parseCharLiteral "'&amp;'"</c> resolves the unqualified
    /// <c>parseCharLiteral</c> to the matching entry point.
    /// </param>
    /// <param name="maxOptimizationRounds">
    /// Number of inlining/specialization rounds the optimization pipeline runs. Forwarded to
    /// <see cref="ElmCompiler.CompileInteractiveEnvironment(FileTree, IReadOnlyList{IReadOnlyList{string}}, bool, int)"/>
    /// and reflected in the syntax-interpreter declaration dictionary
    /// (<see cref="CompilationPipelineStageResults.ModulesForCompilation"/>).
    /// </param>
    public static CompareInterpreterWithIntermediateVM Prepare(
        FileTree appCodeTree,
        IReadOnlyList<IReadOnlyList<string>> rootFilePaths,
        IReadOnlyList<DeclQualifiedName> entryPoints,
        int maxOptimizationRounds)
    {
        if (entryPoints is null || entryPoints.Count is 0)
        {
            throw new ArgumentException(
                "At least one entry point is required.",
                nameof(entryPoints));
        }

        var (compiledEnvValue, pipelineStageResults) =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths: rootFilePaths,
                disableInlining: false,
                maxOptimizationRounds: maxOptimizationRounds)
            .Extract(
                err => throw new Exception(
                    "CompareInterpreterWithIntermediateVM: failed compiling: " + err));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnvValue)
            .Extract(
                err => throw new Exception(
                    "CompareInterpreterWithIntermediateVM: failed parsing interactive environment: " + err));

        var entryFunctionValuesByQualifiedName =
            new Dictionary<DeclQualifiedName, PineValue>();

        var entryQualifiedNameBySimpleName =
            new Dictionary<string, DeclQualifiedName>();

        foreach (var entryPoint in entryPoints)
        {
            var moduleNameFlat = string.Join(".", entryPoint.Namespaces);

            var moduleHit =
                parsedEnv.Modules.FirstOrDefault(m => m.moduleName == moduleNameFlat);

            if (moduleHit.moduleContent is not { } moduleContent)
            {
                throw new Exception(
                    "CompareInterpreterWithIntermediateVM: module '"
                    + moduleNameFlat
                    + "' for entry point '" + entryPoint.FullName + "' not found in compiled environment. "
                    + "Available modules: "
                    + string.Join(", ", parsedEnv.Modules.Select(m => m.moduleName)));
            }

            if (!moduleContent.FunctionDeclarations.TryGetValue(entryPoint.DeclName, out var functionValue))
            {
                throw new Exception(
                    "CompareInterpreterWithIntermediateVM: function '"
                    + entryPoint.DeclName + "' not found in module '" + moduleNameFlat
                    + "'. Available declarations: "
                    + string.Join(", ", moduleContent.FunctionDeclarations.Keys));
            }

            entryFunctionValuesByQualifiedName[entryPoint] = functionValue;

            if (entryQualifiedNameBySimpleName.ContainsKey(entryPoint.DeclName))
            {
                throw new ArgumentException(
                    "Multiple entry points share the simple name '" + entryPoint.DeclName
                    + "'. CompareInterpreterWithIntermediateVM resolves unqualified root-expression "
                    + "function references by simple name and therefore requires unique simple names.",
                    nameof(entryPoints));
            }

            entryQualifiedNameBySimpleName[entryPoint.DeclName] = entryPoint;
        }

        var interpreterDeclarations =
            BuildInterpreterDeclarations(appCodeTree, pipelineStageResults);

        var vm = ElmCompilerTestHelper.PineVMForProfiling(_ => { });

        return
            new CompareInterpreterWithIntermediateVM(
                maxOptimizationRounds: maxOptimizationRounds,
                vm: vm,
                entryFunctionValuesByQualifiedName: entryFunctionValuesByQualifiedName,
                entryQualifiedNameBySimpleName: entryQualifiedNameBySimpleName,
                interpreterDeclarations: interpreterDeclarations);
    }

    /// <summary>
    /// Convenience overload accepting a flat list of Elm module source texts. Uses
    /// <see cref="ElmCompilerTests.TestCase.DefaultAppWithoutPackages(IReadOnlyList{string})"/>
    /// to build the compiler input. Suitable for tests whose corpus is just a handful of
    /// hand-written modules.
    /// </summary>
    public static CompareInterpreterWithIntermediateVM Prepare(
        IReadOnlyList<string> elmModuleTexts,
        IReadOnlyList<DeclQualifiedName> entryPoints,
        int maxOptimizationRounds)
    {
        var testCase =
            Pine.Core.Tests.Elm.ElmCompilerTests.TestCase.DefaultAppWithoutPackages(elmModuleTexts);

        var appCodeTree = testCase.AsFileTree();

        var rootFilePaths =
            appCodeTree.EnumerateFilesTransitive()
            .Where(b => b.path[^1].EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
            .Select(b => b.path)
            .ToList();

        return
            Prepare(
                appCodeTree: appCodeTree,
                rootFilePaths: rootFilePaths,
                entryPoints: entryPoints,
                maxOptimizationRounds: maxOptimizationRounds);
    }

    /// <summary>
    /// Result of evaluating one root expression on both paths.
    /// </summary>
    /// <param name="Value">
    /// The evaluated <see cref="ElmValue"/>. The framework asserts that the VM and interpreter
    /// agree on this value (compared via their rendered Elm-expression strings) before
    /// returning, so the two paths' results are guaranteed identical.
    /// </param>
    /// <param name="VmCounters">
    /// Aggregated <see cref="PerformanceCounters"/> from the intermediate VM evaluation —
    /// the same metric shape returned by
    /// <c>CoreLibraryTestHelper.ApplyAndProfileUnary</c>.
    /// </param>
    /// <param name="InterpreterCounters">
    /// <see cref="ElmSyntaxInterpreterPerformanceCounters"/> from the syntax-interpreter
    /// evaluation.
    /// </param>
    /// <param name="ApplicationLog">
    /// Log of every function application (direct or function-value) the syntax interpreter
    /// dispatched during evaluation, in source-order. Tests can <c>Where</c>/<c>Select</c>
    /// over this list to extract a trace.
    /// </param>
    public sealed record EvalReport(
        ElmValue Value,
        PerformanceCounters VmCounters,
        ElmSyntaxInterpreterPerformanceCounters InterpreterCounters,
        IReadOnlyList<ApplicationLogEntry> ApplicationLog);

    /// <summary>
    /// Evaluates <paramref name="rootExpressionText"/> on both paths and returns the merged
    /// report. The root expression must be either a bare reference to one of the configured
    /// entry points (<c>foo</c>) or an application of one to literal-style argument expressions
    /// (<c>foo "hello" [ 1, 2, 3 ]</c>): each argument is evaluated independently via the
    /// syntax interpreter against the same declaration dictionary, then the resolved values
    /// are dispatched onto the VM-side compiled function value.
    /// </summary>
    public EvalReport Eval(string rootExpressionText)
    {
        var (entryPointName, argumentValues) = DecomposeRootExpression(rootExpressionText);

        if (!_entryFunctionValuesByQualifiedName.TryGetValue(entryPointName, out var functionValue))
        {
            throw new InvalidOperationException(
                "CompareInterpreterWithIntermediateVM.Eval: entry point '"
                + entryPointName.FullName + "' is not registered. Registered entry points: "
                + string.Join(", ", _entryFunctionValuesByQualifiedName.Keys.Select(n => n.FullName)));
        }

        // ------------------ VM path ------------------

        var pineArguments = new PineValue[argumentValues.Count];

        for (var i = 0; i < argumentValues.Count; i++)
        {
            pineArguments[i] = ElmValueEncoding.ElmValueAsPineValue(argumentValues[i]);
        }

        PineValue vmResultPine;
        PerformanceCounters vmCounters;

        if (pineArguments.Length is 0)
        {
            // The framework targets root expressions of shape `funcName arg1 arg2 ...`. A bare
            // reference to a top-level value is valid Elm but the existing ApplyAndProfileUnary
            // path expects at least one argument; evaluate the function value directly so
            // zero-arg entry points work too.
            var report =
                _vm.EvaluateExpressionOnCustomStack(
                    new Expression.ParseAndEval(
                        encoded: Expression.LiteralInstance(functionValue),
                        environment: Expression.LiteralInstance(PineValue.EmptyList)),
                    PineValue.EmptyBlob,
                    config: ElmCompilerTestHelper.DefaultTestEvaluationConfig)
                .Extract(err => throw new Exception("CompareInterpreterWithIntermediateVM: VM eval failed: " + err));

            vmResultPine = report.ReturnValue.Evaluate();
            vmCounters = report.Counters;
        }
        else
        {
            (vmResultPine, vmCounters) =
                CoreLibraryTestHelper.ApplyGenericPineWithProfiling(
                    functionValue,
                    pineArguments,
                    _vm);
        }

        var vmResultElm =
            ElmValueEncoding.PineValueAsElmValue(vmResultPine, null, null)
            .Extract(
                err => throw new Exception(
                    "CompareInterpreterWithIntermediateVM: failed decoding VM result as Elm value: " + err));

        // ------------------ Interpreter path ------------------

        var entries = new List<ApplicationLogEntry>();

        var (interpResult, interpCounters) =
            ElmSyntaxInterpreter.ParseAndInterpretWithCounters(
                rootExpressionText,
                _interpreterDeclarations,
                entries.Add);

        var interpValue =
            interpResult.Extract(
                err => throw new Exception(
                    "CompareInterpreterWithIntermediateVM: interpreter evaluation failed: " + err));

        // ------------------ Equality assertion ------------------

        var vmRendered = ElmValue.RenderAsElmExpression(vmResultElm).expressionString;
        var interpRendered = ElmValue.RenderAsElmExpression(interpValue).expressionString;

        // Render both via the same formatter and compare. Comparing rendered strings (rather than
        // Equals on ElmValue directly) keeps the assertion message readable on a mismatch and
        // matches what the spec asks for ("Should().Be( using the representation rendered as
        // expression string").
        interpRendered.Should().Be(
            vmRendered,
            because:
            "CompareInterpreterWithIntermediateVM: the Elm syntax interpreter and the intermediate "
            + "VM must agree on the result of evaluating the same root expression.");

        return
            new EvalReport(
                Value: interpValue,
                VmCounters: vmCounters,
                InterpreterCounters: interpCounters,
                ApplicationLog: entries);
    }

    /// <summary>
    /// Parses <paramref name="rootExpressionText"/> via
    /// <see cref="ElmSyntaxInterpreter.ParseRootExpressionForDecomposition(string)"/> and pulls
    /// out the top-level function call's qualified name and resolved argument values. The
    /// argument expressions are interpreted against the framework's full declaration
    /// dictionary so any literal-style or simple constructor arguments are supported.
    /// </summary>
    private (DeclQualifiedName Name, IReadOnlyList<ElmValue> Arguments)
        DecomposeRootExpression(string rootExpressionText)
    {
        var parseResult =
            ElmSyntaxInterpreter.ParseRootExpressionForDecomposition(rootExpressionText)
            .Extract(
                err => throw new Exception(
                    "CompareInterpreterWithIntermediateVM: failed parsing root expression: " + err));

        // Unwrap the optional outer parenthesization the parser introduces around things like
        // `(f x)` so the call shape is reachable.
        var innerExpression = UnwrapParenthesized(parseResult);

        switch (innerExpression)
        {
            case SyntaxModel.Expression.FunctionOrValue zeroArgRef:
                return (ResolveEntryPointName(zeroArgRef), []);

            case SyntaxModel.Expression.Application application:
                {
                    var head = UnwrapParenthesized(application.Function.Value);

                    if (head is not SyntaxModel.Expression.FunctionOrValue funcRef)
                    {
                        throw new InvalidOperationException(
                            "CompareInterpreterWithIntermediateVM only supports root expressions whose "
                            + "head is a bare function-or-value reference. Got: " + head.GetType().Name);
                    }

                    var qualifiedName = ResolveEntryPointName(funcRef);

                    var arguments = new ElmValue[application.Arguments.Count];

                    for (var i = 0; i < application.Arguments.Count; i++)
                    {
                        arguments[i] =
                            EvaluateArgumentExpression(application.Arguments[i].Value);
                    }

                    return (qualifiedName, arguments);
                }

            default:
                throw new InvalidOperationException(
                    "CompareInterpreterWithIntermediateVM only supports root expressions of shape "
                    + "`funcName arg1 arg2 ...` or a bare function reference. Got: "
                    + innerExpression.GetType().Name);
        }
    }

    private static SyntaxModel.Expression UnwrapParenthesized(SyntaxModel.Expression expression)
    {
        while (expression is SyntaxModel.Expression.ParenthesizedExpression parens)
        {
            expression = parens.Expression.Value;
        }

        return expression;
    }

    private DeclQualifiedName ResolveEntryPointName(SyntaxModel.Expression.FunctionOrValue functionOrValue)
    {
        if (functionOrValue.ModuleName.Count is 0)
        {
            if (_entryQualifiedNameBySimpleName.TryGetValue(functionOrValue.Name, out var qualified))
            {
                return qualified;
            }

            throw new InvalidOperationException(
                "Root expression references unqualified function '" + functionOrValue.Name
                + "', which is not one of the registered entry points: "
                + string.Join(", ", _entryQualifiedNameBySimpleName.Keys));
        }

        var explicitlyQualified =
            new DeclQualifiedName([.. functionOrValue.ModuleName], functionOrValue.Name);

        if (_entryFunctionValuesByQualifiedName.ContainsKey(explicitlyQualified))
        {
            return explicitlyQualified;
        }

        throw new InvalidOperationException(
            "Root expression references function '" + explicitlyQualified.FullName
            + "', which is not one of the registered entry points.");
    }

    /// <summary>
    /// Evaluates an argument expression node by interpreting it against the framework's
    /// declaration dictionary. Used so that argument expressions like <c>"hello"</c>,
    /// <c>[ 1, 2, 3 ]</c>, or <c>Just 7</c> all resolve to <see cref="ElmValue"/> instances
    /// without the framework having to mirror the AST→value conversion for every literal kind.
    /// </summary>
    private ElmValue EvaluateArgumentExpression(SyntaxModel.Expression argumentExpression) =>
        ElmSyntaxInterpreter.Interpret(argumentExpression, _interpreterDeclarations)
        .Extract(
            err => throw new Exception(
                "CompareInterpreterWithIntermediateVM: failed evaluating argument expression as ElmValue: "
                + err));

    /// <summary>
    /// Builds the declaration dictionary the syntax interpreter uses, by overlaying all
    /// bundled kernel-module declarations (e.g. <c>Basics</c>, <c>List</c>, <c>Char</c>) on
    /// the bottom — these are needed because the compiler may strip natively-implemented
    /// modules from the post-pipeline output but the syntax interpreter still needs to
    /// resolve calls like <c>Basics.mul</c> or <c>List.cons</c> — and then layering the
    /// post-optimization module list (<see cref="CompilationPipelineStageResults.ModulesForCompilation"/>)
    /// on top so the interpreter sees the same optimized declarations as the bytecode-emission
    /// backend.
    /// </summary>
    private static IReadOnlyDictionary<DeclQualifiedName, SyntaxModel.Declaration>
        BuildInterpreterDeclarations(
        FileTree appCodeTree,
        CompilationPipelineStageResults pipelineStageResults)
    {
        var declarations = new Dictionary<DeclQualifiedName, SyntaxModel.Declaration>();

        // Step 1: overlay bundled kernel-module declarations as a low-priority baseline so
        // calls to declarations stripped from the post-pipeline output still resolve.
        foreach (var (qualifiedName, declaration)
            in EnumerateBundledKernelModuleDeclarations(appCodeTree))
        {
            declarations[qualifiedName] = declaration;
        }

        // Step 2: overlay the post-optimization module list. ModulesForCompilation is the
        // exact input the bytecode-emission backend consumes, so the declarations the
        // interpreter sees are aligned with the declarations the VM emitted bytecode for.
        foreach (var stil4mFile in pipelineStageResults.ModulesForCompilation)
        {
            var fullModuleFile = Stil4mToFull.Convert(stil4mFile);

            var moduleNameParts =
                SyntaxModel.Module.GetModuleName(fullModuleFile.ModuleDefinition.Value).Value;

            foreach (var declNode in fullModuleFile.Declarations)
            {
                if (declNode.Value is SyntaxModel.Declaration.InfixDeclaration infixDecl)
                {
                    declarations[new DeclQualifiedName(moduleNameParts, infixDecl.Infix.Operator.Value)] =
                        declNode.Value;

                    continue;
                }

                var declName = DeclarationSimpleName(declNode.Value);

                if (declName is null)
                    continue;

                declarations[new DeclQualifiedName(moduleNameParts, declName)] = declNode.Value;
            }
        }

        return declarations;
    }

    private static IEnumerable<(DeclQualifiedName Name, SyntaxModel.Declaration Declaration)>
        EnumerateBundledKernelModuleDeclarations(FileTree appCodeTree)
    {
        // Look for the kernel-modules directory in the supplied appCodeTree first; fall back
        // to the bundled compiler source container, which is what test apps built with
        // TestCase.DefaultAppWithoutPackages rely on (their FileTree has no
        // 'elm-kernel-modules' subdirectory because the compiler treats the elm-core
        // dependency as bundled).
        var kernelNode =
            appCodeTree.GetNodeAtPath(["elm-kernel-modules"])
            ?? BundledFiles.ElmKernelModulesDefault.Value;

        if (kernelNode is not { } kernelNodeNonNull)
            yield break;

        foreach (var (_, fileContent) in kernelNodeNonNull.EnumerateFilesTransitive())
        {
            var moduleText = Encoding.UTF8.GetString(fileContent.Span);

            var headerResult = ElmSyntaxParser.ParseModuleHeader(moduleText);

            if (headerResult.IsOkOrNull() is not { } header)
            {
                continue;
            }

            var parseResult = ElmSyntaxParser.ParseModuleText(moduleText);

            if (parseResult.IsOkOrNull() is not { } parsedFile)
                continue;

            var moduleNameParts = header.ModuleName;

            foreach (var declNode in parsedFile.Declarations)
            {
                if (declNode.Value is SyntaxModel.Declaration.InfixDeclaration infixDecl)
                {
                    yield return (
                        new DeclQualifiedName(moduleNameParts, infixDecl.Infix.Operator.Value),
                        declNode.Value);

                    continue;
                }

                var declName = DeclarationSimpleName(declNode.Value);

                if (declName is null)
                    continue;

                yield return (
                    new DeclQualifiedName(moduleNameParts, declName),
                    declNode.Value);
            }
        }
    }

    private static string? DeclarationSimpleName(SyntaxModel.Declaration declaration) =>
        declaration switch
        {
            SyntaxModel.Declaration.FunctionDeclaration functionDeclaration =>
            functionDeclaration.Function.Declaration.Value.Name.Value,

            SyntaxModel.Declaration.AliasDeclaration aliasDeclaration =>
            aliasDeclaration.TypeAlias.Name.Value,

            SyntaxModel.Declaration.ChoiceTypeDeclaration choiceTypeDeclaration =>
            choiceTypeDeclaration.TypeDeclaration.Name.Value,

            _ =>
            null,
        };

    /// <summary>
    /// Renders an <see cref="ApplicationLogEntry"/> trace into a human-readable multi-line
    /// string suitable for snapshot assertions. Each line contains the dispatch kind
    /// (<c>direct</c> / <c>fnvalue</c>), the function identity, and the rendered argument
    /// list.
    /// </summary>
    public static string RenderApplicationLog(IReadOnlyList<ApplicationLogEntry> entries) =>
        string.Join("\n", entries.Select(RenderApplicationLogEntry));

    /// <summary>
    /// Renders a single <see cref="ApplicationLogEntry"/> using the same conventions as
    /// <see cref="RenderApplicationLog"/>.
    /// </summary>
    public static string RenderApplicationLogEntry(ApplicationLogEntry entry) =>
        entry switch
        {
            ApplicationLogEntry.Direct direct =>
            "direct " + direct.Application.FunctionName.FullName
            + " " + RenderArgumentList(direct.Application.Arguments),

            ApplicationLogEntry.FunctionValue funcValue =>
            "fnvalue " + RenderFunctionIdentity(funcValue.Function)
            + " " + RenderArgumentList(funcValue.NewArguments),

            _ =>
            "<unknown application log entry: " + entry.GetType().FullName + ">",
        };

    private static string RenderArgumentList(IReadOnlyList<ElmValue> arguments) =>
        "[ "
        + string.Join(
            ", ",
            arguments.Select(arg => ElmValue.RenderAsElmExpression(arg).expressionString))
        + " ]";

    /// <summary>
    /// Renders the identity of a function value (closure) in the trace. For closures derived
    /// from a top-level declaration, this is the declaration's qualified name; for lambdas it
    /// is the synthetic <c>&lt;lambda@row:col&gt;</c>-style name; otherwise it falls back to the
    /// rendered Elm-expression form (which for a closure is typically <c>&lt;fn&gt;</c>).
    /// </summary>
    private static string RenderFunctionIdentity(ElmValue functionValue)
    {
        if (functionValue is ElmValue.ElmFunction closure)
        {
            return closure.Source switch
            {
                ElmValue.ElmFunction.SourceRef.Declared declared =>
                declared.Name.FullName,

                ElmValue.ElmFunction.SourceRef.Lambda lambda =>
                "<lambda@"
                + lambda.LambdaStruct.BackslashLocation.Row
                + ":" + lambda.LambdaStruct.BackslashLocation.Column
                + ">",

                _ =>
                "<closure: " + closure.Source.GetType().Name + ">",
            };
        }

        return ElmValue.RenderAsElmExpression(functionValue).expressionString;
    }
}
