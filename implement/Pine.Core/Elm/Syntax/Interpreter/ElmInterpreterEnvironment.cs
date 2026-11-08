using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Functional;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using SyntaxTypes = Pine.Core.Elm.Syntax.Concrete;

namespace Pine.Core.Elm.Syntax.Interpreter;

/// <summary>
/// An immutable interpreter program and source scope. Each declaration submission gets its own
/// module identity: shadowing changes lookup for future submissions, never existing references.
/// Snapshots can be shared, branched, or discarded without changing their parents.
/// </summary>
public sealed class ElmInterpreterEnvironment
{
    // Not an Elm source identifier, so the wrapper cannot capture a user's unqualified reference.
    private const string ExpressionEntryPoint = "$expression";

    private readonly ImmutableDictionary<string, SyntaxTypes.File> _modules;

    private readonly ImplicitImportConfig _imports;

    private readonly int _nextSubmission;

    private readonly Canonicalization.DeclarationCache _canonicalCache;

    private ElmInterpreterEnvironment(
        ElmSyntaxInterpreter.Prepared program,
        ImmutableDictionary<string, SyntaxTypes.File> modules,
        ImplicitImportConfig imports,
        int nextSubmission,
        Canonicalization.DeclarationCache? canonicalCache = null)
    {
        Program = program;
        _modules = modules;
        _imports = imports;
        _nextSubmission = nextSubmission;
        _canonicalCache = canonicalCache ?? new();
    }

    /// <summary>All executable declarations, including shadowed declarations still reachable from older bindings.</summary>
    public ElmSyntaxInterpreter.Prepared Program { get; }

    /// <summary>Canonical source modules, keyed by their stable identities, including submission history.</summary>
    public IReadOnlyDictionary<string, SyntaxTypes.File> Modules => _modules;

    /// <summary>The unqualified value-import map, including the latest submitted values and constructors.</summary>
    public IReadOnlyDictionary<string, DeclQualifiedName> ValueBindings =>
        _imports.ValueImports.ToImmutableDictionary(
            item => item.Key,
            item => DeclQualifiedName.Create(item.Value, item.Key));

    /// <summary>The visible unqualified type imports, separately from the value namespace.</summary>
    public IReadOnlyDictionary<string, DeclQualifiedName> TypeBindings =>
        _imports.TypeImports.ToImmutableDictionary(
            item => item.Key,
            item => DeclQualifiedName.Create(item.Value, item.Key));

    /// <summary>Prepares and evaluates an expression; preparation failures use the same structured error channel.</summary>
    public Result<ElmInterpretationError, PineValueInProcess> Evaluate(
        string expression,
        ElmSyntaxInterpreter.EvaluationConfig? config = null) =>
        PrepareExpression(expression)
        .MapError(error => new ElmInterpretationError(error, []))
        .AndThen(prepared => prepared.Evaluate(config));

    /// <summary>Invokes a stable qualified identity, including an older shadowed declaration.</summary>
    public Result<ElmInterpretationError, PineValueInProcess> Invoke(
        DeclQualifiedName name,
        IReadOnlyList<PineValueInProcess> arguments,
        ElmSyntaxInterpreter.EvaluationConfig? config = null) =>
        ElmSyntaxInterpreter.Interpret(
            name,
            arguments,
            Program,
            config ?? ElmSyntaxInterpreter.EvaluationConfig.Default);

    /// <summary>Parses and prepares module texts without loading packages or performing app lowering.</summary>
    public static Result<string, ElmInterpreterEnvironment> Create(
        IReadOnlyList<string> moduleTexts,
        ImplicitImportConfig? imports = null)
    {
        var files = new List<SyntaxTypes.File>();

        for (var index = 0; index < moduleTexts.Count; index++)
        {
            var parsed = ElmSyntaxParser.ParseModuleText(moduleTexts[index]);

            if (parsed.IsErrOrNullable() is { } error)
                return "Failed parsing module #" + index + ": " + error;

            files.Add(parsed.Extract(error => throw new InvalidOperationException(error.ToString())));
        }

        return Create(files, imports);
    }

    /// <summary>Prepares parsed modules; recovered syntax errors are rejected, not silently omitted.</summary>
    public static Result<string, ElmInterpreterEnvironment> Create(
        IReadOnlyList<SyntaxTypes.File> modules,
        ImplicitImportConfig? imports = null)
    {
        imports ??= ImplicitImportConfig.Default;

        foreach (var module in modules)
        {
            var errors =
                module.AdditionalParseErrors.Concat(
                    module.IncompleteDeclarations.Select(node => node.Value.ParseError)).ToArray();

            if (errors.Length > 0)
            {
                return
                    "Invalid module " + ModuleName(module) + ":\n" +
                    string.Join("\n", errors.Select(error => error.ToString()));
            }
        }

        var canonicalized = Canonicalization.Canonicalize(modules, imports);

        if (canonicalized.IsErrOrNull() is { } failure)
            return failure;

        var results = canonicalized.Extract(error => throw new InvalidOperationException(error));

        foreach (var (name, (_, errors, _)) in results)
        {
            if (errors.Count > 0)
            {
                return
                    "Failed preparing " + string.Join(".", name) + ":\n" +
                    string.Join("\n", errors.Select(ElmCompiler.RenderCanonicalizationError));
            }
        }

        var sources =
            results.Values.Select(result => result.File)
            .ToImmutableDictionary(ModuleName, StringComparer.Ordinal);

        foreach (var (name, moduleName) in Canonicalization.UniqueExportedConstructors([.. sources.Values]))
        {
            if (!imports.ValueImports.ContainsKey(name) && !imports.TypeImports.ContainsKey(name))
                imports = imports with { ValueImports = imports.ValueImports.Add(name, moduleName) };
        }

        return
            new ElmInterpreterEnvironment(
                ElmSyntaxInterpreter.PrepareCanonicalModules([.. sources.Values]),
                sources,
                imports with
                {
                    ModuleImports =
                    imports.ModuleImports.Union(
                        sources.Keys.Select(name => ImportedModule.WithoutAlias([.. name.Split('.')]))),
                },
                nextSubmission: 0);
    }

    /// <summary>
    /// Prepares one declaration or expression. Preparation never mutates this environment or
    /// commits a submission; consumers choose when to adopt a declaration's returned snapshot.
    /// </summary>
    public Result<string, ElmInterpreterSubmission> PrepareSubmission(string text)
    {
        var parsed = ElmSyntaxParser.ParseDeclarationOrExpression(text);

        if (parsed.IsErrOrNull() is { } failure)
            return "Failed to parse submission: " + failure;

        var syntax = parsed.Extract(error => throw new InvalidOperationException(error));

        switch (syntax)
        {
            case ElmSyntaxParser.DeclarationOrExpression.DeclarationSyntax declaration:
                return
                    AddDeclaration(declaration.Declaration)
                    .Map<ElmInterpreterSubmission>(prepared => prepared);

            case ElmSyntaxParser.DeclarationOrExpression.ExpressionSyntax expression:
                return
                    PrepareExpression(expression.Expression)
                    .Map<ElmInterpreterSubmission>(prepared => new ElmInterpreterSubmission.Expression(prepared));

            default:
                throw new NotImplementedException(
                    $"{nameof(PrepareSubmission)} does not handle syntax variant: {syntax.GetType().Name}");
        }
    }

    /// <summary>Prepares an expression in this snapshot's visible scope without adding it to submission history.</summary>
    public Result<string, ElmInterpreterExpression> PrepareExpression(string text)
    {
        var parsed = ElmSyntaxParser.ParseExpression(text);

        if (parsed.IsErrOrNull() is { } failure)
            return "Failed parsing expression: " + failure;

        return PrepareExpression(parsed.Extract(error => throw new InvalidOperationException(error.ToString())));
    }

    /// <summary>Prepares a parsed expression, retaining its original source ranges.</summary>
    public Result<string, ElmInterpreterExpression> PrepareExpression(SyntaxTypes.Expression expression)
    {
        var scope = NewSubmissionModule();
        var template = (SyntaxTypes.Declaration.FunctionDeclaration)scope.Declarations[0].Value;
        var implementation = template.Function.Declaration;

        var declaration =
            template with
            {
                Function =
                template.Function with
                {
                    Declaration =
                    implementation with
                    {
                        Value =
                        implementation.Value with
                        {
                            Name = implementation.Value.Name with { Value = ExpressionEntryPoint },
                            Expression = implementation.Value.Expression with { Value = expression },
                        },
                    },
                },
            };

        scope = scope with { Declarations = [scope.Declarations[0] with { Value = declaration }] };

        return
            PrepareIncrement(scope).Map(
                increment =>
                new ElmInterpreterExpression(
                    increment.program,
                    DeclQualifiedName.Create([ModuleName(scope)], ExpressionEntryPoint)));
    }

    /// <summary>Prepares a declaration and returns its stable identity and candidate next environment.</summary>
    public Result<string, ElmInterpreterSubmission.Declaration> AddDeclaration(string text) =>
        PrepareSubmission(text).AndThen(
            submission =>
            submission switch
            {
                ElmInterpreterSubmission.Declaration declaration =>
                Result<string, ElmInterpreterSubmission.Declaration>.ok(declaration),

                ElmInterpreterSubmission.Expression =>
                Result<string, ElmInterpreterSubmission.Declaration>.err(
                    "Expected a declaration submission, not an expression."),

                _ =>
                throw new NotImplementedException(
                    $"{nameof(AddDeclaration)} does not handle submission variant: {submission.GetType().Name}"),
            });

    /// <summary>Prepares a parsed declaration, preserving its source ranges.</summary>
    public Result<string, ElmInterpreterSubmission.Declaration> AddDeclaration(SyntaxTypes.Declaration declaration)
    {
        if (declaration is SyntaxTypes.Declaration.PortDeclaration)
            return "Port declarations require a host runtime and cannot be submitted to the syntax interpreter.";

        if (declaration is SyntaxTypes.Declaration.InfixDeclaration submittedInfix &&
            !_imports.ValueImports.ContainsKey(submittedInfix.Infix.FunctionName.Value))
        {
            return "Infix function is not bound: " + submittedInfix.Infix.FunctionName.Value;
        }

        var scope = NewSubmissionModule();
        scope = scope with { Declarations = [scope.Declarations[0] with { Value = declaration }] };

        return
            PrepareIncrement(scope).Map(
                increment =>
                {
                    var name = ModuleName(scope);
                    var module = increment.module;

                    var nextImports =
                        _imports with
                        {
                            ModuleImports = _imports.ModuleImports.Add(ImportedModule.WithoutAlias([name])),
                        };

                    void BindValue(string symbol) =>
                        nextImports =
                            nextImports with { ValueImports = nextImports.ValueImports.SetItem(symbol, [name]) };

                    void BindType(string symbol) =>
                        nextImports =
                            nextImports with { TypeImports = nextImports.TypeImports.SetItem(symbol, [name]) };

                    switch (declaration)
                    {
                        case SyntaxTypes.Declaration.FunctionDeclaration function:
                            BindValue(function.Function.Declaration.Value.Name.Value);
                            break;

                        case SyntaxTypes.Declaration.AliasDeclaration alias:
                            BindType(alias.TypeAlias.Name.Value);

                            if (alias.TypeAlias.TypeAnnotation.Value is SyntaxTypes.TypeAnnotation.Record)
                                BindValue(alias.TypeAlias.Name.Value);

                            break;

                        case SyntaxTypes.Declaration.ChoiceTypeDeclaration choice:
                            BindType(choice.TypeDeclaration.Name.Value);

                            foreach (var constructor in choice.TypeDeclaration.Constructors)
                                BindValue(constructor.Value.Name.Value);

                            break;

                        case SyntaxTypes.Declaration.InfixDeclaration infix:
                            var functionName = infix.Infix.FunctionName.Value;

                            var functionModule =
                                nextImports.ValueImports.TryGetValue(functionName, out var boundModule)
                                ?
                                boundModule
                                :
                                [
                                name
                                ];

                            nextImports =
                                nextImports with
                                {
                                    OperatorToFunction =
                                    nextImports.OperatorToFunction.SetItem(
                                        infix.Infix.Operator.Value,
                                        (functionModule, functionName)),
                                };

                            break;

                        case SyntaxTypes.Declaration.PortDeclaration port:
                            BindValue(port.Signature.Name.Value);
                            break;

                        default:
                            throw new NotImplementedException(
                                $"{nameof(AddDeclaration)} does not handle declaration variant: {declaration.GetType().Name}");
                    }

                    return
                        new ElmInterpreterSubmission.Declaration(
                            new ElmInterpreterEnvironment(
                                increment.program,
                                _modules.Add(name, module),
                                nextImports,
                                _nextSubmission + 1,
                                _canonicalCache),
                            DeclQualifiedName.Create([name], Canonicalization.GetDeclarationName(declaration)));
                });
    }

    private Result<string, (ElmSyntaxInterpreter.Prepared program, SyntaxTypes.File module)> PrepareIncrement(
        SyntaxTypes.File module)
    {
        var name =
            DeclQualifiedName.Create(
                [
                ModuleName(module)
                ],
                Canonicalization.GetDeclarationName(module.Declarations[0].Value));

        var canonicalized =
            Canonicalization.CanonicalizeDeclarations(
                [.. _modules.Values, module],
                [name],
                _imports,
                cache: _canonicalCache,
                canonicalizedDependencies: Program.Declarations.Keys.ToHashSet());

        if (canonicalized.IsErrOrNull() is { } failure)
            return failure;

        var result = canonicalized.Extract(error => throw new InvalidOperationException(error));

        foreach (var (declarationName, declaration) in result.Declarations)
        {
            if (declaration.Errors.Count > 0)
            {
                return
                    "Failed preparing " + declarationName.FullName + ":\n" +
                    string.Join("\n", declaration.Errors.Select(ElmCompiler.RenderCanonicalizationError));
            }
        }

        module = module with { Declarations = [result.Declarations[name].Value] };
        var prepared = ElmSyntaxInterpreter.PrepareCanonicalModules([module], reduceClosedExpressions: false);

        return
            (new ElmSyntaxInterpreter.Prepared(Program.Declarations.ToImmutableDictionary().AddRange(prepared.Declarations)), module);
    }

    private SyntaxTypes.File NewSubmissionModule()
    {
        var index = _nextSubmission;

        while (_modules.ContainsKey("PineInteractiveSubmission" + index))
            index++;

        return
            ElmSyntaxParser.ParseModuleText(
                "module PineInteractiveSubmission" + index + " exposing (..)\n\npine_interactive_expression = ()\n")
            .Extract(error => throw new InvalidOperationException(error.ToString()));
    }

    private static string ModuleName(SyntaxTypes.File module) =>
        string.Join(".", SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value);
}

/// <summary>A canonicalized expression ready to execute repeatedly with caller-selected quotas.</summary>
public sealed record ElmInterpreterExpression(ElmSyntaxInterpreter.Prepared Program, DeclQualifiedName EntryPoint)
{
    /// <summary>Evaluates without materializing functions or function-bearing values into Pine values.</summary>
    public Result<ElmInterpretationError, PineValueInProcess> Evaluate(
        ElmSyntaxInterpreter.EvaluationConfig? config = null) =>
        ElmSyntaxInterpreter.Interpret(
            EntryPoint,
            [],
            Program,
            config ?? ElmSyntaxInterpreter.EvaluationConfig.Default);
}

/// <summary>A prepared submission, not a mutable session or an automatically committed evaluation.</summary>
public abstract record ElmInterpreterSubmission
{
    /// <summary>A candidate next snapshot and stable declaration identity. The input snapshot remains valid.</summary>
    public sealed record Declaration(ElmInterpreterEnvironment Environment, DeclQualifiedName Name) : ElmInterpreterSubmission;

    /// <summary>An expression evaluated against the snapshot in which it was prepared.</summary>
    public sealed record Expression(ElmInterpreterExpression Prepared) : ElmInterpreterSubmission;
}
