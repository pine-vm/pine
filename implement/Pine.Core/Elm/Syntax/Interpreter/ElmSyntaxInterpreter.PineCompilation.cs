using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;

using AST = Pine.Core.Elm.Syntax.Abstract;
using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

namespace Pine.Core.Elm.Syntax.Interpreter;

/// <summary>A self-contained Pine evaluation, independent of the syntax interpreter that prepared it.</summary>
public sealed record PineEvaluation(Expression Expression, PineValue Environment);

public partial class ElmSyntaxInterpreter
{
    /// <summary>
    /// Compiles applications of already constructed interpreter functions, preserving captured values.
    /// Only code reachable from these applications is emitted; suite construction is not replayed.
    /// </summary>
    internal static IReadOnlyList<PineEvaluation> CompileApplicationsToPine(
        IReadOnlyList<(PineValueInProcess Function, IReadOnlyList<PineValueInProcess> Arguments)> applications,
        IReadOnlyDictionary<DeclQualifiedName, AST.Declaration> canonicalDeclarations,
        IReadOnlyDictionary<DeclQualifiedName, UnitExport> canonicalUnits,
        CancellationToken cancellationToken,
        DeclarationUnitCache? unitCache = null)
    {
        unitCache ??= new();

        var export =
            new PineClosureCompiler(
                canonicalDeclarations,
                canonicalUnits,
                cancellationToken,
                unitCache);

        var roots =
            applications.Select(
                application => export.AddApplication(application.Function, application.Arguments))
            .ToArray();

        return export.Compile(roots);
    }

    private sealed class PineClosureCompiler(
        IReadOnlyDictionary<DeclQualifiedName, AST.Declaration> canonicalDeclarations,
        IReadOnlyDictionary<DeclQualifiedName, UnitExport> canonicalUnits,
        CancellationToken cancellationToken,
        DeclarationUnitCache unitCache)
    {
        private static readonly DeclQualifiedName s_ignoredDebugLog =
            DeclQualifiedName.Create(["Debug"], "log");

        private readonly Dictionary<PineValueInProcess, DeclQualifiedName> m_values =
            new(ReferenceEqualityComparer.Instance);

        private readonly Queue<(DeclQualifiedName Name, PineValueInProcess Value)> m_pending = new();

        private readonly Dictionary<DeclQualifiedName, AST.Declaration> m_generated = [];

        private readonly Dictionary<DeclQualifiedName, Model.Expression> m_bodies = [];

        private readonly Dictionary<DeclQualifiedName, (string Name, Model.Expression Value)[]> m_captures = [];

        private int m_nextName;

        private DeclQualifiedName FreshName() =>
            DeclQualifiedName.Create(["$PineSyntaxClosure"], "value" + m_nextName++);

        private static AST.Declaration Function(
            DeclQualifiedName name, IReadOnlyList<AST.Pattern> arguments, AST.Expression body) =>
            new AST.Declaration.FunctionDeclaration(new(null, new(name.DeclName, arguments, body)));

        private DeclQualifiedName Stub()
        {
            var name = FreshName();
            m_generated.Add(name, Function(name, [], new AST.Expression.UnitExpr()));
            return name;
        }

        private static Model.Expression Reference(DeclQualifiedName name) =>
            new Model.Expression.ReferenceExpression(new Model.Reference.Function(new Model.FunctionId.Source(name)));

        private DeclQualifiedName Export(PineValueInProcess value)
        {
            if (m_values.TryGetValue(value, out var existing))
                return existing;
            // Reserve identity before traversing captures: local recursive closures can form cycles.
            var name = Stub();
            m_values.Add(value, name);
            m_pending.Enqueue((name, value));
            return name;
        }

        public DeclQualifiedName AddApplication(
            PineValueInProcess function,
            IReadOnlyList<PineValueInProcess> arguments)
        {
            var name = Stub();

            m_bodies.Add(
                name,
                new Model.Expression.Application(
                    Reference(Export(function)),
                    new(arguments.Select(argument => Reference(Export(argument))))));

            return name;
        }

        public IReadOnlyList<PineEvaluation> Compile(IReadOnlyList<DeclQualifiedName> roots)
        {
            while (m_pending.TryDequeue(out var pending))
            {
                cancellationToken.ThrowIfCancellationRequested();
                m_bodies.Add(pending.Name, CompileValue(pending.Value));
            }

            var valueOwners = new Dictionary<DeclQualifiedName, DeclQualifiedName>();
            var typeOwners = new Dictionary<DeclQualifiedName, DeclQualifiedName>();

            foreach (var (name, declaration) in canonicalDeclarations)
            {
                switch (declaration)
                {
                    case AST.Declaration.FunctionDeclaration:
                    case AST.Declaration.PortDeclaration:
                    case AST.Declaration.InfixDeclaration:
                        valueOwners.Add(name, name);
                        break;

                    case AST.Declaration.ChoiceTypeDeclaration choice:
                        typeOwners.Add(name, name);

                        foreach (var constructor in choice.TypeDeclaration.Constructors)
                            valueOwners.Add(DeclQualifiedName.Create(name.Namespaces, constructor.Name), name);

                        break;

                    case AST.Declaration.AliasDeclaration alias:
                        typeOwners.Add(name, name);

                        if (alias.TypeAlias.TypeAnnotation is AST.TypeAnnotation.Record)
                            valueOwners.Add(name, name);

                        break;

                    default:
                        throw new NotImplementedException(
                            nameof(Compile) + " does not handle declaration variant: " + declaration.GetType().Name);
                }
            }

            var sourceUnits = new Dictionary<string, UnitExport>();

            foreach (var intrinsic in new[] { "String.String", "Char.Char" })
            {
                if (canonicalUnits.TryGetValue(DeclQualifiedName.FromString(intrinsic), out var unit))
                    sourceUnits.Add(intrinsic, unit);
            }

            DeclQualifiedName Require(DeclQualifiedName name, bool isType)
            {
                if (!SccValidation.Native(name) &&
                    (isType ? typeOwners : valueOwners).TryGetValue(name, out var owner))
                {
                    sourceUnits[owner.FullName] = canonicalUnits[owner];
                }

                return name;
            }

            foreach (var declaration in m_generated.Values)
                Canonicalize.MapNames(declaration, Require);

            foreach (var body in m_bodies.Values.Concat(
                m_captures.Values.SelectMany(captures => captures.Select(capture => capture.Value))))
            {
                foreach (var reference in Model.Compilation.References(body))
                {
                    switch (reference)
                    {
                        case Model.Reference.Function function:
                            Require(function.Id.SourceDeclaration, false);
                            break;

                        case Model.Reference.Constructor constructor:
                            Require(constructor.Name, false);
                            break;

                        case Model.Reference.Local:
                        case Model.Reference.Native:
                            break;

                        default:
                            throw new NotImplementedException(
                                nameof(Compile) + " does not handle reference variant: " + reference.GetType().Name);
                    }
                }
            }

            var transformed =
                SccCompiler.Transform(
                    sourceUnits,
                    lowerOperators: true,
                    lowerCoreBasics: false,
                    removeDebugLog: true,
                    unitCache)
                .Extract(
                    error => throw new ElmCompilationException("Failed lowering discovered Elm test sources: " + error));

            var sourceProgram =
                Model.LambdaLifting.Lift(
                    SccCompiler.Lower(transformed.Values, unitCache),
                    unitCache.LambdaLifting);

            var valueNames = new Dictionary<DeclQualifiedName, DeclQualifiedName>();
            var typeNames = new Dictionary<DeclQualifiedName, DeclQualifiedName>();

            foreach (var (symbol, owner) in valueOwners)
            {
                if (transformed.TryGetValue(owner.FullName, out var export))
                    valueNames.Add(symbol, UnitConstruction.MemberName(export, symbol.DeclName));
            }

            foreach (var (symbol, owner) in typeOwners)
            {
                if (transformed.TryGetValue(owner.FullName, out var export))
                    typeNames.Add(symbol, UnitConstruction.MemberName(export, symbol.DeclName));
            }

            var rebind = new SourceLoweringRebinding(name => valueNames.GetValueOrDefault(name, name));

            Model.Function Rebind(Model.Function function) =>
                rebind.Apply(new(function, [])).Function;

            foreach (var name in m_bodies.Keys.ToArray())
                m_bodies[name] = Rebind(new([], m_bodies[name])).Body;

            foreach (var name in m_captures.Keys.ToArray())
            {
                m_captures[name] =
                    [
                    .. m_captures[name].Select(
                        capture => (capture.Name, Rebind(new([], capture.Value)).Body))
                    ];
            }

            var declarations =
                UnitConstruction.Flatten(transformed.Values, unitCache.Projections)
                .Concat(
                    m_generated.Select(
                        entry => KeyValuePair.Create(
                            entry.Key,
                            DebugLogRemoval.RewriteDeclaration(
                                Canonicalize.MapNames(
                                    entry.Value,
                                    (name, isType) =>
                                    (isType ? typeNames : valueNames).GetValueOrDefault(name, name))))))
                .Append(
                    KeyValuePair.Create(
                        s_ignoredDebugLog,
                        Function(
                            s_ignoredDebugLog,
                            [new AST.Pattern.AllPattern(), new AST.Pattern.VarPattern("value")],
                            AST.Expression.Identifier.Create([], "value"))))
                .ToImmutableDictionary();

            // Preserve source compilation's pipe and short-circuit semantics before creating resolved IR.
            declarations =
                BuiltinOperatorLowering.Apply(
                    declarations,
                    new(LowerBuiltinOperators: true, LowerElmCoreBasics: false),
                    cancellationToken,
                    selectedFunctions: m_generated.Keys.Append(s_ignoredDebugLog).ToHashSet())
                .Extract(
                    error => throw new ElmCompilationException("Failed lowering discovered Elm test closures: " + error));

            var sourceReachable = new HashSet<DeclQualifiedName>();
            var pendingSource = new Stack<DeclQualifiedName>(roots);

            while (pendingSource.TryPop(out var name))
            {
                cancellationToken.ThrowIfCancellationRequested();

                if (!sourceReachable.Add(name))
                    continue;

                if (m_bodies.TryGetValue(name, out var body))
                {
                    foreach (var reference in Model.Compilation.References(body).OfType<Model.Reference.Function>())
                    {
                        if (reference.Id is Model.FunctionId.Source source)
                            pendingSource.Push(source.Name);
                    }
                }
                else if (declarations.TryGetValue(name, out var declaration) &&
                    declaration is AST.Declaration.FunctionDeclaration function)
                {
                    _ = Visit(function.Function.Declaration.Expression);

                    AST.Expression Visit(AST.Expression expression)
                    {
                        if (expression is AST.Expression.Identifier identifier)
                            PushReference(AST.SyntaxAnalysis.ResolveReference(identifier, name.Namespaces));

                        if (expression is AST.Expression.RecordUpdateExpression update)
                            PushReference(DeclQualifiedName.Create(name.Namespaces, update.RecordName));

                        return ElmSyntaxAbstractTransformations.MapChildExpressions(expression, Visit);
                    }

                    void PushReference(DeclQualifiedName reference)
                    {
                        if (declarations.TryGetValue(reference, out var target) &&
                            target is AST.Declaration.FunctionDeclaration)
                        {
                            pendingSource.Push(reference);
                        }
                    }
                }

                if (m_captures.TryGetValue(name, out var captures))
                {
                    foreach (var capture in captures)
                    {
                        foreach (var reference in Model.Compilation.References(capture.Value).OfType<Model.Reference.Function>())
                        {
                            if (reference.Id is Model.FunctionId.Source source)
                                pendingSource.Push(source.Name);
                        }
                    }
                }
            }

            var program =
                Model.FromCanonicalSyntax.Convert(
                    declarations,
                    sourceReachable.Except(m_bodies.Keys).Where(
                        name => m_generated.ContainsKey(name) || name == s_ignoredDebugLog).ToHashSet(),
                    cancellationToken);

            var functions = program.Functions.ToBuilder();

            program =
                sourceProgram with
                {
                    BindingNames = sourceProgram.BindingNames.AddRange(program.BindingNames)
                };

            foreach (var (name, body) in m_bodies)
            {
                var id = new Model.FunctionId.Source(name);

                if (sourceReachable.Contains(name))
                    functions.Add(id, new Model.Function(new([]), body));
            }

            foreach (var (name, captures) in m_captures)
            {
                var id = new Model.FunctionId.Source(name);

                if (!functions.TryGetValue(id, out var function))
                    continue;
                // Substitute binding identities, not names, so nested shadowing remains lexical.
                var replacements =
                    captures.Select(
                        (capture, index) =>
                        (Binding: ((Model.Pattern.VarPattern)function.Parameters[index]).Binding, capture.Value))
                    .ToDictionary(entry => entry.Binding, entry => entry.Value);

                Model.Expression Substitute(Model.Expression expression)
                {
                    if (expression is Model.Expression.ReferenceExpression { Reference: Model.Reference.Local local } &&
                        replacements.TryGetValue(local.Binding, out var replacement))
                    {
                        return replacement;
                    }

                    return Model.ExpressionTransform.MapChildren(expression, Substitute);
                }

                functions[id] =
                    function with
                    {
                        Parameters = new(function.Parameters.Skip(captures.Length)),
                        Body = Substitute(function.Body)
                    };
            }

            program = Model.LambdaLifting.Lift(program with { Functions = functions.ToImmutable() });
            program = program with { Functions = sourceProgram.Functions.AddRange(program.Functions) };
            var reachable = new HashSet<Model.FunctionId>();
            var pendingFunctions = new Stack<Model.FunctionId>(roots.Select(name => new Model.FunctionId.Source(name)));

            while (pendingFunctions.TryPop(out var id))
            {
                cancellationToken.ThrowIfCancellationRequested();

                if (!reachable.Add(id))
                    continue;

                foreach (var reference in Model.Compilation.References(program.Functions[id].Body).OfType<Model.Reference.Function>())
                    pendingFunctions.Push(reference.Id);
            }

            program =
                program with { Functions = program.Functions.Where(entry => reachable.Contains(entry.Key)).ToImmutableDictionary() };

            Model.ProgramValidation.Validate(program);

            var compiled =
                Model.Compilation.Compile(
                    program,
                    compileScc:
                    input =>
                    {
                        // Captured runtime values belong to this execution, not retained semantic storage.
                        if (input.Members.Any(member => !sourceProgram.Functions.ContainsKey(member.Key)))
                            return Model.Compilation.CompileScc(input);

                        if (unitCache.Emission.TryGetValue(input, out var cached))
                            return cached;

                        var result = Model.Compilation.CompileScc(input);
                        unitCache.Emission.Set(input, result);
                        return result;
                    })
                .Extract(
                    error =>
                    throw new ElmCompilationException("Failed compiling discovered Elm test closures: " + error));

            var parseCache = new PineVMParseCache();

            return
                [
                .. roots.Select(
                    name =>
                    {
                        var callable = compiled[new Model.FunctionId.Source(name)];

                        return
                            new PineEvaluation(
                                parseCache.ParseExpression(callable.EncodedBody).Extract(
                                    error => throw new InvalidOperationException(error)),
                                PineValue.List([PineValue.List([.. callable.EnvFunctions])]));
                    })
                ];
        }

        private Model.Expression CompileValue(PineValueInProcess value)
        {
            switch (value)
            {
                case ElmClosureInProcess closure:
                    return CompileClosure(closure);

                case ElmChoiceTagConstructorInProcess constructor:
                    return
                        Apply(
                            new Model.Expression.ReferenceExpression(
                                new Model.Reference.Constructor(
                                    DeclQualifiedName.Create(constructor.TypeName.Namespaces, constructor.TagName))),
                            constructor.Arguments);

                case ElmRecordTypeConstructorInProcess constructor:
                    return
                        Apply(
                            new Model.Expression.ReferenceExpression(
                                new Model.Reference.Constructor(constructor.TypeName)),
                            constructor.Arguments);

                case ElmRecordAccessChainInProcess access:
                    var argumentName = FreshName();
                    var argument = new Model.BindingId.Local(new Model.FunctionId.Source(argumentName), 0);

                    Model.Expression body =
                        new Model.Expression.ReferenceExpression(new Model.Reference.Local(argument));

                    foreach (var field in access.FieldNames)
                        body = new Model.Expression.RecordAccess(body, field.FieldName);

                    return
                        new Model.Expression.Lambda(
                            new Model.FunctionId.Source(argumentName),
                            new([new Model.Pattern.VarPattern(argument)]),
                            body);

                case PineValueInProcess data:
                    if (data.GetType() != typeof(PineValueInProcess))
                    {
                        throw new NotImplementedException(
                            nameof(CompileValue) + " does not handle value variant: " + data.GetType().Name);
                    }

                    if (value.UnevaluatedStructuralItemsOrNull() is { } items)
                        return new Model.Expression.List(new(items.Select(item => Reference(Export(item)))));

                    return new Model.Expression.Literal(value.Evaluate());

                default:
                    throw new NotImplementedException(
                        nameof(CompileValue) + " does not handle value variant: " + value.GetType().Name);
            }
        }

        private Model.Expression Apply(Model.Expression function, IReadOnlyList<PineValueInProcess> arguments) =>
            arguments.Count == 0
            ?
            function
            :
            new Model.Expression.Application(function, new(arguments.Select(argument => Reference(Export(argument)))));

        private Model.Expression CompileClosure(ElmClosureInProcess closure)
        {
            if (closure.Source is ElmClosureInProcess.SourceRef.Declared declared &&
                (canonicalDeclarations.ContainsKey(declared.Name) ||
                declared.Name.Namespaces.Count == 1 && declared.Name.Namespaces[0] is "Basics" or "Debug"))
            {
                // Fully applied calls are erased; captured/curried loggers use the silent definition above.
                var native =
                    declared.Name != s_ignoredDebugLog &&
                    declared.Name.Namespaces.Count == 1 && declared.Name.Namespaces[0] is "Basics" or "Debug";

                return
                    Apply(
                        native
                        ?
                        new Model.Expression.ReferenceExpression(new Model.Reference.Native(declared.Name))
                        :
                        Reference(declared.Name),
                        closure.ArgumentsAlreadyCollected);
            }

            var (arguments, expression) =
                closure.Source switch
                {
                    ElmClosureInProcess.SourceRef.Declared source =>
                    (source.Implementation.Arguments, source.Implementation.Expression),

                    ElmClosureInProcess.SourceRef.Lambda source =>
                    (source.LambdaExpression.Arguments, source.LambdaExpression.Expression),

                    _ =>
                    throw new NotImplementedException(
                        nameof(CompileClosure) + " does not handle source variant: " + closure.Source.GetType().Name)
                };

            var shadowed = new HashSet<string>(StringComparer.Ordinal);

            foreach (var argument in arguments)
                CollectPatternNames(argument, shadowed);

            var free = new HashSet<string>(StringComparer.Ordinal);
            CollectFreeNames(expression, shadowed, free);

            var captures =
                free.Where(closure.CapturedBindings.ContainsKey).Order(StringComparer.Ordinal)
                .Select(name => (Name: name, Value: Reference(Export(closure.CapturedBindings[name])))).ToArray();

            var implementationName = FreshName();

            m_generated.Add(
                implementationName,
                Function(
                    implementationName,
                    [.. captures.Select(capture => new AST.Pattern.VarPattern(capture.Name)), .. arguments],
                    RestoreExpression(expression)));

            m_captures.Add(implementationName, captures);
            return Apply(Reference(implementationName), closure.ArgumentsAlreadyCollected);
        }

        private AST.Expression RestoreExpression(PreparedExpression expression) =>
            expression switch
            {
                PreparedExpression.ValueLiteral literal => new AST.Expression.Identifier(Export(literal.Value)),
                PreparedExpression.Identifier identifier => new AST.Expression.Identifier(identifier.QualifiedName),

                PreparedExpression.Negation negation =>
                new AST.Expression.Negation(RestoreExpression(negation.Expression)),

                PreparedExpression.ListExpr list =>
                new AST.Expression.ListExpr([.. list.Elements.Select(RestoreExpression)]),

                PreparedExpression.TupledExpression tuple =>
                new AST.Expression.TupledExpression([.. tuple.Elements.Select(RestoreExpression)]),

                PreparedExpression.IfBlock conditional =>
                new AST.Expression.IfBlock(
                    RestoreExpression(conditional.Condition),
                    RestoreExpression(conditional.ThenBlock),
                    RestoreExpression(conditional.ElseBlock)),

                PreparedExpression.PrefixOperator prefix => new AST.Expression.PrefixOperator(prefix.Operator),

                PreparedExpression.Application application =>
                new AST.Expression.Application(
                    RestoreExpression(application.Function),
                    [.. application.Arguments.Select(RestoreExpression)]),

                PreparedExpression.OperatorApplication operation =>
                new AST.Expression.OperatorApplication(
                    operation.Operator,
                    operation.Direction,
                    RestoreExpression(operation.Left),
                    RestoreExpression(operation.Right)),

                PreparedExpression.LambdaExpression lambda =>
                new AST.Expression.LambdaExpression(lambda.Arguments, RestoreExpression(lambda.Expression)),

                PreparedExpression.CaseExpression match =>
                new AST.Expression.CaseExpression(
                    RestoreExpression(match.Expression),
                    [.. match.Dispatch.Steps.SelectMany(RestoreCases)]),

                PreparedExpression.LetExpression let =>
                new AST.Expression.LetExpression(
                    [.. let.Declarations.Select(RestoreLet)],
                    RestoreExpression(let.Expression)),

                PreparedExpression.RecordExpr record =>
                new AST.Expression.RecordExpr([.. record.Fields.Select(RestoreField)]),

                PreparedExpression.RecordAccess access =>
                new AST.Expression.RecordAccess(
                    RestoreExpression(access.Record),
                    access.FieldName,
                    access.FieldNameValue),

                PreparedExpression.RecordAccessFunction access =>
                new AST.Expression.RecordAccessFunction(access.FieldName, access.FieldNameValue),

                PreparedExpression.RecordUpdateExpression update =>
                new AST.Expression.RecordUpdateExpression(update.RecordName, [.. update.Fields.Select(RestoreField)]),

                PreparedExpression.GLSLExpression shader => new AST.Expression.GLSLExpression(shader.Code),

                _ =>
                throw new NotImplementedException(
                    nameof(RestoreExpression) + " does not handle expression variant: " + expression.GetType().Name)
            };

        private IEnumerable<AST.Case> RestoreCases(PreparedCaseDispatchSegment segment) =>
            segment switch
            {
                PreparedCaseDispatchSegment.ConstantValuePatterns constants =>
                constants.Cases.Select(item => new AST.Case(item.Pattern, RestoreExpression(item.Expression))),

                PreparedCaseDispatchSegment.PatternCase pattern =>
                [new AST.Case(pattern.Pattern, RestoreExpression(pattern.Expression))],

                PreparedCaseDispatchSegment.DiscardCase discard =>
                [new AST.Case(new AST.Pattern.AllPattern(), RestoreExpression(discard.Expression))],

                _ =>
                throw new NotImplementedException(
                    nameof(RestoreCases) + " does not handle dispatch variant: " + segment.GetType().Name)
            };

        private AST.LetDeclaration RestoreLet(PreparedLetDeclaration declaration) =>
            declaration switch
            {
                PreparedLetDeclaration.LetFunction function =>
                new AST.LetDeclaration.LetFunction(
                    new(
                        function.Function.Signature,
                        new(
                            function.Function.Declaration.Name,
                            function.Function.Declaration.Arguments,
                            RestoreExpression(function.Function.Declaration.Expression)))),

                PreparedLetDeclaration.LetDestructuring destructure =>
                new AST.LetDeclaration.LetDestructuring(destructure.Pattern, RestoreExpression(destructure.Expression)),

                _ =>
                throw new NotImplementedException(
                    nameof(RestoreLet) + " does not handle declaration variant: " + declaration.GetType().Name)
            };

        private AST.RecordSetter RestoreField(PreparedRecordSetter field) =>
            new(field.FieldName, field.FieldNameValue, RestoreExpression(field.Value));
    }
}
