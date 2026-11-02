using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
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
        CancellationToken cancellationToken)
    {
        var export = new PineClosureCompiler(canonicalDeclarations, cancellationToken);

        var roots =
            applications.Select(
                application => export.AddApplication(application.Function, application.Arguments))
            .ToArray();

        return export.Compile(roots);
    }

    private sealed class PineClosureCompiler(
        IReadOnlyDictionary<DeclQualifiedName, AST.Declaration> canonicalDeclarations,
        CancellationToken cancellationToken)
    {
        private static readonly DeclQualifiedName s_ignoredDebugLog =
            DeclQualifiedName.Create(["Debug"], "log");

        private readonly Dictionary<PineValueInProcess, DeclQualifiedName> m_values =
            new(ReferenceEqualityComparer.Instance);

        private readonly Queue<(DeclQualifiedName Name, PineValueInProcess Value)> m_pending = new();

        private readonly Dictionary<DeclQualifiedName, AST.Declaration> m_generated = new();

        private readonly Dictionary<DeclQualifiedName, Model.Expression> m_bodies = new();

        private readonly Dictionary<DeclQualifiedName, (string Name, Model.Expression Value)[]> m_captures = new();

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

            var declarations =
                canonicalDeclarations
                .Where(
                    entry => entry.Key.Namespaces.Count != 1 ||
                        entry.Key.Namespaces[0] is not ("Basics" or "Debug") ||
                        entry.Value is not AST.Declaration.FunctionDeclaration)
                .Concat(m_generated)
                .Append(
                    KeyValuePair.Create(
                        s_ignoredDebugLog,
                        Function(
                            s_ignoredDebugLog,
                            [new AST.Pattern.AllPattern(), new AST.Pattern.VarPattern("value")],
                            AST.Expression.Identifier.Create([], "value"))))
                .ToImmutableDictionary(
                    entry => entry.Key,
                    entry => DebugLogRemoval.RewriteDeclaration(LowerOperatorApplications(entry.Value)));

            // Preserve source compilation's pipe and short-circuit semantics before creating resolved IR.
            declarations =
                BuiltinOperatorLowering.Apply(declarations)
                .Extract(
                    error => throw new ElmCompilationException("Failed lowering discovered Elm test closures: " + error));

            var program = Model.FromCanonicalSyntax.Convert(declarations);
            var functions = program.Functions.ToBuilder();

            foreach (var (name, body) in m_bodies)
            {
                var id = new Model.FunctionId.Source(name);
                functions[id] = functions[id] with { Body = body };
            }

            foreach (var (name, captures) in m_captures)
            {
                var id = new Model.FunctionId.Source(name);
                var function = functions[id];
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
                Model.Compilation.Compile(program)
                .Extract(
                    error =>
                    throw new ElmCompilationException("Failed compiling discovered Elm test closures: " + error));

            var parseCache = new PineVMParseCache();

            return
                roots.Select(
                    name =>
                    {
                        var callable = compiled[new Model.FunctionId.Source(name)];

                        return
                            new PineEvaluation(
                                parseCache.ParseExpression(callable.EncodedBody).Extract(
                                    error => throw new InvalidOperationException(error)),
                                PineValue.List([PineValue.List([.. callable.EnvFunctions])]));
                    }).ToArray();
        }

        private static AST.Declaration LowerOperatorApplications(AST.Declaration declaration) =>
            declaration is AST.Declaration.FunctionDeclaration function
            ?
            new AST.Declaration.FunctionDeclaration(
                function.Function with
                {
                    Declaration =
                    function.Function.Declaration with
                    {
                        Expression = LowerOperatorApplications(function.Function.Declaration.Expression)
                    }
                })
            :
            declaration;

        private static AST.Expression LowerOperatorApplications(AST.Expression expression)
        {
            var lowered =
                ElmSyntaxAbstractTransformations.MapChildExpressions(expression, LowerOperatorApplications);

            return
                lowered is AST.Expression.OperatorApplication operation && operation.Operator.Contains('.')
                ?
                new AST.Expression.Application(
                    new AST.Expression.Identifier(DeclQualifiedName.FromString(operation.Operator)),
                    [operation.Left, operation.Right])
                :
                lowered;
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
