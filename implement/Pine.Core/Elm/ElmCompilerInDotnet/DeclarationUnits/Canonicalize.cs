using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;
using Pine.Core.Elm.Syntax.Abstract;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Expression = Pine.Core.Elm.Syntax.Abstract.Expression;
using Pattern = Pine.Core.Elm.Syntax.Abstract.Pattern;

namespace Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;

/// <summary>Pure declaration-local syntax processing over explicit, anonymous resolution answers.</summary>
public static class Canonicalize
{
    /// <summary>Finds the lexical questions needed by the complete declaration, including unused lets.</summary>
    public static ValueList<NameQuery> Questions(Declaration syntax)
    {
        var worker = new Worker([], true, false);
        worker.RewriteDeclaration(syntax, SyntaxPath.Root, SyntaxPath.Root, []);
        return new(worker.Questions);
    }

    /// <summary>Transforms exactly one declaration. Dependency bodies are not inputs to this function.</summary>
    public static CanonicalDeclaration Apply(CanonicalizationInput input)
    {
        var worker = new Worker(input.Answers, false, input.PreserveOperators);
        var syntax = worker.RewriteDeclaration(input.Syntax, SyntaxPath.Root, SyntaxPath.Root, []);
        return new(syntax, new(worker.References), new(worker.Diagnostics), new(worker.Projections));
    }

    /// <summary>Rebinds resolved references without consulting any source scope.</summary>
    public static Declaration MapNames(Declaration syntax, Func<DeclQualifiedName, DeclQualifiedName> map) =>
        new Worker([], true, true, map).RewriteDeclaration(syntax, SyntaxPath.Root, SyntaxPath.Root, []);

    /// <summary>Temporary declaration-local references disappear when SCC dependencies are attached.</summary>
    public static DeclQualifiedName SlotName(int slot, string symbol) =>
        DeclQualifiedName.Create(["PineReference", "R" + slot], symbol);

    /// <summary>Recognizes an anonymous reference, not a source namespace.</summary>
    public static int? ReferenceSlot(IReadOnlyList<string> module) =>
        module is ["PineReference", var slot] && slot.StartsWith('R') &&
        int.TryParse(slot.AsSpan(1), out var index)
        ?
        index
        :
        null;

    private sealed class Worker(IEnumerable<ResolutionAnswer> answers, bool planning, bool preserveOperators,
        Func<DeclQualifiedName, DeclQualifiedName>? map = null)
    {
        private readonly Dictionary<NameQuery, ResolutionAnswer> answers = answers.ToDictionary(item => item.Query);

        private readonly HashSet<NameQuery> seen = [];

        public List<NameQuery> Questions { get; } = [];

        public List<ReferenceUse> References { get; } = [];

        public List<SyntaxDiagnostic> Diagnostics { get; } = [];

        public List<PathProjection> Projections { get; } = [];

        private void Project(SyntaxPath input, SyntaxPath output) => Projections.Add(new(output, input));

        private ResolutionAnswer Ask(NameQuery query)
        {
            if (seen.Add(query))
                Questions.Add(query);

            if (planning)
                return new(query, ResolutionKind.Absent);

            return
                answers.TryGetValue(query, out var answer)
                ?
                answer
                :
                throw new InvalidOperationException("Missing explicit resolution answer for " + query);
        }

        private DeclQualifiedName Resolve(NameQuery query, SyntaxPath path)
        {
            if (map is not null)
            {
                return
                    map(
                        DeclQualifiedName.FromString(
                            (query.Qualifier.Length is 0 ? "" : query.Qualifier + ".") + query.Name));
            }

            var answer = Ask(query);

            switch (answer.Kind)
            {
                case ResolutionKind.Declaration:
                case ResolutionKind.TypeOnly:
                case ResolutionKind.DeferredQualified:
                    References.Add(
                        new(
                            answer.Slot,
                            answer.Symbol,
                            query.Kind is QueryKind.Type,
                            answer.Kind is ResolutionKind.TypeOnly,
                            path));

                    return SlotName(answer.Slot, answer.Symbol);

                case ResolutionKind.Native:
                    return DeclQualifiedName.FromString(answer.NativeModule + "." + answer.Symbol);

                case ResolutionKind.MissingName:
                case ResolutionKind.MissingModule:
                case ResolutionKind.Private:
                case ResolutionKind.WrongNamespace:
                case ResolutionKind.UnimportedQualifier:
                case ResolutionKind.Ambiguous:
                    Diagnostics.Add(new(path, query, answer.Kind));

                    return
                        DeclQualifiedName.FromString(
                            (query.Qualifier.Length is 0 ? "" : query.Qualifier + ".") + query.Name);

                case ResolutionKind.Absent when planning:
                    return
                        DeclQualifiedName.FromString(
                            (query.Qualifier.Length is 0 ? "" : query.Qualifier + ".") + query.Name);

                case ResolutionKind.Present:
                case ResolutionKind.Absent:
                    throw new InvalidOperationException("A binding-presence answer cannot resolve a reference.");

                default:
                    throw new NotImplementedException(nameof(Resolve) + " does not handle resolution: " + answer.Kind);
            }
        }

        private void Bind(string name, SyntaxPath path, HashSet<string> bound, ImmutableHashSet<string> outer)
        {
            if (name is "_")
                return;

            var query = new NameQuery(QueryKind.ModuleBinding, "", name);
            var present = Ask(query).Kind is ResolutionKind.Present;

            if (!bound.Add(name) || outer.Contains(name) || present)
                Diagnostics.Add(new(path, query, ResolutionKind.Present));
        }

        private void BindPattern(
            Pattern pattern,
            SyntaxPath path,
            HashSet<string> bound,
            ImmutableHashSet<string> outer)
        {
            switch (pattern)
            {
                case Pattern.VarPattern variable:
                    Bind(variable.Name, path, bound, outer);
                    break;

                case Pattern.RecordPattern record:
                    foreach (var (field, index) in record.Fields.Select((field, index) => (field, index)))
                        Bind(field.FieldName, path.Append(SyntaxStepKind.Child, index), bound, outer);

                    break;

                case Pattern.TuplePattern tuple:
                    for (var index = 0; index < tuple.Elements.Count; index++)
                        BindPattern(tuple.Elements[index], path.Append(SyntaxStepKind.Child, index), bound, outer);

                    break;

                case Pattern.ListPattern list:
                    for (var index = 0; index < list.Elements.Count; index++)
                        BindPattern(list.Elements[index], path.Append(SyntaxStepKind.Child, index), bound, outer);

                    break;

                case Pattern.UnConsPattern cons:
                    BindPattern(cons.Head, path.Append(SyntaxStepKind.Child), bound, outer);
                    BindPattern(cons.Tail, path.Append(SyntaxStepKind.Child, 1), bound, outer);
                    break;

                case Pattern.NamedPattern named:
                    for (var index = 0; index < named.Arguments.Count; index++)
                        BindPattern(named.Arguments[index], path.Append(SyntaxStepKind.Argument, index), bound, outer);

                    break;

                case Pattern.AsPattern alias:
                    BindPattern(alias.Pattern, path.Append(SyntaxStepKind.Child), bound, outer);
                    Bind(alias.Name, path, bound, outer);
                    break;

                case Pattern.AllPattern:
                case Pattern.UnitPattern:
                case Pattern.CharPattern:
                case Pattern.StringPattern:
                case Pattern.IntPattern:
                case Pattern.FloatPattern:
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(BindPattern) + " does not handle pattern: " + pattern.GetType().Name);
            }
        }

        public Declaration RewriteDeclaration(
            Declaration declaration,
            SyntaxPath input,
            SyntaxPath output,
            ImmutableHashSet<string> scope)
        {
            Project(input, output);

            switch (declaration)
            {
                case Declaration.FunctionDeclaration function:
                    return new Declaration.FunctionDeclaration(Function(function.Function, input, output, scope));

                case Declaration.AliasDeclaration alias:
                    return
                        new Declaration.AliasDeclaration(
                            new TypeAlias(
                                alias.TypeAlias.Name,
                                alias.TypeAlias.Generics,
                                Type(
                                    alias.TypeAlias.TypeAnnotation,
                                    input.Append(SyntaxStepKind.TypeAnnotation),
                                    output.Append(SyntaxStepKind.TypeAnnotation))
                                ));

                case Declaration.ChoiceTypeDeclaration choice:
                    return
                        new Declaration.ChoiceTypeDeclaration(
                            new ChoiceTypeStruct(
                                choice.TypeDeclaration.Name,
                                choice.TypeDeclaration.Generics,
                                [
                                    .. choice.TypeDeclaration.Constructors.Select(
                                        (constructor, index) =>
                                        {
                                            var from = input.Append(SyntaxStepKind.Constructor, index);
                                            var to = output.Append(SyntaxStepKind.Constructor, index);
                                            Project(from, to);

                                            return
                                                ValueConstructor.Create(
                                                    constructor.Name,
                                                    [
                                                        .. constructor.Arguments
                                                        .Select(
                                                            (argument, argumentIndex) =>
                                                            Type(
                                                                argument,
                                                                from.Append(SyntaxStepKind.Argument, argumentIndex),
                                                                to.Append(SyntaxStepKind.Argument, argumentIndex)))
                                                    ]);
                                        })
                                ]));

                case Declaration.PortDeclaration port:
                    return
                        new Declaration.PortDeclaration(
                            Signature(
                                port.Signature,
                                input.Append(SyntaxStepKind.Signature),
                                output.Append(SyntaxStepKind.Signature)));

                case Declaration.InfixDeclaration infix:
                    var target = Resolve(new(QueryKind.Value, "", infix.Infix.FunctionName), input);
                    return new Declaration.InfixDeclaration(infix.Infix with { FunctionName = target.FullName });

                default:
                    throw new NotImplementedException(
                        nameof(Declaration) + " does not handle declaration: " + declaration.GetType().Name);
            }
        }

        private Signature Signature(Signature signature, SyntaxPath input, SyntaxPath output)
        {
            Project(input, output);

            return
                signature with
                {
                    TypeAnnotation =
                    Type(
                        signature.TypeAnnotation,
                        input.Append(SyntaxStepKind.TypeAnnotation),
                        output.Append(SyntaxStepKind.TypeAnnotation))
                };
        }

        private FunctionStruct Function(
            FunctionStruct function,
            SyntaxPath input,
            SyntaxPath output,
            ImmutableHashSet<string> scope)
        {
            var from = input.Append(SyntaxStepKind.Implementation);
            var to = output.Append(SyntaxStepKind.Implementation);
            Project(from, to);
            var parameters = new HashSet<string>();

            for (var index = 0; index < function.Declaration.Arguments.Count; index++)
            {
                BindPattern(
                    function.Declaration.Arguments[index],
                    from.Append(SyntaxStepKind.Argument, index),
                    parameters,
                    []);
            }

            return
                new(
                    function.Signature is null
                    ?
                    null
                    :
                    Signature(
                        function.Signature,
                        input.Append(SyntaxStepKind.Signature),
                        output.Append(SyntaxStepKind.Signature)),
                    new FunctionImplementation(
                        function.Declaration.Name,
                        [
                            .. function.Declaration.Arguments.Select(
                                (pattern, index) =>
                                RewritePattern(
                                    pattern,
                                    from.Append(SyntaxStepKind.Argument, index),
                                    to.Append(SyntaxStepKind.Argument, index)))
                        ],
                        RewriteExpression(
                            function.Declaration.Expression,
                            from.Append(SyntaxStepKind.Body),
                            to.Append(SyntaxStepKind.Body),
                            scope.Union(parameters))
                        ));
        }

        private TypeAnnotation Type(TypeAnnotation type, SyntaxPath input, SyntaxPath output)
        {
            Project(input, output);

            switch (type)
            {
                case TypeAnnotation.Typed typed:
                    var name = Resolve(new(QueryKind.Type, string.Join(".", typed.ModuleName), typed.Name), input);

                    return
                        new TypeAnnotation.Typed(
                            name.Namespaces,
                            name.DeclName,
                            [
                                .. typed.TypeArguments.Select(
                                    (argument, index) => Type(
                                        argument,
                                        input.Append(SyntaxStepKind.Argument, index),
                                        output.Append(SyntaxStepKind.Argument, index)))
                            ]);

                case TypeAnnotation.Tupled tuple:
                    return
                        new TypeAnnotation.Tupled(
                            [
                                .. tuple.TypeAnnotations.Select(
                                    (child, index) => Type(
                                        child,
                                        input.Append(SyntaxStepKind.Child, index),
                                        output.Append(SyntaxStepKind.Child, index)))
                            ]);

                case TypeAnnotation.FunctionTypeAnnotation function:
                    return
                        new TypeAnnotation.FunctionTypeAnnotation(
                            Type(
                                function.ArgumentType,
                                input.Append(SyntaxStepKind.Child),
                                output.Append(SyntaxStepKind.Child)),
                            Type(
                                function.ReturnType,
                                input.Append(SyntaxStepKind.Child, 1),
                                output.Append(SyntaxStepKind.Child, 1)));

                case TypeAnnotation.Record record:
                    return new TypeAnnotation.Record(RecordType(record.RecordDefinition, input, output));

                case TypeAnnotation.GenericRecord record:
                    return
                        new TypeAnnotation.GenericRecord(
                            record.GenericName,
                            RecordType(record.RecordDefinition, input, output));

                case TypeAnnotation.GenericType:
                case TypeAnnotation.Unit:
                    return type;

                default:
                    throw new NotImplementedException(nameof(Type) + " does not handle type: " + type.GetType().Name);
            }
        }

        private RecordDefinition RecordType(RecordDefinition record, SyntaxPath input, SyntaxPath output)
        {
            var occurrences = new Dictionary<string, int>();

            return
                new(
                    [
                        .. record.Fields.Select(
                            field =>
                            {
                                var occurrence = occurrences.GetValueOrDefault(field.FieldName);
                                occurrences[field.FieldName] = occurrence + 1;

                                var from =
                                    input.Append(
                                        SyntaxStepKind.RecordField,
                                        name: field.FieldName,
                                        occurrence: occurrence);

                                var to =
                                    output.Append(
                                        SyntaxStepKind.RecordField,
                                        name: field.FieldName,
                                        occurrence: occurrence);

                                Project(from, to);

                                return
                                    RecordField.Create(
                                        field.FieldName,
                                        Type(
                                            field.FieldType,
                                            from.Append(SyntaxStepKind.TypeAnnotation),
                                            to.Append(SyntaxStepKind.TypeAnnotation)));
                            })
                    ]);
        }

        private Pattern RewritePattern(Pattern pattern, SyntaxPath input, SyntaxPath output)
        {
            Project(input, output);

            switch (pattern)
            {
                case Pattern.NamedPattern named:
                    var name =
                        Resolve(new(QueryKind.Value, string.Join(".", named.Name.ModuleName), named.Name.Name), input);

                    return
                        new Pattern.NamedPattern(
                            new QualifiedNameRef(name.Namespaces, name.DeclName),
                            [
                                .. named.Arguments.Select(
                                    (argument, index) => RewritePattern(
                                        argument,
                                        input.Append(SyntaxStepKind.Argument, index),
                                        output.Append(SyntaxStepKind.Argument, index)))
                            ]);

                case Pattern.TuplePattern tuple:
                    return
                        new Pattern.TuplePattern(
                            [
                                .. tuple.Elements.Select(
                                    (child, index) => RewritePattern(
                                        child,
                                        input.Append(SyntaxStepKind.Child, index),
                                        output.Append(SyntaxStepKind.Child, index)))
                            ]);

                case Pattern.ListPattern list:
                    return
                        new Pattern.ListPattern(
                            [
                                .. list.Elements.Select(
                                    (child, index) => RewritePattern(
                                        child,
                                        input.Append(SyntaxStepKind.Child, index),
                                        output.Append(SyntaxStepKind.Child, index)))
                            ]);

                case Pattern.UnConsPattern cons:
                    return
                        new Pattern.UnConsPattern(
                            RewritePattern(
                                cons.Head,
                                input.Append(SyntaxStepKind.Child),
                                output.Append(SyntaxStepKind.Child)),
                            RewritePattern(
                                cons.Tail,
                                input.Append(SyntaxStepKind.Child, 1),
                                output.Append(SyntaxStepKind.Child, 1)));

                case Pattern.AsPattern alias:
                    return
                        new Pattern.AsPattern(
                            RewritePattern(
                                alias.Pattern,
                                input.Append(SyntaxStepKind.Child),
                                output.Append(SyntaxStepKind.Child)),
                            alias.Name);

                case Pattern.AllPattern:
                case Pattern.VarPattern:
                case Pattern.UnitPattern:
                case Pattern.CharPattern:
                case Pattern.StringPattern:
                case Pattern.IntPattern:
                case Pattern.FloatPattern:
                case Pattern.RecordPattern:
                    return pattern;

                default:
                    throw new NotImplementedException(nameof(Pattern) + " does not handle pattern: " + pattern.GetType().Name);
            }
        }

        private IReadOnlyList<RecordSetter> Fields(
            IReadOnlyList<RecordSetter> fields,
            SyntaxPath input,
            SyntaxPath output,
            ImmutableHashSet<string> scope)
        {
            var occurrences = new Dictionary<string, int>();

            return
                [
                .. fields.Select(
                    field =>
                    {
                        var occurrence = occurrences.GetValueOrDefault(field.FieldName);
                        occurrences[field.FieldName] = occurrence + 1;

                        var from =
                            input.Append(SyntaxStepKind.RecordField, name: field.FieldName, occurrence: occurrence);

                        var to =
                            output.Append(SyntaxStepKind.RecordField, name: field.FieldName, occurrence: occurrence);

                        Project(from, to);
                        return field with { Value = RewriteExpression(field.Value, from.Append(SyntaxStepKind.Child), to.Append(SyntaxStepKind.Child), scope) };
                    })
                ];
        }

        private Expression RewriteExpression(
            Expression expression,
            SyntaxPath input,
            SyntaxPath output,
            ImmutableHashSet<string> scope)
        {
            Project(input, output);

            Expression Child(Expression child, int index) =>
                RewriteExpression(
                    child,
                    input.Append(SyntaxStepKind.Child, index),
                    output.Append(SyntaxStepKind.Child, index),
                    scope);

            switch (expression)
            {
                case Expression.Identifier identifier:
                    var name = identifier.QualifiedName;

                    return
                        name.Namespaces.Count is 0 && scope.Contains(name.DeclName)
                        ?
                        expression
                        :
                        new Expression.Identifier(
                            Resolve(new(QueryKind.Value, string.Join(".", name.Namespaces), name.DeclName), input));

                case Expression.PrefixOperator prefix:
                    if (map is not null)
                        return prefix;

                    return new Expression.Identifier(Resolve(new(QueryKind.Operator, "", prefix.Operator), input));

                case Expression.OperatorApplication operation:
                    var function = Resolve(new(QueryKind.Operator, "", operation.Operator), input);

                    if (preserveOperators || planning)
                    {
                        return
                            new Expression.OperatorApplication(
                                function.FullName,
                                operation.Direction,
                                Child(operation.Left, 0),
                                Child(operation.Right, 1));
                    }

                    Project(input, output.Append(SyntaxStepKind.Child));

                    return
                        new Expression.Application(
                            new Expression.Identifier(function),
                            [
                            RewriteExpression(
                                operation.Left,
                                input.Append(SyntaxStepKind.Child),
                                output.Append(SyntaxStepKind.Child, 1),
                                scope),
                            RewriteExpression(
                                operation.Right,
                                input.Append(SyntaxStepKind.Child, 1),
                                output.Append(SyntaxStepKind.Child, 2),
                                scope)
                            ]);

                case Expression.Application application:
                    return
                        new Expression.Application(
                            Child(application.Function, 0),
                            [.. application.Arguments.Select((argument, index) => Child(argument, index + 1))]);

                case Expression.IfBlock conditional:
                    return
                        new Expression.IfBlock(
                            Child(conditional.Condition, 0),
                            Child(conditional.ThenBlock, 1),
                            Child(conditional.ElseBlock, 2));

                case Expression.Negation negation:
                    return new Expression.Negation(Child(negation.Expression, 0));

                case Expression.ListExpr list:
                    return new Expression.ListExpr([.. list.Elements.Select(Child)]);

                case Expression.TupledExpression tuple:
                    return new Expression.TupledExpression([.. tuple.Elements.Select(Child)]);

                case Expression.RecordExpr record:
                    return new Expression.RecordExpr(Fields(record.Fields, input, output, scope));

                case Expression.RecordAccess access:
                    return access with { Record = Child(access.Record, 0) };

                case Expression.RecordUpdateExpression update:
                    if (map is not null || scope.Contains(update.RecordName))
                    {
                        return
                            new Expression.RecordUpdateExpression(
                                update.RecordName,
                                Fields(update.Fields, input, output, scope));
                    }

                    var target = Resolve(new(QueryKind.Value, "", update.RecordName), input);
                    var fresh = update.RecordName + "__recordUpdateBase";

                    if (planning)
                        Ask(new(QueryKind.FreshBinding, "", fresh));

                    if (!planning)
                    {
                        for (var index = 1; scope.Contains(fresh) ||
                            Ask(new(QueryKind.FreshBinding, "", fresh)).Kind is ResolutionKind.Present; index++)
                        {
                            fresh = update.RecordName + "__recordUpdateBase" + index;
                        }
                    }

                    var bindingPath = output.Append(SyntaxStepKind.LetDeclaration);
                    Project(input, bindingPath);
                    Project(input, bindingPath.Append(SyntaxStepKind.Pattern));
                    Project(input, bindingPath.Append(SyntaxStepKind.Body));
                    Project(input, output.Append(SyntaxStepKind.Body));

                    return
                        new Expression.LetExpression(
                            [
                            new LetDeclaration.LetDestructuring(
                                new Pattern.VarPattern(fresh),
                                new Expression.Identifier(target))
                            ],
                            new Expression.RecordUpdateExpression(
                                fresh,
                                Fields(update.Fields, input, output.Append(SyntaxStepKind.Body), scope)));

                case Expression.LambdaExpression lambda:
                    var parameters = new HashSet<string>();

                    for (var index = 0; index < lambda.Arguments.Count; index++)
                    {
                        BindPattern(
                            lambda.Arguments[index],
                            input.Append(SyntaxStepKind.Argument, index),
                            parameters,
                            scope);
                    }

                    return
                        new Expression.LambdaExpression(
                            [
                                .. lambda.Arguments
                                .Select(
                                    (pattern, index) => RewritePattern(
                                        pattern,
                                        input.Append(SyntaxStepKind.Argument, index),
                                        output.Append(SyntaxStepKind.Argument, index)))
                            ],
                            RewriteExpression(
                                lambda.Expression,
                                input.Append(SyntaxStepKind.Body),
                                output.Append(SyntaxStepKind.Body),
                                scope.Union(parameters)));

                case Expression.CaseExpression cases:
                    return
                        new Expression.CaseExpression(
                            Child(cases.Expression, 0),
                            [
                                .. cases.Cases
                                .Select(
                                    (branch, index) =>
                                    {
                                        var from = input.Append(SyntaxStepKind.CaseBranch, index);
                                        var to = output.Append(SyntaxStepKind.CaseBranch, index);
                                        Project(from, to);
                                        var bindings = new HashSet<string>();
                                        BindPattern(branch.Pattern, from.Append(SyntaxStepKind.Pattern), bindings, scope);

                                        return
                                            new Case(
                                                RewritePattern(
                                                    branch.Pattern,
                                                    from.Append(SyntaxStepKind.Pattern),
                                                    to.Append(SyntaxStepKind.Pattern)),
                                                RewriteExpression(
                                                    branch.Expression,
                                                    from.Append(SyntaxStepKind.Body),
                                                    to.Append(SyntaxStepKind.Body),
                                                    scope.Union(bindings)));
                                    })
                            ]);

                case Expression.LetExpression let:
                    var locals = new HashSet<string>();

                    for (var index = 0; index < let.Declarations.Count; index++)
                    {
                        var from = input.Append(SyntaxStepKind.LetDeclaration, index);

                        switch (let.Declarations[index])
                        {
                            case LetDeclaration.LetFunction local:
                                Bind(
                                    local.Function.Declaration.Name,
                                    from.Append(SyntaxStepKind.Implementation),
                                    locals,
                                    scope);

                                break;

                            case LetDeclaration.LetDestructuring local:
                                BindPattern(local.Pattern, from.Append(SyntaxStepKind.Pattern), locals, scope);
                                break;

                            default:
                                throw new NotImplementedException(
                                    nameof(Expression) + " does not handle let: " +
                                    let.Declarations[index].GetType().Name);
                        }
                    }

                    var extended = scope.Union(locals);

                    return
                        new Expression.LetExpression(
                            [
                                .. let.Declarations.Select(
                                    (local, index) =>
                                    {
                                        var from = input.Append(SyntaxStepKind.LetDeclaration, index);
                                        var to = output.Append(SyntaxStepKind.LetDeclaration, index);
                                        Project(from, to);

                                        return local switch
                                        {
                                            LetDeclaration.LetFunction functionDeclaration =>
                                            (LetDeclaration)new LetDeclaration.LetFunction(
                                                Function(functionDeclaration.Function, from, to, extended)),

                                            LetDeclaration.LetDestructuring destructuring =>
                                            new LetDeclaration.LetDestructuring(
                                                RewritePattern(
                                                    destructuring.Pattern,
                                                    from.Append(SyntaxStepKind.Pattern),
                                                    to.Append(SyntaxStepKind.Pattern)),
                                                RewriteExpression(
                                                    destructuring.Expression,
                                                    from.Append(SyntaxStepKind.Body),
                                                    to.Append(SyntaxStepKind.Body),
                                                    extended)),

                                            _ =>
                                            throw new NotImplementedException(
                                                nameof(Expression) + " does not handle let: " + local.GetType().Name)
                                        };
                                    })
                            ],
                            RewriteExpression(
                                let.Expression,
                                input.Append(SyntaxStepKind.Body),
                                output.Append(SyntaxStepKind.Body),
                                extended));

                case Expression.UnitExpr:
                case Expression.StringLiteral:
                case Expression.CharLiteral:
                case Expression.IntegerLiteral:
                case Expression.FloatLiteral:
                case Expression.RecordAccessFunction:
                case Expression.GLSLExpression:
                    return expression;

                default:
                    throw new NotImplementedException(
                        nameof(Expression) + " does not handle expression: " + expression.GetType().Name);
            }
        }
    }
}
