using Pine.Core.CodeAnalysis;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using IR = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;
using AST = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;
using Convert = Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation.ConvertFromAbstractSyntax;
using Type = Pine.Core.Elm.ElmCompilerInDotnet.TypeInference.InferredType;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>Type evidence for a record read that still requires a generic implementation.</summary>
public sealed record GenericRecordAccessEvidence(Type SubjectType, RecordOperationProvenance Provenance);

/// <summary>The lowered declaration dictionary and evidence attached to its residual generic reads.</summary>
public sealed record IntermediateRepresentationLoweringResult(
    ImmutableDictionary<DeclQualifiedName, IR.Declaration> Declarations,
    ImmutableDictionary<IR.Expression, GenericRecordAccessEvidence> GenericAccessEvidence);

/// <summary>
/// Maps AST declarations to IR, selecting numeric and append implementations and lowering closed-record reads to projections.
/// Record patterns become explicit bindings so no generic read is introduced behind the IR's back.
/// </summary>
public static class LowerToIntermediateRepresentation
{
    private sealed record Context(
        string ModuleName,
        ImmutableDictionary<string, Type> Bindings,
        ImmutableHashSet<string> Parameters);

    /// <summary>Lowers a flat AST program. Generic function bodies retain their open-record types.</summary>
    public static IntermediateRepresentationLoweringResult Lower(
        IReadOnlyDictionary<DeclQualifiedName, AST.Declaration> declarations)
    {
        var aliases = ImmutableDictionary.CreateBuilder<DeclQualifiedName, TypeInference.TypeAliasDefinition>();
        var signatures = ImmutableDictionary.CreateBuilder<string, Type>();

        foreach (var (name, declaration) in declarations)
        {
            TypeInference.CollectFunctionSignaturesFromDeclaration(
                declaration,
                string.Join(".", name.Namespaces),
                signatures);

            if (declaration is AST.Declaration.AliasDeclaration alias)
            {
                aliases.Add(
                    name,
                    new TypeInference.TypeAliasDefinition(
                        alias.TypeAlias.Generics,
                        TypeInference.TypeAnnotationToInferredType(alias.TypeAlias.TypeAnnotation)));
            }
        }

        var aliasTypes = aliases.ToImmutable();
        var environment = new TypeInference.FunctionInferenceEnvironment(signatures.ToImmutable());
        var functionTypes = environment.FunctionTypes.ToBuilder();
        var constructorTypes = ImmutableDictionary.CreateBuilder<DeclQualifiedName, FunctionTypeInfo>();

        foreach (var (name, declaration) in declarations)
        {
            if (declaration is not AST.Declaration.ChoiceTypeDeclaration choice)
                continue;

            foreach (var constructor in choice.TypeDeclaration.Constructors)
            {
                var constructorName = DeclQualifiedName.Create(name.Namespaces, constructor.Name);
                var type = environment.FunctionTypes[constructorName];

                // Resolve constructor-argument aliases in the constructor's defining module.
                constructorTypes[constructorName] =
                    new FunctionTypeInfo(
                        type.ReturnType,
                        [
                        .. type.ParameterTypes.Select(
                            parameter => TypeInference.ExpandTypeAliases(parameter, aliasTypes, name.Namespaces))
                        ]);
            }
        }

        foreach (var (name, declaration) in declarations)
        {
            if (declaration is not AST.Declaration.FunctionDeclaration function)
                continue;

            if (function.Function.Signature is not null)
            {
                functionTypes[name] =
                    new FunctionTypeInfo(
                        TypeInference.GetFunctionReturnType(function.Function),
                        TypeInference.GetFunctionParameterTypes(function.Function));

                continue;
            }

            var inferred =
                TypeInference.InferFunctionDeclarationType(
                    function.Function.Declaration.Expression,
                    function.Function.Declaration.Arguments,
                    string.Join(".", name.Namespaces),
                    environment);

            functionTypes[name] =
                new FunctionTypeInfo(
                    inferred.returnType,
                    [
                    .. function.Function.Declaration.Arguments.Select(
                        argument =>
                        argument is AST.Pattern.VarPattern variable &&
                        inferred.parameterTypes.TryGetValue(variable.Name, out var type)
                        ?
                        type
                        :
                        new Type.UnknownType())
                    ]);
        }

        // The inference registry also contains implicit record constructors, unlike the emitter's AllFunctions.
        var runtimeModules =
            declarations
            .Where(entry => entry.Value is AST.Declaration.FunctionDeclaration)
            .Select(entry => string.Join(".", entry.Key.Namespaces))
            .ToImmutableHashSet(StringComparer.Ordinal);

        var program =
            new Rewriter(
                aliasTypes,
                functionTypes.ToImmutable(),
                constructorTypes.ToImmutable(),
                environment,
                runtimeModules);

        var result = ImmutableDictionary.CreateBuilder<DeclQualifiedName, IR.Declaration>();

        foreach (var (name, declaration) in declarations)
        {
            result[name] =
                declaration switch
                {
                    AST.Declaration.FunctionDeclaration function =>
                    new IR.Declaration.FunctionDeclaration(
                        program.RewriteFunction(function.Function, string.Join(".", name.Namespaces))),

                    AST.Declaration.ChoiceTypeDeclaration choice => Convert.FromDeclaration(choice),
                    AST.Declaration.AliasDeclaration alias => Convert.FromDeclaration(alias),
                    AST.Declaration.PortDeclaration port => Convert.FromDeclaration(port),
                    AST.Declaration.InfixDeclaration infix => Convert.FromDeclaration(infix),

                    _ =>
                    throw new NotImplementedException(
                        $"{nameof(Lower)} does not handle declaration variant: {declaration.GetType().Name}")
                };
        }

        return new(result.ToImmutable(), program.Evidence.ToImmutable());
    }

    private sealed class Rewriter(
        IReadOnlyDictionary<DeclQualifiedName, TypeInference.TypeAliasDefinition> aliases,
        IReadOnlyDictionary<DeclQualifiedName, FunctionTypeInfo> functions,
        IReadOnlyDictionary<DeclQualifiedName, FunctionTypeInfo> constructors,
        TypeInference.FunctionInferenceEnvironment environment,
        ImmutableHashSet<string> runtimeModules)
    {
        public ImmutableDictionary<IR.Expression, GenericRecordAccessEvidence>.Builder Evidence { get; } =
            ImmutableDictionary.CreateBuilder<IR.Expression, GenericRecordAccessEvidence>(
                ReferenceEqualityComparer.Instance);

        private HashSet<string> m_usedNames = [];

        private int m_nextName;

        private readonly Dictionary<string, ImmutableDictionary<DeclQualifiedName, TypeInference.TypeAliasDefinition>> m_aliasesByModule =
            [];

        public IR.FunctionStruct RewriteFunction(AST.FunctionStruct function, string moduleName)
        {
            m_usedNames = [];
            m_nextName = 0;
            CollectNames(function.Declaration.Expression);

            foreach (var argument in function.Declaration.Arguments)
                CollectPatternNames(argument, m_usedNames);

            return RewriteFunction(function, new Context(moduleName, [], []));
        }

        private IR.FunctionStruct RewriteFunction(AST.FunctionStruct function, Context outer)
        {
            var inferred =
                TypeInference.InferFunctionDeclarationType(
                    function.Declaration.Expression,
                    function.Declaration.Arguments,
                    outer.ModuleName,
                    environment);

            var parameterTypes =
                ElmCompiler.ExtractParameterTypes(function, constructors, functions, outer.ModuleName);

            var argumentTypes =
                function.Signature is null
                ?
                Enumerable.Repeat<Type>(new Type.UnknownType(), function.Declaration.Arguments.Count).ToArray()
                :
                TypeInference.GetFunctionParameterTypes(function).Select(type => Expand(type, outer)).ToArray();

            var context = outer;
            var arguments = new List<IR.Pattern>();
            var bindings = new List<IR.LetDeclaration>();

            for (var index = 0; index < function.Declaration.Arguments.Count; index++)
            {
                var argument = function.Declaration.Arguments[index];
                var names = new HashSet<string>();
                CollectPatternNames(argument, names);

                foreach (var name in names)
                {
                    context =
                        context with
                        {
                            Bindings =
                            context.Bindings.SetItem(
                                name,
                                parameterTypes.GetValueOrDefault(
                                    name,
                                    inferred.parameterTypes.GetValueOrDefault(name, new Type.UnknownType()))),
                            Parameters = context.Parameters.Add(name)
                        };
                }

                var matchedType =
                    function.Signature is null &&
                    argument is AST.Pattern.VarPattern variable &&
                    context.Bindings.TryGetValue(variable.Name, out var parameterType)
                    ?
                    parameterType
                    :
                    index < argumentTypes.Length ? argumentTypes[index] : new Type.UnknownType();

                arguments.Add(RewritePattern(argument, matchedType, bindings, ref context));
            }

            return
                new IR.FunctionStruct(
                    function.Signature is null ? null : Convert.FromSignature(function.Signature),
                    new IR.FunctionImplementation(
                        function.Declaration.Name,
                        arguments,
                        WithBindings(bindings, Rewrite(function.Declaration.Expression, context))));
        }

        private Type Expand(Type type, Context context)
        {
            if (!m_aliasesByModule.TryGetValue(context.ModuleName, out var trustedAliases))
            {
                // Retain the conservative alias-provenance boundary until canonical type identities are stronger.
                trustedAliases =
                    aliases.Where(
                        entry =>
                        string.Join(".", entry.Key.Namespaces) == context.ModuleName ||
                        (!runtimeModules.Contains(string.Join(".", entry.Key.Namespaces)) &&
                        aliases.Keys.Count(alias => alias.Namespaces.SequenceEqual(entry.Key.Namespaces)) is 1))
                    .ToImmutableDictionary();

                m_aliasesByModule.Add(context.ModuleName, trustedAliases);
            }

            return TypeInference.ExpandTypeAliases(type, trustedAliases, context.ModuleName.Split('.'));
        }

        private Type Infer(AST.Expression expression, Context context)
        {
            if (expression is AST.Expression.RecordAccess access)
            {
                var receiverType = Infer(access.Record, context);

                var fieldType =
                    receiverType is Type.RecordType closed
                    ?
                    closed.Fields.FirstOrDefault(field => field.FieldName == access.FieldName).FieldType
                    :
                    receiverType is Type.OpenRecordType open
                    ?
                    open.KnownFields.FirstOrDefault(field => field.FieldName == access.FieldName).FieldType
                    :
                    null;

                if (fieldType is not null)
                    return Expand(fieldType, context);
            }

            if (expression is AST.Expression.Application
                {
                    Function: AST.Expression.RecordAccessFunction accessor,
                    Arguments.Count: 1
                } application)
            {
                return
                    Infer(
                        new AST.Expression.RecordAccess(
                            application.Arguments[0],
                            accessor.FieldName,
                            accessor.FieldNameValue),
                        context);
            }

            return
                Expand(
                    TypeInference.InferExpressionType(
                        expression,
                        ImmutableDictionary<string, int>.Empty,
                        ImmutableDictionary<string, Type>.Empty,
                        context.Bindings,
                        context.ModuleName,
                        functions),
                    context);
        }

        private IR.Expression Rewrite(AST.Expression expression, Context context)
        {
            if (BuiltinOperatorLowering.TryLowerNumericAndAppend(expression, operand => Infer(operand, context)) is { } lowered)
                return Rewrite(lowered, context);

            switch (expression)
            {
                case AST.Expression.RecordAccess access:
                    return
                        Read(
                            Rewrite(access.Record, context),
                            access.FieldName,
                            access.FieldNameValue,
                            Infer(access.Record, context),
                            context,
                            access.Record);

                case AST.Expression.RecordAccessFunction accessor:
                    var loweredAccessor = Convert.FromExpression(accessor);

                    Evidence[loweredAccessor] =
                        new(new Type.UnknownType(), RecordOperationProvenance.RecordAccessFunctionEscaped);

                    return loweredAccessor;

                case AST.Expression.Application application:
                    if (application.Function is AST.Expression.RecordAccessFunction direct &&
                        application.Arguments.Count > 0)
                    {
                        var receiver = application.Arguments[0];

                        var read =
                            Read(
                                Rewrite(receiver, context),
                                direct.FieldName,
                                direct.FieldNameValue,
                                Infer(receiver, context),
                                context,
                                receiver);

                        return
                            application.Arguments.Count is 1
                            ?
                            read
                            :
                            new IR.Expression.Application(
                                read,
                                [
                                .. application.Arguments.Skip(1).Select(argument => Rewrite(argument, context))
                                ]);
                    }

                    return
                        new IR.Expression.Application(
                            Rewrite(application.Function, context),
                            [
                            .. application.Arguments.Select(argument => Rewrite(argument, context))
                            ]);

                case AST.Expression.LetExpression let:
                    return RewriteLet(let, context);

                case AST.Expression.CaseExpression cases:
                    var matchedType = Infer(cases.Expression, context);
                    var branches = new List<IR.Case>();

                    foreach (var branch in cases.Cases)
                    {
                        var branchContext = context;
                        var bindings = new List<IR.LetDeclaration>();
                        var pattern = RewritePattern(branch.Pattern, matchedType, bindings, ref branchContext);
                        branches.Add(new(pattern, WithBindings(bindings, Rewrite(branch.Expression, branchContext))));
                    }

                    return new IR.Expression.CaseExpression(Rewrite(cases.Expression, context), branches);

                case AST.Expression.LambdaExpression lambda:
                    var function =
                        RewriteFunction(
                            new AST.FunctionStruct(
                                null,
                                new AST.FunctionImplementation("", lambda.Arguments, lambda.Expression)),
                            context);

                    return
                        new IR.Expression.LambdaExpression(
                            function.Declaration.Arguments,
                            function.Declaration.Expression);

                case AST.Expression.Negation negation:
                    return new IR.Expression.Negation(Rewrite(negation.Expression, context));

                case AST.Expression.ListExpr list:
                    return new IR.Expression.ListExpr([.. list.Elements.Select(item => Rewrite(item, context))]);

                case AST.Expression.IfBlock conditional:
                    return
                        new IR.Expression.IfBlock(
                            Rewrite(conditional.Condition, context),
                            Rewrite(conditional.ThenBlock, context),
                            Rewrite(conditional.ElseBlock, context));

                case AST.Expression.OperatorApplication operation:
                    return
                        new IR.Expression.OperatorApplication(
                            operation.Operator,
                            operation.Direction,
                            Rewrite(operation.Left, context),
                            Rewrite(operation.Right, context));

                case AST.Expression.TupledExpression tuple:
                    return new IR.Expression.TupledExpression([.. tuple.Elements.Select(item => Rewrite(item, context))]);

                case AST.Expression.RecordExpr record:
                    return
                        new IR.Expression.RecordExpr(
                            [
                            .. record.Fields.Select(
                                field =>
                                new IR.RecordSetter(
                                    field.FieldName,
                                    field.FieldNameValue,
                                    Rewrite(field.Value, context)))
                            ]);

                case AST.Expression.RecordUpdateExpression update:
                    return RewriteRecordUpdate(update, context);

                case AST.Expression.UnitExpr:
                case AST.Expression.StringLiteral:
                case AST.Expression.CharLiteral:
                case AST.Expression.IntegerLiteral:
                case AST.Expression.FloatLiteral:
                case AST.Expression.Identifier:
                case AST.Expression.PrefixOperator:
                case AST.Expression.GLSLExpression:
                    return Convert.FromExpression(expression);

                default:
                    throw new NotImplementedException(
                        $"{nameof(Rewrite)} does not handle expression variant: {expression.GetType().Name}");
            }
        }

        private IR.Expression RewriteRecordUpdate(AST.Expression.RecordUpdateExpression update, Context context)
        {
            var receiver = IR.Expression.Identifier.Create([], update.RecordName);
            var type = Infer(AST.Expression.Identifier.Create([], update.RecordName), context);

            if (type is not Type.RecordType record)
            {
                return
                    new IR.Expression.RecordUpdateExpression(
                        update.RecordName,
                        [
                        .. update.Fields.Select(
                            field =>
                            new IR.RecordSetter(field.FieldName, field.FieldNameValue, Rewrite(field.Value, context)))
                        ]);
            }

            var fields = record.Fields.OrderBy(field => field.FieldName, StringComparer.Ordinal).ToArray();
            var updates = update.Fields.ToDictionary(field => field.FieldName, StringComparer.Ordinal);

            foreach (var name in updates.Keys)
            {
                if (!fields.Any(field => field.FieldName == name))
                    throw new InvalidOperationException($"Closed record type does not contain field '{name}'.");
            }

            var items = new List<IR.Expression> { new IR.Expression.ProjectItem(receiver, 0) };

            for (var index = 0; index < fields.Length; index++)
            {
                items.Add(new IR.Expression.ProjectItem(receiver, checked(2 * index + 1)));

                items.Add(
                    updates.TryGetValue(fields[index].FieldName, out var replacement)
                    ?
                    Rewrite(replacement.Value, context)
                    :
                    new IR.Expression.ProjectItem(receiver, checked(2 * index + 2)));
            }

            return new IR.Expression.TupledExpression(items);
        }

        private IR.Expression Read(
            IR.Expression receiver,
            string field,
            PineValue fieldValue,
            Type type,
            Context context,
            AST.Expression originalReceiver,
            bool patternRead = false)
        {
            type = Expand(type, context);

            if (type is Type.RecordType closed)
            {
                var fields = closed.Fields.OrderBy(item => item.FieldName, StringComparer.Ordinal).ToArray();

                for (var index = 0; index < fields.Length; index++)
                {
                    if (fields[index].FieldName == field)
                        return new IR.Expression.ProjectItem(receiver, checked(2 * index + 2));
                }

                throw new InvalidOperationException($"Closed record type does not contain field '{field}'.");
            }

            var access = new IR.Expression.RecordAccess(receiver, field, fieldValue);

            var provenance =
                type is Type.OpenRecordType
                ?
                RecordOperationProvenance.OpenRecordSourceType
                :
                type is Type.ChoiceType
                ?
                RecordOperationProvenance.UnresolvedNamedAlias
                :
                patternRead
                ?
                RecordOperationProvenance.UnknownRecordLayout
                :
                originalReceiver is not AST.Expression.Identifier identifier
                ?
                RecordOperationProvenance.NonIdentifierRecordExpression
                :
                context.Parameters.Contains(identifier.QualifiedName.DeclName)
                ?
                RecordOperationProvenance.MissingParameterType
                :
                context.Bindings.ContainsKey(identifier.QualifiedName.DeclName)
                ?
                RecordOperationProvenance.MissingLocalBindingType
                :
                RecordOperationProvenance.UnknownRecordLayout;

            Evidence.Add(access, new(type, provenance));
            return access;
        }

        private IR.Expression RewriteLet(AST.Expression.LetExpression let, Context outer)
        {
            var context = outer;

            foreach (var declaration in let.Declarations)
            {
                var names = new HashSet<string>();

                switch (declaration)
                {
                    case AST.LetDeclaration.LetFunction function:
                        names.Add(function.Function.Declaration.Name);
                        break;

                    case AST.LetDeclaration.LetDestructuring binding:
                        CollectPatternNames(binding.Pattern, names);
                        break;

                    default:
                        throw new NotImplementedException(
                            $"{nameof(RewriteLet)} does not handle let declaration variant: {declaration.GetType().Name}");
                }

                foreach (var name in names)
                    context =
                        context with { Bindings = context.Bindings.SetItem(name, new Type.UnknownType()), Parameters = context.Parameters.Remove(name) };
            }

            for (var iteration = 0; iteration < let.Declarations.Count; iteration++)
            {
                var before = context.Bindings;

                foreach (var declaration in let.Declarations)
                {
                    switch (declaration)
                    {
                        case AST.LetDeclaration.LetFunction function:
                            var type =
                                TypeInference.BuildFunctionTypeFromSignatureOrNull(function.Function)
                                ?? (function.Function.Declaration.Arguments.Count is 0
                                ?
                                Infer(function.Function.Declaration.Expression, context)
                                :
                                new Type.UnknownType());

                            context =
                                context with { Bindings = context.Bindings.SetItem(function.Function.Declaration.Name, Expand(type, context)) };

                            break;

                        case AST.LetDeclaration.LetDestructuring binding:
                            context =
                                context with
                                {
                                    Bindings =
                                    TypeInference.ExtractPatternBindingTypesFromInferredWithChoiceTagTypes(
                                        binding.Pattern,
                                        Infer(binding.Expression, context),
                                        context.Bindings,
                                        functions)
                                };

                            break;

                        default:
                            throw new NotImplementedException(
                                $"{nameof(RewriteLet)} does not handle let declaration variant: {declaration.GetType().Name}");
                    }
                }

                if (context.Bindings.Count == before.Count &&
                    context.Bindings.All(entry => Equals(entry.Value, before[entry.Key])))
                {
                    break;
                }
            }

            var declarations = new List<IR.LetDeclaration>();

            foreach (var declaration in let.Declarations)
            {
                switch (declaration)
                {
                    case AST.LetDeclaration.LetFunction function:
                        declarations.Add(new IR.LetDeclaration.LetFunction(RewriteFunction(function.Function, context)));
                        break;

                    case AST.LetDeclaration.LetDestructuring binding:
                        var fieldBindings = new List<IR.LetDeclaration>();

                        var pattern =
                            RewritePattern(
                                binding.Pattern,
                                Infer(binding.Expression, context),
                                fieldBindings,
                                ref context);

                        declarations.Add(
                            new IR.LetDeclaration.LetDestructuring(pattern, Rewrite(binding.Expression, context)));

                        declarations.AddRange(fieldBindings);
                        break;

                    default:
                        throw new NotImplementedException(
                            $"{nameof(RewriteLet)} does not handle let declaration variant: {declaration.GetType().Name}");
                }
            }

            return new IR.Expression.LetExpression(declarations, Rewrite(let.Expression, context));
        }

        private IR.Pattern RewritePattern(
            AST.Pattern pattern, Type matchedType, List<IR.LetDeclaration> bindings, ref Context context)
        {
            matchedType = Expand(matchedType, context);

            switch (pattern)
            {
                case AST.Pattern.RecordPattern record:
                    var temporary = FreshName();
                    context = context with { Bindings = context.Bindings.SetItem(temporary, matchedType) };
                    var receiver = IR.Expression.Identifier.Create([], temporary);

                    foreach (var field in record.Fields)
                    {
                        var read =
                            Read(
                                receiver,
                                field.FieldName,
                                field.FieldNameValue,
                                matchedType,
                                context,
                                AST.Expression.Identifier.Create([], temporary),
                                patternRead: true);

                        var fieldType =
                            matchedType is Type.RecordType closed
                            ?
                            closed.Fields.First(item => item.FieldName == field.FieldName).FieldType
                            :
                            matchedType is Type.OpenRecordType open
                            ?
                            open.KnownFields.FirstOrDefault(item => item.FieldName == field.FieldName).FieldType ??
                            new Type.UnknownType()
                            :
                            new Type.UnknownType();

                        context =
                            context with { Bindings = context.Bindings.SetItem(field.FieldName, Expand(fieldType, context)) };

                        bindings.Add(new IR.LetDeclaration.LetFunction(new(null, new(field.FieldName, [], read))));
                    }

                    return new IR.Pattern.VarPattern(temporary);

                case AST.Pattern.VarPattern variable:
                    context = context with { Bindings = context.Bindings.SetItem(variable.Name, matchedType) };
                    return new IR.Pattern.VarPattern(variable.Name);

                case AST.Pattern.TuplePattern tuple:
                    var elements = new List<IR.Pattern>();

                    for (var index = 0; index < tuple.Elements.Count; index++)
                    {
                        var type =
                            matchedType is Type.TupleType tupleType && index < tupleType.ElementTypes.Count
                            ?
                            tupleType.ElementTypes[index]
                            :
                            new Type.UnknownType();

                        elements.Add(RewritePattern(tuple.Elements[index], type, bindings, ref context));
                    }

                    return new IR.Pattern.TuplePattern(elements);

                case AST.Pattern.NamedPattern named:
                    var name =
                        DeclQualifiedName.Create(
                            named.Name.ModuleName.Count is 0 ? context.ModuleName.Split('.') : named.Name.ModuleName,
                            named.Name.Name);

                    IReadOnlyList<Type> argumentTypes =
                        constructors.TryGetValue(name, out var constructor)
                        ?
                        TypeInference.SpecializeTypesFromMatch(
                            constructor.ReturnType,
                            matchedType,
                            constructor.ParameterTypes)
                        :
                        [];

                    var arguments = new List<IR.Pattern>();

                    for (var index = 0; index < named.Arguments.Count; index++)
                    {
                        arguments.Add(
                            RewritePattern(
                                named.Arguments[index],
                                index < argumentTypes.Count ? argumentTypes[index] : new Type.UnknownType(),
                                bindings,
                                ref context));
                    }

                    return
                        new IR.Pattern.NamedPattern(
                            Convert.FromQualifiedNameRef(named.Name),
                            arguments,
                            named.TagNameAsPineValue);

                case AST.Pattern.ListPattern list:
                    var items = new List<IR.Pattern>();

                    var elementType =
                        matchedType is Type.ListType listType ? listType.ElementType : new Type.UnknownType();

                    foreach (var item in list.Elements)
                        items.Add(RewritePattern(item, elementType, bindings, ref context));

                    return new IR.Pattern.ListPattern(items);

                case AST.Pattern.UnConsPattern cons:
                    var headType =
                        matchedType is Type.ListType consType ? consType.ElementType : new Type.UnknownType();

                    var head = RewritePattern(cons.Head, headType, bindings, ref context);

                    return
                        new IR.Pattern.UnConsPattern(
                            head,
                            RewritePattern(cons.Tail, matchedType, bindings, ref context));

                case AST.Pattern.AsPattern alias:
                    context = context with { Bindings = context.Bindings.SetItem(alias.Name, matchedType) };

                    return
                        new IR.Pattern.AsPattern(
                            RewritePattern(alias.Pattern, matchedType, bindings, ref context),
                            alias.Name);

                case AST.Pattern.AllPattern:
                case AST.Pattern.UnitPattern:
                case AST.Pattern.CharPattern:
                case AST.Pattern.StringPattern:
                case AST.Pattern.IntPattern:
                case AST.Pattern.FloatPattern:
                    return Convert.FromPattern(pattern);

                default:
                    throw new NotImplementedException(
                        $"{nameof(RewritePattern)} does not handle pattern variant: {pattern.GetType().Name}");
            }
        }

        private static IR.Expression WithBindings(IReadOnlyList<IR.LetDeclaration> bindings, IR.Expression body) =>
            bindings.Count is 0 ? body : new IR.Expression.LetExpression(bindings, body);

        private string FreshName()
        {
            string name;

            do                name = GeneratedNameSuffixes.RecordProjectionBindingPrefix + m_nextName++;

            while (!m_usedNames.Add(name));

            return name;
        }

        private void CollectNames(AST.Expression expression)
        {
            switch (expression)
            {
                case AST.Expression.Identifier identifier:
                    m_usedNames.Add(identifier.QualifiedName.DeclName);
                    break;

                case AST.Expression.RecordUpdateExpression update:
                    m_usedNames.Add(update.RecordName);

                    foreach (var field in update.Fields)
                        CollectNames(field.Value);

                    break;

                case AST.Expression.LambdaExpression lambda:
                    foreach (var pattern in lambda.Arguments)
                        CollectPatternNames(pattern, m_usedNames);

                    CollectNames(lambda.Expression);
                    break;

                case AST.Expression.CaseExpression cases:
                    CollectNames(cases.Expression);

                    foreach (var branch in cases.Cases)
                    {
                        CollectPatternNames(branch.Pattern, m_usedNames);
                        CollectNames(branch.Expression);
                    }

                    break;

                case AST.Expression.LetExpression let:
                    foreach (var declaration in let.Declarations)
                    {
                        switch (declaration)
                        {
                            case AST.LetDeclaration.LetFunction function:
                                m_usedNames.Add(function.Function.Declaration.Name);

                                foreach (var pattern in function.Function.Declaration.Arguments)
                                    CollectPatternNames(pattern, m_usedNames);

                                CollectNames(function.Function.Declaration.Expression);
                                break;

                            case AST.LetDeclaration.LetDestructuring binding:
                                CollectPatternNames(binding.Pattern, m_usedNames);
                                CollectNames(binding.Expression);
                                break;

                            default:
                                throw new NotImplementedException(
                                    $"{nameof(CollectNames)} does not handle let declaration variant: {declaration.GetType().Name}");
                        }
                    }

                    CollectNames(let.Expression);
                    break;

                case AST.Expression.Negation negation:
                    CollectNames(negation.Expression);
                    break;

                case AST.Expression.ListExpr list:
                    foreach (var element in list.Elements)
                        CollectNames(element);

                    break;

                case AST.Expression.IfBlock conditional:
                    CollectNames(conditional.Condition);
                    CollectNames(conditional.ThenBlock);
                    CollectNames(conditional.ElseBlock);
                    break;

                case AST.Expression.Application application:
                    CollectNames(application.Function);

                    foreach (var argument in application.Arguments)
                        CollectNames(argument);

                    break;

                case AST.Expression.OperatorApplication operation:
                    CollectNames(operation.Left);
                    CollectNames(operation.Right);
                    break;

                case AST.Expression.TupledExpression tuple:
                    foreach (var element in tuple.Elements)
                        CollectNames(element);

                    break;

                case AST.Expression.RecordExpr record:
                    foreach (var field in record.Fields)
                        CollectNames(field.Value);

                    break;

                case AST.Expression.RecordAccess access:
                    CollectNames(access.Record);
                    break;

                case AST.Expression.UnitExpr:
                case AST.Expression.StringLiteral:
                case AST.Expression.CharLiteral:
                case AST.Expression.IntegerLiteral:
                case AST.Expression.FloatLiteral:
                case AST.Expression.PrefixOperator:
                case AST.Expression.RecordAccessFunction:
                case AST.Expression.GLSLExpression:
                    break;

                default:
                    throw new NotImplementedException(
                        $"{nameof(CollectNames)} does not handle expression variant: {expression.GetType().Name}");
            }
        }

        private static void CollectPatternNames(AST.Pattern pattern, HashSet<string> names) =>
            names.UnionWith(AST.SyntaxAnalysis.CollectNamesBoundByPattern(pattern));
    }
}
