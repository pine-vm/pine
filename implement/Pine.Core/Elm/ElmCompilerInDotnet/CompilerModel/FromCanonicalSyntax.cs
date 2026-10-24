using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using AST = Pine.Core.Elm.Syntax.Abstract;
using Type = Pine.Core.Elm.ElmCompilerInDotnet.TypeInference.InferredType;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

/// <summary>Resolved source lowering and evidence for residual generic record operations.</summary>
public sealed record SourceLoweringResult(Program Program,
    ImmutableDictionary<Expression, GenericRecordAccessEvidence> GenericAccessEvidence);

/// <summary>Resolves lexical identities while lowering source types and record operations. No name-bearing IR is produced.</summary>
public static class FromCanonicalSyntax
{
    private sealed record Context(string ModuleName, ImmutableDictionary<string, Type> Bindings,
        ImmutableHashSet<string> Parameters, FunctionId Owner, ImmutableDictionary<string, BindingId> Scope);

    /// <summary>Converts canonical source declarations into resolved compiler expressions.</summary>
    public static Program Convert(IReadOnlyDictionary<DeclQualifiedName, Declaration> declarations) =>
        Lower(declarations).Program;

    /// <summary>Converts canonical declarations and retains type evidence for analysis.</summary>
    public static SourceLoweringResult Lower(IReadOnlyDictionary<DeclQualifiedName, Declaration> declarations)
    {
        var aliasTypes =
            declarations.Where(entry => entry.Value is Declaration.AliasDeclaration)
            .ToImmutableDictionary(
                entry => entry.Key,
                entry =>
                {
                    var alias = ((Declaration.AliasDeclaration)entry.Value).TypeAlias;

                    return
                        new TypeInference.TypeAliasDefinition(
                            alias.Generics,
                            TypeInference.TypeAnnotationToInferredType(alias.TypeAnnotation));
                });

        var signatures = ImmutableDictionary.CreateBuilder<string, Type>();

        foreach (var (name, declaration) in declarations)
        {
            if (ElmCompiler.GetDeclarationName(declaration) != name.DeclName)
            {
                throw new ArgumentException(
                    "Canonical declaration keys must agree with their source declarations.",
                    nameof(declarations));
            }

            TypeInference.CollectFunctionSignaturesFromDeclaration(
                declaration,
                string.Join(".", name.Namespaces),
                signatures);
        }

        var environment = new TypeInference.FunctionInferenceEnvironment(signatures.ToImmutable());
        var functionTypes = environment.FunctionTypes.ToBuilder();
        var constructorTypes = ImmutableDictionary.CreateBuilder<DeclQualifiedName, FunctionTypeInfo>();
        var constructorNames = new HashSet<DeclQualifiedName>();

        foreach (var (name, declaration) in declarations)
        {
            if (declaration is Declaration.AliasDeclaration { TypeAlias.TypeAnnotation: TypeAnnotation.Record })
                constructorNames.Add(name);

            if (declaration is not Declaration.ChoiceTypeDeclaration choice)
                continue;

            foreach (var constructor in choice.TypeDeclaration.Constructors)
            {
                var id = DeclQualifiedName.Create(name.Namespaces, constructor.Name);
                constructorNames.Add(id);
                var type = environment.FunctionTypes[id];

                constructorTypes.Add(
                    id,
                    new FunctionTypeInfo(
                        type.ReturnType,
                        [
                        .. type.ParameterTypes.Select(
                            parameter => TypeInference.ExpandTypeAliases(parameter, aliasTypes, name.Namespaces))
                        ]));
            }
        }

        var sourceFunctions = declarations.Where(entry => entry.Value is Declaration.FunctionDeclaration).ToArray();

        foreach (var (name, declaration) in sourceFunctions)
        {
            var function = ((Declaration.FunctionDeclaration)declaration).Function;

            if (function.Signature is not null)
            {
                functionTypes[name] =
                    new(
                        TypeInference.GetFunctionReturnType(function),
                        TypeInference.GetFunctionParameterTypes(function));

                continue;
            }

            var inferred =
                TypeInference.InferFunctionDeclarationType(
                    function.Declaration.Expression,
                    function.Declaration.Arguments,
                    string.Join(".", name.Namespaces),
                    environment);

            functionTypes[name] =
                new(
                    inferred.returnType,
                    [
                        .. function.Declaration.Arguments.Select(
                            argument => argument is AST.Pattern.VarPattern variable &&
                                inferred.parameterTypes.TryGetValue(variable.Name, out var type)
                            ?
                            type
                            :
                            new Type.UnknownType())
                    ]);
        }

        var rewriter =
            new Rewriter(
                aliasTypes,
                functionTypes.ToImmutable(),
                constructorTypes.ToImmutable(),
                environment,
                sourceFunctions.Select(entry => entry.Key).ToHashSet(),
                constructorNames);

        var functions =
            sourceFunctions.OrderBy(entry => entry.Key).ToImmutableDictionary(
                entry => (FunctionId)new FunctionId.Source(entry.Key),
                entry =>
                rewriter.RewriteFunction(((Declaration.FunctionDeclaration)entry.Value).Function, entry.Key));

        return
            new(
                new Program(
                    functions,
                    declarations.Where(entry => entry.Value is not Declaration.FunctionDeclaration)
                    .ToImmutableDictionary())
                {
                    BindingNames = rewriter.BindingNames.ToImmutable()
                },
                rewriter.Evidence.ToImmutable());
    }

    private sealed class Rewriter(
        IReadOnlyDictionary<DeclQualifiedName, TypeInference.TypeAliasDefinition> aliases,
        IReadOnlyDictionary<DeclQualifiedName, FunctionTypeInfo> functions,
        IReadOnlyDictionary<DeclQualifiedName, FunctionTypeInfo> constructors,
        TypeInference.FunctionInferenceEnvironment environment,
        IReadOnlySet<DeclQualifiedName> functionNames, IReadOnlySet<DeclQualifiedName> constructorNames)
    {
        public ImmutableDictionary<Expression, GenericRecordAccessEvidence>.Builder Evidence { get; } =
            ImmutableDictionary.CreateBuilder<Expression, GenericRecordAccessEvidence>(
                ReferenceEqualityComparer.Instance);

        public ImmutableDictionary<BindingId, string>.Builder BindingNames { get; } =
            ImmutableDictionary.CreateBuilder<BindingId, string>();

        private readonly Dictionary<FunctionId, int> nextBinding = [];

        private readonly Dictionary<FunctionId, int> nextSite = [];

        private int m_nextTemporary;

        private BindingId Allocate(string name, FunctionId owner)
        {
            var slot = nextBinding.GetValueOrDefault(owner);
            nextBinding[owner] = slot + 1;
            var id = new BindingId.Local(owner, slot);
            BindingNames.Add(id, name);
            return id;
        }

        private FunctionId Site(FunctionId owner)
        {
            var site = nextSite.GetValueOrDefault(owner);
            nextSite[owner] = site + 1;
            return new FunctionId.Lifted(owner, site);
        }

        private Expression ReferenceTo(DeclQualifiedName name, Context context)
        {
            if (name.Namespaces.Count is 0 && context.Scope.TryGetValue(name.DeclName, out var binding))
                return new Expression.ReferenceExpression(new Reference.Local(binding));

            var qualified =
                name.Namespaces.Count is 0
                ?
                DeclQualifiedName.Create(context.ModuleName.Split('.'), name.DeclName)
                :
                name;

            Reference reference =
                functionNames.Contains(qualified)
                ?
                new Reference.Function(new FunctionId.Source(qualified))
                :
                constructorNames.Contains(qualified)
                ?
                new Reference.Constructor(qualified)
                :
                new Reference.Native(qualified);

            return new Expression.ReferenceExpression(reference);
        }

        public Function RewriteFunction(FunctionStruct function, DeclQualifiedName name)
        {
            m_nextTemporary = 0;

            return
                RewriteFunction(
                    function,
                    new Context(string.Join(".", name.Namespaces), [], [], new FunctionId.Source(name), []));
        }

        private Function RewriteFunction(FunctionStruct function, Context outer)
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
                [.. TypeInference.GetFunctionParameterTypes(function).Select(type => Expand(type, outer))];

            var context = outer;
            var arguments = new List<Pattern>();
            var bindings = new List<Binding>();

            for (var index = 0; index < function.Declaration.Arguments.Count; index++)
            {
                var argument = function.Declaration.Arguments[index];
                var names = PatternNames(argument);

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
                new Function(
                    new(arguments),
                    WithBindings(bindings, Rewrite(function.Declaration.Expression, context)));
        }

        private Type Expand(Type type, Context context) =>
            TypeInference.ExpandTypeAliases(type, aliases, context.ModuleName.Split('.'));

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

        private Expression Rewrite(AST.Expression expression, Context context)
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
                    var loweredAccessor = new Expression.RecordAccessor(accessor.FieldName);

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
                            new Expression.Application(
                                read,
                                [
                                .. application.Arguments.Skip(1).Select(argument => Rewrite(argument, context))
                                ]);
                    }

                    return
                        new Expression.Application(
                            Rewrite(application.Function, context),
                            [
                            .. application.Arguments.Select(argument => Rewrite(argument, context))
                            ]);

                case AST.Expression.LetExpression let:
                    return RewriteLet(let, context);

                case AST.Expression.CaseExpression cases:
                    var matchedType = Infer(cases.Expression, context);
                    var branches = new List<Branch>();

                    foreach (var branch in cases.Cases)
                    {
                        var branchContext = context;
                        var bindings = new List<Binding>();
                        var pattern = RewritePattern(branch.Pattern, matchedType, bindings, ref branchContext);
                        branches.Add(new(pattern, WithBindings(bindings, Rewrite(branch.Expression, branchContext))));
                    }

                    return new Expression.Match(Rewrite(cases.Expression, context), new(branches));

                case AST.Expression.LambdaExpression lambda:
                    var lambdaId = Site(context.Owner);

                    var function =
                        RewriteFunction(
                            new FunctionStruct(
                                null,
                                new FunctionImplementation("", lambda.Arguments, lambda.Expression)),
                            context with { Owner = lambdaId });

                    return
                        new Expression.Lambda(
                            lambdaId,
                            function.Parameters,
                            function.Body);

                case AST.Expression.Negation negation:
                    return new Expression.Negate(Rewrite(negation.Expression, context));

                case AST.Expression.ListExpr list:
                    return new Expression.List([.. list.Elements.Select(item => Rewrite(item, context))]);

                case AST.Expression.IfBlock conditional:
                    return
                        new Expression.Conditional(
                            Rewrite(conditional.Condition, context),
                            Rewrite(conditional.ThenBlock, context),
                            Rewrite(conditional.ElseBlock, context));

                case AST.Expression.OperatorApplication operation:
                    return
                        new Expression.Operator(
                            operation.Operator,
                            operation.Direction,
                            Rewrite(operation.Left, context),
                            Rewrite(operation.Right, context));

                case AST.Expression.TupledExpression tuple:
                    return new Expression.List([.. tuple.Elements.Select(item => Rewrite(item, context))]);

                case AST.Expression.RecordExpr record:
                    return
                        new Expression.Record(
                            [
                            .. record.Fields.Select(
                                field =>
                                new Field(
                                    field.FieldName,
                                    Rewrite(field.Value, context)))
                            ]);

                case AST.Expression.RecordUpdateExpression update:
                    return RewriteRecordUpdate(update, context);

                case AST.Expression.UnitExpr:
                    return new Expression.Literal(PineValue.EmptyList);

                case AST.Expression.StringLiteral text:
                    return new Expression.Literal(text.ValueAsPineValue);

                case AST.Expression.CharLiteral character:
                    return new Expression.Literal(character.ValueAsPineValue);

                case AST.Expression.IntegerLiteral integer:
                    return new Expression.Literal(integer.ValueAsPineValue);

                case AST.Expression.FloatLiteral floating:
                    return
                        new Expression.Literal(
                            ElmValueEncoding.ElmValueAsPineValue(
                                ElmValue.ElmFloat.Normalized(floating.Numerator, floating.Denominator)));

                case AST.Expression.Identifier identifier:
                    return ReferenceTo(identifier.QualifiedName, context);

                case AST.Expression.PrefixOperator prefix:
                    return new Expression.PrefixOperator(prefix.Operator);

                case AST.Expression.GLSLExpression shader:
                    return new Expression.Shader(shader.ShaderCode);

                default:
                    throw new NotImplementedException(
                        $"{nameof(Rewrite)} does not handle expression variant: {expression.GetType().Name}");
            }
        }

        private Expression RewriteRecordUpdate(AST.Expression.RecordUpdateExpression update, Context context)
        {
            var receiver = ReferenceTo(DeclQualifiedName.Create([], update.RecordName), context);
            var type = Infer(AST.Expression.Identifier.Create([], update.RecordName), context);

            if (type is not Type.RecordType record)
            {
                return
                    new Expression.RecordUpdate(
                        receiver,
                        [
                        .. update.Fields.Select(
                            field =>
                            new Field(field.FieldName, Rewrite(field.Value, context)))
                        ]);
            }

            var fields = record.Fields.OrderBy(field => field.FieldName, StringComparer.Ordinal).ToArray();
            var updates = update.Fields.ToDictionary(field => field.FieldName, StringComparer.Ordinal);

            foreach (var name in updates.Keys)
            {
                if (!fields.Any(field => field.FieldName == name))
                    throw new InvalidOperationException($"Closed record type does not contain field '{name}'.");
            }

            var items = new List<Expression> { new Expression.Project(receiver, 0) };

            for (var index = 0; index < fields.Length; index++)
            {
                items.Add(new Expression.Project(receiver, checked(2 * index + 1)));

                items.Add(
                    updates.TryGetValue(fields[index].FieldName, out var replacement)
                    ?
                    Rewrite(replacement.Value, context)
                    :
                    new Expression.Project(receiver, checked(2 * index + 2)));
            }

            return new Expression.List(new(items));
        }

        private Expression Read(
            Expression receiver,
            string field,
            PineValue fieldValue,
            Type type,
            Context context,
            AST.Expression? originalReceiver,
            bool patternRead = false)
        {
            type = Expand(type, context);

            if (type is Type.RecordType closed)
            {
                var fields = closed.Fields.OrderBy(item => item.FieldName, StringComparer.Ordinal).ToArray();

                for (var index = 0; index < fields.Length; index++)
                {
                    if (fields[index].FieldName == field)
                        return new Expression.Project(receiver, checked(2 * index + 2));
                }

                throw new InvalidOperationException($"Closed record type does not contain field '{field}'.");
            }

            var access = new Expression.RecordAccess(receiver, field);

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

        private Expression RewriteLet(AST.Expression.LetExpression let, Context outer)
        {
            var context = outer;

            foreach (var declaration in let.Declarations)
            {
                IEnumerable<string> names;

                switch (declaration)
                {
                    case LetDeclaration.LetFunction function:
                        names = [function.Function.Declaration.Name];
                        break;

                    case LetDeclaration.LetDestructuring binding:
                        names = PatternNames(binding.Pattern);
                        break;

                    default:
                        throw new NotImplementedException(
                            $"{nameof(RewriteLet)} does not handle let declaration variant: {declaration.GetType().Name}");
                }

                foreach (var name in names)
                {
                    context =
                        context with
                        {
                            Bindings = context.Bindings.SetItem(name, new Type.UnknownType()),
                            Parameters = context.Parameters.Remove(name),
                            Scope = context.Scope.SetItem(name, Allocate(name, context.Owner))
                        };
                }
            }

            for (var iteration = 0; iteration < let.Declarations.Count; iteration++)
            {
                var before = context.Bindings;

                foreach (var declaration in let.Declarations)
                {
                    switch (declaration)
                    {
                        case LetDeclaration.LetFunction function:
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

                        case LetDeclaration.LetDestructuring binding:
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

            var declarations = new List<Binding>();

            foreach (var declaration in let.Declarations)
            {
                switch (declaration)
                {
                    case LetDeclaration.LetFunction function:
                        Expression value;

                        if (function.Function.Declaration.Arguments.Count > 0)
                        {
                            var id = Site(context.Owner);
                            var local = RewriteFunction(function.Function, context with { Owner = id });
                            value = new Expression.Lambda(id, local.Parameters, local.Body);
                        }
                        else
                            value = Rewrite(function.Function.Declaration.Expression, context);

                        declarations.Add(
                            new(
                                new Pattern.VarPattern(context.Scope[function.Function.Declaration.Name]),
                                value));

                        break;

                    case LetDeclaration.LetDestructuring binding:
                        var fieldBindings = new List<Binding>();

                        var pattern =
                            RewritePattern(
                                binding.Pattern,
                                Infer(binding.Expression, context),
                                fieldBindings,
                                ref context,
                                predeclared: true);

                        declarations.Add(
                            new Binding(pattern, Rewrite(binding.Expression, context)));

                        declarations.AddRange(fieldBindings);
                        break;

                    default:
                        throw new NotImplementedException(
                            $"{nameof(RewriteLet)} does not handle let declaration variant: {declaration.GetType().Name}");
                }
            }

            return new Expression.Let(new(declarations), Rewrite(let.Expression, context));
        }

        private Pattern RewritePattern(
            AST.Pattern pattern, Type matchedType, List<Binding> bindings, ref Context context, bool predeclared = false)
        {
            matchedType = Expand(matchedType, context);

            switch (pattern)
            {
                case AST.Pattern.RecordPattern record:
                    var temporary = new BindingId.Temporary(context.Owner, m_nextTemporary++);
                    var receiver = new Expression.ReferenceExpression(new Reference.Local(temporary));

                    foreach (var field in record.Fields)
                    {
                        var read =
                            Read(
                                receiver,
                                field.FieldName,
                                field.FieldNameValue,
                                matchedType,
                                context,
                                originalReceiver: null,
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

                        var binding =
                            predeclared ? context.Scope[field.FieldName] : Allocate(field.FieldName, context.Owner);

                        context =
                            context with
                            {
                                Bindings = context.Bindings.SetItem(field.FieldName, Expand(fieldType, context)),
                                Scope = context.Scope.SetItem(field.FieldName, binding)
                            };

                        bindings.Add(new(new Pattern.VarPattern(binding), read));
                    }

                    return new Pattern.VarPattern(temporary);

                case AST.Pattern.VarPattern variable:
                    var variableId =
                        predeclared ? context.Scope[variable.Name] : Allocate(variable.Name, context.Owner);

                    context =
                        context with
                        {
                            Bindings = context.Bindings.SetItem(variable.Name, matchedType),
                            Scope = context.Scope.SetItem(variable.Name, variableId)
                        };

                    return new Pattern.VarPattern(variableId);

                case AST.Pattern.TuplePattern tuple:
                    var elements = new List<Pattern>();

                    for (var index = 0; index < tuple.Elements.Count; index++)
                    {
                        var type =
                            matchedType is Type.TupleType tupleType && index < tupleType.ElementTypes.Count
                            ?
                            tupleType.ElementTypes[index]
                            :
                            new Type.UnknownType();

                        elements.Add(RewritePattern(tuple.Elements[index], type, bindings, ref context, predeclared));
                    }

                    return new Pattern.TuplePattern(new(elements));

                case AST.Pattern.NamedPattern named:
                    var name =
                        DeclQualifiedName.Create(
                            named.Name.ModuleName.Count is 0 ? context.ModuleName.Split('.') : named.Name.ModuleName,
                            named.Name.Name);

                    var argumentTypes =
                        constructors.TryGetValue(name, out var constructor)
                        ?
                        TypeInference.SpecializeTypesFromMatch(
                            constructor.ReturnType,
                            matchedType,
                            constructor.ParameterTypes)
                        :
                        [];

                    var arguments = new List<Pattern>();

                    for (var index = 0; index < named.Arguments.Count; index++)
                    {
                        arguments.Add(
                            RewritePattern(
                                named.Arguments[index],
                                index < argumentTypes.Count ? argumentTypes[index] : new Type.UnknownType(),
                                bindings,
                                ref context,
                                predeclared));
                    }

                    return
                        new Pattern.NamedPattern(
                            name,
                            new(arguments));

                case AST.Pattern.ListPattern list:
                    var items = new List<Pattern>();

                    var elementType =
                        matchedType is Type.ListType listType ? listType.ElementType : new Type.UnknownType();

                    foreach (var item in list.Elements)
                        items.Add(RewritePattern(item, elementType, bindings, ref context, predeclared));

                    return new Pattern.ListPattern(new(items));

                case AST.Pattern.UnConsPattern cons:
                    var headType =
                        matchedType is Type.ListType consType ? consType.ElementType : new Type.UnknownType();

                    var head = RewritePattern(cons.Head, headType, bindings, ref context, predeclared);

                    return
                        new Pattern.UnConsPattern(
                            head,
                            RewritePattern(cons.Tail, matchedType, bindings, ref context, predeclared));

                case AST.Pattern.AsPattern alias:
                    var inner = RewritePattern(alias.Pattern, matchedType, bindings, ref context, predeclared);
                    var aliasId = predeclared ? context.Scope[alias.Name] : Allocate(alias.Name, context.Owner);

                    context =
                        context with
                        {
                            Bindings = context.Bindings.SetItem(alias.Name, matchedType),
                            Scope = context.Scope.SetItem(alias.Name, aliasId)
                        };

                    return new Pattern.AsPattern(inner, aliasId);

                case AST.Pattern.AllPattern:
                    return new Pattern.AllPattern();

                case AST.Pattern.UnitPattern:
                    return new Pattern.UnitPattern();

                case AST.Pattern.CharPattern character:
                    return new Pattern.CharPattern(character.Value);

                case AST.Pattern.StringPattern text:
                    return new Pattern.StringPattern(text.Value);

                case AST.Pattern.IntPattern integer:
                    return new Pattern.IntPattern(integer.Value);

                case AST.Pattern.FloatPattern floating:
                    return new Pattern.FloatPattern(floating.Value);

                default:
                    throw new NotImplementedException(
                        $"{nameof(RewritePattern)} does not handle pattern variant: {pattern.GetType().Name}");
            }
        }

        private static Expression WithBindings(IReadOnlyList<Binding> bindings, Expression body) =>
            bindings.Count is 0 ? body : new Expression.Let(new(bindings), body);

        private static IEnumerable<string> PatternNames(AST.Pattern pattern) =>
            pattern switch
            {
                AST.Pattern.VarPattern variable => [variable.Name],
                AST.Pattern.AsPattern alias => PatternNames(alias.Pattern).Append(alias.Name),
                AST.Pattern.RecordPattern record => record.Fields.Select(field => field.FieldName),
                AST.Pattern.TuplePattern tuple => tuple.Elements.SelectMany(PatternNames),
                AST.Pattern.ListPattern list => list.Elements.SelectMany(PatternNames),
                AST.Pattern.UnConsPattern cons => PatternNames(cons.Head).Concat(PatternNames(cons.Tail)),
                AST.Pattern.NamedPattern named => named.Arguments.SelectMany(PatternNames),
                AST.Pattern.AllPattern => [],
                AST.Pattern.UnitPattern => [],
                AST.Pattern.IntPattern => [],
                AST.Pattern.FloatPattern => [],
                AST.Pattern.StringPattern => [],
                AST.Pattern.CharPattern => [],

                _ =>
                throw new NotImplementedException(
                    $"{nameof(PatternNames)} does not handle pattern variant: {pattern.GetType().Name}")
            };
    }
}
