using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Abstract;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;

using Type = Pine.Core.Elm.ElmCompilerInDotnet.TypeInference.InferredType;
using Expression = Pine.Core.Elm.Syntax.Abstract.Expression;
using Pattern = Pine.Core.Elm.Syntax.Abstract.Pattern;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Noncached binding adapter. Slots follow reference traversal, never source-name ordering.
/// Dependency implementations are deliberately absent: lowering consults their types, not their bodies.
/// Type bindings and constructor bindings remain distinct even when their source spellings coincide.
/// </summary>
internal sealed class SourceLoweringProjection
{
    /// <summary>Caller-side lookup data shared within a pass, never part of a cached semantic input.</summary>
    internal sealed class Context
    {
        private readonly Dictionary<Declaration, Declaration> syntax = [];

        private readonly Dictionary<string, Declaration> syntaxByDigest = [];

        private readonly Dictionary<Declaration, Declaration> ownerSyntax = [];

        internal SourceCompilationCaches.EnvironmentFingerprintCache Fingerprints { get; } = new();

        public IReadOnlyDictionary<DeclQualifiedName, Declaration> Declarations { get; }

        public ImmutableDictionary<DeclQualifiedName, (DeclQualifiedName Owner, int Index)> Constructors { get; }

        public Context(
            IReadOnlyDictionary<DeclQualifiedName, Declaration> declarations,
            CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            Declarations = declarations;

            var constructors =
                ImmutableDictionary.CreateBuilder<DeclQualifiedName, (DeclQualifiedName Owner, int Index)>();

            foreach (var (key, declaration) in declarations)
            {
                cancellationToken.ThrowIfCancellationRequested();

                if (declaration is not Declaration.ChoiceTypeDeclaration choice)
                    continue;

                for (var index = 0; index < choice.TypeDeclaration.Constructors.Count; index++)
                {
                    cancellationToken.ThrowIfCancellationRequested();

                    constructors.Add(
                        DeclQualifiedName.Create(key.Namespaces, choice.TypeDeclaration.Constructors[index].Name),
                        (key, index));
                }
            }

            Constructors = constructors.ToImmutable();
        }

        internal Declaration InternSyntax(Declaration declaration)
        {
            if (syntax.TryGetValue(declaration, out var previous))
                return previous;

            var digest = SourceTransformationInput.SyntaxHash(declaration);

            if (syntaxByDigest.TryGetValue(digest, out previous))
                return previous;

            var normalized = SourceCompilationCaches.NormalizeSyntax(declaration);
            syntaxByDigest.Add(digest, normalized);
            syntax[normalized] = normalized;
            return normalized;
        }

        internal Declaration ShareOwnerSyntax(Declaration declaration)
        {
            if (ownerSyntax.TryGetValue(declaration, out var previous))
                return previous;
            // Input interning normalizes bodies only on a miss, after checking their content address.
            ownerSyntax.Add(declaration, declaration);
            return declaration;
        }
    }

    public DeclQualifiedName Name { get; }

    public Declaration Syntax { get; }

    public ImmutableDictionary<DeclQualifiedName, Declaration> Declarations { get; }

    public ImmutableDictionary<DeclQualifiedName, FunctionTypeInfo> FunctionTypes { get; }

    private readonly Dictionary<DeclQualifiedName, DeclQualifiedName> reverse = [];

    public SourceLoweringProjection(
        DeclQualifiedName owner,
        Context context,
        IReadOnlyDictionary<DeclQualifiedName, FunctionTypeInfo>? functionTypes = null)
    {
        var declarations = context.Declarations;
        var constructors = context.Constructors;
        var names = new Dictionary<DeclQualifiedName, DeclQualifiedName>();
        var values = new Dictionary<DeclQualifiedName, DeclQualifiedName>();
        var pending = new List<DeclQualifiedName>();

        DeclQualifiedName Allocate(DeclQualifiedName source)
        {
            if (names.TryGetValue(source, out var previous))
                return previous;

            if (IsIntrinsic(source))
            {
                names.Add(source, source);
                reverse[source] = source;

                if (declarations.ContainsKey(source))
                    pending.Add(source);

                return source;
            }

            var slot = pending.Count;
            // Type inference distinguishes callable references from constructor tags by casing.
            var isValue =
                declarations.TryGetValue(source, out var declaration)
                ?
                declaration is Declaration.FunctionDeclaration or Declaration.PortDeclaration or Declaration.InfixDeclaration
                :
                !ElmValueEncoding.StringIsValidTagName(source.DeclName);

            var anonymous = DeclQualifiedName.Create(["PineLowering", "R" + slot], isValue ? "symbol" : "Symbol");
            names.Add(source, anonymous);
            reverse.Add(anonymous, source);
            pending.Add(source);
            return anonymous;
        }

        DeclQualifiedName AllocateValue(DeclQualifiedName source)
        {
            if (values.TryGetValue(source, out var previous))
                return previous;

            if (!constructors.TryGetValue(source, out var constructor))
                return Allocate(source);

            var type = Allocate(constructor.Owner);

            var target =
                IsIntrinsic(source)
                ?
                source
                :
                DeclQualifiedName.Create(
                    type.Namespaces,
                    IsRuntimeCollectionConstructor(source.DeclName) ? source.DeclName : "C" + constructor.Index);

            values.Add(source, target);
            reverse[target] = source;
            return target;
        }

        DeclQualifiedName Resolve(DeclQualifiedName source, DeclQualifiedName context, bool isType)
        {
            if (source.Namespaces.Count is 0)
            {
                if (IsUnqualifiedIntrinsic(source.DeclName))
                    return source;

                source = DeclQualifiedName.Create(context.Namespaces, source.DeclName);
            }

            return isType ? Allocate(source) : AllocateValue(source);
        }

        var projected = ImmutableDictionary.CreateBuilder<DeclQualifiedName, Declaration>();
        var types = ImmutableDictionary.CreateBuilder<DeclQualifiedName, FunctionTypeInfo>();
        Name = Allocate(owner);
        Syntax = context.ShareOwnerSyntax(Project(declarations[owner], owner));
        projected[Name] = Syntax;

        for (var index = 0; index < pending.Count; index++)
        {
            var source = pending[index];
            var target = names[source];

            if (declarations.TryGetValue(source, out var declaration) && source != owner)
                projected[target] = context.InternSyntax(Project(Metadata(declaration), source));

            if (functionTypes is not null && !constructors.ContainsKey(source) &&
                functionTypes.TryGetValue(source, out var info))
            {
                types[target] = MapInfo(info, source);
            }

            if (declaration is Declaration.ChoiceTypeDeclaration choice)
            {
                foreach (var constructor in choice.TypeDeclaration.Constructors)
                {
                    var constructorName = DeclQualifiedName.Create(source.Namespaces, constructor.Name);
                    var constructorTarget = AllocateValue(constructorName);

                    if (functionTypes is not null && functionTypes.TryGetValue(constructorName, out var type))
                        types[constructorTarget] = MapInfo(type, source);
                }
            }
        }

        Declarations = projected.ToImmutable();
        FunctionTypes = types.ToImmutable();
        fingerprints = context.Fingerprints;

        FunctionTypeInfo MapInfo(FunctionTypeInfo info, DeclQualifiedName context) =>
            new(
                MapType(info.ReturnType, name => Resolve(name, context, isType: true)),
                [.. info.ParameterTypes.Select(type => MapType(type, name => Resolve(name, context, isType: true)))]);

        Declaration Project(Declaration declaration, DeclQualifiedName context)
        {
            var typeQuestions = new Dictionary<DeclQualifiedName, DeclQualifiedName>();

            var typed =
                new SourceLoweringTypeNames(
                    name =>
                    {
                        // '$' cannot occur in an Elm module name.
                        var question =
                            DeclQualifiedName.Create(["$PineLoweringType", "R" + typeQuestions.Count], "Symbol");

                        typeQuestions.Add(question, name);
                        return question;
                    }).Apply(declaration);

            var mapped =
                DeclarationUnits.Canonicalize.MapNames(
                    typed,
                    name =>
                    typeQuestions.TryGetValue(name, out var type)
                    ?
                    Resolve(type, context, isType: true)
                    :
                    Resolve(name, context, isType: false));

            return
                Rename(
                    mapped,
                    names[context],
                    constructor =>
                    AllocateValue(DeclQualifiedName.Create(context.Namespaces, constructor)).DeclName);
        }
    }

    private readonly SourceCompilationCaches.EnvironmentFingerprintCache fingerprints;

    private readonly Dictionary<(string Configuration, bool IncludeFunctionTypes), SourceTransformationInput> inputs =
        [];

    public SourceTransformationInput Input(string configuration, bool includeFunctionTypes)
    {
        var key = (configuration, includeFunctionTypes);

        if (inputs.TryGetValue(key, out var previous))
            return previous;

        var input =
            SourceCompilationCaches.Intern(
                new(
                    Name,
                    Syntax,
                    SourceCompilationCaches.EnvironmentHash(
                        Declarations,
                        includeFunctionTypes ? FunctionTypes : null,
                        fingerprints),
                    configuration));

        inputs.Add(key, input);
        return input;
    }

    public DeclQualifiedName Rebind(DeclQualifiedName name) =>
        reverse.TryGetValue(name, out var source) ? source : name;

    public Declaration Rebind(Declaration syntax) =>
        Rename(
            DeclarationUnits.Canonicalize.MapNames(syntax, Rebind),
            Rebind(Name),
            constructor =>
            Rebind(DeclQualifiedName.Create(Name.Namespaces, constructor)).DeclName);

    private static bool IsUnqualifiedIntrinsic(string name) =>
        name is "Int" or "Float" or "Bool" or "String" or "Char" or "List" or
            "+" or "-" or "*" or "/" or "//" or "^" or "==" or "/=" or "<" or ">" or
            "<=" or ">=" or "&&" or "||" or "++" or "::" or "|>" or "<|" or "<<" or ">>";

    private static bool IsIntrinsic(DeclQualifiedName name) =>
        name.Namespaces is ["Basics"] or ["Debug"] or ["Pine_builtin"] or ["Pine_kernel"] ||
        name.Namespaces is ["List"] && name.DeclName is "List" ||
        name.Namespaces is ["String"] && name.DeclName is "String" ||
        name.Namespaces is ["Char"] && name.DeclName is "Char";

    internal static bool IsRuntimeCollectionConstructor(string name) =>
        name is "RBNode_elm_builtin" or "RBEmpty_elm_builtin" or "Set_elm_builtin";

    private static Declaration Metadata(Declaration declaration) =>
        declaration switch
        {
            Declaration.FunctionDeclaration function =>
            new Declaration.FunctionDeclaration(
                new(
                    function.Function.Signature,
                    new FunctionImplementation(
                        function.Function.Declaration.Name,
                        [
                            .. function.Function.Declaration.Arguments.Select(
                                (_, index) => (Pattern)new Pattern.VarPattern("p" + index))
                        ],
                        new Expression.IntegerLiteral(0, Encodings.IntegerEncoding.EncodeSignedInteger(0))))),

            Declaration.AliasDeclaration => declaration,
            Declaration.ChoiceTypeDeclaration => declaration,
            Declaration.PortDeclaration => declaration,
            Declaration.InfixDeclaration => declaration,

            _ =>
            throw new NotImplementedException(
                nameof(Metadata) + " does not handle declaration variant: " + declaration.GetType().Name)
        };

    private static Declaration Rename(
        Declaration declaration, DeclQualifiedName name, Func<string, string> constructorName) =>
        declaration switch
        {
            Declaration.FunctionDeclaration function =>
            new Declaration.FunctionDeclaration(
                new(
                    function.Function.Signature is { } signature
                    ?
                    new Signature(name.DeclName, signature.TypeAnnotation)
                    :
                    null,
                    new FunctionImplementation(
                        name.DeclName,
                        function.Function.Declaration.Arguments,
                        function.Function.Declaration.Expression))),

            Declaration.AliasDeclaration alias =>
            new Declaration.AliasDeclaration(
                new(
                    name.DeclName,
                    alias.TypeAlias.Generics,
                    alias.TypeAlias.TypeAnnotation)),

            Declaration.ChoiceTypeDeclaration choice =>
            new Declaration.ChoiceTypeDeclaration(
                new(
                    name.DeclName,
                    choice.TypeDeclaration.Generics,
                    [
                        .. choice.TypeDeclaration.Constructors.Select(
                            constructor =>
                            ValueConstructor.Create(constructorName(constructor.Name), constructor.Arguments))
                    ])),

            Declaration.PortDeclaration port =>
            new Declaration.PortDeclaration(new Signature(name.DeclName, port.Signature.TypeAnnotation)),

            Declaration.InfixDeclaration => declaration,

            _ =>
            throw new NotImplementedException(
                nameof(Rename) + " does not handle declaration variant: " + declaration.GetType().Name)
        };

    internal static Type MapType(Type type, Func<DeclQualifiedName, DeclQualifiedName> map) =>
        type switch
        {
            Type.ChoiceType choice => MapChoice(choice, map),

            Type.FunctionType function =>
            new Type.FunctionType(
                MapType(function.ArgumentType, map),
                MapType(function.ReturnType, map)),

            Type.ListType list => new Type.ListType(MapType(list.ElementType, map)),
            Type.TupleType tuple => new Type.TupleType([.. tuple.ElementTypes.Select(type => MapType(type, map))]),

            Type.RecordType record =>
            new Type.RecordType(
                [
                    .. record.Fields.Select(
                        field => (field.FieldName, MapType(field.FieldType, map)))
                ]),

            Type.OpenRecordType record =>
            new Type.OpenRecordType(
                record.ExtensionVariable,
                [
                    .. record.KnownFields.Select(
                        field => (field.FieldName, MapType(field.FieldType, map)))
                ]),

            Type.IntType => type,
            Type.FloatType => type,
            Type.StringType => type,
            Type.CharType => type,
            Type.BoolType => type,
            Type.NumberType => type,
            Type.TypeVariable => type,
            Type.UnknownType => type,

            _ =>
            throw new NotImplementedException(
                nameof(MapType) + " does not handle inferred type variant: " + type.GetType().Name)
        };

    private static Type MapChoice(Type.ChoiceType choice, Func<DeclQualifiedName, DeclQualifiedName> map)
    {
        var name = map(DeclQualifiedName.Create(choice.ModuleName, choice.TypeName));

        return
            new Type.ChoiceType(
                name.Namespaces,
                name.DeclName,
                [.. choice.TypeArguments.Select(type => MapType(type, map))]);
    }
}
