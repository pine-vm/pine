using Pine.Core.Addressing;
using Pine.Core.Caching;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Encodings;
using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;

namespace Pine.Core.Elm.Syntax.Interpreter;

public partial class ElmSyntaxInterpreter
{
    /// <summary>
    /// Immutable, dependency-closed application input. Source bindings and runtime objects are
    /// replaced by rooted graph slots, including functions embedded in arguments and captures.
    /// </summary>
    public sealed record ApplicationCacheInput(PineValue Graph, bool ContainsDebug)
    {
        /// <summary>Domain-separated SHA-256 address, stable across processes and package provenance.</summary>
        public ReadOnlyMemory<byte> ComputeHash() =>
            PineValueHashFlat.ComputeHashForValue(
                PineValue.List([Graph, IntegerEncoding.EncodeSignedInteger(ContainsDebug ? 1 : 0)])).hashBytes;
    }

    /// <summary>Caller-owned storage and eligibility policy for successful function applications.</summary>
    public sealed record ApplicationCaching(
        ICache<ApplicationCacheInput, PineValue> Cache,
        Func<DeclQualifiedName, bool> ShouldCache)
    {
        /// <summary>
        /// Optional notifications for lookup, publication and semantic exclusions. Source names here
        /// are caller-side observations, not part of cached inputs or stored results.
        /// </summary>
        public Action<ApplicationCacheEvent>? OnEvent { get; init; }
    }

    /// <summary>Outcome of an application-cache operation, including explicit eligibility boundaries.</summary>
    public enum ApplicationCacheEventKind
    {
        /// <summary>An exact input was found.</summary>
        Hit,
        /// <summary>An exact input was not found.</summary>
        Miss,
        /// <summary>A completed, immutable data result was published.</summary>
        Stored,
        /// <summary>The dependency graph includes a Debug facility.</summary>
        DebugExcluded,
        /// <summary>Expression observers require execution rather than result reuse.</summary>
        ObserversExcluded,
        /// <summary>A function-bearing result cannot be stored as a source-free Pine data value.</summary>
        FunctionResultExcluded
    }

    /// <summary>Caller-side cache observation; the input itself remains provenance-free.</summary>
    public sealed record ApplicationCacheEvent(
        ApplicationCacheEventKind Kind,
        DeclQualifiedName? FunctionName,
        ApplicationCacheInput? Input);

    /// <summary>Lossless data-result codec for FileStoreCache and other caller-selected persistent tiers.</summary>
    public static ICacheCodec<PineValue> ApplicationResultCodec { get; } = new ApplicationDataCodec();

    private sealed class ApplicationDataCodec : ICacheCodec<PineValue>
    {
        public ReadOnlyMemory<byte> Encode(PineValue value)
        {
            using var stream = new MemoryStream();
            ValueEncodingBinaryDeterministic.Encode(stream, value);
            return stream.ToArray();
        }

        public PineValue Decode(ReadOnlyMemory<byte> bytes)
        {
            var value =
                ValueEncodingBinaryDeterministic.DecodeRoot(bytes)
                .Extract(error => throw new InvalidDataException("Invalid interpreter application result: " + error));

            if (!Encode(value).Span.SequenceEqual(bytes.Span))
                throw new InvalidDataException("Noncanonical or trailing data in interpreter application result.");

            return value;
        }
    }

    /// <summary>
    /// Source-bound adapter for constructing source-free runtime identities. The program is an
    /// immutable snapshot, never an application-cache input. Identity construction does not run Elm.
    /// </summary>
    public sealed class ApplicationCacheModel
    {
        private readonly Prepared _program;

        private readonly IReadOnlyDictionary<string, DeclQualifiedName> _operators;

        private readonly PreparedDeclarationResolverIndex _resolverIndex;

        private readonly bool _enableDefaultBuiltins;

        /// <summary>Uses the same builtin mode as the evaluation whose results will be stored.</summary>
        public ApplicationCacheModel(Prepared program, bool enableDefaultBuiltins = true)
        {
            this._program = new(program.Declarations.ToImmutableDictionary());
            this._enableDefaultBuiltins = enableDefaultBuiltins;
            _operators = BuildInfixOperatorMap(this._program.Declarations);
            _resolverIndex = GetDeclarationResolverIndex(this._program.Declarations);
        }

        /// <summary>Models a declared function together with its complete executable dependencies.</summary>
        public ApplicationCacheInput DescribeFunction(
            DeclQualifiedName name,
            CancellationToken cancellationToken = default) =>
            DescribeValue(Function(name), cancellationToken);

        /// <summary>Models data, functions, partial applications, and recursively captured function graphs.</summary>
        public ApplicationCacheInput DescribeValue(
            PineValueInProcess value,
            CancellationToken cancellationToken = default) =>
            new GraphBuilder(this, cancellationToken).Build(value, arguments: null);

        /// <summary>Models a declared application without interpreting its body.</summary>
        public ApplicationCacheInput DescribeApplication(
            DeclQualifiedName name,
            IReadOnlyList<PineValueInProcess> arguments,
            CancellationToken cancellationToken = default) =>
            DescribeApplication(Function(name), arguments, cancellationToken);

        /// <summary>Models an application of an evaluated, possibly partially applied function value.</summary>
        public ApplicationCacheInput DescribeApplication(
            PineValueInProcess function,
            IReadOnlyList<PineValueInProcess> arguments,
            CancellationToken cancellationToken = default)
        {
            if (function is ElmClosureInProcess closure)
            {
                if (closure.ArgumentsAlreadyCollected.Count > 0)
                    arguments = [.. closure.ArgumentsAlreadyCollected, .. arguments];

                if (closure.Source is ElmClosureInProcess.SourceRef.Declared declared &&
                    PolicyName(closure, declared.Name) is { } name && name != declared.Name)
                {
                    function =
                        new ElmClosureInProcess(
                            new ElmClosureInProcess.SourceRef.Declared(name, declared.Implementation),
                            closure.ParameterCount,
                            [],
                            closure.CapturedBindings,
                            closure.CapturedTopLevel);
                }
                else if (closure.ArgumentsAlreadyCollected.Count > 0)
                    function = closure.With([]);
            }

            return new GraphBuilder(this, cancellationToken).Build(function, arguments);
        }

        /// <summary>Derives a cryptographic identity for a declaration and its executable dependency tree.</summary>
        public ReadOnlyMemory<byte> ComputeFunctionHash(
            DeclQualifiedName name, CancellationToken cancellationToken = default) =>
            DescribeFunction(name, cancellationToken).ComputeHash();

        /// <summary>Derives a cryptographic identity for a possibly function-bearing runtime value.</summary>
        public ReadOnlyMemory<byte> ComputeValueHash(
            PineValueInProcess value, CancellationToken cancellationToken = default) =>
            DescribeValue(value, cancellationToken).ComputeHash();

        /// <summary>Derives a cryptographic identity for a higher-order application.</summary>
        public ReadOnlyMemory<byte> ComputeApplicationHash(
            PineValueInProcess function,
            IReadOnlyList<PineValueInProcess> arguments,
            CancellationToken cancellationToken = default) =>
            DescribeApplication(function, arguments, cancellationToken).ComputeHash();

        private PineValueInProcess Function(DeclQualifiedName name)
        {
            var resolution = Resolve(name, name);

            return resolution switch
            {
                ApplicationResolution.ContinueWithFunction function =>
                new ElmClosureInProcess(
                    new ElmClosureInProcess.SourceRef.Declared(function.ResolvedName ?? name, function.Function),
                    function.Function.Arguments.Count,
                    [],
                    LocalBindingEnvironment.Empty,
                    function.ResolvedName ?? name),

                ApplicationResolution.Resolved resolved => resolved.Value,

                null =>
                throw new ArgumentException("No interpreter function or constructor found: " + name, nameof(name)),

                _ =>
                throw new NotImplementedException(
                    nameof(Function) + " does not handle resolution: " + resolution.GetType().Name)
            };
        }

        private ApplicationResolution? Resolve(DeclQualifiedName name, DeclQualifiedName owner) =>
            UserDefinedResolver(
                new(name, [], new ApplicationContext(owner, LocalBindingEnvironment.Empty)),
                _resolverIndex);

        internal ApplicationResolution.ContinueWithFunction? ResolveDeclared(Application application) =>
            Resolve(application.FunctionName, application.Context.CurrentTopLevel)
            is ApplicationResolution.ContinueWithFunction function
            ?
            function
            :
            null;

        internal DeclQualifiedName PolicyName(ElmClosureInProcess closure, DeclQualifiedName fallback) =>
            closure.Source is ElmClosureInProcess.SourceRef.Declared declared &&
            _program.Declarations.TryGetValue(closure.CapturedTopLevel, out var declaration) &&
            declaration is PreparedDeclaration.FunctionDeclaration function &&
            ReferenceEquals(function.Function.Declaration, declared.Implementation)
            ?
            closure.CapturedTopLevel
            :
            fallback;

        private sealed record GraphNode(PineValue Label, ImmutableArray<int> Edges);

        private sealed class GraphBuilder(ApplicationCacheModel model, CancellationToken cancellationToken)
        {
            private readonly List<GraphNode?> _nodes = [];

            private readonly Queue<(int Slot, PineValueInProcess Value)> _pending = [];

            private readonly Dictionary<PineValueInProcess, int> _values = new(ReferenceEqualityComparer.Instance);

            private readonly Dictionary<DeclQualifiedName, int> _declarations = [];

            private readonly Dictionary<BindingIdentity, int> _bindings = [];

            private readonly Dictionary<DeclQualifiedName, int> _nominalBindings = [];

            private bool _containsDebug;

            private readonly record struct BindingIdentity(object Source, DeclQualifiedName TopLevel, DeclQualifiedName? Name)
            {
                public bool Equals(BindingIdentity other) =>
                    ReferenceEquals(Source, other.Source) && TopLevel == other.TopLevel && Name == other.Name;

                public override int GetHashCode() =>
                    HashCode.Combine(RuntimeHelpers.GetHashCode(Source), TopLevel, Name);
            }

            public ApplicationCacheInput Build(
                PineValueInProcess function,
                IReadOnlyList<PineValueInProcess>? arguments)
            {
                cancellationToken.ThrowIfCancellationRequested();
                var root = AddValue(function);

                if (arguments is not null)
                {
                    root = _nodes.Count;
                    _nodes.Add(null);

                    _nodes[root] =
                        new(
                            Node("application", Number(arguments.Count)),
                            [AddValue(function), .. arguments.Select(AddValue)]);
                }

                while (_pending.TryDequeue(out var next))
                {
                    cancellationToken.ThrowIfCancellationRequested();
                    _nodes[next.Slot] = EncodeValue(next.Value);
                }

                return new(CanonicalGraph(root), _containsDebug);
            }

            private int AddValue(PineValueInProcess value)
            {
                if (_values.TryGetValue(value, out var previous))
                    return previous;

                var slot = _nodes.Count;
                _values.Add(value, slot);
                _nodes.Add(null);
                _pending.Enqueue((slot, value));
                return slot;
            }

            private int Binding(BindingIdentity source)
            {
                if (_bindings.TryGetValue(source, out var previous))
                    return previous;

                var slot = _bindings.Count;
                _bindings.Add(source, slot);
                return slot;
            }

            private int Nominal(DeclQualifiedName name)
            {
                if (_nominalBindings.TryGetValue(name, out var previous))
                    return previous;

                var slot = _nominalBindings.Count;
                _nominalBindings.Add(name, slot);
                return slot;
            }

            private GraphNode EncodeValue(PineValueInProcess value)
            {
                if (value is ElmClosureInProcess closure)
                {
                    IReadOnlyList<Pattern> parameters;
                    PreparedExpression body;
                    object binding;
                    DeclQualifiedName? bindingName;

                    switch (closure.Source)
                    {
                        case ElmClosureInProcess.SourceRef.Declared declared:
                            parameters = declared.Implementation.Arguments;
                            body = declared.Implementation.Expression;
                            binding = declared.Implementation;
                            bindingName = declared.Name;
                            _containsDebug |= declared.Name.Namespaces is ["Debug"];
                            break;

                        case ElmClosureInProcess.SourceRef.Lambda lambda:
                            parameters = lambda.LambdaExpression.Arguments;
                            body = lambda.LambdaExpression.Expression;
                            binding = lambda.LambdaExpression;
                            bindingName = null;
                            break;

                        default:
                            throw new NotImplementedException(
                                nameof(EncodeValue) + " does not handle source: " + closure.Source.GetType().Name);
                    }

                    var locals = new HashSet<string>(StringComparer.Ordinal);

                    foreach (var parameter in parameters)
                        CollectPatternNames(parameter, locals);
                    // Interpreter equality observes the whole captured environment, even bindings
                    // not read by the body. Their values therefore remain semantic input.
                    var captures = closure.CapturedBindings.Keys.Order(StringComparer.Ordinal).ToArray();
                    locals.UnionWith(captures);
                    var edges = new List<int>();
                    var encodedBody = Expression(body, locals, closure.CapturedTopLevel, edges);
                    var dependencyCount = edges.Count;
                    edges.AddRange(captures.Select(name => AddValue(closure.CapturedBindings[name])));
                    edges.AddRange(closure.ArgumentsAlreadyCollected.Select(AddValue));

                    return
                        new(
                            Node(
                                "function",
                                Number(Binding(new(binding, closure.CapturedTopLevel, bindingName))),
                                Number(closure.ParameterCount),
                                List(parameters.Select(PatternValue)),
                                encodedBody,
                                Number(dependencyCount),
                                List(captures.Select(Text)),
                                Number(closure.ArgumentsAlreadyCollected.Count)),
                            [.. edges]);
                }

                if (value is ElmChoiceTagConstructorInProcess tag)
                {
                    return
                        new(
                            Node(
                                "constructor",
                                Number(Nominal(tag.TypeName)),
                                Text(tag.TagName),
                                Number(tag.TotalArity)),
                            [.. tag.Arguments.Select(AddValue)]);
                }

                if (value is ElmRecordTypeConstructorInProcess record)
                {
                    return
                        new(
                            Node(
                                "record-constructor",
                                Number(Nominal(record.TypeName)),
                                List(
                                    record.FieldNames.Select(
                                        field => Node("field", Text(field.FieldName), field.FieldNameValue)))),
                            [.. record.Arguments.Select(AddValue)]);
                }

                if (value is ElmRecordAccessChainInProcess access)
                {
                    return
                        new(
                            Node(
                                "accessor",
                                List(
                                    access.FieldNames.Select(
                                        field => Node("field", Text(field.FieldName), field.FieldNameValue)))),
                            []);
                }

                if (!ContainsOpaque(value))
                    return new(Node("data", value.Evaluate()), []);

                var items =
                    AsListItems(value) ??
                    throw new ArgumentException("Unsupported function-bearing interpreter value: " + value.GetType().Name);

                return new(Node("list"), [.. items.Select(AddValue)]);
            }

            private int Global(DeclQualifiedName requested, DeclQualifiedName owner)
            {
                var effective =
                    requested.Namespaces.Count is 0
                    ?
                    DeclQualifiedName.Create(owner.Namespaces, requested.DeclName)
                    :
                    requested;

                _containsDebug |= effective.Namespaces is ["Debug"];

                if (_declarations.TryGetValue(effective, out var previous))
                    return previous;

                var resolved = model.Resolve(requested, owner);

                if (resolved is not null)
                {
                    var value =
                        resolved switch
                        {
                            ApplicationResolution.ContinueWithFunction function =>
                            new ElmClosureInProcess(
                                new ElmClosureInProcess.SourceRef.Declared(
                                    function.ResolvedName ?? effective,
                                    function.Function),
                                function.Function.Arguments.Count,
                                [],
                                LocalBindingEnvironment.Empty,
                                function.ResolvedName ?? effective),

                            ApplicationResolution.Resolved complete => complete.Value,

                            _ =>
                            throw new NotImplementedException(
                                nameof(Global) + " does not handle resolution: " + resolved.GetType().Name)
                        };

                    var slot = AddValue(value);
                    _declarations.Add(effective, slot);
                    return slot;
                }

                if (effective.Namespaces is ["Pine_builtin"] or ["Pine_kernel"] or ["Debug"] ||
                    model._enableDefaultBuiltins && s_builtinFunctionResolvers.ContainsKey(effective))
                {
                    var slot = _nodes.Count;

                    _nodes.Add(
                        new(
                            Node(
                                "native",
                                Text(string.Join(".", effective.Namespaces)),
                                Text(effective.DeclName)),
                            []));

                    _declarations.Add(effective, slot);
                    return slot;
                }

                throw new ArgumentException(
                    "Unresolved dependency while modelling interpreter function: " + effective.FullName);
            }

            private PineValue Reference(DeclQualifiedName name, DeclQualifiedName owner, List<int> edges)
            {
                var slot = edges.Count;
                edges.Add(Global(name, owner));
                return Node("dependency", Number(slot));
            }

            private PineValue Expression(
                PreparedExpression expression, HashSet<string> locals, DeclQualifiedName owner, List<int> edges)
            {
                cancellationToken.ThrowIfCancellationRequested();
                PineValue Child(PreparedExpression child) => Expression(child, locals, owner, edges);

                PineValue Scope(PreparedExpression child, IEnumerable<Pattern> patterns)
                {
                    var nested = new HashSet<string>(locals, StringComparer.Ordinal);

                    foreach (var pattern in patterns)
                        CollectPatternNames(pattern, nested);

                    return Expression(child, nested, owner, edges);
                }

                switch (expression)
                {
                    case PreparedExpression.ValueLiteral literal:
                        var slot = edges.Count;
                        edges.Add(AddValue(literal.Value));
                        return Node("literal", Number(slot));

                    case PreparedExpression.Identifier identifier:
                        return
                            identifier.QualifiedName.Namespaces.Count is 0 &&
                            locals.Contains(identifier.QualifiedName.DeclName)
                            ?
                            Node("local", Text(identifier.QualifiedName.DeclName))
                            :
                            Reference(identifier.QualifiedName, owner, edges);

                    case PreparedExpression.Negation negation:
                        return Node("negate", Child(negation.Expression));

                    case PreparedExpression.ListExpr list:
                        return Node("list-expression", List(list.Elements.Select(Child)));

                    case PreparedExpression.TupledExpression tuple:
                        return Node("tuple", List(tuple.Elements.Select(Child)));

                    case PreparedExpression.IfBlock conditional:
                        return
                            Node(
                                "if",
                                Child(conditional.Condition),
                                Child(conditional.ThenBlock),
                                Child(conditional.ElseBlock));

                    case PreparedExpression.Application application:
                        return Node("apply", Child(application.Function), List(application.Arguments.Select(Child)));

                    case PreparedExpression.PrefixOperator prefix:
                        return Reference(Operator(prefix.Operator), owner, edges);

                    case PreparedExpression.OperatorApplication application:
                        return
                            Node(
                                "operator",
                                Reference(Operator(application.Operator), owner, edges),
                                Child(application.Left),
                                Child(application.Right));

                    case PreparedExpression.LambdaExpression lambda:
                        return
                            Node(
                                "lambda",
                                List(lambda.Arguments.Select(PatternValue)),
                                Scope(lambda.Expression, lambda.Arguments));

                    case PreparedExpression.CaseExpression match:
                        return
                            Node(
                                "case",
                                Child(match.Expression),
                                List(
                                    match.Dispatch.Steps.Select(
                                        segment =>
                                        {
                                            switch (segment)
                                            {
                                                case PreparedCaseDispatchSegment.ConstantValuePatterns constants:
                                                    return
                                                        Node(
                                                            "constants",
                                                            List(
                                                                constants.Cases.Select(
                                                                    branch =>
                                                                    Node("branch", branch.Value, Scope(branch.Expression, [branch.Pattern])))));

                                                case PreparedCaseDispatchSegment.PatternCase branch:
                                                    return
                                                        Node(
                                                            "branch",
                                                            PatternValue(branch.Pattern),
                                                            Scope(branch.Expression, [branch.Pattern]));

                                                case PreparedCaseDispatchSegment.DiscardCase branch:
                                                    return Node("discard", Child(branch.Expression));

                                                default:
                                                    throw new NotImplementedException(
                                                        nameof(Expression) + " does not handle case segment: " +
                                                        segment.GetType().Name);
                                            }
                                        })));

                    case PreparedExpression.LetExpression let:
                        var nested = new HashSet<string>(locals, StringComparer.Ordinal);

                        foreach (var declaration in let.Declarations)
                        {
                            switch (declaration)
                            {
                                case PreparedLetDeclaration.LetFunction function:
                                    nested.Add(function.Function.Declaration.Name);
                                    break;

                                case PreparedLetDeclaration.LetDestructuring destructuring:
                                    CollectPatternNames(destructuring.Pattern, nested);
                                    break;

                                default:
                                    throw new NotImplementedException(
                                        nameof(Expression) + " does not handle let declaration: " +
                                        declaration.GetType().Name);
                            }
                        }

                        return
                            Node(
                                "let",
                                List(
                                    let.Declarations.Select(
                                        declaration =>
                                        {
                                            switch (declaration)
                                            {
                                                case PreparedLetDeclaration.LetFunction function:
                                                    var functionLocals =
                                                        new HashSet<string>(nested, StringComparer.Ordinal);

                                                    foreach (var parameter in function.Function.Declaration.Arguments)
                                                        CollectPatternNames(parameter, functionLocals);

                                                    return
                                                        Node(
                                                            "binding",
                                                            Text(function.Function.Declaration.Name),
                                                            List(function.Function.Declaration.Arguments.Select(PatternValue)),
                                                            Expression(function.Function.Declaration.Expression, functionLocals, owner, edges));

                                                case PreparedLetDeclaration.LetDestructuring destructuring:
                                                    return
                                                        Node(
                                                            "destructure",
                                                            PatternValue(destructuring.Pattern),
                                                            Expression(destructuring.Expression, nested, owner, edges));

                                                default:
                                                    throw new NotImplementedException(
                                                        nameof(Expression) + " does not handle let declaration: " +
                                                        declaration.GetType().Name);
                                            }
                                        })),
                                Expression(let.Expression, nested, owner, edges));

                    case PreparedExpression.RecordExpr record:
                        return
                            Node(
                                "record",
                                List(
                                    record.Fields.Select(
                                        field =>
                                        Node("field", Text(field.FieldName), field.FieldNameValue, Child(field.Value)))));

                    case PreparedExpression.RecordAccess access:
                        return Node("access", Child(access.Record), Text(access.FieldName), access.FieldNameValue);

                    case PreparedExpression.RecordAccessFunction access:
                        return Node("accessor", Text(access.FieldName), access.FieldNameValue);

                    case PreparedExpression.RecordUpdateExpression update:
                        return
                            Node(
                                "update",
                                Child(PreparedExpression.Identifier.Create([], update.RecordName)),
                                List(
                                    update.Fields.Select(
                                        field =>
                                        Node("field", Text(field.FieldName), field.FieldNameValue, Child(field.Value)))));

                    case PreparedExpression.GLSLExpression glsl:
                        return Node("glsl", Text(glsl.Code));

                    default:
                        throw new NotImplementedException(
                            nameof(Expression) + " does not handle expression: " + expression.GetType().Name);
                }
            }

            private DeclQualifiedName Operator(string symbol) =>
                model._operators.TryGetValue(symbol, out var name)
                ?
                name
                :
                throw new ArgumentException("No infix declaration found while modelling operator: " + symbol);

            private static PineValue PatternValue(Pattern pattern) =>
                pattern switch
                {
                    Pattern.AllPattern => Node("any"),
                    Pattern.VarPattern variable => Node("variable", Text(variable.Name)),
                    Pattern.UnitPattern => Node("unit"),
                    Pattern.CharPattern character => Node("char", character.ValueAsPineValue),
                    Pattern.StringPattern text => Node("string", text.ValueAsPineValue),
                    Pattern.IntPattern integer => Node("integer", integer.ValueAsPineValue),

                    Pattern.FloatPattern floating =>
                    Node(
                        "float",
                        IntegerEncoding.EncodeSignedInteger(floating.Numerator),
                        IntegerEncoding.EncodeSignedInteger(floating.Denominator)),

                    Pattern.TuplePattern tuple => Node("tuple", List(tuple.Elements.Select(PatternValue))),

                    Pattern.RecordPattern record =>
                    Node(
                        "record",
                        List(
                            record.Fields.Select(
                                field =>
                                Node("field", Text(field.FieldName), field.FieldNameValue)))),

                    Pattern.UnConsPattern cons => Node("cons", PatternValue(cons.Head), PatternValue(cons.Tail)),
                    Pattern.ListPattern list => Node("list", List(list.Elements.Select(PatternValue))),

                    Pattern.NamedPattern named =>
                    Node("tag", named.TagNameAsPineValue, List(named.Arguments.Select(PatternValue))),

                    Pattern.AsPattern alias => Node("alias", Text(alias.Name), PatternValue(alias.Pattern)),

                    _ =>
                    throw new NotImplementedException(
                        nameof(PatternValue) + " does not handle pattern: " + pattern.GetType().Name)
                };

            private PineValue CanonicalGraph(int root)
            {
                // Ordered partition refinement removes irrelevant runtime object sharing, including
                // recursive capture cycles. Binding/nominal slots preserve observable alias relationships.
                var labels =
                    _nodes.Select(node => node!.Label).Distinct().OrderBy(
                        label => Convert.ToHexString(PineValueHashFlat.ComputeHashForValue(label).hashBytes.Span),
                        StringComparer.Ordinal).ThenBy(
                        label => Convert.ToHexString(ApplicationResultCodec.Encode(label).Span),
                        StringComparer.Ordinal).ToArray();

                var labelColors =
                    labels.Select((label, index) => (label, index)).ToDictionary(item => item.label, item => item.index);

                var colors = _nodes.Select(node => labelColors[node!.Label]).ToArray();
                var count = labels.Length;

                while (true)
                {
                    cancellationToken.ThrowIfCancellationRequested();

                    var shapes =
                        _nodes.Select(
                            (node, index) =>
                            (Index: index, Old: colors[index], Edges: node!.Edges.Select(edge => colors[edge]).ToArray()))
                        .OrderBy(shape => shape.Old).ThenBy(shape => shape.Edges, IntegerSequenceComparer.Instance).ToArray();

                    var next = new int[_nodes.Count];
                    var nextCount = 0;

                    for (var index = 0; index < shapes.Length; index++)
                    {
                        if (index is 0 || shapes[index].Old != shapes[index - 1].Old ||
                            !shapes[index].Edges.SequenceEqual(shapes[index - 1].Edges))
                        {
                            nextCount++;
                        }

                        next[shapes[index].Index] = nextCount - 1;
                    }

                    colors = next;

                    if (nextCount == count)
                        break;

                    count = nextCount;
                }

                var representatives = new GraphNode[count];

                for (var index = 0; index < _nodes.Count; index++)
                    representatives[colors[index]] = _nodes[index]!;

                return
                    Node(
                        "pine-elm-interpreter-application-v1",
                        Text(model._enableDefaultBuiltins ? "default-builtins" : "source-builtins"),
                        Number(colors[root]),
                        List(
                            representatives.Select(
                                node =>
                                PineValue.List([node.Label, List(node.Edges.Select(edge => Number(colors[edge])))]))));
            }
        }

        private sealed class IntegerSequenceComparer : IComparer<int[]>
        {
            public static IntegerSequenceComparer Instance { get; } = new();

            public int Compare(int[]? left, int[]? right)
            {
                if (left is null || right is null)
                    return left is null ? right is null ? 0 : -1 : 1;

                for (var index = 0; index < Math.Min(left.Length, right.Length); index++)
                {
                    var comparison = left[index].CompareTo(right[index]);

                    if (comparison is not 0)
                        return comparison;
                }

                return left.Length.CompareTo(right.Length);
            }
        }

        private static PineValue Text(string value) => StringEncoding.ValueFromString(value);

        private static PineValue Number(int value) => IntegerEncoding.EncodeSignedInteger(value);

        private static PineValue List(IEnumerable<PineValue> values) => PineValue.List([.. values]);

        private static PineValue Node(string tag, params PineValue[] values) => PineValue.List([Text(tag), .. values]);
    }
}
