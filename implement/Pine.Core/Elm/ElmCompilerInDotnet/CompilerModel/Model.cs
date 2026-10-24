using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Concrete;
using Pine.Core.Encodings;
using Pine.Core.Json;
using Pine.Core.PineLanguage;
using System.Collections.Immutable;
using System.Text.Json.Serialization;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

/// <summary>
/// Compiler-owned declarations. Source declarations provide type and constructor metadata;
/// generated functions are indexed only by structural identity.
/// </summary>
[JsonConverter(typeof(ProgramJsonConverter))]
public sealed record Program(
    ImmutableDictionary<FunctionId, Function> Functions,
    ImmutableDictionary<DeclQualifiedName, Syntax.Abstract.Declaration> SourceDeclarations)
{
    /// <summary>Source metadata cannot contain a second implementation of a compiled function.</summary>
    public ImmutableDictionary<DeclQualifiedName, Syntax.Abstract.Declaration> SourceDeclarations { get; } =
        ValidateMetadata(SourceDeclarations);

    private static ImmutableDictionary<DeclQualifiedName, Syntax.Abstract.Declaration> ValidateMetadata(
        ImmutableDictionary<DeclQualifiedName, Syntax.Abstract.Declaration> declarations)
    {
        System.ArgumentNullException.ThrowIfNull(declarations);

        foreach (var (name, declaration) in declarations)
        {
            if (declaration is null || declaration is Syntax.Abstract.Declaration.FunctionDeclaration ||
                ElmCompiler.GetDeclarationName(declaration) != name.DeclName)
            {
                throw new System.ArgumentException(
                    "Source metadata must contain only consistently named, non-executable declarations.",
                    nameof(declarations));
            }
        }

        return declarations;
    }

    /// <summary>Optional source spellings for diagnostics. Never consulted for binding resolution.</summary>
    public ImmutableDictionary<BindingId, string> BindingNames { get; init; } =
        [];
}

/// <summary>A function implementation, with no duplicate string name in its body.</summary>
public sealed record Function(ValueList<Pattern> Parameters, Expression Body)
{
    private long _cachedHashCode;

    private Function(Function original)
    {
        Parameters = original.Parameters;
        Body = original.Body;
    }

    /// <inheritdoc/>
    public bool Equals(Function? other) =>
        ReferenceEquals(this, other) ||
        other is not null && Equals(Parameters, other.Parameters) && Equals(Body, other.Body);

    /// <inheritdoc/>
    public override int GetHashCode() =>
        StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Function), Parameters, Body);
}

/// <summary>
/// Immutable optimization expressions. Literal and pattern encodings share the backend's value types;
/// lexical and function references never use generated strings.
/// </summary>
[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record Expression
{
    private protected long _cachedHashCode;

    private Expression() { }

    /// <summary>Copies an expression without its memoized hash.</summary>
    protected Expression(Expression original) { }

    /// <inheritdoc/>
    public virtual bool Equals(Expression? other) => other is not null && EqualityContract == other.EqualityContract;

    /// <inheritdoc/>
    public override int GetHashCode() => EqualityContract.GetHashCode();

    /// <summary>A normalized literal.</summary>
    public sealed record Literal(PineValue Value) : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode() => StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Literal), Value);
    }

    /// <summary>A resolved lexical, function, constructor, or native reference.</summary>
    public sealed record ReferenceExpression(Reference Reference) : Expression
    {
        /// <summary>The mandatory resolved reference.</summary>
        public Reference Reference { get; } = Reference ?? throw new System.ArgumentNullException(nameof(Reference));

        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(ReferenceExpression), Reference);
    }

    /// <summary>An ordered list of values.</summary>
    public sealed record List(ValueList<Expression> Items) : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode() => StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(List), Items);
    }

    /// <summary>A conditional.</summary>
    public sealed record Conditional(Expression Condition, Expression Then, Expression Else) : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Conditional), Condition, Then, Else);
    }

    /// <summary>Function application, including partial and over-application.</summary>
    public sealed record Application(Expression Function, ValueList<Expression> Arguments) : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Application), Function, Arguments);
    }

    /// <summary>A lambda with a site identity assigned before transformations.</summary>
    public sealed record Lambda(FunctionId Id, ValueList<Pattern> Parameters, Expression Body) : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Lambda), Id, Parameters, Body);
    }

    /// <summary>A recursive lexical declaration group.</summary>
    public sealed record Let(ValueList<Binding> Bindings, Expression Body) : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Let), Bindings, Body);
    }

    /// <summary>A pattern match.</summary>
    public sealed record Match(Expression Value, ValueList<Branch> Branches) : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Match), Value, Branches);
    }

    /// <summary>A record in the standard runtime representation.</summary>
    public sealed record Record(ValueList<Field> Fields) : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode() => StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Record), Fields);
    }

    /// <summary>A generic record read.</summary>
    public sealed record RecordAccess(Expression Value, string FieldName) : Expression
    {
        /// <summary>Derived runtime label encoding.</summary>
        [JsonIgnore]
        public PineValue FieldNameValue => StringEncoding.ValueFromString(FieldName);

        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(RecordAccess), Value, FieldName);
    }

    /// <summary>A first-class record accessor.</summary>
    public sealed record RecordAccessor(string FieldName) : Expression
    {
        /// <summary>Derived runtime label encoding.</summary>
        [JsonIgnore]
        public PineValue FieldNameValue => StringEncoding.ValueFromString(FieldName);

        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(RecordAccessor), FieldName);
    }

    /// <summary>A generic record update.</summary>
    public sealed record RecordUpdate(Expression Receiver, ValueList<Field> Fields) : Expression
    {
        /// <summary>The only receiver representation.</summary>
        public Expression Receiver { get; } = Receiver ?? throw new System.ArgumentNullException(nameof(Receiver));

        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(RecordUpdate), Receiver, Fields);
    }

    /// <summary>A record or tuple projection with a statically established offset.</summary>
    public sealed record Project : Expression
    {
        /// <summary>The projected value.</summary>
        public Expression Value { get; }

        /// <summary>A nonnegative physical index.</summary>
        public int Index { get; }

        /// <summary>Constructs a valid projection.</summary>
        public Project(Expression value, int index)
        {
            System.ArgumentNullException.ThrowIfNull(value);
            System.ArgumentOutOfRangeException.ThrowIfNegative(index);
            Value = value;
            Index = index;
        }

        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Project), Value, Index);
    }

    /// <summary>An operator that retains the existing backend's implementation choice.</summary>
    public sealed record Operator(string Name, InfixDirection Direction, Expression Left, Expression Right) : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Operator), Name, Direction, Left, Right);
    }

    /// <summary>A first-class operator.</summary>
    public sealed record PrefixOperator(string Name) : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(PrefixOperator), Name);
    }

    /// <summary>Numeric negation.</summary>
    public sealed record Negate(Expression Value) : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode() => StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Negate), Value);
    }

    /// <summary>Unsupported shader syntax remains explicit and produces the backend's standard error.</summary>
    public sealed record Shader(string Code) : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode() => StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Shader), Code);
    }
}

/// <summary>A lexical binding. Local functions are represented by lambda values, not nullable side metadata.</summary>
public sealed record Binding(Pattern Pattern, Expression Value)
{
    private long _cachedHashCode;

    private Binding(Binding original)
    {
        Pattern = original.Pattern;
        Value = original.Value;
    }

    /// <inheritdoc/>
    public bool Equals(Binding? other) =>
        ReferenceEquals(this, other) ||
        other is not null && Equals(Pattern, other.Pattern) && Equals(Value, other.Value);

    /// <inheritdoc/>
    public override int GetHashCode() =>
        StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Binding), Pattern, Value);
}

/// <summary>A pattern-match branch.</summary>
public sealed record Branch(Pattern Pattern, Expression Body)
{
    private long _cachedHashCode;

    private Branch(Branch original)
    {
        Pattern = original.Pattern;
        Body = original.Body;
    }

    /// <inheritdoc/>
    public bool Equals(Branch? other) =>
        ReferenceEquals(this, other) || other is not null && Equals(Pattern, other.Pattern) && Equals(Body, other.Body);

    /// <inheritdoc/>
    public override int GetHashCode() =>
        StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Branch), Pattern, Body);
}

/// <summary>A record field. Field labels are runtime data, not compiler identities.</summary>
public sealed record Field(string Name, Expression Value)
{
    private long _cachedHashCode;

    private Field(Field original)
    {
        Name = original.Name;
        Value = original.Value;
    }

    /// <inheritdoc/>
    public bool Equals(Field? other) =>
        ReferenceEquals(this, other) || other is not null && Name == other.Name && Equals(Value, other.Value);

    /// <inheritdoc/>
    public override int GetHashCode() => StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Field), Name, Value);

    /// <summary>Derived runtime label encoding.</summary>
    [JsonIgnore]
    public PineValue NameValue => StringEncoding.ValueFromString(Name);
}
