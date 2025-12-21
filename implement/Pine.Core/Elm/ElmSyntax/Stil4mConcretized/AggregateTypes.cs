using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using System.Collections.Generic;
using System.Linq;

using InfixDirection = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.InfixDirection;
using Location = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.Location;
using Range = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.Range;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

namespace Pine.Core.Elm.ElmSyntax.Stil4mConcretized;

/// <summary>
/// A list of syntax nodes separated by delimiters, such as commas in a parameter list.
/// This model stores the locations of delimiter tokens alongside the nodes they separate.
/// It does not store the actual delimiter tokens, therefore only fit for contexts where the delimiter type is known and fixed.
/// </summary>
public abstract record SeparatedSyntaxList<TNode>
{
    /// <summary>
    /// The case of an empty list.
    /// </summary>
    public sealed record Empty
        : SeparatedSyntaxList<TNode>;

    /// <summary>
    /// Represents a separated syntax list that is guaranteed to contain at least one node.
    /// </summary>
    public sealed record NonEmpty(
        TNode First,
        IReadOnlyList<(Location SeparatorLocation, TNode Node)> Rest)
        : SeparatedSyntaxList<TNode>
    {
        /// <inheritdoc/>
        public bool Equals(NonEmpty? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                EqualityComparer<TNode>.Default.Equals(First, other.First) &&
                Enumerable.SequenceEqual(Rest, other.Rest);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            hashCode.Add(First);

            foreach (var item in Rest)
                hashCode.Add(item);

            return hashCode.ToHashCode();
        }
    }

    /// <summary>
    /// Gets the count of elements in the list.
    /// </summary>
    public int Count => this switch
    {
        Empty =>
        0,

        NonEmpty nonEmpty =>
        1 + nonEmpty.Rest.Count,

        _ =>
        throw new System.NotImplementedException(
            "Unexpected type: " + GetType().FullName)
    };

    /// <summary>
    /// Gets all nodes in the list as an enumerable.
    /// </summary>
    public IEnumerable<TNode> Nodes =>
        this switch
        {
            Empty =>
            [],

            NonEmpty nonEmpty =>
            new[] { nonEmpty.First }.Concat(nonEmpty.Rest.Select(r => r.Node)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected type: " + GetType().FullName)
        };

    /// <summary>
    /// Gets the element at the specified index.
    /// </summary>
    public TNode this[int index] => this switch
    {
        NonEmpty nonEmpty when index == 0 =>
        nonEmpty.First,

        NonEmpty nonEmpty when index > 0 && index <= nonEmpty.Rest.Count =>
        nonEmpty.Rest[index - 1].Node,

        _ =>
        throw new System.ArgumentOutOfRangeException(nameof(index))
    };
}

/// <summary>
/// Represents an incomplete declaration that could not be fully parsed.
/// Contains the original text of the declaration for preservation during roundtrip,
/// along with error information to help diagnose the parsing failure.
/// </summary>
/// <param name="OriginalText">The original text of the incomplete declaration.</param>
/// <param name="ErrorLocation">The location where the parsing error occurred.</param>
/// <param name="ErrorMessage">The error message describing why parsing failed.</param>
public record IncompleteDeclaration(
    string OriginalText,
    Location ErrorLocation,
    string ErrorMessage);

/// <summary>
/// Root of an Elm source file: module definition, imports, top declarations and comments.
/// </summary>
public record File(
    Node<Module> ModuleDefinition,
    IReadOnlyList<Node<Import>> Imports,
    IReadOnlyList<Node<Declaration>> Declarations,
    IReadOnlyList<Node<string>> Comments,
    IReadOnlyList<Node<IncompleteDeclaration>> IncompleteDeclarations)
{
    /// <inheritdoc/>
    public virtual bool Equals(File? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            ModuleDefinition.Equals(other.ModuleDefinition) &&
            Enumerable.SequenceEqual(Imports, other.Imports) &&
            Enumerable.SequenceEqual(Declarations, other.Declarations) &&
            Enumerable.SequenceEqual(Comments, other.Comments) &&
            Enumerable.SequenceEqual(IncompleteDeclarations, other.IncompleteDeclarations);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new System.HashCode();

        hashCode.Add(ModuleDefinition);

        foreach (var item in Imports)
            hashCode.Add(item);

        foreach (var item in Declarations)
            hashCode.Add(item);

        foreach (var item in Comments)
            hashCode.Add(item);

        foreach (var item in IncompleteDeclarations)
            hashCode.Add(item);

        return hashCode.ToHashCode();
    }
}

/// <summary>
/// Elm import statement containing target module name, optional alias and exposing list.
/// </summary>
public record Import(
    Location ImportTokenLocation,
    Node<ModuleName> ModuleName,
    (Location AsTokenLocation, Node<ModuleName> Alias)? ModuleAlias,
    (Location ExposingTokenLocation, Node<Exposing> ExposingList)? ExposingList);

/// <summary>
/// Elm module kinds: normal, port or effect (with possible commands/subscriptions).
/// </summary>
public abstract record Module
{
    /// <summary>Standard module.</summary>
    public sealed record NormalModule(
        Location ModuleTokenLocation,
        DefaultModuleData ModuleData)
        : Module;

    /// <summary>Port module exposing native interop.</summary>
    public sealed record PortModule(
        Location PortTokenLocation,
        Location ModuleTokenLocation,
        DefaultModuleData ModuleData)
        : Module;

    /// <summary>Effect module with commands and subscriptions.</summary>
    public sealed record EffectModule(
        Location EffectTokenLocation,
        Location ModuleTokenLocation,
        EffectModuleData ModuleData)
        : Module;

    /// <summary>
    /// Retrieve the module name syntax node for the specified module instance.
    /// </summary>
    public static Node<ModuleName> GetModuleName(Module module) =>
        module switch
        {
            NormalModule normalModule =>
                normalModule.ModuleData.ModuleName,

            PortModule portModule =>
                portModule.ModuleData.ModuleName,

            EffectModule effectModule =>
                effectModule.ModuleData.ModuleName,

            _ =>
            throw new System.NotImplementedException(
                "Unexpected module type: " + module.GetType().Name),
        };
}

/// <summary>
/// Shared data for normal/port modules: name and exposing list.
/// </summary>
public record DefaultModuleData(
    Node<ModuleName> ModuleName,
    Location ExposingTokenLocation,
    Node<Exposing> ExposingList);

/// <summary>
/// Data for effect modules including optional command and subscription identifiers.
/// </summary>
public record EffectModuleData(
    Node<ModuleName> ModuleName,
    Location ExposingTokenLocation,
    Node<Exposing> ExposingList,
    Node<string>? Command,
    Node<string>? Subscription);

/// <summary>
/// How a module exposes its definitions: all or explicit list.
/// </summary>
public abstract record Exposing
{
    /// <summary>Expose everything (represented by .. in Elm).</summary>
    public sealed record All(
        Range Range)
        : Exposing;

    /// <summary>Explicit list of exposed top-level items.</summary>
    public sealed record Explicit(
        IReadOnlyList<Node<TopLevelExpose>> Nodes)
        : Exposing
    {
        /// <inheritdoc/>
        public bool Equals(Explicit? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return Enumerable.SequenceEqual(Nodes, other.Nodes);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            foreach (var item in Nodes)
                hashCode.Add(item);

            return hashCode.ToHashCode();
        }
    }
}

/// <summary>
/// Individual item exposed from a module: operators, functions, types, or type with constructors.
/// </summary>
public abstract record TopLevelExpose
{
    /// <summary>Exposed operator (infix).</summary>
    public sealed record InfixExpose(
        string Name)
        : TopLevelExpose;

    /// <summary>Exposed function or value.</summary>
    public sealed record FunctionExpose(
        string Name)
        : TopLevelExpose;

    /// <summary>Exposed type or type alias without constructors.</summary>
    public sealed record TypeOrAliasExpose(
        string Name)
        : TopLevelExpose;

    /// <summary>Exposed custom type including maybe constructors via exposed range.</summary>
    public sealed record TypeExpose(
        ExposedType ExposedType)
        : TopLevelExpose;
}

/// <summary>
/// Custom type exposure specification: name and optional opening range for constructors.
/// </summary>
public record ExposedType(
    string Name,
    Range? Open);

/// <summary>
/// Top-level declarations in a module (functions, types, aliases, ports, infix directives).
/// </summary>
public abstract record Declaration
{
    /// <summary>Function declaration including implementation.</summary>
    public sealed record FunctionDeclaration(
        FunctionStruct Function)
        : Declaration;

    /// <summary>Custom type declaration.</summary>
    public sealed record CustomTypeDeclaration(
        TypeStruct TypeDeclaration)
        : Declaration;

    /// <summary>Type alias declaration.</summary>
    public sealed record AliasDeclaration(
        TypeAlias TypeAlias)
        : Declaration;

    /// <summary>Port declaration with signature.</summary>
    public sealed record PortDeclaration(
        Location PortTokenLocation,
        Signature Signature)
        : Declaration;

    /// <summary>Infix operator metadata declaration.</summary>
    public sealed record InfixDeclaration(
        Infix Infix)
        : Declaration;
}

/// <summary>
/// Infix operator properties: direction, precedence, operator symbol and implementing function.
/// </summary>
public record Infix(
    Location InfixTokenLocation,
    Node<InfixDirection> Direction,
    Node<int> Precedence,
    Node<string> Operator,
    Location EqualsTokenLocation,
    Node<string> FunctionName);

/// <summary>
/// Type alias with optional documentation, generics and underlying type annotation.
/// </summary>
public record TypeAlias(
    Node<string>? Documentation,
    Location TypeTokenLocation,
    Location AliasTokenLocation,
    Node<string> Name,
    IReadOnlyList<Node<string>> Generics,
    Location EqualsTokenLocation,
    Node<TypeAnnotation> TypeAnnotation)
{
    /// <inheritdoc/>
    public virtual bool Equals(TypeAlias? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            EqualityComparer<Node<string>?>.Default.Equals(Documentation, other.Documentation) &&
            TypeTokenLocation.Equals(other.TypeTokenLocation) &&
            AliasTokenLocation.Equals(other.AliasTokenLocation) &&
            Name.Equals(other.Name) &&
            Enumerable.SequenceEqual(Generics, other.Generics) &&
            EqualsTokenLocation.Equals(other.EqualsTokenLocation) &&
            TypeAnnotation.Equals(other.TypeAnnotation);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new System.HashCode();

        hashCode.Add(Documentation);
        hashCode.Add(TypeTokenLocation);
        hashCode.Add(AliasTokenLocation);
        hashCode.Add(Name);

        foreach (var item in Generics)
            hashCode.Add(item);

        hashCode.Add(EqualsTokenLocation);
        hashCode.Add(TypeAnnotation);

        return hashCode.ToHashCode();
    }
}

/// <summary>
/// Custom type declaration including constructors.
/// </summary>
public record TypeStruct(
    Node<string>? Documentation,
    Location TypeTokenLocation,
    Node<string> Name,
    IReadOnlyList<Node<string>> Generics,
    Location EqualsTokenLocation,
    IReadOnlyList<(Location? PipeTokenLocation, Node<ValueConstructor> Constructor)> Constructors)
{
    /// <inheritdoc/>
    public virtual bool Equals(TypeStruct? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            EqualityComparer<Node<string>?>.Default.Equals(Documentation, other.Documentation) &&
            TypeTokenLocation.Equals(other.TypeTokenLocation) &&
            Name.Equals(other.Name) &&
            Enumerable.SequenceEqual(Generics, other.Generics) &&
            EqualsTokenLocation.Equals(other.EqualsTokenLocation) &&
            Enumerable.SequenceEqual(Constructors, other.Constructors);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new System.HashCode();

        hashCode.Add(Documentation);
        hashCode.Add(TypeTokenLocation);
        hashCode.Add(Name);

        foreach (var item in Generics)
            hashCode.Add(item);

        hashCode.Add(EqualsTokenLocation);

        foreach (var item in Constructors)
            hashCode.Add(item);

        return hashCode.ToHashCode();
    }
}

/// <summary>
/// Constructor for a custom type with argument type annotations.
/// </summary>
public record ValueConstructor(
    Node<string> Name,
    IReadOnlyList<Node<TypeAnnotation>> Arguments)
{
    /// <inheritdoc/>
    public virtual bool Equals(ValueConstructor? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            Name.Equals(other.Name) &&
            Enumerable.SequenceEqual(Arguments, other.Arguments);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new System.HashCode();

        hashCode.Add(Name);

        foreach (var item in Arguments)
            hashCode.Add(item);

        return hashCode.ToHashCode();
    }
}

/// <summary>
/// Elm type annotations: generics, concrete typed names, tuples, records, functions, etc.
/// </summary>
public abstract record TypeAnnotation
{
    /// <summary>Reference to a generic type variable.</summary>
    public sealed record GenericType(
        string Name)
        : TypeAnnotation;

    /// <summary>Concrete type optionally namespaced and with arguments.</summary>
    public sealed record Typed(
        Node<(ModuleName ModuleName, string Name)> TypeName,
        IReadOnlyList<Node<TypeAnnotation>> TypeArguments)
        : TypeAnnotation
    {
        /// <inheritdoc/>
        public bool Equals(Typed? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                TypeName.Equals(other.TypeName) &&
                Enumerable.SequenceEqual(TypeArguments, other.TypeArguments);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            hashCode.Add(TypeName);

            foreach (var item in TypeArguments)
                hashCode.Add(item);

            return hashCode.ToHashCode();
        }
    }

    /// <summary>Unit type annotation.</summary>
    public sealed record Unit
        : TypeAnnotation;

    /// <summary>Tuple type annotation.</summary>
    public sealed record Tupled(
        Location OpenParenLocation,
        SeparatedSyntaxList<Node<TypeAnnotation>> TypeAnnotations,
        Location CloseParenLocation)
        : TypeAnnotation;

    /// <summary>Record type annotation.</summary>
    public sealed record Record(
        Location OpenBraceLocation,
        RecordDefinition RecordDefinition,
        Location CloseBraceLocation)
        : TypeAnnotation;

    /// <summary>Record type annotation that extends a generic record.</summary>
    public sealed record GenericRecord(
        Location OpenBraceLocation,
        Node<string> GenericName,
        Location PipeLocation,
        Node<RecordDefinition> RecordDefinition,
        Location CloseBraceLocation)
        : TypeAnnotation;

    /// <summary>Function type annotation mapping argument to return.</summary>
    public sealed record FunctionTypeAnnotation(
        Node<TypeAnnotation> ArgumentType,
        Location ArrowLocation,
        Node<TypeAnnotation> ReturnType)
        : TypeAnnotation;
}

/// <summary>
/// Record type definition listing fields.
/// </summary>
public record RecordDefinition(
    SeparatedSyntaxList<Node<RecordField>> Fields);

/// <summary>
/// Single record field definition pairing name with type annotation.
/// </summary>
public record RecordField(
    Node<string> FieldName,
    Location ColonLocation,
    Node<TypeAnnotation> FieldType);

/// <summary>
/// Function declaration parts: optional docs and signature plus implementation.
/// </summary>
public record FunctionStruct(
    Node<string>? Documentation,
    Node<Signature>? Signature,
    Node<FunctionImplementation> Declaration);

/// <summary>
/// Function implementation name, argument patterns and body expression.
/// </summary>
public record FunctionImplementation(
    Node<string> Name,
    IReadOnlyList<Node<Pattern>> Arguments,
    Location EqualsTokenLocation,
    Node<Expression> Expression)
{
    /// <inheritdoc/>
    public virtual bool Equals(FunctionImplementation? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            Name.Equals(other.Name) &&
            Enumerable.SequenceEqual(Arguments, other.Arguments) &&
            EqualsTokenLocation.Equals(other.EqualsTokenLocation) &&
            Expression.Equals(other.Expression);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new System.HashCode();

        hashCode.Add(Name);

        foreach (var item in Arguments)
            hashCode.Add(item);

        hashCode.Add(EqualsTokenLocation);
        hashCode.Add(Expression);

        return hashCode.ToHashCode();
    }
}

/// <summary>
/// Type signature for a value/function: name and type annotation.
/// </summary>
public record Signature(
    Node<string> Name,
    Location ColonLocation,
    Node<TypeAnnotation> TypeAnnotation);

/// <summary>
/// Pattern matching forms used in destructuring and case expressions.
/// </summary>
public abstract record Pattern
{
    /// <summary>Pattern matching any value.</summary>
    public sealed record AllPattern
        : Pattern;

    /// <summary>Pattern binding a value to a variable name.</summary>
    public sealed record VarPattern(
        string Name)
        : Pattern;

    /// <summary>Pattern matching the unit value.</summary>
    public sealed record UnitPattern
        : Pattern;

    /// <summary>Pattern matching a specific character literal.</summary>
    public sealed record CharPattern(
        int Value)
        : Pattern;

    /// <summary>Pattern matching a specific string literal.</summary>
    public sealed record StringPattern(
        string Value)
        : Pattern;

    /// <summary>Pattern matching a decimal integer literal.</summary>
    public sealed record IntPattern(
        long Value)
        : Pattern;

    /// <summary>Pattern matching a hexadecimal integer literal.</summary>
    public sealed record HexPattern(
        long Value)
        : Pattern;

    /// <summary>Pattern matching a floating-point literal.</summary>
    public sealed record FloatPattern(
        float Value)
        : Pattern;

    /// <summary>Pattern matching a tuple.</summary>
    public sealed record TuplePattern(
        Location OpenParenLocation,
        IReadOnlyList<Node<Pattern>> Elements,
        Location CloseParenLocation)
        : Pattern
    {
        /// <inheritdoc/>
        public bool Equals(TuplePattern? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                OpenParenLocation.Equals(other.OpenParenLocation) &&
                Enumerable.SequenceEqual(Elements, other.Elements) &&
                CloseParenLocation.Equals(other.CloseParenLocation);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            hashCode.Add(OpenParenLocation);

            foreach (var item in Elements)
                hashCode.Add(item);

            hashCode.Add(CloseParenLocation);

            return hashCode.ToHashCode();
        }
    }

    /// <summary>Pattern matching a record with specified fields.</summary>
    public sealed record RecordPattern(
        Location OpenBraceLocation,
        IReadOnlyList<Node<string>> Fields,
        Location CloseBraceLocation)
        : Pattern
    {
        /// <inheritdoc/>
        public bool Equals(RecordPattern? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                OpenBraceLocation.Equals(other.OpenBraceLocation) &&
                Enumerable.SequenceEqual(Fields, other.Fields) &&
                CloseBraceLocation.Equals(other.CloseBraceLocation);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            hashCode.Add(OpenBraceLocation);

            foreach (var item in Fields)
                hashCode.Add(item);

            hashCode.Add(CloseBraceLocation);

            return hashCode.ToHashCode();
        }
    }

    /// <summary>List cons pattern separating head and tail.</summary>
    public sealed record UnConsPattern(
        Node<Pattern> Head,
        Location ConsOperatorLocation,
        Node<Pattern> Tail)
        : Pattern;

    /// <summary>Pattern matching a list of elements.</summary>
    public sealed record ListPattern(
        Location OpenBracketLocation,
        IReadOnlyList<Node<Pattern>> Elements,
        Location CloseBracketLocation)
        : Pattern
    {
        /// <inheritdoc/>
        public bool Equals(ListPattern? other)
        {
            if (other is null)
                return false;

            if (ReferenceEquals(this, other))
                return true;

            return
                OpenBracketLocation.Equals(other.OpenBracketLocation) &&
                Enumerable.SequenceEqual(Elements, other.Elements) &&
                CloseBracketLocation.Equals(other.CloseBracketLocation);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            hashCode.Add(OpenBracketLocation);

            foreach (var item in Elements)
                hashCode.Add(item);

            hashCode.Add(CloseBracketLocation);

            return hashCode.ToHashCode();
        }
    }

    /// <summary>Pattern matching a named constructor with arguments.</summary>
    public sealed record NamedPattern(
        QualifiedNameRef Name,
        IReadOnlyList<Node<Pattern>> Arguments)
        : Pattern
    {
        /// <inheritdoc/>
        public bool Equals(NamedPattern? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                EqualityComparer<QualifiedNameRef>.Default.Equals(Name, other.Name) &&
                Enumerable.SequenceEqual(Arguments, other.Arguments);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            hashCode.Add(Name);

            foreach (var item in Arguments)
                hashCode.Add(item);

            return hashCode.ToHashCode();
        }
    }

    /// <summary>Pattern that aliases a match to a name.</summary>
    public sealed record AsPattern(
        Node<Pattern> Pattern,
        Location AsTokenLocation,
        Node<string> Name)
        : Pattern;

    /// <summary>Pattern wrapped in parentheses.</summary>
    public sealed record ParenthesizedPattern(
        Location OpenParenLocation,
        Node<Pattern> Pattern,
        Location CloseParenLocation)
        : Pattern;
}

/// <summary>
/// Qualified reference to a name with its module path.
/// </summary>
public record QualifiedNameRef(
    ModuleName ModuleName,
    string Name)
    : System.IEquatable<QualifiedNameRef>
{
    /// <inheritdoc/>
    virtual public bool Equals(QualifiedNameRef? obj)
    {
        if (obj is not QualifiedNameRef other)
            return false;

        if (Name != other.Name)
            return false;

        if (!Enumerable.SequenceEqual(ModuleName, other.ModuleName))
            return false;

        return true;
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hash = new System.HashCode();

        foreach (var part in ModuleName)
        {
            hash.Add(part);
        }

        hash.Add(Name);

        return hash.ToHashCode();
    }

    /// <summary>
    /// Creates a new instance of <see cref="QualifiedNameRef"/> from a fully qualified name string, splitting it into
    /// module and name components.
    /// </summary>
    public static QualifiedNameRef FromFullName(string fullName)
    {
        var parts = fullName.Split('.');

        if (parts.Length is 0)
            throw new System.ArgumentException("Full name cannot be empty", nameof(fullName));


        var moduleName =
            parts.Length > 1
            ?
            parts[..^1]
            :
            [];

        var name = parts[^1];

        return new QualifiedNameRef(moduleName, name);
    }
}

/// <summary>
/// Elm expressions: literals, applications, control flow, data structures, lambda, let, case, etc.
/// A more concrete form of <see cref="Stil4mElmSyntax7.Expression"/>.
/// </summary>
public abstract record Expression
{
    /// <summary>Expression producing unit.</summary>
    public sealed record UnitExpr
        : Expression;

    /// <summary>String literal expression.</summary>
    public sealed record Literal(
        string Value,
        /*
         * Note: Property 'IsTripleQuoted' does not exist in V7 of upstream, planned to be added in V8:
         * https://github.com/stil4m/elm-syntax/issues/57
         * https://github.com/stil4m/elm-syntax/commit/25403ee0b4e2f78265f37fd27b0682fe6f89ea71
         * */
        bool IsTripleQuoted = false)
        : Expression;

    /// <summary>Character literal expression.</summary>
    public sealed record CharLiteral(
        int Value)
        : Expression;

    /// <summary>Decimal integer literal expression.</summary>
    public sealed record Integer(
        long Value)
        : Expression;

    /// <summary>Hexadecimal integer literal expression.</summary>
    public sealed record Hex(
        long Value)
        : Expression;

    /// <summary>Floating-point literal expression.</summary>
    public sealed record Floatable(
        double Value)
        : Expression;

    /// <summary>Arithmetic negation of an expression.</summary>
    public sealed record Negation(
        Node<Expression> Expression)
        : Expression;

    /// <summary>List literal expression.</summary>
    public sealed record ListExpr(
        SeparatedSyntaxList<Node<Expression>> Elements)
        : Expression;

    /// <summary>Reference to a function or value.</summary>
    public sealed record FunctionOrValue(
        ModuleName ModuleName,
        string Name)
        : Expression, System.IEquatable<FunctionOrValue>
    {
        /// <inheritdoc/>
        public bool Equals(FunctionOrValue? other) =>
            other is not null &&
            Name == other.Name &&
            Enumerable.SequenceEqual(ModuleName, other.ModuleName);

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hash = new System.HashCode();

            foreach (var part in ModuleName)
            {
                hash.Add(part);
            }

            hash.Add(Name);

            return hash.ToHashCode();
        }

        /// <summary>
        /// Creates a new instance of <see cref="FunctionOrValue"/> from a fully qualified name, splitting the name into
        /// module and value components.
        /// </summary>
        public static FunctionOrValue FromFullName(string fullName)
        {
            var parts = fullName.Split('.');

            if (parts.Length is 0)
                throw new System.ArgumentException("Full name cannot be empty", nameof(fullName));

            var value = parts[^1];

            var moduleName =
                parts.Length > 1
                ?
                parts[..^1]
                :
                [];

            return new FunctionOrValue(moduleName, value);
        }
    }

    /// <summary>Conditional expression with then/else branches.</summary>
    public sealed record IfBlock(
        Location IfTokenLocation,
        Node<Expression> Condition,
        Location ThenTokenLocation,
        Node<Expression> ThenBlock,
        Location ElseTokenLocation,
        Node<Expression> ElseBlock)
        : Expression;

    /// <summary>Prefix operator expression.</summary>
    public sealed record PrefixOperator(
        string Operator)
        : Expression;

    /// <summary>Parenthesized subexpression.</summary>
    public sealed record ParenthesizedExpression(
        Location OpenParenLocation,
        Node<Expression> Expression,
        Location CloseParenLocation)
        : Expression;

    /// <summary>Function application expression.</summary>
    public sealed record Application(
        IReadOnlyList<Node<Expression>> Arguments)
        : Expression
    {
        /// <inheritdoc/>
        public bool Equals(Application? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return Enumerable.SequenceEqual(Arguments, other.Arguments);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            foreach (var item in Arguments)
                hashCode.Add(item);

            return hashCode.ToHashCode();
        }
    }

    /// <summary>Operator application with explicit direction.</summary>
    public sealed record OperatorApplication(
        Node<string> Operator,
        InfixDirection Direction,
        Node<Expression> Left,
        Node<Expression> Right)
        : Expression;

    /// <summary>Tuple literal expression.</summary>
    public sealed record TupledExpression(
        SeparatedSyntaxList<Node<Expression>> Elements)
        : Expression;

    /// <summary>Lambda expression.</summary>
    public sealed record LambdaExpression(
        LambdaStruct Lambda)
        : Expression;

    /// <summary>Case expression matching an expression over patterns.</summary>
    public sealed record CaseExpression(
        CaseBlock CaseBlock)
        : Expression;

    /// <summary>Let expression introducing local declarations.</summary>
    public sealed record LetExpression(
        LetBlock Value)
        : Expression;

    /// <summary>
    /// Let block containing declarations and final expression.
    /// </summary>
    public sealed record LetBlock(
        Location LetTokenLocation,
        IReadOnlyList<Node<LetDeclaration>> Declarations,
        Location InTokenLocation,
        Node<Expression> Expression)
    {
        /// <inheritdoc/>
        public bool Equals(LetBlock? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                LetTokenLocation.Equals(other.LetTokenLocation) &&
                Enumerable.SequenceEqual(Declarations, other.Declarations) &&
                InTokenLocation.Equals(other.InTokenLocation) &&
                Expression.Equals(other.Expression);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            hashCode.Add(LetTokenLocation);

            foreach (var item in Declarations)
                hashCode.Add(item);

            hashCode.Add(InTokenLocation);
            hashCode.Add(Expression);

            return hashCode.ToHashCode();
        }
    }

    /// <summary>
    /// Declarations inside a let: nested functions or destructuring bindings.
    /// </summary>
    public abstract record LetDeclaration
    {
        /// <summary>Local function declaration inside a let block.</summary>
        public sealed record LetFunction(
            FunctionStruct Function)
            : LetDeclaration;

        /// <summary>Local binding via pattern destructuring.</summary>
        public sealed record LetDestructuring(
            Node<Pattern> Pattern,
            Location EqualsTokenLocation,
            Node<Expression> Expression)
            : LetDeclaration;
    }

    /// <summary>Record literal expression.</summary>
    public sealed record RecordExpr(
        SeparatedSyntaxList<RecordExprField> Fields)
        : Expression;

    /// <summary>Expression accessing a record field.</summary>
    public sealed record RecordAccess(
        Node<Expression> Record,
        Node<string> FieldName)
        : Expression;

    /// <summary>Expression yielding a getter function for a record field.</summary>
    public sealed record RecordAccessFunction(
        string FunctionName)
        : Expression;

    /// <summary>Expression updating a record value.</summary>
    public sealed record RecordUpdateExpression(
        Node<string> RecordName,
        Location PipeLocation,
        SeparatedSyntaxList<RecordExprField> Fields)
        : Expression;
}

/// <summary>
/// A record expression field with field name, equals sign location, and value expression.
/// Used in RecordExpr and RecordUpdateExpression.
/// </summary>
public record RecordExprField(
    Node<string> FieldName,
    Location EqualsLocation,
    Node<Expression> ValueExpr);


/// <summary>
/// Lambda expression capturing argument patterns and body.
/// </summary>
public record LambdaStruct(
    Location BackslashLocation,
    IReadOnlyList<Node<Pattern>> Arguments,
    Location ArrowLocation,
    Node<Expression> Expression)
{
    /// <inheritdoc/>
    public virtual bool Equals(LambdaStruct? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            BackslashLocation.Equals(other.BackslashLocation) &&
            Enumerable.SequenceEqual(Arguments, other.Arguments) &&
            ArrowLocation.Equals(other.ArrowLocation) &&
            Expression.Equals(other.Expression);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new System.HashCode();

        hashCode.Add(BackslashLocation);

        foreach (var item in Arguments)
            hashCode.Add(item);

        hashCode.Add(ArrowLocation);
        hashCode.Add(Expression);

        return hashCode.ToHashCode();
    }
}

/// <summary>
/// Case expression block containing scrutinee and branches.
/// </summary>
public record CaseBlock(
    Location CaseTokenLocation,
    Node<Expression> Expression,
    Location OfTokenLocation,
    IReadOnlyList<Case> Cases)
{
    /// <inheritdoc/>
    public virtual bool Equals(CaseBlock? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            CaseTokenLocation.Equals(other.CaseTokenLocation) &&
            Expression.Equals(other.Expression) &&
            OfTokenLocation.Equals(other.OfTokenLocation) &&
            Enumerable.SequenceEqual(Cases, other.Cases);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new System.HashCode();

        hashCode.Add(CaseTokenLocation);
        hashCode.Add(Expression);
        hashCode.Add(OfTokenLocation);

        foreach (var item in Cases)
            hashCode.Add(item);

        return hashCode.ToHashCode();
    }
}

/// <summary>
/// Single case: pattern and resulting expression.
/// </summary>
public record Case(
    Node<Pattern> Pattern,
    Location ArrowLocation,
    Node<Expression> Expression);
