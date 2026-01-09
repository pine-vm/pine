using System.Collections.Generic;

/*
 * Types for modelling parsed concrete syntax, based on
 * https://github.com/stil4m/elm-syntax/tree/58671250026416cdae72100bb0c67da17dec92ee/src/Elm/Syntax
 * */

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

namespace Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

/// <summary>
/// Wrapper pairing a value with its source range in the parsed text.
/// </summary>
public record Node<T>(
    Range Range,
    T Value)
{
    /// <summary>
    /// Casts the contained value to another type without changing range.
    /// </summary>
    public Node<TOther> Cast<TOther>()
    {
        return new Node<TOther>(Range, (TOther)(object)Value);
    }

    /// <summary>
    /// Creates a new node with the specified start and end locations, updating the range accordingly.
    /// </summary>
    public Node<T> WithRange(
        Location newStart,
        Location newEnd) =>
        this
        with
        {
            Range = new Range(newStart, newEnd)
        };

    /// <inheritdoc/>
    public virtual bool Equals(Node<T>? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        if (Range != other.Range)
            return false;

        if (Value is ModuleName list && other.Value is ModuleName otherList)
        {
            return System.Linq.Enumerable.SequenceEqual(list, otherList);
        }

        // Handle tuples containing ModuleName (e.g., (ModuleName ModuleName, string Name))
        if (Value is System.ValueTuple<ModuleName, string> tuple &&
            other.Value is System.ValueTuple<ModuleName, string> otherTuple)
        {
            return System.Linq.Enumerable.SequenceEqual(tuple.Item1, otherTuple.Item1) &&
                   tuple.Item2 == otherTuple.Item2;
        }

        return EqualityComparer<T>.Default.Equals(Value, other.Value);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new System.HashCode();

        hashCode.Add(Range);

        if (Value is ModuleName list)
        {
            foreach (var item in list)
            {
                hashCode.Add(item);
            }
        }
        else if (Value is System.ValueTuple<ModuleName, string> tuple)
        {
            foreach (var item in tuple.Item1)
            {
                hashCode.Add(item);
            }
            hashCode.Add(tuple.Item2);
        }
        else
        {
            hashCode.Add(Value);
        }

        return hashCode.ToHashCode();
    }
}

/// <summary>
/// Inclusive start / exclusive end positions spanning a source fragment.
/// </summary>
public record Range(
    Location Start,
    Location End);

/// <summary>
/// One-based line and column within source text.
/// </summary>
public readonly record struct Location(
    int Row,
    int Column);

/// <summary>
/// Root of an Elm source file: module definition, imports, top declarations and comments.
/// </summary>
public record File(
    Node<Module> ModuleDefinition,
    IReadOnlyList<Node<Import>> Imports,
    IReadOnlyList<Node<Declaration>> Declarations,
    IReadOnlyList<Node<string>> Comments)
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
            System.Linq.Enumerable.SequenceEqual(Imports, other.Imports) &&
            System.Linq.Enumerable.SequenceEqual(Declarations, other.Declarations) &&
            System.Linq.Enumerable.SequenceEqual(Comments, other.Comments);
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

        return hashCode.ToHashCode();
    }
}

/// <summary>
/// Elm import statement containing target module name, optional alias and exposing list.
/// </summary>
public record Import(
    Node<ModuleName> ModuleName,
    Node<ModuleName>? ModuleAlias,
    Node<Exposing>? ExposingList);

/// <summary>
/// Elm module kinds: normal, port or effect (with possible commands/subscriptions).
/// </summary>
public abstract record Module
{
    /// <summary>Standard module.</summary>
    public sealed record NormalModule(
        DefaultModuleData ModuleData)
        : Module;

    /// <summary>Port module exposing native interop.</summary>
    public sealed record PortModule(
        DefaultModuleData ModuleData)
        : Module;

    /// <summary>Effect module with commands and subscriptions.</summary>
    public sealed record EffectModule(
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
    Node<Exposing> ExposingList);

/// <summary>
/// Data for effect modules including optional command and subscription identifiers.
/// </summary>
public record EffectModuleData(
    Node<ModuleName> ModuleName,
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

            return System.Linq.Enumerable.SequenceEqual(Nodes, other.Nodes);
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
    Node<InfixDirection> Direction,
    Node<int> Precedence,
    Node<string> Operator,
    Node<string> FunctionName);

/// <summary>
/// Type alias with optional documentation, generics and underlying type annotation.
/// </summary>
public record TypeAlias(
    Node<string>? Documentation,
    Node<string> Name,
    IReadOnlyList<Node<string>> Generics,
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
            Name.Equals(other.Name) &&
            System.Linq.Enumerable.SequenceEqual(Generics, other.Generics) &&
            TypeAnnotation.Equals(other.TypeAnnotation);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new System.HashCode();

        hashCode.Add(Documentation);
        hashCode.Add(Name);

        foreach (var item in Generics)
            hashCode.Add(item);

        hashCode.Add(TypeAnnotation);

        return hashCode.ToHashCode();
    }
}

/// <summary>
/// Custom type declaration including constructors.
/// </summary>
public record TypeStruct(
    Node<string>? Documentation,
    Node<string> Name,
    IReadOnlyList<Node<string>> Generics,
    IReadOnlyList<Node<ValueConstructor>> Constructors)
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
            Name.Equals(other.Name) &&
            System.Linq.Enumerable.SequenceEqual(Generics, other.Generics) &&
            System.Linq.Enumerable.SequenceEqual(Constructors, other.Constructors);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new System.HashCode();

        hashCode.Add(Documentation);
        hashCode.Add(Name);

        foreach (var item in Generics)
            hashCode.Add(item);

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
            System.Linq.Enumerable.SequenceEqual(Arguments, other.Arguments);
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
                System.Linq.Enumerable.SequenceEqual(TypeArguments, other.TypeArguments);
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
        IReadOnlyList<Node<TypeAnnotation>> TypeAnnotations)
        : TypeAnnotation
    {
        /// <inheritdoc/>
        public bool Equals(Tupled? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return System.Linq.Enumerable.SequenceEqual(TypeAnnotations, other.TypeAnnotations);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            foreach (var item in TypeAnnotations)
                hashCode.Add(item);

            return hashCode.ToHashCode();
        }
    }

    /// <summary>Record type annotation.</summary>
    public sealed record Record(
        RecordDefinition RecordDefinition)
        : TypeAnnotation;

    /// <summary>Record type annotation that extends a generic record.</summary>
    public sealed record GenericRecord(
        Node<string> GenericName,
        Node<RecordDefinition> RecordDefinition)
        : TypeAnnotation;

    /// <summary>Function type annotation mapping argument to return.</summary>
    public sealed record FunctionTypeAnnotation(
        Node<TypeAnnotation> ArgumentType,
        Node<TypeAnnotation> ReturnType)
        : TypeAnnotation;
}

/// <summary>
/// Record type definition listing fields.
/// </summary>
public record RecordDefinition(
    IReadOnlyList<Node<RecordField>> Fields)
{
    /// <inheritdoc/>
    public virtual bool Equals(RecordDefinition? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return System.Linq.Enumerable.SequenceEqual(Fields, other.Fields);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new System.HashCode();

        foreach (var item in Fields)
            hashCode.Add(item);

        return hashCode.ToHashCode();
    }
}

/// <summary>
/// Single record field definition pairing name with type annotation.
/// </summary>
public record RecordField(
    Node<string> FieldName,
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
            System.Linq.Enumerable.SequenceEqual(Arguments, other.Arguments) &&
            Expression.Equals(other.Expression);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new System.HashCode();

        hashCode.Add(Name);

        foreach (var item in Arguments)
            hashCode.Add(item);

        hashCode.Add(Expression);

        return hashCode.ToHashCode();
    }
}

/// <summary>
/// Type signature for a value/function: name and type annotation.
/// </summary>
public record Signature(
    Node<string> Name,
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
        IReadOnlyList<Node<Pattern>> Elements)
        : Pattern
    {
        /// <inheritdoc/>
        public bool Equals(TuplePattern? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return System.Linq.Enumerable.SequenceEqual(Elements, other.Elements);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            foreach (var item in Elements)
                hashCode.Add(item);

            return hashCode.ToHashCode();
        }
    }

    /// <summary>Pattern matching a record with specified fields.</summary>
    public sealed record RecordPattern(
        IReadOnlyList<Node<string>> Fields)
        : Pattern
    {
        /// <inheritdoc/>
        public bool Equals(RecordPattern? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return System.Linq.Enumerable.SequenceEqual(Fields, other.Fields);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            foreach (var item in Fields)
                hashCode.Add(item);

            return hashCode.ToHashCode();
        }
    }

    /// <summary>List cons pattern separating head and tail.</summary>
    public sealed record UnConsPattern(
        Node<Pattern> Head,
        Node<Pattern> Tail)
        : Pattern;

    /// <summary>Pattern matching a list of elements.</summary>
    public sealed record ListPattern(
        IReadOnlyList<Node<Pattern>> Elements)
        : Pattern
    {
        /// <inheritdoc/>
        public bool Equals(ListPattern? other)
        {
            if (other is null)
                return false;

            if (ReferenceEquals(this, other))
                return true;

            return System.Linq.Enumerable.SequenceEqual(Elements, other.Elements);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            foreach (var item in Elements)
                hashCode.Add(item);

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
                System.Linq.Enumerable.SequenceEqual(Arguments, other.Arguments);
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
        Node<string> Name)
        : Pattern;

    /// <summary>Pattern wrapped in parentheses.</summary>
    public sealed record ParenthesizedPattern(
        Node<Pattern> Pattern)
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

        if (!System.Linq.Enumerable.SequenceEqual(ModuleName, other.ModuleName))
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
        IReadOnlyList<Node<Expression>> Elements)
        : Expression
    {
        /// <inheritdoc/>
        public bool Equals(ListExpr? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return System.Linq.Enumerable.SequenceEqual(Elements, other.Elements);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            foreach (var item in Elements)
                hashCode.Add(item);

            return hashCode.ToHashCode();
        }
    }

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
            System.Linq.Enumerable.SequenceEqual(ModuleName, other.ModuleName);

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

            var value = parts[0];

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
        Node<Expression> Condition,
        Node<Expression> ThenBlock,
        Node<Expression> ElseBlock)
        : Expression;

    /// <summary>Prefix operator expression.</summary>
    public sealed record PrefixOperator(
        string Operator)
        : Expression;

    /// <summary>Parenthesized subexpression.</summary>
    public sealed record ParenthesizedExpression(
        Node<Expression> Expression)
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

            return System.Linq.Enumerable.SequenceEqual(Arguments, other.Arguments);
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
        string Operator,
        InfixDirection Direction,
        Node<Expression> Left,
        Node<Expression> Right)
        : Expression;

    /// <summary>Tuple literal expression.</summary>
    public sealed record TupledExpression(
        IReadOnlyList<Node<Expression>> Elements)
        : Expression
    {
        /// <inheritdoc/>
        public bool Equals(TupledExpression? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return System.Linq.Enumerable.SequenceEqual(Elements, other.Elements);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            foreach (var item in Elements)
                hashCode.Add(item);

            return hashCode.ToHashCode();
        }
    }

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
        IReadOnlyList<Node<LetDeclaration>> Declarations,
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
                System.Linq.Enumerable.SequenceEqual(Declarations, other.Declarations) &&
                Expression.Equals(other.Expression);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            foreach (var item in Declarations)
                hashCode.Add(item);

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
            Node<Expression> Expression)
            : LetDeclaration;
    }

    /// <summary>Record literal expression.</summary>
    public sealed record RecordExpr(
        IReadOnlyList<Node<(Node<string> fieldName, Node<Expression> valueExpr)>> Fields)
        : Expression
    {
        /// <inheritdoc/>
        public bool Equals(RecordExpr? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return System.Linq.Enumerable.SequenceEqual(Fields, other.Fields);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            foreach (var item in Fields)
                hashCode.Add(item);

            return hashCode.ToHashCode();
        }
    }

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
        IReadOnlyList<Node<(Node<string> fieldName, Node<Expression> valueExpr)>> Fields)
        : Expression
    {
        /// <inheritdoc/>
        public bool Equals(RecordUpdateExpression? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                RecordName.Equals(other.RecordName) &&
                System.Linq.Enumerable.SequenceEqual(Fields, other.Fields);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new System.HashCode();

            hashCode.Add(RecordName);

            foreach (var item in Fields)
                hashCode.Add(item);

            return hashCode.ToHashCode();
        }
    }
}

/// <summary>
/// Lambda expression capturing argument patterns and body.
/// </summary>
public record LambdaStruct(
    IReadOnlyList<Node<Pattern>> Arguments,
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
            System.Linq.Enumerable.SequenceEqual(Arguments, other.Arguments) &&
            Expression.Equals(other.Expression);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new System.HashCode();

        foreach (var item in Arguments)
            hashCode.Add(item);

        hashCode.Add(Expression);

        return hashCode.ToHashCode();
    }
}

/// <summary>
/// Case expression block containing scrutinee and branches.
/// </summary>
public record CaseBlock(
    Node<Expression> Expression,
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
            Expression.Equals(other.Expression) &&
            System.Linq.Enumerable.SequenceEqual(Cases, other.Cases);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        var hashCode = new System.HashCode();

        hashCode.Add(Expression);

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
    Node<Expression> Expression);

/// <summary>
/// Direction of infix operator association.
/// </summary>
public enum InfixDirection
{
    /// <summary>Left-associative operator.</summary>
    Left,
    /// <summary>Right-associative operator.</summary>
    Right,
    /// <summary>Non-associative operator.</summary>
    Non,
}
