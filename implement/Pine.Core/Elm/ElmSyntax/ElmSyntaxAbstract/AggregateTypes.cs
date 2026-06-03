using Pine.Core.PopularEncodings;
using System.Collections.Generic;
using System.Numerics;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

namespace Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

/*
 * The abstract Elm syntax model is optimized for use in an interpreter or as input for
 * compilation into lower-level languages.
 *
 * Compared to the concrete model (namespace 'SyntaxModel'), this model:
 *
 * + Carries no source locations or ranges.
 * + Carries no trivia such as comments, documentation or redundant parentheses.
 * + Stores literals in normalized form (e.g. integers are parsed to their numeric value,
 *   floats to a rational structure, strings have their escape sequences applied).
 *
 * In addition, some nodes carry precomputed <see cref="PineValue"/> instances so that an
 * interpreter operating on <see cref="PineValue"/> can reuse them instead of repeating the
 * conversion on every evaluation:
 *
 * + <see cref="Expression.Integer"/> carries the <see cref="PineValue"/> from
 *   <see cref="Pine.Core.PopularEncodings.IntegerEncoding"/>.
 * + <see cref="Expression.RecordAccess"/>, <see cref="Expression.RecordAccessFunction"/> and
 *   the field names in <see cref="RecordSetter"/> carry the <see cref="PineValue"/> string
 *   encoding of the field name, used to scan a record for the matching field.
 * + The literal patterns that an interpreter compares against the scrutinee by value equality
 *   (<see cref="Pattern.IntPattern"/>, <see cref="Pattern.CharPattern"/>,
 *   <see cref="Pattern.StringPattern"/>) carry the precomputed <see cref="PineValue"/> of the
 *   value to match, and <see cref="Pattern.NamedPattern"/> (tag pattern) carries the
 *   <see cref="PineValue"/> string encoding of the constructor tag name to compare against.
 *
 * For conversion from the concrete model, see <see cref="ConvertFromConcrete"/>; for conversion
 * back to the concrete model, see <see cref="ConvertToConcrete"/>.
 */

/// <summary>
/// Helper methods for value-equality and hashing of composite structures in the abstract model.
/// </summary>
internal static class AbstractModelEquality
{
    /// <summary>
    /// Compares two (optionally null) lists element-wise using the default equality comparer.
    /// </summary>
    public static bool SequenceEqual<T>(IReadOnlyList<T>? a, IReadOnlyList<T>? b)
    {
        if (ReferenceEquals(a, b))
            return true;

        if (a is null || b is null)
            return false;

        if (a.Count != b.Count)
            return false;

        for (var i = 0; i < a.Count; i++)
        {
            if (!EqualityComparer<T>.Default.Equals(a[i], b[i]))
                return false;
        }

        return true;
    }

    /// <summary>
    /// Adds every element of the given list to the hash code.
    /// </summary>
    public static void AddItems<T>(ref System.HashCode hashCode, IReadOnlyList<T>? items)
    {
        if (items is null)
            return;

        foreach (var item in items)
            hashCode.Add(item);
    }
}

/// <summary>
/// Root of an Elm source file in the abstract model: module definition, imports and top-level declarations.
/// </summary>
public record File(
    Module ModuleDefinition,
    IReadOnlyList<Import> Imports,
    IReadOnlyList<Declaration> Declarations)
{
    private readonly int _slimHashCode = ComputeSlimHashCode(ModuleDefinition, Imports, Declarations);

    private static int ComputeSlimHashCode(
        Module moduleDefinition,
        IReadOnlyList<Import> imports,
        IReadOnlyList<Declaration> declarations)
    {
        var hashCode = new System.HashCode();

        hashCode.Add(moduleDefinition);
        AbstractModelEquality.AddItems(ref hashCode, imports);
        AbstractModelEquality.AddItems(ref hashCode, declarations);

        return hashCode.ToHashCode();
    }

    /// <inheritdoc/>
    public virtual bool Equals(File? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            ModuleDefinition.Equals(other.ModuleDefinition) &&
            AbstractModelEquality.SequenceEqual(Imports, other.Imports) &&
            AbstractModelEquality.SequenceEqual(Declarations, other.Declarations);
    }

    /// <inheritdoc/>
    public override int GetHashCode() => _slimHashCode;
}

/// <summary>
/// Elm import statement: target module name, optional alias and optional exposing list.
/// </summary>
public record Import(
    ModuleName ModuleName,
    ModuleName? ModuleAlias,
    Exposing? ExposingList)
{
    private readonly int _slimHashCode = ComputeSlimHashCode(ModuleName, ModuleAlias, ExposingList);

    private static int ComputeSlimHashCode(
        ModuleName moduleName,
        ModuleName? moduleAlias,
        Exposing? exposingList)
    {
        var hashCode = new System.HashCode();

        AbstractModelEquality.AddItems(ref hashCode, moduleName);
        AbstractModelEquality.AddItems(ref hashCode, moduleAlias);
        hashCode.Add(exposingList);

        return hashCode.ToHashCode();
    }

    /// <inheritdoc/>
    public virtual bool Equals(Import? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            AbstractModelEquality.SequenceEqual(ModuleName, other.ModuleName) &&
            AbstractModelEquality.SequenceEqual(ModuleAlias, other.ModuleAlias) &&
            EqualityComparer<Exposing?>.Default.Equals(ExposingList, other.ExposingList);
    }

    /// <inheritdoc/>
    public override int GetHashCode() => _slimHashCode;
}

/// <summary>
/// Elm module kinds: normal, port or effect.
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
}

/// <summary>
/// Shared data for normal/port modules: name and exposing list.
/// </summary>
public record DefaultModuleData(
    ModuleName ModuleName,
    Exposing ExposingList)
{
    private readonly int _slimHashCode = ComputeSlimHashCode(ModuleName, ExposingList);

    private static int ComputeSlimHashCode(
        ModuleName moduleName,
        Exposing exposingList)
    {
        var hashCode = new System.HashCode();

        AbstractModelEquality.AddItems(ref hashCode, moduleName);
        hashCode.Add(exposingList);

        return hashCode.ToHashCode();
    }

    /// <inheritdoc/>
    public virtual bool Equals(DefaultModuleData? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            AbstractModelEquality.SequenceEqual(ModuleName, other.ModuleName) &&
            ExposingList.Equals(other.ExposingList);
    }

    /// <inheritdoc/>
    public override int GetHashCode() => _slimHashCode;
}

/// <summary>
/// Data for effect modules including optional command and subscription identifiers.
/// </summary>
public record EffectModuleData(
    ModuleName ModuleName,
    Exposing ExposingList,
    string? Command,
    string? Subscription)
{
    private readonly int _slimHashCode = ComputeSlimHashCode(ModuleName, ExposingList, Command, Subscription);

    private static int ComputeSlimHashCode(
        ModuleName moduleName,
        Exposing exposingList,
        string? command,
        string? subscription)
    {
        var hashCode = new System.HashCode();

        AbstractModelEquality.AddItems(ref hashCode, moduleName);
        hashCode.Add(exposingList);
        hashCode.Add(command);
        hashCode.Add(subscription);

        return hashCode.ToHashCode();
    }

    /// <inheritdoc/>
    public virtual bool Equals(EffectModuleData? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            AbstractModelEquality.SequenceEqual(ModuleName, other.ModuleName) &&
            ExposingList.Equals(other.ExposingList) &&
            Command == other.Command &&
            Subscription == other.Subscription;
    }

    /// <inheritdoc/>
    public override int GetHashCode() => _slimHashCode;
}

/// <summary>
/// How a module exposes its definitions: all or an explicit list.
/// </summary>
public abstract record Exposing
{
    /// <summary>Expose everything (represented by <c>(..)</c> in Elm).</summary>
    public sealed record All
        : Exposing;

    /// <summary>Explicit list of exposed top-level items.</summary>
    public sealed record Explicit(
        IReadOnlyList<TopLevelExpose> Exposes)
        : Exposing
    {
        private readonly int _slimHashCode = ComputeSlimHashCode(Exposes);

        private static int ComputeSlimHashCode(IReadOnlyList<TopLevelExpose> exposes)
        {
            var hashCode = new System.HashCode();

            AbstractModelEquality.AddItems(ref hashCode, exposes);

            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public bool Equals(Explicit? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return AbstractModelEquality.SequenceEqual(Exposes, other.Exposes);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;
    }
}

/// <summary>
/// Individual item exposed from a module: operator, function, type or type with constructors.
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

    /// <summary>Exposed choice type, optionally including its constructors.</summary>
    public sealed record TypeExpose(
        ExposedType ExposedType)
        : TopLevelExpose;
}

/// <summary>
/// Choice type exposure specification: name and whether constructors are exposed (the <c>(..)</c> form).
/// </summary>
public record ExposedType(
    string Name,
    bool ExposesConstructors);

/// <summary>
/// Top-level declarations in a module.
/// </summary>
public abstract record Declaration
{
    /// <summary>Function declaration including implementation.</summary>
    public sealed record FunctionDeclaration(
        FunctionStruct Function)
        : Declaration;

    /// <summary>Choice type declaration.</summary>
    public sealed record ChoiceTypeDeclaration(
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
    SyntaxModel.InfixDirection Direction,
    int Precedence,
    string Operator,
    string FunctionName);

/// <summary>
/// Type alias with generics and underlying type annotation.
/// </summary>
public record TypeAlias(
    string Name,
    ModuleName Generics,
    TypeAnnotation TypeAnnotation)
{
    private readonly int _slimHashCode = ComputeSlimHashCode(Name, Generics, TypeAnnotation);

    private static int ComputeSlimHashCode(
        string name,
        ModuleName generics,
        TypeAnnotation typeAnnotation)
    {
        var hashCode = new System.HashCode();

        hashCode.Add(name);
        AbstractModelEquality.AddItems(ref hashCode, generics);
        hashCode.Add(typeAnnotation);

        return hashCode.ToHashCode();
    }

    /// <inheritdoc/>
    public virtual bool Equals(TypeAlias? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            Name == other.Name &&
            AbstractModelEquality.SequenceEqual(Generics, other.Generics) &&
            TypeAnnotation.Equals(other.TypeAnnotation);
    }

    /// <inheritdoc/>
    public override int GetHashCode() => _slimHashCode;
}

/// <summary>
/// Choice type declaration including constructors.
/// </summary>
public record TypeStruct(
    string Name,
    ModuleName Generics,
    IReadOnlyList<ValueConstructor> Constructors)
{
    private readonly int _slimHashCode = ComputeSlimHashCode(Name, Generics, Constructors);

    private static int ComputeSlimHashCode(
        string name,
        ModuleName generics,
        IReadOnlyList<ValueConstructor> constructors)
    {
        var hashCode = new System.HashCode();

        hashCode.Add(name);
        AbstractModelEquality.AddItems(ref hashCode, generics);
        AbstractModelEquality.AddItems(ref hashCode, constructors);

        return hashCode.ToHashCode();
    }

    /// <inheritdoc/>
    public virtual bool Equals(TypeStruct? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            Name == other.Name &&
            AbstractModelEquality.SequenceEqual(Generics, other.Generics) &&
            AbstractModelEquality.SequenceEqual(Constructors, other.Constructors);
    }

    /// <inheritdoc/>
    public override int GetHashCode() => _slimHashCode;
}

/// <summary>
/// Constructor for a choice type with argument type annotations.
/// </summary>
public record ValueConstructor(
    string Name,
    IReadOnlyList<TypeAnnotation> Arguments)
{
    private readonly int _slimHashCode = ComputeSlimHashCode(Name, Arguments);

    private static int ComputeSlimHashCode(
        string name,
        IReadOnlyList<TypeAnnotation> arguments)
    {
        var hashCode = new System.HashCode();

        hashCode.Add(name);
        AbstractModelEquality.AddItems(ref hashCode, arguments);

        return hashCode.ToHashCode();
    }

    /// <inheritdoc/>
    public virtual bool Equals(ValueConstructor? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            Name == other.Name &&
            AbstractModelEquality.SequenceEqual(Arguments, other.Arguments);
    }

    /// <inheritdoc/>
    public override int GetHashCode() => _slimHashCode;
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
        ModuleName ModuleName,
        string Name,
        IReadOnlyList<TypeAnnotation> TypeArguments)
        : TypeAnnotation
    {
        private readonly int _slimHashCode = ComputeSlimHashCode(ModuleName, Name, TypeArguments);

        private static int ComputeSlimHashCode(
            ModuleName moduleName,
            string name,
            IReadOnlyList<TypeAnnotation> typeArguments)
        {
            var hashCode = new System.HashCode();

            AbstractModelEquality.AddItems(ref hashCode, moduleName);
            hashCode.Add(name);
            AbstractModelEquality.AddItems(ref hashCode, typeArguments);

            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public bool Equals(Typed? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                AbstractModelEquality.SequenceEqual(ModuleName, other.ModuleName) &&
                Name == other.Name &&
                AbstractModelEquality.SequenceEqual(TypeArguments, other.TypeArguments);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>Unit type annotation.</summary>
    public sealed record Unit
        : TypeAnnotation;

    /// <summary>Tuple type annotation.</summary>
    public sealed record Tupled(
        IReadOnlyList<TypeAnnotation> TypeAnnotations)
        : TypeAnnotation
    {
        private readonly int _slimHashCode = ComputeSlimHashCode(TypeAnnotations);

        private static int ComputeSlimHashCode(IReadOnlyList<TypeAnnotation> typeAnnotations)
        {
            var hashCode = new System.HashCode();

            AbstractModelEquality.AddItems(ref hashCode, typeAnnotations);

            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public bool Equals(Tupled? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return AbstractModelEquality.SequenceEqual(TypeAnnotations, other.TypeAnnotations);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>Record type annotation.</summary>
    public sealed record Record(
        RecordDefinition RecordDefinition)
        : TypeAnnotation;

    /// <summary>Record type annotation that extends a generic record.</summary>
    public sealed record GenericRecord(
        string GenericName,
        RecordDefinition RecordDefinition)
        : TypeAnnotation;

    /// <summary>Function type annotation mapping argument to return.</summary>
    public sealed record FunctionTypeAnnotation(
        TypeAnnotation ArgumentType,
        TypeAnnotation ReturnType)
        : TypeAnnotation;
}

/// <summary>
/// Record type definition listing fields.
/// </summary>
public record RecordDefinition(
    IReadOnlyList<RecordField> Fields)
{
    private readonly int _slimHashCode = ComputeSlimHashCode(Fields);

    private static int ComputeSlimHashCode(IReadOnlyList<RecordField> fields)
    {
        var hashCode = new System.HashCode();

        AbstractModelEquality.AddItems(ref hashCode, fields);

        return hashCode.ToHashCode();
    }

    /// <inheritdoc/>
    public virtual bool Equals(RecordDefinition? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return AbstractModelEquality.SequenceEqual(Fields, other.Fields);
    }

    /// <inheritdoc/>
    public override int GetHashCode() => _slimHashCode;
}

/// <summary>
/// Single record field definition pairing name with type annotation.
/// </summary>
public record RecordField(
    string FieldName,
    TypeAnnotation FieldType);

/// <summary>
/// Function declaration parts: optional signature plus implementation.
/// </summary>
public record FunctionStruct(
    Signature? Signature,
    FunctionImplementation Declaration);

/// <summary>
/// Function implementation name, argument patterns and body expression.
/// </summary>
public record FunctionImplementation(
    string Name,
    IReadOnlyList<Pattern> Arguments,
    Expression Expression)
{
    private readonly int _slimHashCode = ComputeSlimHashCode(Name, Arguments, Expression);

    private static int ComputeSlimHashCode(
        string name,
        IReadOnlyList<Pattern> arguments,
        Expression expression)
    {
        var hashCode = new System.HashCode();

        hashCode.Add(name);
        AbstractModelEquality.AddItems(ref hashCode, arguments);
        hashCode.Add(expression);

        return hashCode.ToHashCode();
    }

    /// <inheritdoc/>
    public virtual bool Equals(FunctionImplementation? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            Name == other.Name &&
            AbstractModelEquality.SequenceEqual(Arguments, other.Arguments) &&
            Expression.Equals(other.Expression);
    }

    /// <inheritdoc/>
    public override int GetHashCode() => _slimHashCode;
}

/// <summary>
/// Type signature for a value/function: name and type annotation.
/// </summary>
public record Signature(
    string Name,
    TypeAnnotation TypeAnnotation);

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
    /// <param name="Value">The matched Unicode code point.</param>
    /// <param name="ValueAsPineValue">
    /// The precomputed <see cref="PineValue"/> from
    /// <see cref="ElmValueEncoding.ElmCharAsPineValue(int)"/>, so an interpreter operating on
    /// <see cref="PineValue"/> can reuse the instance for the equality check instead of repeating the conversion.
    /// </param>
    public sealed record CharPattern(
        int Value,
        PineValue ValueAsPineValue)
        : Pattern
    {
        /// <summary>
        /// Creates a <see cref="CharPattern"/>, precomputing <see cref="ValueAsPineValue"/> from <paramref name="value"/>.
        /// </summary>
        public CharPattern(int value)
            : this(value, ElmValueEncoding.ElmCharAsPineValue(value))
        {
        }
    }

    /// <summary>Pattern matching a specific string literal.</summary>
    /// <param name="Value">The matched string, with escape sequences already applied.</param>
    /// <param name="ValueAsPineValue">
    /// The precomputed <see cref="PineValue"/> from
    /// <see cref="ElmValueEncoding.StringAsPineValue(string)"/>, so an interpreter operating on
    /// <see cref="PineValue"/> can reuse the instance for the equality check instead of repeating the conversion.
    /// </param>
    public sealed record StringPattern(
        string Value,
        PineValue ValueAsPineValue)
        : Pattern
    {
        /// <summary>
        /// Creates a <see cref="StringPattern"/>, precomputing <see cref="ValueAsPineValue"/> from <paramref name="value"/>.
        /// </summary>
        public StringPattern(string value)
            : this(value, ElmValueEncoding.StringAsPineValue(value))
        {
        }
    }

    /// <summary>
    /// Pattern matching an integer literal.
    /// The hexadecimal/decimal distinction of the concrete model is normalized away here.
    /// </summary>
    /// <param name="Value">The numeric value of the literal.</param>
    /// <param name="ValueAsPineValue">
    /// The precomputed <see cref="PineValue"/> from
    /// <see cref="IntegerEncoding.EncodeSignedInteger(BigInteger)"/>, so an interpreter
    /// operating on <see cref="PineValue"/> can reuse the instance for the equality check instead of repeating the
    /// conversion.
    /// </param>
    public sealed record IntPattern(
        BigInteger Value,
        PineValue ValueAsPineValue)
        : Pattern
    {
        /// <summary>
        /// Creates an <see cref="IntPattern"/>, precomputing <see cref="ValueAsPineValue"/> from <paramref name="value"/>.
        /// </summary>
        public IntPattern(BigInteger value)
            : this(value, IntegerEncoding.EncodeSignedInteger(value))
        {
        }
    }

    /// <summary>Pattern matching a floating-point literal.</summary>
    public sealed record FloatPattern(
        double Value)
        : Pattern;

    /// <summary>Pattern matching a tuple.</summary>
    public sealed record TuplePattern(
        IReadOnlyList<Pattern> Elements)
        : Pattern
    {
        private readonly int _slimHashCode = ComputeSlimHashCode(Elements);

        private static int ComputeSlimHashCode(IReadOnlyList<Pattern> elements)
        {
            var hashCode = new System.HashCode();

            AbstractModelEquality.AddItems(ref hashCode, elements);

            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public bool Equals(TuplePattern? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return AbstractModelEquality.SequenceEqual(Elements, other.Elements);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>Pattern matching a record with specified fields.</summary>
    public sealed record RecordPattern(
        ModuleName Fields)
        : Pattern
    {
        private readonly int _slimHashCode = ComputeSlimHashCode(Fields);

        private static int ComputeSlimHashCode(ModuleName fields)
        {
            var hashCode = new System.HashCode();

            AbstractModelEquality.AddItems(ref hashCode, fields);

            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public bool Equals(RecordPattern? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return AbstractModelEquality.SequenceEqual(Fields, other.Fields);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>List cons pattern separating head and tail.</summary>
    public sealed record UnConsPattern(
        Pattern Head,
        Pattern Tail)
        : Pattern;

    /// <summary>Pattern matching a list of elements.</summary>
    public sealed record ListPattern(
        IReadOnlyList<Pattern> Elements)
        : Pattern
    {
        private readonly int _slimHashCode = ComputeSlimHashCode(Elements);

        private static int ComputeSlimHashCode(IReadOnlyList<Pattern> elements)
        {
            var hashCode = new System.HashCode();

            AbstractModelEquality.AddItems(ref hashCode, elements);

            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public bool Equals(ListPattern? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return AbstractModelEquality.SequenceEqual(Elements, other.Elements);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>Pattern matching a named constructor (tag) with arguments.</summary>
    /// <param name="Name">The qualified constructor name.</param>
    /// <param name="Arguments">The argument sub-patterns.</param>
    /// <param name="TagNameAsPineValue">
    /// The precomputed <see cref="PineValue"/> string encoding of the constructor tag name
    /// (<see cref="QualifiedNameRef.Name"/>), so an interpreter operating on <see cref="PineValue"/> can reuse the
    /// instance for the tag equality check instead of repeating the conversion.
    /// </param>
    public sealed record NamedPattern(
        QualifiedNameRef Name,
        IReadOnlyList<Pattern> Arguments,
        PineValue TagNameAsPineValue)
        : Pattern
    {
        /// <summary>
        /// Creates a <see cref="NamedPattern"/>, precomputing <see cref="TagNameAsPineValue"/> from the tag name
        /// (<see cref="QualifiedNameRef.Name"/>).
        /// </summary>
        public NamedPattern(
            QualifiedNameRef name,
            IReadOnlyList<Pattern> arguments)
            : this(name, arguments, StringEncoding.ValueFromString(name.Name))
        {
        }

        /// <inheritdoc/>
        public bool Equals(NamedPattern? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                Name.Equals(other.Name) &&
                AbstractModelEquality.SequenceEqual(Arguments, other.Arguments) &&
                TagNameAsPineValue.Equals(other.TagNameAsPineValue);
        }

        private readonly int _slimHashCode = ComputeSlimHashCode(Name, Arguments, TagNameAsPineValue);

        private static int ComputeSlimHashCode(
            QualifiedNameRef name,
            IReadOnlyList<Pattern> arguments,
            PineValue tagNameAsPineValue)
        {
            var hashCode = new System.HashCode();

            hashCode.Add(name);
            AbstractModelEquality.AddItems(ref hashCode, arguments);
            hashCode.Add(tagNameAsPineValue);

            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>Pattern that aliases a match to a name.</summary>
    public sealed record AsPattern(
        Pattern Pattern,
        string Name)
        : Pattern;
}

/// <summary>
/// Qualified reference to a name with its module path.
/// </summary>
public record QualifiedNameRef(
    ModuleName ModuleName,
    string Name)
{
    private readonly int _slimHashCode = ComputeSlimHashCode(ModuleName, Name);

    private static int ComputeSlimHashCode(
        ModuleName moduleName,
        string name)
    {
        var hashCode = new System.HashCode();

        AbstractModelEquality.AddItems(ref hashCode, moduleName);
        hashCode.Add(name);

        return hashCode.ToHashCode();
    }

    /// <inheritdoc/>
    public virtual bool Equals(QualifiedNameRef? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            Name == other.Name &&
            AbstractModelEquality.SequenceEqual(ModuleName, other.ModuleName);
    }

    /// <inheritdoc/>
    public override int GetHashCode() => _slimHashCode;

    /// <inheritdoc/>
    public override string ToString() =>
        ModuleName.Count > 0
        ?
        string.Join(".", ModuleName) + "." + Name
        :
        Name;
}

/// <summary>
/// Elm expressions: literals, applications, control flow, data structures, lambda, let, case, etc.
/// Literals are normalized and redundant parentheses are removed.
/// </summary>
public abstract record Expression
{
    /// <summary>Expression producing unit.</summary>
    public sealed record UnitExpr
        : Expression
    {
        /// <summary>
        /// The singleton instance of <see cref="UnitExpr"/>, since it has no state and all instances are equal.
        /// </summary>
        public static readonly UnitExpr Instance = new();
    }

    /// <summary>String literal expression, with escape sequences already applied.</summary>
    public sealed record StringLiteral(
        string Value,
        PineValue ValueAsPineValue)
        : Expression
    {
        /// <summary>
        /// Creates a <see cref="StringLiteral"/>, precomputing <see cref="ValueAsPineValue"/> from the string literal value.
        /// </summary>
        public static StringLiteral Create(string value) =>
            new(value, ElmValueEncoding.StringAsPineValue(value));
    }

    /// <summary>Character literal expression (Unicode code point).</summary>
    public sealed record CharLiteral(
        int Value,
        PineValue ValueAsPineValue)
        : Expression
    {
        /// <summary>
        /// Creates a <see cref="CharLiteral"/>, precomputing <see cref="ValueAsPineValue"/> from the character literal value.
        /// </summary>
        public static CharLiteral Create(int value) =>
            new(value, ElmValueEncoding.ElmCharAsPineValue(value));
    }

    /// <summary>
    /// Integer literal expression, normalized to its numeric value.
    /// </summary>
    /// <param name="Value">The numeric value of the literal.</param>
    /// <param name="ValueAsPineValue">
    /// The precomputed <see cref="PineValue"/> from
    /// <see cref="IntegerEncoding.EncodeSignedInteger(BigInteger)"/>, so an
    /// interpreter operating on <see cref="PineValue"/> can reuse the instance instead of repeating the conversion.
    /// </param>
    public sealed record Integer(
        BigInteger Value,
        PineValue ValueAsPineValue)
        : Expression;

    /// <summary>
    /// Floating-point literal expression, normalized to a rational number (numerator over denominator).
    /// </summary>
    public sealed record Floatable(
        BigInteger Numerator,
        BigInteger Denominator)
        : Expression;

    /// <summary>Arithmetic negation of an expression.</summary>
    public sealed record Negation(
        Expression Expression)
        : Expression;

    /// <summary>List literal expression.</summary>
    public sealed record ListExpr(
        IReadOnlyList<Expression> Elements)
        : Expression
    {
        private readonly int _slimHashCode = ComputeSlimHashCode(Elements);

        private static int ComputeSlimHashCode(IReadOnlyList<Expression> elements)
        {
            var hashCode = new System.HashCode();

            AbstractModelEquality.AddItems(ref hashCode, elements);

            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public bool Equals(ListExpr? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return AbstractModelEquality.SequenceEqual(Elements, other.Elements);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>Reference to a function or value.</summary>
    public sealed record FunctionOrValue(
        ModuleName ModuleName,
        string Name)
        : Expression
    {
        private readonly int _slimHashCode = ComputeSlimHashCode(ModuleName, Name);

        private static int ComputeSlimHashCode(
            ModuleName moduleName,
            string name)
        {
            var hashCode = new System.HashCode();

            AbstractModelEquality.AddItems(ref hashCode, moduleName);
            hashCode.Add(name);

            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public bool Equals(FunctionOrValue? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                Name == other.Name &&
                AbstractModelEquality.SequenceEqual(ModuleName, other.ModuleName);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>Conditional expression with then/else branches.</summary>
    public sealed record IfBlock(
        Expression Condition,
        Expression ThenBlock,
        Expression ElseBlock)
        : Expression;

    /// <summary>Prefix operator expression (the operator used as a function, e.g. <c>(+)</c>).</summary>
    public sealed record PrefixOperator(
        string Operator)
        : Expression;

    /// <summary>Function application expression.</summary>
    public sealed record Application(
        Expression Function,
        IReadOnlyList<Expression> Arguments)
        : Expression
    {
        private readonly int _slimHashCode = ComputeSlimHashCode(Function, Arguments);

        private static int ComputeSlimHashCode(
            Expression function,
            IReadOnlyList<Expression> arguments)
        {
            var hashCode = new System.HashCode();

            hashCode.Add(function);
            AbstractModelEquality.AddItems(ref hashCode, arguments);

            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public bool Equals(Application? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                Function.Equals(other.Function) &&
                AbstractModelEquality.SequenceEqual(Arguments, other.Arguments);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>Operator application with explicit direction.</summary>
    public sealed record OperatorApplication(
        string Operator,
        SyntaxModel.InfixDirection Direction,
        Expression Left,
        Expression Right)
        : Expression;

    /// <summary>Tuple literal expression.</summary>
    public sealed record TupledExpression(
        IReadOnlyList<Expression> Elements)
        : Expression
    {
        private readonly int _slimHashCode = ComputeSlimHashCode(Elements);

        private static int ComputeSlimHashCode(IReadOnlyList<Expression> elements)
        {
            var hashCode = new System.HashCode();

            AbstractModelEquality.AddItems(ref hashCode, elements);

            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public bool Equals(TupledExpression? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return AbstractModelEquality.SequenceEqual(Elements, other.Elements);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>Lambda expression.</summary>
    public sealed record LambdaExpression(
        IReadOnlyList<Pattern> Arguments,
        Expression Expression)
        : Expression
    {
        private readonly int _slimHashCode = ComputeSlimHashCode(Arguments, Expression);

        private static int ComputeSlimHashCode(
            IReadOnlyList<Pattern> arguments,
            Expression expression)
        {
            var hashCode = new System.HashCode();

            AbstractModelEquality.AddItems(ref hashCode, arguments);
            hashCode.Add(expression);

            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public bool Equals(LambdaExpression? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                AbstractModelEquality.SequenceEqual(Arguments, other.Arguments) &&
                Expression.Equals(other.Expression);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>Case expression matching a scrutinee expression over patterns.</summary>
    public sealed record CaseExpression(
        Expression Expression,
        IReadOnlyList<Case> Cases)
        : Expression
    {
        private readonly int _slimHashCode = ComputeSlimHashCode(Expression, Cases);

        private static int ComputeSlimHashCode(
            Expression expression,
            IReadOnlyList<Case> cases)
        {
            var hashCode = new System.HashCode();

            hashCode.Add(expression);
            AbstractModelEquality.AddItems(ref hashCode, cases);

            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public bool Equals(CaseExpression? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                Expression.Equals(other.Expression) &&
                AbstractModelEquality.SequenceEqual(Cases, other.Cases);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>Let expression introducing local declarations.</summary>
    public sealed record LetExpression(
        IReadOnlyList<LetDeclaration> Declarations,
        Expression Expression)
        : Expression
    {
        private readonly int _slimHashCode = ComputeSlimHashCode(Declarations, Expression);

        private static int ComputeSlimHashCode(
            IReadOnlyList<LetDeclaration> declarations,
            Expression expression)
        {
            var hashCode = new System.HashCode();

            AbstractModelEquality.AddItems(ref hashCode, declarations);
            hashCode.Add(expression);

            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public bool Equals(LetExpression? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                AbstractModelEquality.SequenceEqual(Declarations, other.Declarations) &&
                Expression.Equals(other.Expression);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>Record literal expression.</summary>
    public sealed record RecordExpr(
        IReadOnlyList<RecordSetter> Fields)
        : Expression
    {
        private readonly int _slimHashCode = ComputeSlimHashCode(Fields);

        private static int ComputeSlimHashCode(IReadOnlyList<RecordSetter> fields)
        {
            var hashCode = new System.HashCode();

            AbstractModelEquality.AddItems(ref hashCode, fields);

            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public bool Equals(RecordExpr? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return AbstractModelEquality.SequenceEqual(Fields, other.Fields);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>
    /// Expression accessing a record field.
    /// </summary>
    /// <param name="Record">The expression evaluating to the record.</param>
    /// <param name="FieldName">The accessed field name.</param>
    /// <param name="FieldNameValue">
    /// The precomputed <see cref="PineValue"/> string encoding of <paramref name="FieldName"/>, used by an
    /// interpreter to scan the record for the matching field.
    /// </param>
    public sealed record RecordAccess(
        Expression Record,
        string FieldName,
        PineValue FieldNameValue)
        : Expression;

    /// <summary>
    /// Expression yielding a getter function for a record field (the <c>.field</c> form).
    /// </summary>
    /// <param name="FieldName">The field name, without the leading dot.</param>
    /// <param name="FieldNameValue">
    /// The precomputed <see cref="PineValue"/> string encoding of <paramref name="FieldName"/>.
    /// </param>
    public sealed record RecordAccessFunction(
        string FieldName,
        PineValue FieldNameValue)
        : Expression;

    /// <summary>
    /// Expression updating a record value.
    /// </summary>
    /// <param name="RecordName">The name of the variable holding the record being updated.</param>
    /// <param name="Fields">The fields being updated, each carrying the field name's <see cref="PineValue"/>.</param>
    public sealed record RecordUpdateExpression(
        string RecordName,
        IReadOnlyList<RecordSetter> Fields)
        : Expression
    {
        private readonly int _slimHashCode = ComputeSlimHashCode(RecordName, Fields);

        private static int ComputeSlimHashCode(
            string recordName,
            IReadOnlyList<RecordSetter> fields)
        {
            var hashCode = new System.HashCode();

            hashCode.Add(recordName);
            AbstractModelEquality.AddItems(ref hashCode, fields);

            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public bool Equals(RecordUpdateExpression? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                RecordName == other.RecordName &&
                AbstractModelEquality.SequenceEqual(Fields, other.Fields);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => _slimHashCode;
    }

    /// <summary>GLSL shader expression.</summary>
    public sealed record GLSLExpression(
        string ShaderCode)
        : Expression;
}

/// <summary>
/// A record field assignment used in record literals and record updates.
/// </summary>
/// <param name="FieldName">The field name.</param>
/// <param name="FieldNameValue">
/// The precomputed <see cref="PineValue"/> string encoding of <paramref name="FieldName"/>, used by an
/// interpreter to scan the record for the matching field.
/// </param>
/// <param name="Value">The expression producing the field value.</param>
public record RecordSetter(
    string FieldName,
    PineValue FieldNameValue,
    Expression Value);

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
        Pattern Pattern,
        Expression Expression)
        : LetDeclaration;
}

/// <summary>
/// A single case branch: pattern and resulting expression.
/// </summary>
public record Case(
    Pattern Pattern,
    Expression Expression);
