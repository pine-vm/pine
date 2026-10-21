using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;
using Pine.Core.Json;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json.Serialization;

using InfixDirection = Pine.Core.Elm.ElmSyntax.SyntaxModel.InfixDirection;

namespace Pine.Core.Elm.ElmCompilerInDotnet.IntermediateRepresentation;

/// <summary>An emitted function has parameters and a body; its identity belongs only to the containing program map.</summary>
public sealed record FunctionStruct(ValueList<Pattern> Arguments, Expression Expression);

/// <summary>Resolved emission expressions. Source names, unresolved references, and local functions are not representable.</summary>
[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record Expression
{
    private Expression() { }

    /// <summary>A normalized encoded literal.</summary>
    public sealed record LiteralValue(PineValue Value) : Expression;

    /// <summary>One resolved reference; there is no alternate string identity.</summary>
    public sealed record Identifier(Reference Reference) : Expression
    {
        /// <summary>The mandatory resolved reference.</summary>
        public Reference Reference { get; } = Reference ?? throw new System.ArgumentNullException(nameof(Reference));
    }

    /// <summary>Numeric negation.</summary>
    public sealed record Negation(Expression Expression) : Expression;

    /// <summary>An ordered list or tuple in the runtime representation.</summary>
    public sealed record ListExpr(ValueList<Expression> Elements) : Expression;

    /// <summary>A conditional.</summary>
    public sealed record IfBlock(Expression Condition, Expression ThenBlock, Expression ElseBlock) : Expression;

    /// <summary>A first-class operator.</summary>
    public sealed record PrefixOperator(string Operator) : Expression;

    /// <summary>A function application.</summary>
    public sealed record Application(Expression Function, ValueList<Expression> Arguments) : Expression;

    /// <summary>A generic binary operation.</summary>
    public sealed record OperatorApplication(string Operator, InfixDirection Direction, Expression Left, Expression Right) : Expression;

    /// <summary>A physical list projection.</summary>
    public sealed record ProjectItem : Expression
    {
        /// <summary>The projected value.</summary>
        public Expression Value { get; }

        /// <summary>A nonnegative physical index.</summary>
        public int Index { get; }

        /// <summary>Constructs a valid projection.</summary>
        public ProjectItem(Expression value, int index)
        {
            System.ArgumentNullException.ThrowIfNull(value);
            System.ArgumentOutOfRangeException.ThrowIfNegative(index);
            Value = value;
            Index = index;
        }
    }

    /// <summary>A pattern match.</summary>
    public sealed record CaseExpression(Expression Expression, ValueList<Case> Cases) : Expression;

    /// <summary>A lexical value-binding group; functions must already have been lifted.</summary>
    public sealed record LetExpression(ValueList<LetDeclaration> Declarations, Expression Expression) : Expression;

    /// <summary>A record.</summary>
    public sealed record RecordExpr(ValueList<RecordSetter> Fields) : Expression
    {
        /// <summary>Constructs a record in physical field order.</summary>
        public static RecordExpr CreateSorted(IReadOnlyList<(string FieldName, Expression Value)> fields) =>
            new(
                new ValueList<RecordSetter>(
                    fields.OrderBy(field => field.FieldName, System.StringComparer.Ordinal)
                    .Select(field => new RecordSetter(field.FieldName, field.Value))));
    }

    /// <summary>A generic record read.</summary>
    public sealed record RecordAccess(Expression Record, string FieldName) : Expression
    {
        /// <summary>Derived runtime label encoding.</summary>
        [JsonIgnore]
        public PineValue FieldNameValue => StringEncoding.ValueFromString(FieldName);
    }

    /// <summary>A first-class record accessor.</summary>
    public sealed record RecordAccessFunction(string FieldName) : Expression
    {
        /// <summary>Derived runtime label encoding.</summary>
        [JsonIgnore]
        public PineValue FieldNameValue => StringEncoding.ValueFromString(FieldName);
    }

    /// <summary>A record update with one authoritative receiver expression.</summary>
    public sealed record RecordUpdateExpression(Expression Receiver, ValueList<RecordSetter> Fields) : Expression
    {
        /// <summary>The only receiver representation.</summary>
        public Expression Receiver { get; } = Receiver ?? throw new System.ArgumentNullException(nameof(Receiver));
    }

    /// <summary>Unsupported shader syntax retained for a structured backend diagnostic.</summary>
    public sealed record GLSLExpression(string ShaderCode) : Expression;
}

/// <summary>A record field. The encoded label is derived, not a second independent representation.</summary>
public sealed record RecordSetter(string FieldName, Expression Value)
{
    /// <summary>Derived runtime label encoding.</summary>
    [JsonIgnore]
    public PineValue FieldNameValue => StringEncoding.ValueFromString(FieldName);
}

/// <summary>A lexical value binding. Named local functions and redundant implementation names are absent.</summary>
public sealed record LetDeclaration(Pattern Pattern, Expression Expression);

/// <summary>A pattern-match branch.</summary>
public sealed record Case(Pattern Pattern, Expression Expression);
