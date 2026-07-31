module ElmSyntax.Concrete.Pattern exposing (..)

import ElmSyntax.Concrete.Node exposing (Node)
import ElmSyntax.Concrete.Range exposing (Location, Range)
import ElmSyntax.Concrete.SeparatedSyntaxList exposing (SeparatedSyntaxList)



{-


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
        SeparatedSyntaxList<Node<Pattern>> Elements)
        : Pattern;

    /// <summary>Pattern matching a record with specified fields.</summary>
    public sealed record RecordPattern(
        SeparatedSyntaxList<Node<string>> Fields)
        : Pattern;

    /// <summary>List cons pattern separating head and tail.</summary>
    public sealed record UnConsPattern(
        Node<Pattern> Head,
        Location ConsOperatorLocation,
        Node<Pattern> Tail)
        : Pattern;

    /// <summary>Pattern matching a list of elements.</summary>
    public sealed record ListPattern(
        SeparatedSyntaxList<Node<Pattern>> Elements)
        : Pattern;

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
        Node<Pattern> Pattern)
        : Pattern;
}

-}


type Pattern
    = AllPattern
    | VarPattern String
    | UnitPattern
    | CharPattern Int
    | StringPattern String
    | IntPattern Int
    | HexPattern Int
    | FloatPattern Float
    | TuplePattern (SeparatedSyntaxList (Node Pattern))
    | RecordPattern (SeparatedSyntaxList (Node String))
    | UnConsPattern (Node Pattern) Location (Node Pattern)
    | ListPattern (SeparatedSyntaxList (Node Pattern))
    | NamedPattern QualifiedNameRef (List (Node Pattern))
    | AsPattern (Node Pattern) Location (Node String)
    | ParenthesizedPattern (Node Pattern)


{-| Qualified name reference such as `Maybe.Just`.
-}
type alias QualifiedNameRef =
    { moduleName : List String
    , name : String
    }
