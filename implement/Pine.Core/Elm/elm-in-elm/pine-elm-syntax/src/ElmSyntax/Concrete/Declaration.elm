module ElmSyntax.Concrete.Declaration exposing (..)

import ElmSyntax.Concrete.Expression as Expression exposing (Expression, Signature)
import ElmSyntax.Concrete.Infix exposing (Infix, InfixDirection)
import ElmSyntax.Concrete.Node exposing (Node)
import ElmSyntax.Concrete.Pattern exposing (Pattern)
import ElmSyntax.Concrete.Range exposing (Location)
import ElmSyntax.Concrete.SeparatedSyntaxList exposing (SeparatedSyntaxList)
import ElmSyntax.Concrete.TypeAnnotation exposing (TypeAnnotation)



{-


   /// <summary>
   /// Top-level declarations in a module (functions, types, aliases, ports, infix directives).
   /// </summary>
   public abstract record Declaration
   {
       /// <summary>Function declaration including implementation.</summary>
       public sealed record FunctionDeclaration(
           FunctionStruct Function)
           : Declaration;

       /// <summary>Choice type declaration.</summary>
       public sealed record ChoiceTypeDeclaration(
           ChoiceTypeStruct TypeDeclaration)
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
   /// Choice type declaration including constructors.
   /// </summary>
   public record ChoiceTypeStruct(
       Node<string>? Documentation,
       Location TypeTokenLocation,
       Node<string> Name,
       IReadOnlyList<Node<string>> Generics,
       Location EqualsTokenLocation,
       SeparatedSyntaxList<Node<ValueConstructor>> Constructors)
   {
       /// <inheritdoc/>
       public virtual bool Equals(ChoiceTypeStruct? other)
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
               Constructors.Equals(other.Constructors);
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

           hashCode.Add(Constructors);

           return hashCode.ToHashCode();
       }
   }

   /// <summary>
   /// Constructor for a choice type with argument type annotations.
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
       /// <remarks>
       /// The open and close parenthesis locations can be derived from the containing node's range:
       /// - OpenParenLocation = ContainingNode.Range.Start
       /// - CloseParenLocation = ContainingNode.Range.End with Column - 1
       /// </remarks>
       public sealed record Tupled(
           SeparatedSyntaxList<Node<TypeAnnotation>> TypeAnnotations)
           : TypeAnnotation;

       /// <summary>Record type annotation.</summary>
       /// <remarks>
       /// The open and close brace locations can be derived from the containing node's range:
       /// - OpenBraceLocation = ContainingNode.Range.Start
       /// - CloseBraceLocation = ContainingNode.Range.End with Column - 1
       /// </remarks>
       public sealed record Record(
           RecordDefinition RecordDefinition)
           : TypeAnnotation;

       /// <summary>Record type annotation that extends a generic record.</summary>
       /// <remarks>
       /// The open and close brace locations can be derived from the containing node's range:
       /// - OpenBraceLocation = ContainingNode.Range.Start
       /// - CloseBraceLocation = ContainingNode.Range.End with Column - 1
       /// </remarks>
       public sealed record GenericRecord(
           Node<string> GenericName,
           Location PipeLocation,
           Node<RecordDefinition> RecordDefinition)
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


-}


type Declaration
    = FunctionDeclaration Expression.FunctionStruct
    | ChoiceTypeDeclaration ChoiceStruct
    | AliasDeclaration TypeAlias
    | PortDeclaration Location Signature
    | InfixDeclaration Infix


type alias ChoiceStruct =
    { documentation : Maybe (Node String)
    , typeTokenLocation : Location
    , name : Node String
    , generics : List (Node String)
    , equalsTokenLocation : Location
    , constructors : SeparatedSyntaxList (Node ValueConstructor)
    }


type alias ValueConstructor =
    { name : Node String
    , arguments : List (Node TypeAnnotation)
    }


type alias TypeAlias =
    { documentation : Maybe (Node String)
    , typeTokenLocation : Location
    , aliasTokenLocation : Location
    , name : Node String
    , generics : List (Node String)
    , equalsTokenLocation : Location
    , typeAnnotation : Node TypeAnnotation
    }
