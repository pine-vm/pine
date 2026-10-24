module ElmSyntax.Concrete.File exposing (..)

import ElmSyntax.Concrete.Declaration exposing (Declaration)
import ElmSyntax.Concrete.Import exposing (Import)
import ElmSyntax.Concrete.Module as Module exposing (Module)
import ElmSyntax.Concrete.Node exposing (Node)
import ElmSyntax.Concrete.Range exposing (Location)



{-


   /// <summary>
   /// Root of an Elm source file: module definition, imports, top declarations and comments.
   /// <para>
   /// To learn about the design goals, see 'guide\elm-syntax-model-and-parser.md'
   /// </para>
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
-}


type alias File =
    { moduleDefinition : Node Module
    , imports : List (Node Import)
    , declarations : List (Node Declaration)
    , comments : List (Node String)
    , incompleteDeclarations : List (Node IncompleteDeclaration)
    }



{-
   public record IncompleteDeclaration(
       string OriginalText,
       ElmSyntaxParseError ParseError);
-}


type alias IncompleteDeclaration =
    { originalText : String
    , parseError : ElmSyntaxParseError
    }


type alias ElmSyntaxParseError =
    { message : String
    , location : Location
    }
