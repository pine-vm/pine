module ElmSyntax.Concrete.Import exposing (..)

import ElmSyntax.Concrete.Exposing exposing (Exposing)
import ElmSyntax.Concrete.Module exposing (ModuleName)
import ElmSyntax.Concrete.Node exposing (Node)
import ElmSyntax.Concrete.Range exposing (Location)



{-

   /// <summary>
   /// Elm import statement containing target module name, optional alias and exposing list.
   /// </summary>
   public record Import(
       Location ImportTokenLocation,
       Node<ModuleName> ModuleName,
       (Location AsTokenLocation, Node<ModuleName> Alias)? ModuleAlias,
       (Location ExposingTokenLocation, Node<Exposing> ExposingList)? ExposingList);

-}


type alias Import =
    { importTokenLocation : Location
    , moduleName : Node ModuleName
    , moduleAlias : Maybe ( Location, Node ModuleName )
    , exposingList : Maybe ( Location, Node Exposing )
    }
