module AliasDecl exposing (..)


type alias AliasInOrder =
    { alfa : Int
    , beta : Int
    , gamma : Int
    }


type alias AliasOutOfOrder =
    { beta : Int
    , alfa : Int
    , gamma : Int
    }
