module Elm.Parser.State exposing (State, addComment, emptyState, expectedColumn, getComments, popIndent, pushColumn, storedColumns)

import Elm.Syntax.Node exposing (Node)


type State
    = State
        { indents : List Int
        , comments : List (Node String)
        }


emptyState : State
emptyState =
    State
        { indents = []
        , comments = []
        }


currentIndent : State -> Int
currentIndent (State { indents }) =
    case indents of
        [] ->
            0

        current :: _ ->
            current


storedColumns : State -> List Int
storedColumns (State { indents }) =
    indents


expectedColumn : State -> Int
expectedColumn state =
    currentIndent state + 1


popIndent : State -> State
popIndent (State s) =
    State
        { indents = List.drop 1 s.indents
        , comments = s.comments
        }


pushIndent : Int -> State -> State
pushIndent x (State s) =
    State
        { indents = x :: s.indents
        , comments = s.comments
        }


pushColumn : Int -> State -> State
pushColumn col state =
    pushIndent (col - 1) state


addComment : Node String -> State -> State
addComment pair (State s) =
    State
        { indents = s.indents
        , comments = pair :: s.comments
        }


getComments : State -> List (Node String)
getComments (State s) =
    List.reverse s.comments
