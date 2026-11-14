module ElmMake.SourceLocation exposing (FileId(..), Location(..), Position, Region, createRegion, end, fromRange, start)

import ElmSyntax.Concrete.Range as Syntax


type FileId
    = ApplicationFile (List String)
    | PackageFile { name : String, version : String, path : List String }


type Location
    = Project
    | File FileId
    | Source FileId Region


{-| One-based Unicode character positions; tabs occupy one column.
Region starts are inclusive and ends exclusive. A zero-width region is valid.
-}
type alias Position =
    { line : Int, column : Int }


type Region
    = Region Position Position


createRegion : Position -> Position -> Result String Region
createRegion first last =
    if first.line < 1 || first.column < 1 || last.line < 1 || last.column < 1 then
        Err "Source positions must be one-based."

    else if ( last.line, last.column ) < ( first.line, first.column ) then
        Err "A source region must not end before it starts."

    else
        Ok (Region first last)


fromRange : Syntax.Range -> Result String Region
fromRange range =
    createRegion { line = range.start.row, column = range.start.column } { line = range.end.row, column = range.end.column }


start : Region -> Position
start (Region first _) =
    first


end : Region -> Position
end (Region _ last) =
    last
