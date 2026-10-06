module AsciiTable exposing (Align(..), Column, view)

import MicroListExtra as List


type Align
    = AlignLeft
    | AlignRight


type alias Column a =
    { toString : a -> String
    , align : Align
    }


{-| This would normally return just the final String, but we need to postprocess
the table rows a little (insert a divider row in between).

So we return the original item next to the rendered row which makes the
postprocessing possible!

-}
view : List (Column a) -> List a -> List { item : a, renderedRow : String }
view columns items =
    let
        columnData : List (List String)
        columnData =
            columns
                |> List.map (\col -> List.map col.toString items)

        columnLengths : List Int
        columnLengths =
            columnData
                |> List.map
                    (\colRows ->
                        List.map String.length colRows
                            |> List.maximum
                            |> Maybe.withDefault 0
                    )

        padFn : Int -> Align -> String -> String
        padFn length align string =
            case align of
                AlignLeft ->
                    String.padRight length ' ' string

                AlignRight ->
                    String.padLeft length ' ' string

        paddedColumnData : List (List String)
        paddedColumnData =
            List.map3 (\col colLength colStrings -> List.map (padFn colLength col.align) colStrings)
                columns
                columnLengths
                columnData
    in
    List.map2
        (\item rowCells ->
            { item = item
            , renderedRow = String.join "  " rowCells
            }
        )
        items
        (List.transpose paddedColumnData)
