module Frontend.ElmTimeLogo exposing (..)

import Axis2d
import BoundingBox2d
import Direction2d
import Html
import Html.Attributes
import Point2d
import Polygon2d
import Quantity
import Svg
import Svg.Attributes
import Vector2d


type alias ShapeConfig =
    { strokeThicknessMikro : Int
    }


main : Html.Html e
main =
    Html.div
        [ Html.Attributes.style "background" "#192A3A"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100%"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "justify-content" "center"
        , Html.Attributes.style "align-items" "center"
        ]
        [ Html.div
            [ Html.Attributes.style "width" "100px"
            , Html.Attributes.style "margin" "1%"
            ]
            [ elmTimeLogoSvg
                { strokeThicknessMikro = 30 }
                { fill = "#8DBFF0"
                , svgAttributes = []
                }
            ]
        ]


elmTimeLogoSvg : ShapeConfig -> { fill : String, svgAttributes : List (Svg.Attribute e) } -> Svg.Svg e
elmTimeLogoSvg shape { fill, svgAttributes } =
    let
        logoPolygons =
            elmTimeLogoShape shape

        allPoints =
            logoPolygons
                |> List.concatMap Polygon2d.outerLoop

        boundingBox =
            BoundingBox2d.hull
                (Maybe.withDefault (Point2d.unitless 0 0) (List.head allPoints))
                allPoints

        polygonElements =
            logoPolygons
                |> List.map
                    (\polygon ->
                        let
                            points =
                                Polygon2d.outerLoop polygon
                                    |> List.map
                                        (Point2d.coordinates
                                            >> Tuple.mapBoth Quantity.toFloat Quantity.toFloat
                                            >> Tuple.mapBoth round round
                                        )
                        in
                        Svg.polygon
                            [ Svg.Attributes.points (svgPolygonPointsString points)
                            , Html.Attributes.style "fill" fill
                            ]
                            []
                    )
    in
    Svg.svg
        (Svg.Attributes.viewBox (svgViewBoxFromBoundingBox boundingBox) :: svgAttributes)
        polygonElements


svgViewBoxFromBoundingBox : BoundingBox2d.BoundingBox2d Quantity.Unitless coordinates -> String
svgViewBoxFromBoundingBox boundingBox =
    let
        x =
            boundingBox
                |> BoundingBox2d.minX
                |> Quantity.toFloat

        y =
            boundingBox
                |> BoundingBox2d.minY
                |> Quantity.toFloat

        ( width, height ) =
            BoundingBox2d.dimensions boundingBox
                |> Tuple.mapBoth Quantity.toFloat Quantity.toFloat
    in
    String.join " "
        [ String.fromFloat x
        , String.fromFloat y
        , String.fromFloat width
        , String.fromFloat height
        ]


elmTimeLogoShape : ShapeConfig -> List (Polygon2d.Polygon2d Quantity.Unitless coordinates)
elmTimeLogoShape config =
    let
        plainPolygons =
            elmTimeLogoPolygons
                |> List.map (List.map (\( x, y ) -> Point2d.unitless (toFloat x) (toFloat y)) >> Polygon2d.singleLoop)

        allPoints =
            plainPolygons
                |> List.concatMap Polygon2d.outerLoop

        boundingBox =
            BoundingBox2d.hull
                (Point2d.unitless 0 0)
                allPoints

        ( boundingBoxDimensionsX, boundingBoxDimensionsY ) =
            BoundingBox2d.dimensions boundingBox

        shrinkThickness =
            Quantity.max boundingBoxDimensionsX boundingBoxDimensionsY
                |> Quantity.toFloat
                |> (*) (toFloat config.strokeThicknessMikro / 2000)
    in
    plainPolygons
        |> List.map (shrinkPolygon shrinkThickness)


shrinkPolygon :
    Float
    -> Polygon2d.Polygon2d Quantity.Unitless coordinates
    -> Polygon2d.Polygon2d Quantity.Unitless coordinates
shrinkPolygon width polygon =
    let
        originalPoints =
            Polygon2d.outerLoop polygon

        pointFromWrappedIndex index =
            originalPoints
                |> List.drop (index |> modBy (List.length originalPoints))
                |> List.head

        mapPoint { previous, following } point =
            Axis2d.throughPoints previous point
                |> Maybe.andThen
                    (\axisFromPrev ->
                        Axis2d.throughPoints point following
                            |> Maybe.map
                                (\axisToFollowing ->
                                    let
                                        axisToFollowingTranslated =
                                            axisToFollowing
                                                |> Axis2d.translateBy
                                                    (axisToFollowing
                                                        |> Axis2d.direction
                                                        |> Direction2d.rotateCounterclockwise
                                                        |> Direction2d.toVector
                                                        |> Vector2d.scaleBy width
                                                    )

                                        axisFromPreviousTranslated =
                                            axisFromPrev
                                                |> Axis2d.translateBy
                                                    (axisFromPrev
                                                        |> Axis2d.direction
                                                        |> Direction2d.rotateCounterclockwise
                                                        |> Direction2d.toVector
                                                        |> Vector2d.scaleBy width
                                                    )
                                    in
                                    axisToFollowingTranslated
                                        |> intersectionPoint axisFromPreviousTranslated
                                        |> Maybe.withDefault point
                                )
                    )
                |> Maybe.withDefault point
    in
    originalPoints
        |> List.indexedMap
            (\index point ->
                mapPoint
                    { previous =
                        pointFromWrappedIndex (index - 1)
                            |> Maybe.withDefault point
                    , following =
                        pointFromWrappedIndex (index + 1)
                            |> Maybe.withDefault point
                    }
                    point
            )
        |> Polygon2d.singleLoop


elmTimeLogoPolygons : List (List ( Int, Int ))
elmTimeLogoPolygons =
    [ [ ( 150, 100 ), ( 350, 100 ), ( 350, 300 ), ( 150, 300 ) ]
    , [ ( 150, 300 ), ( 350, 300 ), ( 150, 500 ) ]
    , [ ( 150, 500 ), ( 350, 300 ), ( 350, 500 ), ( 150, 700 ) ]
    , [ ( 150, 700 ), ( 350, 500 ), ( 350, 900 ) ]
    , [ ( 150, 700 ), ( 350, 900 ), ( 150, 900 ) ]
    , [ ( 450, 100 ), ( 850, 500 ), ( 450, 500 ) ]
    , [ ( 450, 500 ), ( 850, 500 ), ( 450, 900 ) ]
    ]


svgPolygonPointsString : List ( Int, Int ) -> String
svgPolygonPointsString points =
    let
        pointToString ( x, y ) =
            String.fromInt x ++ "," ++ String.fromInt y
    in
    List.map pointToString points
        |> String.join " "


intersectionPoint :
    Axis2d.Axis2d units coordinates
    -> Axis2d.Axis2d units coordinates
    -> Maybe (Point2d.Point2d units coordinates)
intersectionPoint axis1 axis2 =
    let
        a1p1 =
            Axis2d.originPoint axis1

        a1p2 =
            Point2d.translateIn (Axis2d.direction axis1) (Quantity.unsafe 1) a1p1

        a1 =
            Vector2d.from a1p2 a1p1 |> Vector2d.yComponent |> Quantity.unwrap

        b1 =
            Vector2d.from a1p1 a1p2 |> Vector2d.xComponent |> Quantity.unwrap

        c1 =
            a1 * Quantity.unwrap (Point2d.xCoordinate a1p1) + b1 * Quantity.unwrap (Point2d.yCoordinate a1p1)

        a2p1 =
            Axis2d.originPoint axis2

        a2p2 =
            Point2d.translateIn (Axis2d.direction axis2) (Quantity.unsafe 1) a2p1

        a2 =
            Vector2d.from a2p2 a2p1 |> Vector2d.yComponent |> Quantity.unwrap

        b2 =
            Vector2d.from a2p1 a2p2 |> Vector2d.xComponent |> Quantity.unwrap

        c2 =
            a2 * Quantity.unwrap (Point2d.xCoordinate a2p1) + b2 * Quantity.unwrap (Point2d.yCoordinate a2p1)

        delta =
            a1 * b2 - a2 * b1

        x_ =
            (b2 * c1 - b1 * c2) / delta

        y_ =
            (a1 * c2 - a2 * c1) / delta
    in
    if isNaN x_ || isInfinite x_ || isNaN y_ || isInfinite y_ then
        Nothing

    else
        Point2d.unsafe { x = x_, y = y_ } |> Just
