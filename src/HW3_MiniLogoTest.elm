module HW3_MiniLogoTest exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)



-- Elm/SVG auxiliary definitions
--


scale =
    30


xPt : Int -> String
xPt i =
    String.fromInt (scale + i * scale)


yPt : Int -> String
yPt i =
    String.fromInt (398 - i * scale)


svgLine : Line -> Svg a
svgLine ( ( a, b ), ( c, d ) ) =
    line
        [ x1 (xPt a)
        , y1 (yPt b)
        , x2 (xPt c)
        , y2 (yPt d)
        , stroke "green"
        , strokeWidth "4"
        , strokeLinecap "round"
        ]
        []


main : Html msg
main =
    svg [ viewBox "0 0 400 400", width "800", height "800" ]
        (List.map svgLine logoResult)



----- BEGIN HW3 solution


type alias Point =
    ( Int, Int )


type Mode
    = Up
    | Down


type Cmd
    = Pen Mode
    | MoveTo Point
    | Seq Cmd Cmd


type alias State =
    ( Mode, Point )


type alias Line =
    ( Point, Point )


type alias Lines =
    List Line


semCmd : Cmd -> State -> ( State, Lines )
semCmd c s =
    case ( c, s ) of
        ( Pen x, _ ) ->
            ( ( x, second s ), [] )

        ( MoveTo np, ( pm, xy ) ) ->
            case pm of
                Up ->
                    ( ( pm, np ), [] )

                Down ->
                    ( ( pm, np ), [ ( xy, np ) ] )

        ( Seq c1 c2, ns ) ->
            let
                ( s1, l1 ) =
                    semCmd c1 ns

                ( s2, l2 ) =
                    semCmd c2 s1
            in
            ( s2, l1 ++ l2 )


lines : Cmd -> Lines
lines c =
    second (semCmd c ( Up, ( 0, 0 ) ))


logoResult : Lines
logoResult =
    lines
        (Seq
            (MoveTo ( 2, 4 ))
            (Seq
                (Pen Down)
                (Seq
                    (MoveTo ( 2, 2 ))
                    (Seq
                        (Pen Up)
                        (Seq
                            (MoveTo ( 3, 4 ))
                            (Seq
                                (Pen Down)
                                (Seq
                                    (MoveTo ( 3, 2 ))
                                    (Seq
                                        (Pen Up)
                                        (Seq
                                            (MoveTo ( 0, 1 ))
                                            (Seq
                                                (Pen Down)
                                                (Seq
                                                    (MoveTo ( 1, 0 ))
                                                    (Seq
                                                        (MoveTo ( 4, 0 ))
                                                        (Seq
                                                            (MoveTo ( 5, 1 ))
                                                            (Pen Up)
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
