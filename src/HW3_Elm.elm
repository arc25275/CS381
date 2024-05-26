module HW3_Elm exposing (..)

import Tuple exposing (first, second)



{- Part 1 - Stack Language -}
-- Type defs


type Op
    = LD Int
    | ADD
    | MULT
    | DUP


type alias Prog =
    List Op


type alias Stack =
    List Int


type alias D =
    Stack -> Maybe Stack


{-| Does an operation on a stack that is given
-}
semOp : Op -> D
semOp op =
    \s ->
        case ( op, s ) of
            ( LD i, xs ) ->
                Just (i :: xs)

            ( ADD, x :: y :: xs ) ->
                Just ((x + y) :: xs)

            ( MULT, x :: y :: xs ) ->
                Just ((x * y) :: xs)

            ( DUP, x :: xs ) ->
                Just (x :: x :: xs)

            _ ->
                Nothing


{-| Works through a stack given a list of operations and a starting stack
-}
semProg : Prog -> D
semProg prog =
    \s ->
        case prog of
            [] ->
                Just s

            {-
               See if semOp gives an error, if not, see if semProg gives an error with the new stack included.
               This is to protect against cases where there might be valid operations, and then later on invalid ones.
               Instead of returning a partially finished stack, we return the error
            -}
            op :: ops ->
                semOp op s
                    |> Maybe.andThen (\ns -> semProg ops ns)



-- {- Part 2 - Mini Logo -}
-- Type Defs


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


{-| semCmd to create a list of lines by the given command, and the state at the end
-}
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


{-| lines command to extract the line component from semCmd, and give a default state
-}
lines : Cmd -> Lines
lines c =
    second (semCmd c ( Up, ( 0, 0 ) ))



{-
   A nice logo =)
-}


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
