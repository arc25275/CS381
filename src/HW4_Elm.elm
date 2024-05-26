module HW4_Elm exposing (..)

{- Part 1 - Rank Based Type System -}
-- Type Defs


type alias Rank =
    Int


{-| (Taken, Added)
-}
type alias OpRank =
    ( Int, Int )


type Op
    = LD Int
    | ADD
    | MULT
    | DUP
    | DEC
    | SWAP
    | POP Int


type alias Prog =
    List Op


type alias Stack =
    List Int


{-| Determines the rank of a single operation
-}
rankOp : Op -> OpRank
rankOp o =
    case o of
        LD _ ->
            ( 0, 1 )

        ADD ->
            ( 2, 1 )

        MULT ->
            ( 2, 1 )

        DUP ->
            ( 1, 2 )

        DEC ->
            ( 1, 1 )

        SWAP ->
            ( 2, 2 )

        POP k ->
            ( k, 0 )


{-| Determines the rank of a program
-}
rankP : Prog -> Maybe Rank
rankP p =
    rank p 0


{-| Helper function for ranking program
-}
rank : Prog -> Rank -> Maybe Rank
rank p k =
    case p of
        [] ->
            Just k

        o :: xp ->
            case rankOp o of
                ( on, om ) ->
                    if on > k then
                        Nothing

                    else
                        rank xp (k + om - on)



-- Functions from HW3 Modified


type alias D =
    Stack -> Stack


{-| Does an operation on a stack that is given. (Added DEC, SWAP, and POP)
-}
semOp : Op -> D
semOp op =
    \s ->
        case ( op, s ) of
            ( LD i, xs ) ->
                i :: xs

            ( ADD, x :: y :: xs ) ->
                (x + y) :: xs

            ( MULT, x :: y :: xs ) ->
                (x * y) :: xs

            ( DUP, x :: xs ) ->
                x :: x :: xs

            ( DEC, x :: xs ) ->
                (x - 1) :: xs

            ( SWAP, x :: y :: xs ) ->
                y :: x :: xs

            ( POP k, _ :: _ ) ->
                List.drop k s

            _ ->
                s


{-| Works through a stack given a list of operations and a starting stack
-}
semProg : Prog -> D
semProg prog =
    \s ->
        case prog of
            op :: ops ->
                semOp op s |> (\ns -> semProg ops ns)

            [] ->
                s


{-| Semantic Type Checker
-}
semTC : Prog -> Maybe Stack
semTC p =
    case rankP p of
        Just _ ->
            Just (semProg p [])

        _ ->
            Nothing



{- Part 2 - Shape Language -}
-- Type Defs


type Shape
    = X
    | LR Shape Shape
    | TD Shape Shape


type alias BBox =
    ( Int, Int )


{-| Determine the bounding box for a shape
-}
bbox : Shape -> BBox
bbox s =
    case s of
        X ->
            ( 1, 1 )

        LR s1 s2 ->
            case ( bbox s1, bbox s2 ) of
                ( ( x1, y1 ), ( x2, y2 ) ) ->
                    ( x1 + x2, max y1 y2 )

        -- Pick the bigger value to be the y bounds
        TD s1 s2 ->
            case ( bbox s1, bbox s2 ) of
                ( ( x1, y1 ), ( x2, y2 ) ) ->
                    ( max x1 x2, y1 + y2 )


{-| Only allow full rectangular shapes.
This is very similar to bbox, but instead of checking which bound is higher, it checks that they are equal.
-}
rect : Shape -> Maybe BBox
rect s =
    case s of
        X ->
            Just ( 1, 1 )

        LR s1 s2 ->
            case ( rect s1, rect s2 ) of
                ( Just ( x1, y1 ), Just ( x2, y2 ) ) ->
                    if y1 == y2 then
                        Just ( x1 + x2, y1 )

                    else
                        Nothing

                _ ->
                    Nothing

        TD s1 s2 ->
            case ( rect s1, rect s2 ) of
                ( Just ( x1, y1 ), Just ( x2, y2 ) ) ->
                    if x1 == x2 then
                        Just ( x1, y1 + y2 )

                    else
                        Nothing

                _ ->
                    Nothing
