{-
   Alex Clark
   clarka8@oregonstate.edu
   04/09/2024
   Assignment 1
   I used elm-format, which in my opinion uses too much whitespace, but it cannot be configured, so sorry for extra spaces in certain areas
-}


module HW1_Elm exposing (..)

import HW1_Def exposing (..)



{- Pt. 1 - Lists -}


b1 : Bag Int
b1 =
    [ ( 1, 5 ), ( 2, 4 ), ( 3, 1 ) ]


b2 : Bag Int
b2 =
    [ ( 3, 1 ), ( 1, 1 ), ( 5, 2 ) ]


b3 : Bag Int
b3 =
    [ ( 1, 5 ) ]


{-| ins function to add an element
-}
ins : a -> Bag a -> Bag a
ins num bg =
    case bg of
        -- If num is not in Bag, add it
        [] ->
            [ ( num, 1 ) ]

        -- If num is found, increment count
        ( x, count ) :: l ->
            if x == num then
                ( x, count + 1 ) :: l

            else
                ( x, count ) :: ins num l


{-| del function to remove an element
-}
del : a -> Bag a -> Bag a
del num bg =
    case bg of
        -- If num is not in Bag, add it
        [] ->
            []

        -- If num is found, increment count
        ( x, count ) :: l ->
            if x == num && count == 1 then
                l

            else if x == num && count > 1 then
                ( x, count - 1 ) :: l

            else
                ( x, count ) :: del num l


numList : List Int
numList =
    [ 1, 3, 2, 2, 3, 5, 1, 6, 3, 8, 10, 10, 8, 2 ]


{-| bag function to create a bag
-}
bag : List a -> Bag a
bag l =
    case l of
        [] ->
            []

        x :: xs ->
            ins x (bag xs)


{-| subbag function to check if bag A is a subset of bag B
-}
subbag : Bag a -> Bag a -> Bool
subbag ba bb =
    -- For all in bag A, check if there is a pair in bag B. Then, check if the count in bag B is at least the count in bag A
    List.all (\( x, count ) -> List.any (\( y, yCount ) -> y == x && yCount >= count) bb) ba


{-| isSet function to check there is only one of each value in the bag
-}
isSet : Bag a -> Bool
isSet bg =
    case bg of
        [] ->
            True

        ( x, _ ) :: xs ->
            if List.member x (fst (List.unzip xs)) then
                False

            else
                isSet xs


incorrectBag : Bag Int
incorrectBag =
    [ ( 1, 3 ), ( 2, 1 ), ( 4, 1 ), ( 1, 2 ), ( 3, 1 ) ]


{-| size function to calculate number of elements in bag
-}
size : Bag a -> Int
size bg =
    case bg of
        [] ->
            0

        ( _, count ) :: xs ->
            count + size xs



{- Pt. 2 - Graphs -}


g : Graph
g =
    [ ( 1, 2 ), ( 1, 3 ), ( 2, 3 ), ( 2, 4 ), ( 3, 4 ) ]


h : Graph
h =
    [ ( 1, 2 ), ( 1, 3 ), ( 2, 1 ), ( 3, 2 ), ( 4, 4 ) ]


{-| nodes function to calculate a list of nodes in a graph
-}
nodes : Graph -> List Node
nodes graph =
    asSet (fst (List.unzip graph) ++ snd (List.unzip graph))


{-| suc function to calculate a list of successors to a node
-}
suc : Node -> Graph -> List Node
suc node graph =
    List.map snd (List.filter (\( x, y ) -> x == node) graph)


{-| detach function to remove a node along with any edges it is mentioned in
-}
detach : Node -> Graph -> Graph
detach node graph =
    List.filter (\( x, y ) -> x /= node && y /= node) graph



{- Pt. 3 - Data Types -}


f : Figure
f =
    [ Pt ( 4, 4 ), Circle ( 5, 5 ) 3, Rect ( 3, 3 ) 7 2 ]


{-| width function to calculate the width of a shape
-}
width : Shape -> Length
width shape =
    case shape of
        Pt _ ->
            0

        Circle _ r ->
            r * 2

        Rect _ w _ ->
            w


{-| bbox function to calculate the bounding box of a shape, which is the smallest rectangle that will enclose it.
-}
bbox : Shape -> BBox
bbox shape =
    case shape of
        Pt z ->
            ( z, z )

        Circle z r ->
            ( Tuple.mapBoth ((+) -r) ((+) -r) z, Tuple.mapBoth ((+) r) ((+) r) z )

        Rect z w l ->
            ( z, Tuple.mapBoth ((+) w) ((+) l) z )


{-| mijnX function to calculate the minimum x coordinates of a shape
-}
minX : Shape -> Number
minX shape =
    case shape of
        Pt z ->
            fst z

        Circle z r ->
            fst z - r

        Rect z w l ->
            fst z


{-| move function to calculate the new position of a shape based on a vector
-}
move : Point -> Shape -> Shape
move vec shape =
    case shape of
        Pt z ->
            Pt (addPoint z vec)

        Circle z r ->
            Circle (addPoint z vec) r

        Rect z w l ->
            Rect (addPoint z vec) w l


{-| addPoint function to calculate the addition of two points
-}
addPoint : Point -> Point -> Point
addPoint p1 p2 =
    ( fst p1 + fst p2, snd p1 + snd p2 )
