module HW1_Def exposing (..)


-- Some abbreviations
--
fst = Tuple.first
snd = Tuple.second
map = List.map
sum = List.sum
filter = List.filter


-- Exercise 1
--
type alias Bag a = List (a, Int)


-- Exercise 2
--
type alias Node  = Int
type alias Edge  = (Node, Node)
type alias Graph = List Edge


asSet : List comparable -> List comparable
asSet = rmDup << List.sort

rmDup : List comparable -> List comparable
rmDup l = case l of
    x::y::zs -> if x==y then rmDup (y::zs) else x::rmDup (y::zs)
    xs       -> xs


-- Exercise 3
--
type alias Number = Int
type alias Point = (Number, Number)
type alias Length = Number
type Shape = Pt Point
           | Circle Point Length
           | Rect Point Length Length

type alias Figure = List Shape
type alias BBox = (Point, Point)
