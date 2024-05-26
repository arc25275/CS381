module HW2_Elm exposing (..)

{-
   Pt.1 - Mini Logo Syntax
-}
-- Type Defs


type Mode
    = Up
    | Down


type Pos
    = Num Int
    | Name String


type Pars
    = ParsList (List String)


type Vals
    = ValsList (List Int)


type CMD
    = Pen Mode
    | MoveTo Pos Pos
    | Def String Pars CMD
    | Call String Vals
    | Seq CMD CMD


{-| Vector Macro to draw a line from one spot to another
-}
vector =
    Def "vector"
        (ParsList [ "x1", "y1", "x2", "y2" ])
        (Seq
            (MoveTo (Name "x1") (Name "y1"))
            (Seq
                (Pen Down)
                (MoveTo (Name "x2") (Name "y2"))
            )
        )


{-| Steps Function
Generates CMD with sequence of commands to make steps of size i
-}
steps : Int -> CMD
steps i =
    case i of
        0 ->
            Pen Up

        x ->
            if x == i then
                Seq
                    (MoveTo (Num x) (Num x))
                    (Seq
                        (Pen Down)
                        (Seq
                            (MoveTo (Num (x - 1)) (Num x))
                            (Seq
                                (MoveTo (Num (x - 1)) (Num (x - 1)))
                                (steps (x - 1))
                            )
                        )
                    )

            else
                Seq
                    (MoveTo
                        (Num (x - 1))
                        (Num x)
                    )
                    (Seq
                        (MoveTo
                            (Num (x - 1))
                            (Num (x - 1))
                        )
                        (steps (x - 1))
                    )



{- Pt.2 Grammar Grammar -}
-- Grammar Grammar Def


type alias NonTerm =
    String


type alias Term =
    String


type Symbol
    = NT NonTerm
    | T Term


type alias RHS =
    List Symbol


type Prod
    = Product NonTerm (List RHS)


type alias Grammar =
    List Prod


condProd : Prod
condProd =
    Product "cond" [ [ T "T" ], [ T "not", NT "cond" ], [ T "(", NT "cond", T ")" ] ]


stmtProd : Prod
stmtProd =
    Product "stmt" [ [ T "skip" ], [ T "while", NT "cond", T "do", T "{", NT "stmt", T "}" ], [ NT "stmt", T ";", NT "stmt" ] ]


imp : Grammar
imp =
    [ condProd, stmtProd ]


nonterminals : Grammar -> List NonTerm
nonterminals g =
    case g of
        [] ->
            []

        (Product x _) :: xs ->
            x :: nonterminals xs


terminals : Grammar -> List Term
terminals g =
    case g of
        [] ->
            []

        (Product _ prods) :: xs ->
            List.concatMap
                (\rhs ->
                    List.concatMap
                        (\sym ->
                            case sym of
                                NT _ ->
                                    []

                                T t ->
                                    [ t ]
                        )
                        rhs
                )
                prods
                ++ terminals xs
