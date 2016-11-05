module Category
    exposing
        ( Kind(..)
        , Category
        , random
        , getColor
        , fullyDressed
        )

import Random
import IHopeItWorks


type alias Category =
    { kind : Kind
    , color : Int
    }


type Kind
    = Shirt
    | Shoes
    | Pants
    | Scarf


kinds : List Kind
kinds =
    [ Shirt, Shoes, Pants, Scarf ]


fullyDressed : List Category -> Bool
fullyDressed categories =
    List.all (\kind -> List.any (.kind >> (==) kind) categories) kinds


getColor : Kind -> List Category -> Maybe Int
getColor kind categories =
    case IHopeItWorks.find (.kind >> (==) kind) categories of
        Just shirt ->
            Just shirt.color

        _ ->
            Nothing


random : Random.Generator Category
random =
    Random.map2
        Category
        (Random.map (Maybe.withDefault Scarf) (IHopeItWorks.pickRandom kinds))
        (Random.int 0 2)
