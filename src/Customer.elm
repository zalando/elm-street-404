module Customer
    exposing
        ( Customer
        , animate
        , livesHere
        , decHappiness
        , incHappiness
        , isLost
        , rodnams
        )

import MapObject exposing (MapObject)
import Random
import Time exposing (Time)
import Dict exposing (Dict)
import AnimationState exposing (AnimatedObject, animateFrame)
import IHopeItWorks


type alias Customer =
    AnimatedObject
        { typ : Int
        , id : Int
        , location : Maybe MapObject
        , happiness : Int
        , isDressed : Bool
        }


initial : Int -> MapObject -> Int -> Customer
initial id house typ =
    { typ = typ
    , happiness = 2
    , location = Just house
    , elapsed = 0
    , timeout = 150
    , frame = 0
    , id = id
    , isDressed = False
    }


animate : Time -> Customer -> Customer
animate =
    animateFrame 2


rodnam : Int -> MapObject -> Random.Generator Customer
rodnam id house =
    Random.map
        (Maybe.withDefault 0 >> initial id house)
        (IHopeItWorks.pickRandom (List.range 0 6 ++ [ 1, 4, 5 ] ++ [ 1, 4, 5 ]))


rodnams : Int -> List MapObject -> Random.Generator (Dict Int Customer)
rodnams id houses =
    case houses of
        [] ->
            Random.map (always Dict.empty) (Random.int 0 0)

        -- could be Random.succeed []
        house :: rest ->
            Random.map2 (Dict.insert id) (rodnam id house) (rodnams (id + 1) rest)


livesHere : MapObject -> Customer -> Bool
livesHere house { location } =
    location == Just house


isLost : Customer -> Bool
isLost { location } =
    location == Nothing


modHappiness : Int -> Customer -> Customer
modHappiness d ({ happiness, location } as customer) =
    let
        newHappiness =
            if customer.isDressed then
                happiness
            else
                happiness + d

        newLocation =
            if newHappiness < 0 then
                Nothing
            else
                location
    in
        { customer
            | happiness = clamp 0 2 newHappiness
            , location = newLocation
        }


incHappiness : Customer -> Customer
incHappiness =
    modHappiness 1


decHappiness : Customer -> Customer
decHappiness =
    modHappiness -1
