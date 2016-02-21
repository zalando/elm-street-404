module Customer (Customer, animate, livesHere, decHappiness, incHappiness, isLost, rodnams) where

import MapObject exposing (MapObject)
import Random
import Time exposing (Time)
import AnimationState exposing (AnimatedObject, animateObject, rotateFrames)


type Location
  = AtHome MapObject
  | Lost


type alias Customer =
  AnimatedObject
    { typ : Int
    , location : Location
    , happiness : Int
    , frames : List (Int)
    }


initial : MapObject -> Int -> Customer
initial house typ =
  { typ = typ
  , happiness = 2
  , location = AtHome house
  , elapsed = 0
  , timeout = 150
  , frames = [0, 1]
  }


animate : Time -> Customer -> Customer
animate time customer =
  if customer.happiness == 0 then
    animateObject time rotateFrames customer
  else
    customer


rodnam : MapObject -> Random.Generator Customer
rodnam house =
  Random.map (initial house) (Random.int 0 5)


rodnams : List MapObject -> Random.Generator (List Customer)
rodnams houses =
  case houses of
    [] ->
      Random.map (always []) (Random.int 0 0) -- could be Random.succeed []
    house :: rest ->
      Random.map2 (::) (rodnam house) (rodnams rest)


livesHere : MapObject -> Customer -> Bool
livesHere house {location} =
  case location of
    AtHome home -> home == house
    Lost -> False


isLost : Customer -> Bool
isLost {location} = location == Lost


modHappiness : Int -> Customer -> Customer
modHappiness d ({happiness, location} as customer) =
  let
    newHappiness = happiness + d |> min 2
    newLocation = if newHappiness < 0 then Lost else location
  in
    { customer
    | happiness = newHappiness
    , location = newLocation
    }


incHappiness : Customer -> Customer
incHappiness = modHappiness 1


decHappiness : Customer -> Customer
decHappiness = modHappiness -1
