module Customer (Customer, initial, animate, livesHere, decHappiness, incHappiness, isLost, rodnams) where

import House exposing (House)
import Random
import Time exposing (Time)
import AnimationState exposing (animateObject, rotateFrames)


type Location
  = AtHome House
  | Lost


type alias Customer =
  { typ : Int
  , location : Location
  , happiness : Int
  , elapsed: Time
  , frames : List (Int)
  }


initial : House -> Int -> Customer
initial house typ =
  { typ = typ
  , happiness = 2
  , location = AtHome house
  , elapsed = 0
  , frames = [0, 1]
  }


animate : Time -> Customer -> Customer
animate time customer =
  let updateCustomer customer =
    {customer | frames = rotateFrames customer.frames}
  in
    case customer.happiness of
      0 -> animateObject 150 time updateCustomer customer
      _ -> customer


rodnam : House -> Random.Generator Customer
rodnam house =
  Random.map (initial house) (Random.int 0 5)


rodnams : List House -> Random.Generator (List Customer)
rodnams houses =
  case houses of
    [] ->
      Random.map (always []) (Random.int 0 0) -- could be Random.succeed []
    house :: rest ->
      Random.map2 (::) (rodnam house) (rodnams rest)


livesHere : House -> Customer -> Bool
livesHere house customer =
  case customer.location of
    AtHome home -> home == house
    Lost -> False


isLost : Customer -> Bool
isLost customer =
  case customer.location of
    AtHome _ -> False
    Lost -> True


modHappiness : Int -> Customer -> Customer
modHappiness d customer =
  let
    happiness = customer.happiness + d
  in
    { customer
    | happiness = if happiness > 2 then 2 else happiness
    , location =
      if happiness < 0 then
        Lost
      else
        customer.location
    }


incHappiness : Customer -> Customer
incHappiness = modHappiness 1


decHappiness : Customer -> Customer
decHappiness = modHappiness -1
