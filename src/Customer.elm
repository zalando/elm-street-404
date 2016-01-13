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


initial : Int -> House -> Customer
initial typ house =
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


rodnam : House -> Random.Seed -> (Customer, Random.Seed)
rodnam house seed =
  let
    (typ, seed') = Random.generate (Random.int 0 5) seed
  in
    (initial typ house, seed')


rodnams : List House -> Random.Seed -> (List Customer, Random.Seed)
rodnams houses seed =
  case houses of
    [] -> ([], seed)
    house :: otherHouses ->
      let
        (customer, seed') = rodnam house seed
        (otherCustomers, seed'') = rodnams otherHouses seed'
      in
        (customer :: otherCustomers, seed'')


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
