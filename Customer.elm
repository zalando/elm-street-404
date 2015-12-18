module Customer (Customer, initial, livesHere, decHappiness, isLost, rodnams) where

import House exposing (House)
import Random


type Location
  = AtHome House
  | Lost


type alias Customer =
  { typ : Int
  , location : Location
  , happiness : Int
  }


initial : Int -> House -> Customer
initial typ house =
  { typ = typ
  , happiness = 1
  , location = AtHome house
  }


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
  { customer
  | happiness = customer.happiness + d
  }


incHappiness : Customer -> Customer
incHappiness = modHappiness 1


decHappiness : Customer -> Customer
decHappiness = modHappiness -1


isTooUnhappy : Customer -> Bool
isTooUnhappy customer = customer.happiness < 0
