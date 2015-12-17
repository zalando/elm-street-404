module Customer where

import House exposing (House)


type Location
  = AtHome House
  | Lost


type alias Customer =
  { happiness : Int
  , location : Location
  }


initialCustomer : House -> Customer
initialCustomer house =
  { happiness = 1
  , location = AtHome house
  }


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
