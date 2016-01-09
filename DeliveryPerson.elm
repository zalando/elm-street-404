module DeliveryPerson (DeliveryPerson, Location(..), initial, animate, navigateTo) where

import House exposing (House)
import Warehouse exposing (Warehouse)
import Time exposing (Time)
import AnimationState exposing (animateObject, rotateFrames)
import Astar


type Location
  = AtHouse House
  | AtWarehouse Warehouse
  | OnTheWayToHouse House
  | OnTheWayToWarehouse Warehouse
  | Initial


type alias DeliveryPerson =
  { location : Location
  , position : (Float, Float)
  , route : List (Int, Int)
  , elapsed: Time
  , frames : List (Int)
  , capacity : Int
  }


pushThePedals : Time -> DeliveryPerson -> DeliveryPerson
pushThePedals time deliveryPerson =
  let
    updateDeliveryPerson deliveryPerson =
      {deliveryPerson | frames = rotateFrames deliveryPerson.frames}
  in
    case deliveryPerson.location of
      OnTheWayToHouse _ -> animateObject 96 time updateDeliveryPerson deliveryPerson
      OnTheWayToWarehouse _ -> animateObject 96 time updateDeliveryPerson deliveryPerson
      _ -> deliveryPerson


currentDestination : DeliveryPerson -> Maybe (Float, Float)
currentDestination deliveryPerson =
  case deliveryPerson.route of
    [] -> Nothing
    first :: _ -> Just (toFloat (fst first), toFloat (snd first))


absValue : (Float, Float) -> Float
absValue v = sqrt ((fst v) ^ 2 + (snd v) ^ 2)


diff : (Float, Float) -> (Float, Float) -> (Float, Float)
diff a b = ((fst a) - (fst b), (snd a) - (snd b))


add : (Float, Float) -> (Float, Float) -> (Float, Float)
add a b = ((fst a) + (fst b), (snd a) + (snd b))


scale : Float -> (Float, Float) -> (Float, Float)
scale a b = (a * (fst b), a * (snd b))


speed : Float
speed = 0.036


nextLocation : List (Int, Int) -> Location -> Location
nextLocation route location =
  case route of
    [] ->
      case location of
        OnTheWayToHouse house -> AtHouse house
        OnTheWayToWarehouse warehouse -> AtWarehouse warehouse
        _ -> location
    _ -> location


moveToNext : Time -> (Float, Float) -> DeliveryPerson -> DeliveryPerson
moveToNext time dest deliveryPerson =
  let
    maxDelta = diff dest deliveryPerson.position
    absMax = absValue maxDelta
    dvect = scale (1 / absMax) maxDelta
    speedDelta = scale speed dvect
    absSpeed = absValue speedDelta
    actualDelta = if absSpeed > absMax then maxDelta else speedDelta
    remainderTime = if absSpeed > absMax then time - time * absMax / absSpeed else 0
    nextPosition = add deliveryPerson.position actualDelta
    nextRoute =
      if absSpeed >= absMax then
        List.drop 1 deliveryPerson.route
      else
        deliveryPerson.route
    location = nextLocation nextRoute deliveryPerson.location
    updatedPerson =
      { deliveryPerson
      | position = nextPosition
      , location = location
      , route = nextRoute
      }
  in
    if remainderTime > 0 then
      moveOnPath remainderTime updatedPerson
    else
      updatedPerson


stayThere : DeliveryPerson -> DeliveryPerson
stayThere deliveryPerson =
  { deliveryPerson
  | location = nextLocation [] deliveryPerson.location
  , route = []
  }


moveOnPath : Time -> DeliveryPerson -> DeliveryPerson
moveOnPath time deliveryPerson =
  case currentDestination deliveryPerson of
    Nothing -> stayThere deliveryPerson
    Just d -> moveToNext time d deliveryPerson


animate: Time -> DeliveryPerson -> DeliveryPerson
animate time deliveryPerson = pushThePedals time deliveryPerson |> moveOnPath time


initial : (Int, Int) -> DeliveryPerson
initial position =
  { location = Initial
  , position = (toFloat (fst position), toFloat (snd position))
  , route = []
  , elapsed = 0
  , frames = [0, 1, 2]
  , capacity = 4
  }


navigationStart : DeliveryPerson -> (Int, Int)
navigationStart deliveryPerson =
  Maybe.withDefault
    ( round (fst deliveryPerson.position)
    , round (snd deliveryPerson.position)
    )
    (List.head deliveryPerson.route)


appendPath : List (Int, Int) -> List (Int, Int) -> List (Int, Int)
appendPath current new =
  case current of
    [] -> new
    first :: rest -> first :: new


navigateTo : (Int, Int) -> List (Int, Int) -> Location -> (Int, Int) -> DeliveryPerson -> DeliveryPerson
navigateTo gridSize obstacles location destination deliveryPerson =
  { deliveryPerson
  | location = location
  , route = appendPath
      deliveryPerson.route
      (Astar.findPath gridSize obstacles (navigationStart deliveryPerson) destination)
  }
