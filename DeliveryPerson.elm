module DeliveryPerson (DeliveryPerson, Location(..), initial, render, animate, navigateTo) where

import House exposing (House)
import Warehouse exposing (Warehouse)
import Sprite exposing (Sprite)
import Basics exposing (atan2)
import Time exposing (Time)
import AnimationState exposing (animateObject, rotateFrames)
import List exposing (head)
import Pathfinder
import Layers exposing (layers)


onTheWaySprite : Sprite
onTheWaySprite =
  { size = (2, 3)
  , offset = (0, -1)
  , frames = 24
  , src = "img/delivery-person.png"
  }


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
  }


pushThePedals : Time -> DeliveryPerson -> DeliveryPerson
pushThePedals time deliveryPerson =
  let
    updateDeliveryPerson deliveryPerson =
      {deliveryPerson | frames = rotateFrames deliveryPerson.frames}
  in
    case deliveryPerson.location of
      OnTheWayToHouse _ -> animateObject 125 time updateDeliveryPerson deliveryPerson
      OnTheWayToWarehouse _ -> animateObject 125 time updateDeliveryPerson deliveryPerson
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


clipFirst : List a -> List a
clipFirst list =
  case list of
    [] -> []
    _ :: rest -> rest


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
    nextRoute = if absSpeed >= absMax then clipFirst deliveryPerson.route else deliveryPerson.route
    updatedPerson =
      { deliveryPerson
      | position = nextPosition
      , location = nextLocation nextRoute deliveryPerson.location
      , route = nextRoute
      }
  in
    if remainderTime > 0 then
      moveOnPath remainderTime updatedPerson
    else
      updatedPerson


moveOnPath : Time -> DeliveryPerson -> DeliveryPerson
moveOnPath time deliveryPerson =
  case currentDestination deliveryPerson of
    Nothing -> deliveryPerson
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
  }


calculateDirection : (Float, Float) -> Int
calculateDirection (x, y) =
  (2 + round (atan2 y x * 4 / pi)) % 8


direction : DeliveryPerson -> Int
direction deliveryPerson =
  case deliveryPerson.route of
    first :: rest -> calculateDirection
      ( toFloat (fst first) - fst deliveryPerson.position
      , toFloat (snd first) - snd deliveryPerson.position
      )
    _ -> 0


render : DeliveryPerson -> List Sprite.Box
render deliveryPerson =
  let
    box =
    [ { sprite = onTheWaySprite
      , position = deliveryPerson.position
      , layer = layers.deliveryPerson
      , frame = direction deliveryPerson * 3 + Maybe.withDefault 0 (head deliveryPerson.frames)
      , attributes = []
      }
    ]
  in
    case deliveryPerson.location of
      OnTheWayToHouse _ -> box
      OnTheWayToWarehouse _ -> box
      _ -> box


navigationStart : DeliveryPerson -> (Int, Int)
navigationStart deliveryPerson =
  Maybe.withDefault
    ( round (fst deliveryPerson.position)
    , round (snd deliveryPerson.position)
    )
    (List.head deliveryPerson.route)


findPath : (Int, Int) -> List (Int, Int) -> (Int, Int) -> DeliveryPerson -> List (Int, Int)
findPath gridSize obstacles destination deliveryPerson =
  Pathfinder.find gridSize obstacles (navigationStart deliveryPerson) destination


appendPath : List (Int, Int) -> List (Int, Int) -> List (Int, Int)
appendPath current new =
  case current of
    [] -> new
    first :: rest -> first :: new


navigateTo : Location -> (Int, Int) -> List (Int, Int) -> (Int, Int) -> DeliveryPerson -> DeliveryPerson
navigateTo location gridSize obstacles destination deliveryPerson =
  { deliveryPerson
  | location = location
  , route = appendPath
      deliveryPerson.route
      (findPath gridSize obstacles destination deliveryPerson)
  }
