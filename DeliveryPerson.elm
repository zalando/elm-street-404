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
  | OnTheWay


type alias DeliveryPerson =
  { location : Location
  , position : (Float, Float)
  , route : List (Int, Int)
  , elapsed: Time
  , frames : List (Int)
  }


animate: Time -> DeliveryPerson -> DeliveryPerson
animate time deliveryPerson =
  let
    updateDeliveryPerson deliveryPerson =
      {deliveryPerson | frames = rotateFrames deliveryPerson.frames}
  in
    case deliveryPerson.location of
      OnTheWay -> animateObject 250 time updateDeliveryPerson deliveryPerson
      _ -> deliveryPerson

initial : (Int, Int) -> DeliveryPerson
initial position =
  { location = OnTheWay
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
  case deliveryPerson.location of
    OnTheWay ->
      [ { sprite = onTheWaySprite
        , position = deliveryPerson.position
        , layer = layers.deliveryPerson
        , frame =  direction deliveryPerson * 3 + Maybe.withDefault 0 (head deliveryPerson.frames)
        , attributes = []
        }
      ]
    _ -> []


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


navigateTo : (Int, Int) -> List (Int, Int) -> (Int, Int) -> DeliveryPerson -> DeliveryPerson
navigateTo gridSize obstacles destination deliveryPerson =
  { deliveryPerson
  | route = appendPath
      deliveryPerson.route
      (findPath gridSize obstacles destination deliveryPerson)
  }
