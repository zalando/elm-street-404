module DeliveryPersonView (render) where

import DeliveryPerson exposing (DeliveryPerson)
import Box exposing (Box)
import Textures
import Basics exposing (atan2)
import Layers exposing (layers)


calculateDirection : (Float, Float) -> Int
calculateDirection (x, y) =
  round (2 + atan2 y x * 4 / pi) % 8


direction : DeliveryPerson -> Int
direction {route, position} =
  case route of
    (x, y) :: rest -> calculateDirection
      ( toFloat x - fst position
      , toFloat y - snd position
      )
    [] -> 0


render : Int -> DeliveryPerson -> Box
render numberOfBoxes deliveryPerson =
  let
    box frame =
      Box.textured
        Textures.DeliveryPerson
        deliveryPerson.position
        frame
        (layers.obstacle, 0)
  in
    case deliveryPerson.location of
      DeliveryPerson.OnTheWayTo _ ->
        box (direction deliveryPerson * 3 + Maybe.withDefault 0 (List.head deliveryPerson.frames))
      _ ->
        box (24 + numberOfBoxes)
