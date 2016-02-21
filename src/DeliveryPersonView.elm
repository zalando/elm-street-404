module DeliveryPersonView (render) where

import DeliveryPerson exposing (DeliveryPerson)
import Sprite exposing (Sprite)
import Basics exposing (atan2)
import Layers exposing (layers)


sprite : Sprite
sprite =
  { size = (2, 3)
  , offset = (0, -1)
  , frames = 29
  , src = "delivery-person.png"
  }


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


render : Int -> DeliveryPerson -> List Sprite.Box
render numberOfBoxes deliveryPerson =
  let
    box frame =
    [ { sprite = sprite
      , position = deliveryPerson.position
      , layer = layers.deliveryPerson
      , frame = frame
      , attributes = []
      }
    ]
  in
    case deliveryPerson.location of
      DeliveryPerson.OnTheWayTo _ ->
        box (direction deliveryPerson * 3 + Maybe.withDefault 0 (List.head deliveryPerson.frames))
      _ ->
        box (24 + numberOfBoxes)
