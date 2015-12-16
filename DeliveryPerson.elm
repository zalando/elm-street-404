module DeliveryPerson (DeliveryPerson, Location(..), initial, render) where
import House exposing (House)
import Warehouse exposing (Warehouse)
import Sprite exposing (Sprite)


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
  }


initial : (Int, Int) -> DeliveryPerson
initial position =
  { location = OnTheWay
  , position = (toFloat (fst position), toFloat (snd position))
  , route = []
  }


-- returns 0 to 7
calculateDirection : (Float, Float) -> (Float, Float) -> Int
calculateDirection destination source =
  0


direction : DeliveryPerson -> Int
direction deliveryPerson =
  case deliveryPerson.route of
    first :: rest -> calculateDirection (toFloat (fst first), toFloat (snd first)) deliveryPerson.position
    _ -> 0



render : DeliveryPerson -> List Sprite.Box
render deliveryPerson =
  case deliveryPerson.location of
    OnTheWay ->
      [ { sprite = onTheWaySprite
        , position = ( floor (fst deliveryPerson.position)
                     , floor (snd deliveryPerson.position)
                     )
        , layer = 2
        , frame = (direction deliveryPerson) * 3
        , attributes = []
        }
      ]
    _ -> []
