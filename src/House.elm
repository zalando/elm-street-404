module House (House, house) where

import MapObject exposing (MapObject)


type alias House = MapObject { capacity : Int }


house : (Float, Float) -> House
house position =
  { position = position
  , size = (2, 2)
  , capacity = 3
  }
