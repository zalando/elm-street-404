module House (House, house) where

import MapObject exposing (MapObject)


type alias House = MapObject { capacity : Int }


house : House
house =
  { position = (0, 0)
  , size = (2, 2)
  , capacity = 3
  }
