module Warehouse (Warehouse, warehouse) where

import MapObject exposing (MapObject)


type alias Warehouse = MapObject { capacity : Int }


warehouse : (Float, Float) -> Warehouse
warehouse position =
  { position = position
  , size = (3, 3)
  , capacity = 6
  }
