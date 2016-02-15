module Warehouse (Warehouse, warehouse) where

import MapObject exposing (MapObject)


type alias Warehouse = MapObject { capacity : Int }


warehouse : Warehouse
warehouse =
  { position = (0, 0)
  , size = (4, 3)
  , capacity = 6
  }
