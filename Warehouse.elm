module Warehouse (Warehouse, warehouse) where

type alias Warehouse =
  { position : (Float, Float)
  , size : (Float, Float)
  , capacity : Int
  }


warehouse : (Float, Float) -> Warehouse
warehouse position =
  { position = position
  , size = (3, 3)
  , capacity = 6
  }
