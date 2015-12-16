module Warehouse (Warehouse, warehouse) where


type alias Warehouse =
  { position : (Float, Float)
  , size : (Float, Float)
  }


warehouse : (Float, Float) -> Warehouse
warehouse position =
  { position = position
  , size = (3, 3)
  }
