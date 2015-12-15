module DeliveryPerson (DeliveryPerson, Location(..), initial) where
import House exposing (House)
import Warehouse exposing (Warehouse)


type Location
  = AtHouse House
  | AtWarehouse Warehouse
  | OnTheWay


type alias DeliveryPerson =
  { location : Location
  , position : (Float, Float)
  }

initial : (Int, Int) -> DeliveryPerson
initial position =
  { location = OnTheWay
  , position = (toFloat (fst position), toFloat (snd position))
  }
