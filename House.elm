module House (House, house) where


type alias House =
  { position : (Float, Float)
  , size : (Float, Float)
  , happiness : Int
  }


house : (Float, Float) -> House
house position =
  { position = position
  , size = (3, 2)
  , happiness = 1
  }
