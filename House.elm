module House (House, house) where


type alias House =
  { position : (Float, Float)
  , size : (Float, Float)
  }


house : (Float, Float) -> House
house position =
  { position = position
  , size = (3, 2)
  }
