module MapObject (MapObject) where


type alias MapObject a =
  { a
  | position : (Float, Float)
  , size : (Float, Float)
  }
