module MapObject (MapObject) where

import Random


type alias MapObject a =
  { a
  | position : (Float, Float)
  , size : (Float, Float)
  }


type alias Box = MapObject {}


splitBy : Box -> Box -> List Box
splitBy box1 box2 =
  let
    (x1, y1) = box1.position
    (w1, h1) = box1.size
    (x2, y2) = box2.position
    (w2, h2) = box2.size
  in
    List.filter
      (\{size} -> fst size > 0 && snd size > 0)
      [ {position = (x2, y2), size = (x1 - x2, w1 + y1 - y2)}
      , {position = (x1, y2), size = (x2 + w2 - x1, y1 - y2)}
      , {position = (x1 + w1, y1), size = (x2 + w2 - (x1 + w1), h1 + y2 - y1)}
      , {position = (x2, y1 + h1), size = (x1 + w1 - x2, y2 + h2 - (y1 + h1))}
      ]


fitRandom : Box -> (Float, Float) -> Random.Generator Box
fitRandom ({size, position} as box) (w, h) =
  Random.map
    (\(x, y) -> {size = (w, h), position = (toFloat x, toFloat y)})
    ( Random.pair
        (Random.int (floor (fst position)) (floor (fst position + fst size - w)))
        (Random.int (floor (snd position)) (floor (snd position + snd size - h)))
    )


filterSize : (Float, Float) -> List Box -> List Box
filterSize (w, h) =
  List.filter (\{size} -> fst size >= w && snd size >= h)


sortBySize : List Box -> List Box
sortBySize =
  List.sortBy (\{size} -> fst size * snd size) >> List.reverse
