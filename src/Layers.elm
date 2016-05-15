module Layers exposing (layers)


type alias Layers =
  { shadow : Int
  , obstacle : Int
  , bubble : Int
  , article : Int
  , click : Int
  , clickAbove : Int
  , clickToStart : Int
  , clickToStartAbove : Int
  }


layers : Layers
layers =
  { shadow = 1
  , obstacle = 2
  , bubble = 3
  , article = 4
  , click = 5
  , clickAbove = 6
  , clickToStart = 7
  , clickToStartAbove = 8
  }
