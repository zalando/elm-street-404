module Layers exposing (layers)


type alias Layers =
  { shadow : Float
  , obstacle : Float
  , bubble : Float
  , article : Float
  , click : Float
  , clickAbove : Float
  , clickToStart : Float
  , clickToStartAbove : Float
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
