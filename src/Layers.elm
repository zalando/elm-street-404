module Layers (layers) where

type alias Layers =
  { shadow : Int
  , obstacle : Int
  , bubble : Int
  , article : Int
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
  , clickAbove = 5
  , clickToStart = 6
  , clickToStartAbove = 7
  }
