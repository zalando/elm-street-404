module Layers (layers) where

type alias Layers =
  { shadow : Int
  , grid : Int
  , obstacle : Int
  , route : Int
  , customer : Int
  , deliveryPerson : Int
  , fountainSpring : Int
  , bubble : Int
  , article : Int
  , articleReturn : Int
  , clickAbove : Int
  , clickToStart : Int
  }

layers : Layers
layers =
  { shadow = 1
  , grid = 2
  , obstacle = 3
  , route = 3
  , customer = 3
  , deliveryPerson = 3
  , fountainSpring = 6
  , bubble = 6
  , article = 8
  , articleReturn = 9
  , clickAbove = 7
  , clickToStart = 10
  }
