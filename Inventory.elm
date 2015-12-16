module Inventory (render) where
import Article exposing (Article)
import Sprite exposing (Sprite)
import Actions exposing (Action)

render :  Signal.Address Action -> List Article -> List Sprite.Box
render address articles =
  []
