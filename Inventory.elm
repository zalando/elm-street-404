module Inventory (render) where
import Article exposing (Article)
import Sprite exposing (Sprite)
import Actions exposing (Action)
import Category
import CategoryView
import Layers exposing (layers)


bubbleSprite : Sprite
bubbleSprite =
  { size = (7, 3)
  , offset = (0, 0)
  , frames = 1
  , src = "img/inventory-bubble.png"
  }


render : Signal.Address Action -> List Article -> List Sprite.Box
render address articles =
  let
    articlesInDelivery = List.filter Article.inDelivery articles
    placeholders = List.repeat (4 - List.length  articlesInDelivery) Category.Placeholder
    renderCategory number category =
      CategoryView.render (toFloat number + 2, 1) [] category
  in
    { sprite = bubbleSprite
    , position = (0, 0)
    , layer = layers.bubble
    , frame = 0
    , attributes = []
    } :: List.indexedMap renderCategory (List.map .category articlesInDelivery ++ placeholders)
