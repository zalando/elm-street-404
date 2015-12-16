module Inventory (render) where
import Article exposing (Article)
import Sprite exposing (Sprite)
import Actions exposing (Action)
import Category
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
    renderArticle number article =
      Category.render (toFloat number + 2, 1) article.category
  in
    { sprite = bubbleSprite
    , position = (0, 0)
    , layer = layers.bubble
    , frame = 0
    , attributes = []
    } :: List.indexedMap renderArticle (List.filter Article.inDelivery articles)
