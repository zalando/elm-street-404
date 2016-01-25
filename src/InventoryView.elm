module InventoryView (render) where

import Article exposing (Article)
import Sprite exposing (Sprite)
import Actions exposing (Action)
import Category
import ArticleView
import CategoryView
import Layers exposing (layers)


bubbleSprite : Sprite
bubbleSprite =
  { size = (7, 3)
  , offset = (0, 0)
  , frames = 1
  , src = "inventory-bubble.png"
  }


render : Signal.Address Action -> List Article -> List Sprite.Box
render address articles =
  let
    articlesInDelivery = List.filter Article.isPicked articles
    articlesNumber = List.length articlesInDelivery
    placeholders = List.repeat (4 - articlesNumber) Category.Placeholder

    renderArticle number article =
      ArticleView.render address (toFloat number + 2, 1) article

    renderCategory number category =
      CategoryView.render (toFloat (number + articlesNumber) + 2, 1) [] category
  in
    { sprite = bubbleSprite
    , position = (0, 0)
    , layer = layers.bubble
    , frame = 0
    , attributes = []
    }
    :: List.concat (List.indexedMap renderArticle articlesInDelivery)
    ++ List.concat (List.indexedMap renderCategory placeholders)
