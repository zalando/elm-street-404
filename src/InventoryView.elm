module InventoryView (render) where

import Article exposing (Article)
import Sprite
import Actions exposing (Action)
import Category
import ArticleView
import CategoryView
import Layers exposing (layers)


render : Signal.Address Action -> (Int, Int) -> List Article -> List Sprite.Box
render address (width, height) articles =
  let
    x = toFloat (width - 7) / 2
    y = toFloat height - 3

    articlesInDelivery = List.filter Article.isPicked articles
    articlesNumber = List.length articlesInDelivery
    placeholders = List.repeat (4 - articlesNumber) Category.Placeholder

    renderArticle number article =
      ArticleView.render address (toFloat number + x + 2, y + 1) article

    renderCategory number category =
      CategoryView.render (toFloat (number + articlesNumber) + x + 2, y + 1) Nothing category
  in
    ( Sprite.box
        Sprite.InventoryBubble
        (x, y)
        0
        (layers.bubble, 0)
    )
    :: List.concat (List.indexedMap renderArticle articlesInDelivery)
    ++ List.concat (List.indexedMap renderCategory placeholders)
