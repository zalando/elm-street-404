module InventoryView (render) where

import Article exposing (Article)
import Box exposing (Box)
import Textures
import Category
import CategoryView
import Layers exposing (layers)
import Actions


render : (Int, Int) -> List Article -> List Box
render (width, height) articles =
  let
    x = toFloat (width - 7) / 2
    y = toFloat height - 3

    articlesInDelivery = List.filter Article.isPicked articles
    articlesNumber = List.length articlesInDelivery
    placeholders = List.repeat (4 - articlesNumber) Category.Placeholder

    renderArticle number article =
      let
        pos = (toFloat number + x + 2, y + 1)
      in
        [ CategoryView.render pos article.category
        , Box.clickable (1, 1) (0, 0) pos (layers.clickAbove, 0) (Actions.ClickArticle article)
        ]

    renderCategory number =
      CategoryView.render (toFloat (number + articlesNumber) + x + 2, y + 1)

  in
    Box.textured Textures.InventoryBubble (x, y) 0 (layers.bubble, 0)
    :: List.concat (List.indexedMap renderArticle articlesInDelivery)
    ++ List.indexedMap renderCategory placeholders
