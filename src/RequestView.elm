module RequestView (render) where

import CategoryView
import Actions exposing (Action)
import Request exposing (Request)
import Box exposing (Box)
import Category
import Layers exposing (layers)


render : (Float, Float) -> Request -> List Box
render position request =
  let
    renderClickable = Box.clickable (1, 1) (0, 0) position (layers.clickAbove, 0)
  in
    case request.category of
      Request.Return article ->
        renderClickable (Actions.ClickArticle article) ::
        if request.blinkHidden then
          []
        else
          [ CategoryView.render position Category.Return
          , CategoryView.render position article.category
          ]

      Request.Order category ->
        renderClickable (Actions.ClickCategory category) ::
        if request.blinkHidden then
          []
        else
          [CategoryView.render position category]
