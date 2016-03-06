module RequestView (render) where

import CategoryView
import Actions exposing (Action)
import Request exposing (Request)
import Box exposing (Box)
import Category
import Layers exposing (layers)


render : (Float, Float) -> Request -> List Box
render position request =
  if request.blinkHidden then
    []
  else
    case request.category of
      Request.Return article ->
        [ CategoryView.render position Category.Return
        , CategoryView.render position article.category
        , Box.clickable (1, 1) (0, 0) position (layers.clickAbove, 0) (Actions.ClickArticle article)
        ]

      Request.Order category ->
        [ CategoryView.render position category
        , Box.clickable (1, 1) (0, 0) position (layers.clickAbove, 0) (Actions.ClickCategory category)
        ]
