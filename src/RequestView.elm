module RequestView (render) where

import CategoryView
import Actions exposing (Action)
import Request exposing (Request)
import Box exposing (Box)
import Category


render : (Float, Float) -> Request -> List Box
render position request =
  if request.blinkHidden then
    []
  else
    case request.category of
      Request.Return article ->
        CategoryView.render position Nothing Category.Return ++
        CategoryView.render
          position
          (Just (Actions.ClickArticle article))
          article.category

      Request.Order category ->
        CategoryView.render
          position
          (Just (Actions.ClickCategory category))
          category
