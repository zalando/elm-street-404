module RequestView (render) where

import CategoryView
import Actions exposing (Action)
import Html.Events exposing (onClick)
import Request exposing (Request)
import Sprite
import Category


render : Signal.Address Action -> (Float, Float) -> Request -> List Sprite.Box
render address position request =
  case request.category of
    Request.Return article ->
      CategoryView.render
        position
        []
        (if request.blinkHidden then Category.Empty else article.category)
      ++
      CategoryView.render
        position
        [onClick address (Actions.ClickArticle article)]
        (if request.blinkHidden then Category.Empty else Category.Return)

    Request.Order category ->
      CategoryView.render
        position
        [onClick address (Actions.ClickCategory category)]
        (if request.blinkHidden then Category.Empty else category)
