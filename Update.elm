module Update (update) where

import Model exposing (..)
import Actions exposing (..)
import Effects exposing (Effects)
import Time exposing (Time)
import Random
import DeliveryPerson exposing (Location(..))
import Article exposing (State(..), Article)
import Obstacle exposing (Obstacle)
import Request exposing (Request)
import Category exposing (Category)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Init time ->
      ( {model | seed = Random.initialSeed (floor time)}
      , Effects.tick (always Start)
      )
    Start ->
      ({model | state = Playing }, Effects.tick Tick)
    Tick time ->
      if model.state == Playing then
        ( model
          |> Model.animate time animate
          |> Model.timeoutRequests
        , Effects.tick Tick
        )
      else
        ({model | animationState = Nothing}, Effects.none)
    ClickArticle article ->
      (onArticleClick article model, Effects.none)
    ClickCategory category ->
      (onCategoryClick category model, Effects.none)
    ClickWarehouse warehouse ->
      (Model.navigateToWarehouse warehouse model, Effects.none)
    ClickHouse house ->
      (Model.navigateToHouse house model, Effects.none)


animate : Time -> Model -> Model
animate elapsed model =
  model |> animateObstacles elapsed
        |> animateDeliveryPerson elapsed
        |> animateRequests elapsed


animateObstacles : Time -> Model -> Model
animateObstacles elapsed model =
  { model | obstacles = List.map (Obstacle.animate elapsed) model.obstacles }


animateDeliveryPerson : Time -> Model -> Model
animateDeliveryPerson elapsed model =
  { model | deliveryPerson = DeliveryPerson.animate elapsed model.deliveryPerson }


animateRequests : Time -> Model -> Model
animateRequests elapsed model =
 {model | requests = List.map (Request.animate elapsed) model.requests }


onCategoryClick : Category -> Model -> Model
onCategoryClick category model =
  let
    -- find articles from the inventory with the same category
    articles = List.filter
      (\a -> a.category == category && Article.isPicked a)
      model.articles
  in
    case articles of
      article :: _ -> onArticleClick article model
      _ -> model


onArticleClick : Article -> Model -> Model
onArticleClick article model =
  case model.deliveryPerson.location of
    AtHouse house ->
      case article.state of
        AwaitingReturn house' ->
          if house' == house && List.length (List.filter Article.isPicked model.articles) < model.deliveryPerson.capacity then
            { model
            | requests = Request.removeReturns house article model.requests
            , articles = Article.updateState Picked article model.articles
            }
          else
            model
        Picked ->
          if Request.hasOrder house article.category model.requests then
            { model
            | requests = Request.removeOrders house article.category model.requests
            , articles = model.articles
              |> Article.removeDelivered house article.category
              |> Article.updateState (Delivered house) article
            }
          else
            model
        _ ->
          model

    AtWarehouse warehouse' ->
      case article.state of
        InStock warehouse ->
          if warehouse == warehouse' &&
            List.length (List.filter Article.isPicked model.articles) < model.deliveryPerson.capacity then
              {model | articles = Article.updateState Picked article model.articles}
          else
            model
        Picked ->
          if List.length (List.filter (Article.inWarehouse warehouse') model.articles) < warehouse'.capacity then
            {model | articles = Article.updateState (InStock warehouse') article model.articles}
          else
            model
        _ -> model

    _ -> model
