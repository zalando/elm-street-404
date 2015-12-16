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
import Debug


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
        (Model.animate time animate model, Effects.tick Tick)
      else
        ({model | animationState = Nothing}, Effects.none)
    ClickArticle article ->
      (onArticleClick (Debug.log "ClickArticle" article) model, Effects.none)
    ClickRequest request ->
      (onRequestClick (Debug.log "ClickRequest" request) model, Effects.none)
    ClickWarehouse warehouse ->
      (Model.navigateToWarehouse warehouse model, Effects.none)
    ClickHouse house ->
      (Model.navigateToHouse house model, Effects.none)


animate : Time -> Model -> Model
animate elapsed model =
  model |> animateObstacles elapsed
        |> animateDeliveryPerson elapsed


animateObstacles : Time -> Model -> Model
animateObstacles elapsed model =
  { model | obstacles = List.map (Obstacle.animate elapsed) model.obstacles }


animateDeliveryPerson : Time -> Model -> Model
animateDeliveryPerson elapsed model =
  { model | deliveryPerson = DeliveryPerson.animate elapsed model.deliveryPerson }


onRequestClick : Request -> Model -> Model
onRequestClick request model =
  case request of
    Request.OrderRequest house category _ ->
      let
        -- find articles from the inventory with the same category
        articles = List.filter (\a -> a.category == category && Article.inDelivery a) model.articles
      in
        case articles of
          article :: _ -> onArticleClick article model
          _ -> model
    Request.ReturnRequest house article _ ->
      onArticleClick article model


onArticleClick : Article -> Model -> Model
onArticleClick article model =
  case model.deliveryPerson.location of
    AtHouse house ->
      case article.state of
        AwaitingReturn house' ->
          if house' == house then
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
            , articles = Article.removeDelivered house article.category model.articles
                         |> Article.updateState (Delivered house) article
            }
          else
            model
        _ ->
          model

    AtWarehouse warehouse' ->
      case article.state of
        InStock warehouse ->
          if warehouse == warehouse' then
            {model | articles = Article.updateState Picked article model.articles}
          else
            model
        Picked ->
          if List.length (List.filter (Article.inWarehouse warehouse') model.articles) < 6 then
            {model | articles = Article.updateState (InStock warehouse') article model.articles}
          else
            model
        _ -> model

    _ -> model
