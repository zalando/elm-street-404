module Update (update) where
import Model exposing (..)
import Actions exposing (..)
import Effects exposing (Effects)
import Time exposing (Time)
import Random
import DeliveryPerson exposing (Location(..))
import Article exposing (State(..), Article)
import Request

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
    Click article ->
        (onArticleClick model article, Effects.none)


animate : Time -> Model -> Model
animate elapsed model =
  { model | fountain = animateFountain elapsed model.fountain }


onArticleClick : Model -> Article -> Model
onArticleClick model article =
  case model.deliveryPerson.location of
    AtHouse house ->
      case article.state of
        AwaitingReturn house' ->
          if house' == house then
            { model | requests = Request.removeReturns house article model.requests
                    , articles = Article.updateState Picked article model.articles
            }
          else
            model
        Picked ->
          if Request.hasOrder house article.category model.requests then
            { model | requests = Request.removeOrders house article.category model.requests
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
          if List.length (Article.filterInWarehouse warehouse' model.articles) < 6 then
            {model | articles = Article.updateState (InStock warehouse') article model.articles}
          else
            model
        _ -> model

    OnTheWay -> model
