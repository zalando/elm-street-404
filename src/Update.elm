module Update (update) where

import Model exposing (..)
import Actions exposing (..)
import Effects exposing (Effects)
import Time exposing (Time)
import Random
import DeliveryPerson exposing (Location(..))
import Article exposing (State(..), Article)
import MapObject exposing (MapObject, MapObjectCategory(..))
import Request exposing (Request)
import Category exposing (Category)
import Customer exposing (Customer)
import IHopeItWorks
import ImageLoad
import Json.Decode as Decoder
import Task exposing (Task)
import AnimationState exposing (animateGenerator)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Dimensions dimensions ->
      if model.state == Playing then
        ({model | dimensions = dimensions}, Effects.none)
      else
        ( {model | dimensions = dimensions}
          |> Model.resize
          |> Model.cleanupModel
        , Effects.none
        )
    Init time ->
      ( {model | seed = Random.initialSeed (floor time)}
      , (loadImage model.imagesUrl) "score.png"
      )
    ImageLoaded image ->
      if image == "score.png" then
        ({model | state = Loading}, Effects.batch (List.map (loadImage model.imagesUrl) model.images))
      else
        let
          newModel = {model | images = List.filter ((/=) image) model.images}
        in
          if List.length newModel.images == 0 then
            ({newModel | state = Stopped}, Effects.none)
          else
            (newModel, Effects.none)
    Start ->
      (Model.start model, Effects.tick Tick)
    Tick time ->
      if model.state == Playing then
        ( model
          |> Model.animate time animate
          |> Model.timeoutRequests
          |> Model.cleanupLostArticles
          |> Model.cleanupLostRequests
          |> Model.updateCustomers
          |> Model.updateGameState
        , Effects.tick Tick
        )
      else
        ({model | animationState = Nothing}, Effects.none)
    ClickArticle article ->
      ifPlaying (onArticleClick article) model
    ClickCategory category ->
      ifPlaying (onCategoryClick category) model
    ClickMapObject mapObject ->
      ifPlaying (Model.navigateToMapObject mapObject) model


ifPlaying : (Model -> Model) -> Model -> (Model, Effects Action)
ifPlaying fun model =
  if model.state == Playing then
    (fun model, Effects.none)
  else
    (model, Effects.none)


loadImage : String -> String -> Effects Action
loadImage imagesUrl image =
  ImageLoad.load (imagesUrl ++ "/" ++ image) (Decoder.succeed image) `Task.onError` always (Task.succeed image)
  |> Task.map (always (ImageLoaded image))
  |> Effects.task


animate : Time -> Model -> Model
animate elapsed model =
  model |> animateMapObjects elapsed
        |> animateDeliveryPerson elapsed
        |> animateRequests elapsed
        |> animateGenerators elapsed
        |> animateCustomers elapsed


animateGenerators : Time -> Model -> Model
animateGenerators elapsed model =
  { model
  | orderGenerator = animateGenerator elapsed model.orderGenerator
  , returnGenerator = animateGenerator elapsed model.returnGenerator
  , articleGenerator = animateGenerator elapsed model.articleGenerator
  }
  |> dispatchArticles
  |> dispatchOrders
  |> dispatchReturns


dispatchArticles : Model -> Model
dispatchArticles model =
  if model.articleGenerator.active then
    Model.dispatchArticles 1 model
  else
    model


dispatchOrders : Model -> Model
dispatchOrders model =
  if model.orderGenerator.active then
    Model.dispatchOrders 1 model
  else
    model


dispatchReturns : Model -> Model
dispatchReturns model =
  if model.returnGenerator.active then
    Model.dispatchReturns 1 model
  else
    model


animateMapObjects : Time -> Model -> Model
animateMapObjects elapsed model =
  { model | mapObjects = List.map (MapObject.animate elapsed) model.mapObjects }


animateDeliveryPerson : Time -> Model -> Model
animateDeliveryPerson elapsed model =
  { model | deliveryPerson = DeliveryPerson.animate elapsed model.deliveryPerson }


animateRequests : Time -> Model -> Model
animateRequests elapsed model =
 {model | requests = List.map (Request.animate elapsed) model.requests }


animateCustomers : Time -> Model -> Model
animateCustomers elapsed model =
 {model | customers = List.map (Customer.animate elapsed) model.customers }


-- click the 1st picked article that has the same category
onCategoryClick : Category -> Model -> Model
onCategoryClick category model =
  let
    isPickedCategory a =
      a.category == category && Article.isPicked a
  in
    case IHopeItWorks.find isPickedCategory model.articles of
      Just article -> onArticleClick article model
      _ -> model


onArticleClick : Article -> Model -> Model
onArticleClick article model =
  case model.deliveryPerson.location of
    At mapObject ->
      case mapObject.category of
        HouseCategory _ ->
          case article.state of
            AwaitingReturn house ->
              Model.pickupReturn mapObject house article model
            Picked ->
              Model.deliverArticle mapObject article model
            _ ->
              model
        WarehouseCategory _ ->
          case article.state of
            InStock warehouse ->
              Model.pickupArticle warehouse mapObject article model
            Picked ->
              Model.returnArticle mapObject article model
            _ -> model
        _ -> model
    _ -> model
