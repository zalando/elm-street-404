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


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Dimensions dimensions ->
      (Model.resize dimensions model, Effects.none)
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
  { model
  | mapObjects = List.map (MapObject.animate elapsed) model.mapObjects
  , deliveryPerson = DeliveryPerson.animate elapsed model.deliveryPerson
  , requests = List.map (Request.animate elapsed) model.requests
  , customers = List.map (Customer.animate elapsed) model.customers
  }
  |> Model.dispatch elapsed


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
      case (mapObject.category, article.state) of
        (HouseCategory _, AwaitingReturn house) ->
          Model.pickupReturn mapObject house article model
        (HouseCategory _, Picked) ->
          Model.deliverArticle mapObject article model
        (WarehouseCategory _, InStock warehouse) ->
          Model.pickupArticle warehouse mapObject article model
        (WarehouseCategory _, Picked) ->
          Model.returnArticle mapObject article model
        _ -> model
    _ -> model
