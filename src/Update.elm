module Update exposing (update, loadImage)

import Model exposing (..)
import Actions exposing (..)
import Effects exposing (Effects)
import DeliveryPerson exposing (Location(..))
import Article exposing (State(..), Article)
import MapObject exposing (MapObject, MapObjectCategory(..))
import Category exposing (Category)
import IHopeItWorks
import Task exposing (Task)
import WebGL
import Textures exposing (TextureId)
import AllDict


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Dimensions dimensions ->
      (Model.resize dimensions model |> Model.render, Effects.none)
    TextureLoaded textureId texture ->
      let
        loadTexture =
          case AllDict.get textureId model.textures of
            Just data ->
              AllDict.insert textureId {data | texture = texture} model.textures
            Nothing ->
              model.textures
        newModel = {model | textures = loadTexture}
        texturesToLoad = Textures.loadTextures newModel.textures
      in
        if textureId == Textures.Score then
          ( {newModel | state = Loading}
          , Effects.batch (List.map (loadImage model.imagesUrl) texturesToLoad)
          )
        else
          if List.length texturesToLoad == 0 then
            ({newModel | state = Stopped} |> Model.render, Effects.none)
          else
            (Model.render newModel, Effects.none)
    Start ->
      (Model.start model, Effects.tick Tick)
    Tick time ->
      if model.state == Playing then
        (Model.animate time model, Effects.tick Tick)
      else
        ({model | prevTime = Nothing}, Effects.none)
    Click position ->
      let
        effect = case Model.click position model of
          Just action ->
            Effects.task (Task.succeed action)
          Nothing ->
            Effects.none
      in
        (model, effect)
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


loadImage : String -> TextureId -> Effects Action
loadImage imagesUrl textureId =
  ( WebGL.loadTexture (imagesUrl ++ "/" ++ Textures.filename textureId)
    |> Task.map (\r -> TextureLoaded textureId (Just r))
  ) `Task.onError` always (Task.succeed (TextureLoaded textureId Nothing))
  |> Effects.task


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
