port module Update exposing (update, loadImage)

import Model exposing (..)
import Actions exposing (..)
import DeliveryPerson exposing (Location(..))
import Article exposing (State(..), Article)
import MapObject exposing (MapObject, MapObjectCategory(..))
import Category exposing (Category)
import IHopeItWorks
import Task exposing (Task)
import WebGL
import Textures exposing (TextureId)
import AllDict
import Window


port suspended : Bool -> Cmd msg


update : Action -> Model -> (Model, Cmd Action)
update action model =
  case action of
    HoverCloseButton active ->
      {model | closeButtonActive = active} ! []
    Dimensions {width, height} ->
      (Model.resize (width, height) model |> Model.render, Cmd.none)
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
          case model.state of
            Suspended _ ->
              ( {newModel | state = Suspended Loading}
              , Cmd.batch (List.map (loadImage model.imagesUrl) texturesToLoad)
              )
            _ ->
              ( {newModel | state = Loading}
              , Cmd.batch (List.map (loadImage model.imagesUrl) texturesToLoad)
              )
        else
          if List.length texturesToLoad == 0 then
            case model.state of
              Suspended _ ->
                Model.render {newModel | state = Suspended Stopped} ! []
              _ ->
                Model.render {newModel | state = Stopped} ! []
          else
            Model.render newModel ! []
    Start ->
      Model.start model ! []
    Tick time ->
      if model.state == Playing then
        Model.animate time model ! []
      else
        model ! []
    Click {x, y} ->
      let
        effect = case Model.click (x, y) model of
          Just action ->
            Task.succeed action
              |> Task.perform identity identity
          Nothing ->
            Cmd.none
      in
        (model, effect)
    ClickArticle article ->
      ifPlaying (onArticleClick article) model
    ClickCategory category ->
      ifPlaying (onCategoryClick category) model
    ClickMapObject mapObject ->
      ifPlaying (Model.navigateToMapObject mapObject) model
    Suspend ->
      case (model.embed, model.state) of
        (True, Suspended _) ->
          model ! []
        _ ->
          { model | state = Suspended model.state, closeButtonActive = False } ! [suspended True]
    Restore ->
      case (model.embed, model.state) of
        (True, Suspended prevState) ->
          { model | state = prevState } ! [Task.perform identity Dimensions Window.size]
        _ ->
          model ! []


ifPlaying : (Model -> Model) -> Model -> (Model, Cmd Action)
ifPlaying fun model =
  if model.state == Playing then
    fun model ! []
  else
    model ! []


loadImage : String -> TextureId -> Cmd Action
loadImage imagesUrl textureId =
  WebGL.loadTexture (imagesUrl ++ "/" ++ Textures.filename textureId)
    |> Task.perform
      (\_ -> TextureLoaded textureId Nothing)
      (\texture -> TextureLoaded textureId (Just texture))


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
