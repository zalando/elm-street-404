port module Update exposing (update, loadImage)

import Model exposing (..)
import View.Model
import Actions exposing (..)
import DeliveryPerson exposing (Location(..))
import Article exposing (State(..), Article)
import MapObject exposing (MapObject, MapObjectCategory(..))
import Category exposing (Category)
import IHopeItWorks
import Task exposing (Task)
import WebGL.Texture as Texture
import Textures exposing (TextureId)
import AllDict
import Window


port suspended : Bool -> Cmd msg


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        HoverCloseButton active ->
            { model | closeButtonActive = active } ! []

        Dimensions { width, height } ->
            ( Model.resize ( width, height ) model |> View.Model.render, Cmd.none )

        TextureLoaded textureId maybeTexture ->
            let
                loadTexture =
                    case AllDict.get textureId model.textures of
                        Just data ->
                            AllDict.insert
                                textureId
                                { data
                                    | texture =
                                        Maybe.map
                                            (\texture ->
                                                { size =
                                                    ( toFloat (Tuple.first (Texture.size texture))
                                                    , toFloat (Tuple.second (Texture.size texture))
                                                    )
                                                , texture = texture
                                                }
                                            )
                                            maybeTexture
                                }
                                model.textures

                        Nothing ->
                            model.textures

                newModel =
                    { model | textures = loadTexture }

                texturesToLoad =
                    Textures.loadTextures newModel.textures
            in
                if textureId == Textures.Score then
                    case model.state of
                        Suspended _ ->
                            ( { newModel | state = Suspended Loading }
                            , Cmd.batch (List.map (loadImage model.imagesUrl) texturesToLoad)
                            )

                        _ ->
                            ( { newModel | state = Loading }
                            , Cmd.batch (List.map (loadImage model.imagesUrl) texturesToLoad)
                            )
                else if List.length texturesToLoad == 0 then
                    case model.state of
                        Suspended _ ->
                            View.Model.render { newModel | state = Suspended Stopped } ! []

                        _ ->
                            View.Model.render { newModel | state = Stopped } ! []
                else
                    View.Model.render newModel ! []

        BackToStart ->
            View.Model.render { model | state = Stopped } ! []

        Start ->
            Model.start model ! []

        Tick time ->
            let
                ( newModel, maybeAction ) =
                    Model.animate time model
            in
                ( View.Model.render newModel
                , case maybeAction of
                    Just action ->
                        Task.perform identity (Task.succeed action)

                    Nothing ->
                        Cmd.none
                )

        Click { x, y } ->
            let
                effect =
                    case Model.click ( x, y ) model of
                        Just action ->
                            Task.perform identity (Task.succeed action)

                        Nothing ->
                            Cmd.none
            in
                ( model, effect )

        ClickArticle article ->
            ifPlaying (onArticleClick article) model

        ClickCategory category ->
            ifPlaying (onCategoryClick category) model

        ClickMapObject mapObject maybeAction ->
            ifPlaying (Model.navigateToMapObject mapObject maybeAction) model

        Suspend ->
            case ( model.embed, model.state ) of
                ( True, Suspended _ ) ->
                    model ! []

                _ ->
                    { model | state = Suspended model.state, closeButtonActive = False } ! [ suspended True ]

        Restore ->
            case ( model.embed, model.state ) of
                ( True, Suspended prevState ) ->
                    { model | state = prevState } ! [ Task.perform Dimensions Window.size ]

                _ ->
                    model ! []

        Event eventAction ->
            ifPlaying (Model.dispatch eventAction) model

        NoOp ->
            model ! []


ifPlaying : (Model -> Model) -> Model -> ( Model, Cmd Action )
ifPlaying fun model =
    if model.state == Playing then
        fun model ! []
    else
        model ! []


loadImage : String -> TextureId -> Cmd Action
loadImage imagesUrl textureId =
    Texture.load (imagesUrl ++ "/" ++ Textures.filename textureId)
        |> Task.attempt (Result.toMaybe >> TextureLoaded textureId)



-- click the 1st picked article that has the same category


onCategoryClick : Category -> Model -> Model
onCategoryClick category model =
    let
        isPickedCategory a =
            a.category == category && Article.isPicked a
    in
        case IHopeItWorks.find isPickedCategory model.articles of
            Just article ->
                onArticleClick article model

            _ ->
                model


onArticleClick : Article -> Model -> Model
onArticleClick article model =
    case model.deliveryPerson.location of
        At mapObject _ ->
            case ( mapObject.category, article.state ) of
                ( HouseCategory _, AwaitingReturn house ) ->
                    Model.pickupReturn mapObject house article model

                ( HouseCategory _, Picked ) ->
                    Model.deliverArticle mapObject article model

                ( WarehouseCategory _, InStock warehouse ) ->
                    Model.pickupArticle warehouse mapObject article model

                ( WarehouseCategory _, Picked ) ->
                    Model.returnArticle mapObject article model

                _ ->
                    model

        _ ->
            model
