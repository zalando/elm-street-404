module Box
  ( TexturedBoxData
  , ClickableBoxData
  , Box
  , split
  , textured
  , clickable
  ) where


import Actions
import Textures exposing (TextureId)


type Box
  = Clickable ClickableBoxData
  | Textured TexturedBoxData


type alias ClickableBoxData =
  { position : (Float, Float)
  , size : (Float, Float)
  , offset : (Float, Float)
  , onClickAction : Actions.Action
  , layer : (Int, Int)
  }


type alias TexturedBoxData =
  { position : (Float, Float)
  , textureId : TextureId
  , frame : Int
  , layer : (Int, Int)
  }


textured : TextureId -> (Float, Float) -> Int -> (Int, Int) -> Box
textured textureId position frame layer =
  Textured
    { position = position
    , textureId = textureId
    , frame = frame
    , layer = layer
    }


clickable : (Float, Float) -> (Float, Float) -> (Float, Float) -> (Int, Int) -> Actions.Action -> Box
clickable size offset position layer onClickAction =
  Clickable
    { position = position
    , size = size
    , offset = offset
    , layer = layer
    , onClickAction = onClickAction
    }


sortBoxData :
  List {a | layer : (Int, Int), position : (Float, Float)} ->
  List {a | layer : (Int, Int), position : (Float, Float)}
sortBoxData =
  List.sortBy (\{layer, position} -> (fst layer, snd position, snd layer))


split : List Box -> (List TexturedBoxData, List ClickableBoxData)
split boxes =
  let
    (textured, clickable) = split' boxes
  in
    (sortBoxData textured, sortBoxData clickable)


split' : List Box -> (List TexturedBoxData, List ClickableBoxData)
split' boxes =
  case boxes of
    [] -> ([], [])
    box :: rest ->
      let
        (restTextured, restClickable) = split rest
      in
        case box of
          Textured texturedBox ->
            (texturedBox :: restTextured, restClickable)
          Clickable clickableBox ->
            (restTextured, clickableBox :: restClickable)
