module Box exposing
  ( TexturedBoxData
  , ClickableBoxData
  , Box
  , split
  , textured
  , offsetTextured
  , clickable
  , clicked
  , layer
  )

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
  , layer : (Float, Float)
  }


type alias TexturedBoxData =
  { position : (Float, Float)
  , offset : (Float, Float)
  , textureId : TextureId
  , frame : Int
  , layer : (Float, Float)
  }


offsetTextured : (Float, Float) -> TextureId -> (Float, Float) -> Int -> (Float, Float) -> Box
offsetTextured offset textureId position frame layer =
  Textured
    { position = position
    , offset = offset
    , textureId = textureId
    , frame = frame
    , layer = layer
    }


textured : TextureId -> (Float, Float) -> Int -> (Float, Float) -> Box
textured =
  offsetTextured (0, 0)


clickable : (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Actions.Action -> Box
clickable size offset position layer onClickAction =
  Clickable
    { position = position
    , size = size
    , offset = offset
    , layer = layer
    , onClickAction = onClickAction
    }


clicked : (Float, Float) -> ClickableBoxData -> Bool
clicked coordinates ({position, offset, size, onClickAction} as box) =
  let
    left = fst position + fst offset
    top = snd position + snd offset
    right = left + fst size
    bottom = top + snd size
    x = fst coordinates
    y = snd coordinates
  in
    x >= left && x < right && y >= top && y < bottom


layer : {a | layer : (Float, Float), position : (Float, Float)} -> Float
layer {layer, position} =
  fst layer * 1000 +
  snd position * 100 +
  snd layer


split : List Box -> (List TexturedBoxData, List ClickableBoxData)
split boxes =
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
