module Actions (Action(..)) where

import Article exposing (Article)
import MapObject exposing (MapObject)
import Time exposing (Time)
import Category exposing (Category)
import Textures exposing (TextureId)
import WebGL exposing (Texture)


type Action
  = Tick Time
  | Start
  | Click (Int, Int)
  | ClickArticle Article
  | ClickCategory Category
  | ClickMapObject MapObject
  | TextureLoaded TextureId (Maybe Texture)
  | Dimensions (Int, Int)
