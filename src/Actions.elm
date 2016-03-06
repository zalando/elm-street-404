module Actions (Action(..)) where

import Article exposing (Article)
import MapObject exposing (MapObject)
import Time exposing (Time)
import Category exposing (Category)
import Textures exposing (TextureId)
import WebGL


type Action
  = Tick Time
  | Start
  | ClickArticle Article
  | ClickCategory Category
  | ClickMapObject MapObject
  | TextureLoaded TextureId (Maybe WebGL.Texture)
  | Dimensions (Int, Int)
