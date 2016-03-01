module Actions (Action(..)) where

import Article exposing (Article)
import MapObject exposing (MapObject)
import Time exposing (Time)
import Category exposing (Category)
import Sprite exposing (TextureId)
import WebGL

type Action
  = Init Time
  | Tick Time
  | Start
  | ClickArticle Article
  | ClickCategory Category
  | ClickMapObject MapObject
  | TextureLoaded TextureId (Maybe WebGL.Texture)
  | Dimensions (Int, Int)
