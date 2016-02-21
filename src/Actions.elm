module Actions (Action(..)) where

import Article exposing (Article)
import MapObject exposing (MapObject)
import Time exposing (Time)
import Category exposing (Category)


type Action
  = Init Time
  | Tick Time
  | Start
  | ClickArticle Article
  | ClickCategory Category
  | ClickMapObject MapObject
  | ImageLoaded String
  | Dimensions (Int, Int)
