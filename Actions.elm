module Actions (Action(..)) where

import Request exposing (Request)
import Article exposing (Article)
import Time exposing (Time)

type Action
  = Init Time
  | Tick Time
  | Start
  | GoTo (Int, Int)
  | ClickArticle Article
  | ClickRequest Request
