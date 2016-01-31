module Actions (Action(..)) where

import Article exposing (Article)
import House exposing (House)
import Warehouse exposing (Warehouse)
import Time exposing (Time)
import Category exposing (Category)


type Action
  = Init Time
  | Tick Time
  | Start
  | ClickArticle Article
  | ClickCategory Category
  | ClickWarehouse Warehouse
  | ClickHouse House
  | ImageLoaded String
  | Dimensions (Int, Int)
