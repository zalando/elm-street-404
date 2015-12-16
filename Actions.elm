module Actions (Action(..)) where

import Request exposing (Request)
import Article exposing (Article)
import House exposing (House)
import Warehouse exposing (Warehouse)
import Time exposing (Time)

type Action
  = Init Time
  | Tick Time
  | Start
  | ClickArticle Article
  | ClickRequest Request
  | ClickWarehouse Warehouse
  | ClickHouse House
