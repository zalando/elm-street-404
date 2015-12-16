module Actions (Action(..)) where
import Time exposing (Time)
import Article exposing (Article)
import House exposing (House)
import Warehouse exposing (Warehouse)
import Request exposing (Request)

type Action
  = Init Time
  | Tick Time
  | Start
  | ClickArticle Article
  | ClickWarehouse Warehouse
  | ClickHouse House
  | ClickRequest Request
