module Actions exposing (Action(..), EventAction(..))

import Article exposing (Article)
import MapObject exposing (MapObject)
import Time exposing (Time)
import Category exposing (Category)
import Textures exposing (TextureId)
import WebGL exposing (Texture)
import OffsetClick exposing (Position)
import Window exposing (Size)


type EventAction
  = DispatchArticles Int
  | DispatchOrders Int
  | DispatchReturns Int
  | DispatchCustomers
  | TimeoutRequestsAndCleanup


type Action
  = Tick Time
  | Start
  | Suspend
  | Restore
  | Click Position
  | ClickArticle Article
  | ClickCategory Category
  | ClickMapObject MapObject (Maybe Action)
  | TextureLoaded TextureId (Maybe Texture)
  | Dimensions Size
  | HoverCloseButton Bool
  | Event EventAction
  | NoOp
