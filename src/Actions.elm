module Actions exposing (Action(..))

import Article exposing (Article)
import MapObject exposing (MapObject)
import Time exposing (Time)
import Category exposing (Category)
import Textures exposing (TextureId)
import WebGL exposing (Texture)
import OffsetClick exposing (Position)
import Window exposing (Size)

type Action
  = Tick Time
  | Start
  | Suspend
  | Restore
  | Click Position
  | ClickArticle Article
  | ClickCategory Category
  | ClickMapObject MapObject
  | TextureLoaded TextureId (Maybe Texture)
  | Dimensions Size
  | HoverCloseButton Bool
