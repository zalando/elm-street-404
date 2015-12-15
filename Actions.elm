module Actions (Action(..)) where
import Time exposing (Time)
import Article exposing (Article)

type Action
  = Init Time
  | Tick Time
  | Start
  | Click Article
