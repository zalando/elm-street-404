module Actions (Action(..)) where
import Time exposing (Time)

type Action
  = Init Time
  | Tick Time
  | Start
