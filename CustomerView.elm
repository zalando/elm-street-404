module CustomerView where

import Customer exposing (Customer)
import Sprite exposing (Sprite)
import Layers exposing (layers)
import House exposing (House)
import Request exposing (Request)


size : (Int, Int)
size = (2, 3)


emptySprite : Sprite
emptySprite = Sprite.empty size (0, -1)


sprite : Sprite
sprite =
 { size = size
 , offset = (0, 0)
 , frames = 18
 , src = "img/customers.png"
 }


moodFrameOffset : Int -> Int
moodFrameOffset mood = 2 - mood


frame : Customer -> Int
frame customer =
  customer.typ * 3 +
  (moodFrameOffset customer.happiness)


render : List Request -> House -> Customer -> List Sprite.Box
render requests house customer =
  if Customer.isLost customer then
    []
  else
    [ { sprite = sprite
      , position = (fst house.position, snd house.position + 0.01)
      , layer = layers.customer
      , frame = frame customer
      , attributes = []
      }
    ]
