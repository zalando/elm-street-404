module CustomerView where

import Customer exposing (Customer)
import Sprite exposing (Sprite)
import Layers exposing (layers)


sprite : Sprite
sprite =
 { size = (2, 3)
 , offset = (2, 2)
 , frames = 18
 , src = "img/customers.png"
 }


render : Customer -> List Sprite.Box
render _ =
  [ { sprite = sprite
    , position = (3, 3)
    , layer = layers.customer
    , frame = 0
    , attributes = []
    }
  ]
