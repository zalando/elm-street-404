module CustomerView where

import Customer exposing (Customer)
import Sprite exposing (Sprite)
import Layers exposing (layers)
import House exposing (House)


sprite : Sprite
sprite =
 { size = (2, 3)
 , offset = (0, 0)
 , frames = 18
 , src = "img/customers.png"
 }


render : House -> Customer -> List Sprite.Box
render house _ =
  [ { sprite = sprite
    , position = house.position
    , layer = layers.customer
    , frame = 0
    , attributes = []
    }
  ]
