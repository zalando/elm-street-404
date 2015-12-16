module Obstacle (Obstacle, animate, render, fountain, tree) where
import Time exposing (Time)
import AnimationState exposing (animateObject, rotateFrames)
import Sprite
import Layers exposing (layers)

type Category = Fountain | Tree


fountainShadowSprite : Sprite.Sprite
fountainShadowSprite =
  { size = (4, 2)
  , offset = (0, 1)
  , frames = 1
  , src = "img/fountain-shadow.png"
  }

fountainSprite : Sprite.Sprite
fountainSprite =
  { size = (3, 2)
  , offset = (0, 0)
  , frames = 1
  , src = "img/fountain.png"
  }

fountainSpringSprite : Sprite.Sprite
fountainSpringSprite =
  { size = (1, 2)
  , offset = (1, -1)
  , frames = 4
  , src = "img/fountain-spring.png"
  }

treeSprite : Sprite.Sprite
treeSprite =
  { size = (3, 5)
  , offset = (0, -3)
  , frames = 1
  , src = "img/tree.png"
  }


animate : Time -> Obstacle -> Obstacle
animate time obstacle =
  let
    updateFountain fountain =
      { fountain | frames = rotateFrames fountain.frames }
  in
    case obstacle.category of
      Fountain -> animateObject 150 time updateFountain obstacle
      _ -> obstacle


type alias Obstacle =
  { category : Category
  , position : (Int, Int)
  , size : (Int, Int)
  , elapsed: Time
  , frames : List (Int)
  }


fountain : (Int, Int) -> Obstacle
fountain position =
  { category = Fountain
  , position = position
  , size = (3, 2)
  , elapsed = 0
  , frames = [0, 1, 2, 3]
  }


tree : (Int, Int) -> Obstacle
tree position =
  { category = Tree
  , position = position
  , size = (3, 2)
  , elapsed = 0
  , frames = [0]
  }


render : Obstacle -> List Sprite.Box
render obstacle =
  case obstacle.category of
    Fountain ->
      [ { sprite = fountainSprite
        , position = obstacle.position
        , layer = layers.obstacle
        , frame = 0
        , attributes = []
        }
      , { sprite = fountainSpringSprite
        , position = obstacle.position
        , layer = layers.fountainSpring
        , frame = Maybe.withDefault 0 (List.head obstacle.frames)
        , attributes = []
        }
      , { sprite = fountainShadowSprite
        , position = obstacle.position
        , layer = layers.shadow
        , frame = 0
        , attributes = []
        }
      ]
    Tree ->
      [ { sprite = treeSprite
        , position = obstacle.position
        , layer = layers.obstacle
        , frame = 0
        , attributes = []
        }
      ]
