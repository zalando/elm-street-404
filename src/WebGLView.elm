module WebGLView (render) where

import WebGL as GL
import Math.Vector2 exposing (Vec2, vec2)
import Sprite
import AllDict exposing (AllDict)
import Html exposing (div)
import Html.Attributes exposing (style)

type alias Vertex = { position : Vec2 }


mesh : GL.Drawable Vertex
mesh =
  GL.Triangle
    [ ( Vertex (vec2 0 0)
      , Vertex (vec2 1 1)
      , Vertex (vec2 1 0)
      )
    , ( Vertex (vec2 0 0)
      , Vertex (vec2 0 1)
      , Vertex (vec2 1 1)
      )
    ]


render : (Int, Int) -> Int -> AllDict Sprite.TextureId Sprite.TextureData String -> List Sprite.TexturedBoxData -> Html.Html
render ((w, h) as dimensions) tileSize textures boxes =
  div
    [ style
        [ ("transform", "scale(0.5)")
        , ("transform-origin", "left top")
        , ("position", "absolute")
        ]
    ]
    [ GL.webglWithConfig
        [ GL.Enable GL.Blend
        , GL.BlendFunc (GL.One, GL.OneMinusSrcAlpha)
        ]
        (w * tileSize * 2, h * tileSize * 2)
        (List.filterMap (renderTextured dimensions textures) (List.reverse boxes))
      |> Html.fromElement
    ]


renderTextured : (Int, Int) -> AllDict Sprite.TextureId Sprite.TextureData String -> Sprite.TexturedBoxData -> Maybe GL.Renderable
renderTextured (w, h) textures {textureId, position, frame} =
  case AllDict.get textureId textures of
    Nothing -> Nothing
    Just {size, offset, texture} ->
      case texture of
        Nothing -> Nothing
        Just textureValue ->
          Just (
            GL.render
              vertexShader
              fragmentShader
              mesh
              { screenSize = vec2 (toFloat w) (toFloat h)
              , offset = vec2 (fst offset + fst position) (snd offset + snd position)
              , texture = textureValue
              , frame = frame
              , textureSize =
                  vec2
                    (toFloat (fst (GL.textureSize textureValue)))
                    (toFloat (snd (GL.textureSize textureValue)))
              , frameSize = (uncurry vec2) size
              }
          )


-- Shaders

vertexShader : GL.Shader {attr | position : Vec2} {unif | frameSize : Vec2, screenSize : Vec2, offset : Vec2} {texturePos : Vec2}
vertexShader = [glsl|

  precision mediump float;
  attribute vec2 position;
  uniform vec2 offset;
  uniform vec2 frameSize;
  uniform vec2 screenSize;
  varying vec2 texturePos;

  void main () {
    vec2 clipSpace = (position * frameSize + offset) / screenSize * 2.0 - 1.0;
    gl_Position = vec4(clipSpace.x, -clipSpace.y, 0, 1);
    texturePos = position;
  }

|]


fragmentShader : GL.Shader {} {u | texture : GL.Texture, textureSize : Vec2, frameSize : Vec2, frame : Int } {texturePos : Vec2}
fragmentShader = [glsl|

  precision mediump float;
  uniform sampler2D texture;
  uniform vec2 textureSize;
  uniform vec2 frameSize;
  uniform int frame;
  varying vec2 texturePos;

  void main () {
    vec2 size = frameSize / textureSize * 80.0;
    int cols = int(1.0 / size.x);
    vec2 frameOffset = size * vec2(float(frame - frame / cols * cols), -float(frame / cols));
    vec2 textureClipSpace = size * texturePos - 1.0;
    vec4 temp  = texture2D(texture, vec2(textureClipSpace.x, -textureClipSpace.y) + frameOffset);
    float a = temp.a;
    float r = temp.r * a;
    float g = temp.g * a;
    float b = temp.b * a;
    gl_FragColor = vec4(r,g,b,a);
  }

|]
