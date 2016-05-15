module WebGLView exposing (render)

import Element exposing (Element)
import WebGL as GL
import Math.Vector2 exposing (Vec2, vec2)
import Box
import Textures exposing (Textures)
import AllDict exposing (AllDict)


type alias Vertex =
  { position : Vec2 }


type alias Uniform =
  { frameSize : Vec2
  , screenSize : Vec2
  , offset : Vec2
  , texture : GL.Texture
  , textureSize : Vec2
  , frame : Int
  }


type alias Varying =
  { texturePos : Vec2 }


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


render : (Int, Int) -> Int -> Textures -> List Box.TexturedBoxData -> Element
render ((w, h) as dimensions) tileSize textures boxes =
  GL.webglWithConfig
    [ GL.Enable GL.Blend
    , GL.BlendFunc (GL.One, GL.OneMinusSrcAlpha)
    ]
    (w * tileSize, h * tileSize)
    (List.filterMap (renderTextured dimensions textures) (List.reverse boxes))


renderTextured : (Int, Int) -> Textures -> Box.TexturedBoxData -> Maybe GL.Renderable
renderTextured (w, h) textures {textureId, position, frame} =
  AllDict.get textureId textures
  `Maybe.andThen`
  (\{size, offset, texture} ->
    Maybe.map
      (\textureValue ->
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
    texture
  )


vertexShader : GL.Shader Vertex Uniform Varying
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


fragmentShader : GL.Shader {} Uniform Varying
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
