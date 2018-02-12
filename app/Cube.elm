module App.Cube exposing (..)

import List
import Basics exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Color exposing (Color)

type alias Vertex = 
            { x : Float
            , y : Float
            , z : Float 
            }
type alias Face = 
            { v1: Vertex
            , v2: Vertex
            , v3: Vertex
            , v4: Vertex
            }
type alias Cube = List Face

-- construct a cube with given size
constructCube : Float -> Cube
constructCube size =
            [ Face (Vertex size size -size)   (Vertex -size size -size) (Vertex -size -size -size) (Vertex size -size -size)
            , Face (Vertex size size -size)   (Vertex size size size)   (Vertex size -size size)   (Vertex size -size -size)
            , Face (Vertex size size size)    (Vertex -size size size)  (Vertex -size -size size)  (Vertex size -size size)
            , Face (Vertex -size size size)   (Vertex -size size -size) (Vertex -size -size -size) (Vertex -size -size size)
            , Face (Vertex -size size -size)  (Vertex -size size size)  (Vertex size size size)    (Vertex size size -size)
            , Face (Vertex -size -size -size) (Vertex size -size -size) (Vertex size -size size)   (Vertex -size -size size)
            ]

-- rotate Face of a cube about X-axis
rotateX : Float -> Face -> Face
rotateX angle { v1, v2, v3, v4 } =
  let rad  = degrees angle
      cosa = cos(rad)
      sina = sin(rad)
  in Face { v1 | y = (v1.y * cosa - v1.z * sina), z = (v1.y * sina + v1.z * cosa) }
          { v2 | y = (v2.y * cosa - v2.z * sina), z = (v2.y * sina + v2.z * cosa) }
          { v3 | y = (v3.y * cosa - v3.z * sina), z = (v3.y * sina + v3.z * cosa) }
          { v4 | y = (v4.y * cosa - v4.z * sina), z = (v4.y * sina + v4.z * cosa) }

-- rotate Face of a cube about Y-axis
rotateY : Float -> Face -> Face
rotateY angle { v1, v2, v3, v4 } =
  let rad  = degrees angle
      cosa = cos(rad)
      sina = sin(rad)
  in Face { v1 | x = (v1.z * sina + v1.x * cosa), z = (v1.z * cosa - v1.x * sina) }
          { v2 | x = (v2.z * sina + v2.x * cosa), z = (v2.z * cosa - v2.x * sina) }
          { v3 | x = (v3.z * sina + v3.x * cosa), z = (v3.z * cosa - v3.x * sina) }
          { v4 | x = (v4.z * sina + v4.x * cosa), z = (v4.z * cosa - v4.x * sina) }

-- Convert Face of a cube to Form
renderFace : Face -> Form
renderFace { v1, v2, v3, v4 } =
  let
    shape   = polygon [(v1.x, v1.y), (v2.x, v2.y), (v3.x, v3.y), (v4.x, v4.y)]
    border  = outlined (solid Color.black) shape
    bgColor = filled Color.lightGray shape
  in group[bgColor, border]

-- Convert a cube to list of Form
renderCube : Cube -> List Form
renderCube faces =
  let
    sortedFaces  = List.sortBy (\{ v1, v2, v3, v4 } -> (v1.z + v2.z + v3.z + v4.z) / 4) faces
  in List.map renderFace sortedFaces
