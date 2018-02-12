module App.AppKeyBoard exposing (..)

import Html exposing (..)
import Time exposing (..)
import Basics exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Keyboard

import App.Cube as Cube

main = program  { init = init
                , view = view
                , update = update
                , subscriptions = subscriptions
                }

cubeSize : Float
cubeSize = 100

collageSize : Int
collageSize = 500

-----------
-- MODEL --
-----------
type alias Model = 
        { cube : Cube.Cube -- Cube
        , dDeg : Float     -- angular velocity
        , direction : Int  -- direction of rotation
        }
        
init : ( Model, Cmd Msg )
init = ( Model ( Cube.constructCube cubeSize ) 0 0, Cmd.none )

------------
-- UPDATE --
------------
type Msg
    = Tick Time
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Tick newTime ->
      if model.dDeg > 0 then -- Rotate till the angular velocity reaches Zero
        case model.direction of
          ----------------- accelerate ------------------
          1    -> ( { model | dDeg = model.dDeg + 0.05,
                              cube = List.map (Cube.rotateY -model.dDeg) model.cube -- rotate left
                  }, Cmd.none )
          2    -> ( { model | dDeg = model.dDeg + 0.05,
                              cube = List.map (Cube.rotateX -model.dDeg) model.cube -- rotate up
                  }, Cmd.none )
          3    -> ( { model | dDeg = model.dDeg + 0.05,
                              cube = List.map (Cube.rotateY  model.dDeg) model.cube -- rotate right
                  }, Cmd.none )
          4    -> ( { model | dDeg = model.dDeg + 0.03,
                              cube = List.map (Cube.rotateX  model.dDeg) model.cube -- rotate down
                  }, Cmd.none )

          ----------------- decelerate -------------------
          (-1) -> ( { model | dDeg = model.dDeg - 0.04,
                              cube = List.map (Cube.rotateY -model.dDeg) model.cube -- rotate left
                  }, Cmd.none )
          (-2) -> ( { model | dDeg = model.dDeg - 0.04,
                              cube = List.map (Cube.rotateX -model.dDeg) model.cube -- rotate up
                  }, Cmd.none )
          (-3) -> ( { model | dDeg = model.dDeg - 0.04,
                              cube = List.map (Cube.rotateY  model.dDeg) model.cube -- rotate right
                  }, Cmd.none )
          (-4) -> ( { model | dDeg = model.dDeg - 0.04,
                              cube = List.map (Cube.rotateX  model.dDeg) model.cube -- rotate down
                  }, Cmd.none )

          _    -> ( model, Cmd.none)
      else
        ( model, Cmd.none )
    KeyDown code ->
      case code of
        -- left arrow key
        37 -> ( { model | direction = 1,
                          dDeg = if model.direction == 1
                            then model.dDeg else 1.2
                }, Cmd.none )
        -- up arrow key
        38 -> ( { model | direction = 2,
                          dDeg = if model.direction == 2
                            then model.dDeg else 1.2
                }, Cmd.none )
        -- right arrow key
        39 -> ( { model | direction = 3,
                          dDeg = if model.direction == 3
                            then model.dDeg else 1.2
                }, Cmd.none )
        -- down arrow key
        40 -> ( { model | direction = 4,
                          dDeg = if model.direction == 4
                            then model.dDeg else 1.2
                }, Cmd.none )
        _  -> ( model, Cmd.none )
    KeyUp code ->
      ----------------- invert direction -----------------
      ( {model | direction = -model.direction}, Cmd.none )

-------------------
-- SUBSCRIPTIONS --
-------------------
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ Time.every millisecond Tick
            , Keyboard.downs KeyDown
            , Keyboard.ups KeyUp
            ]

----------
-- VIEW --
----------
view : Model -> Html Msg
view model =
  div []
  [ toHtml <| collage collageSize collageSize (Cube.renderCube model.cube)
  , Html.h1 [] [ Html.text "use arrow keys to rotate and accelerate the cube (Press only one key at a time)"]
  ]
