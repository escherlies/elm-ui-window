module Rect.Test exposing (..)

import Expect
import Math.Vector2 exposing (setX, setY, vec2)
import Test exposing (..)
import Window.Rect exposing (bottom, bottomLeft, bottomRight, center, centerX, centerY, left, right, top, topLeft, topRight)


suite : Test
suite =
    let
        rect =
            { position = vec2 50 50
            , size = vec2 50 50
            }

        viewport =
            vec2 100 100
    in
    describe "Positions"
        [ test "TopLeft" (\_ -> topLeft rect |> Expect.equal { rect | position = vec2 0 0 })
        , test "Left" (\_ -> left rect |> Expect.equal { rect | position = setX 0 rect.position })
        , test "Top" (\_ -> top rect |> Expect.equal { rect | position = setY 0 rect.position })
        , test "Right" (\_ -> right viewport rect |> Expect.equal { rect | position = setX 50 rect.position })
        , test "TopRight" (\_ -> topRight viewport rect |> Expect.equal { rect | position = setX 50 rect.position |> setY 0 })
        , test "Bottom" (\_ -> bottom viewport rect |> Expect.equal { rect | position = setY 50 rect.position })
        , test "BottomLeft" (\_ -> bottomLeft viewport rect |> Expect.equal { rect | position = setY 50 rect.position |> setX 0 })
        , test "BottomRight" (\_ -> bottomRight viewport rect |> Expect.equal { rect | position = vec2 50 50 })
        , test "Center" (\_ -> center viewport rect |> Expect.equal { rect | position = vec2 25 25 })
        , test "CenterX" (\_ -> centerX viewport rect |> Expect.equal { rect | position = setX 25 rect.position })
        , test "CenterY" (\_ -> centerY viewport rect |> Expect.equal { rect | position = setY 25 rect.position })
        ]
