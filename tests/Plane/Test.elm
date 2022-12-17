module Plane.Test exposing (..)

import Expect
import Math.Vector2 exposing (setX, setY, vec2)
import Test exposing (..)
import Window.Plane exposing (bottom, bottomLeft, bottomRight, center, centerX, centerY, left, right, top, topLeft, topRight)


suite : Test
suite =
    let
        plane =
            { position = vec2 50 50
            , size = vec2 50 50
            }

        viewport =
            vec2 100 100
    in
    describe "Positions"
        [ test "TopLeft" (\_ -> topLeft plane |> Expect.equal { plane | position = vec2 0 0 })
        , test "Left" (\_ -> left plane |> Expect.equal { plane | position = setX 0 plane.position })
        , test "Top" (\_ -> top plane |> Expect.equal { plane | position = setY 0 plane.position })
        , test "Right" (\_ -> right viewport plane |> Expect.equal { plane | position = setX 50 plane.position })
        , test "TopRight" (\_ -> topRight viewport plane |> Expect.equal { plane | position = setX 50 plane.position |> setY 0 })
        , test "Bottom" (\_ -> bottom viewport plane |> Expect.equal { plane | position = setY 50 plane.position })
        , test "BottomLeft" (\_ -> bottomLeft viewport plane |> Expect.equal { plane | position = setY 50 plane.position |> setX 0 })
        , test "BottomRight" (\_ -> bottomRight viewport plane |> Expect.equal { plane | position = vec2 50 50 })
        , test "Center" (\_ -> center viewport plane |> Expect.equal { plane | position = vec2 25 25 })
        , test "CenterX" (\_ -> centerX viewport plane |> Expect.equal { plane | position = setX 25 plane.position })
        , test "CenterY" (\_ -> centerY viewport plane |> Expect.equal { plane | position = setY 25 plane.position })
        ]
