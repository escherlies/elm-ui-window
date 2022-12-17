module Window.Plane exposing (..)

import Math.Vector2 exposing (Vec2, add)
import Window.Utils exposing (vec2lt)


type alias Plane =
    { position : Vec2
    , size : Vec2
    }


isOnPlane : Plane -> Vec2 -> Bool
isOnPlane { position, size } v =
    vec2lt position v && vec2lt v (add position size)
