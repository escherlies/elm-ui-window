module Area exposing (..)

import Math.Vector2 exposing (Vec2, add, getX, getY, setX, setY, vec2)


type alias Area =
    { position : Vec2
    , size : Vec2
    }


isInArea : Area -> Vec2 -> Bool
isInArea { position, size } v =
    vec2lt position v && vec2lt v (add position size)



-- Vector helpers


{-| Add only the x component of a to b
-}
addXof : Vec2 -> Vec2 -> Vec2
addXof =
    add << setY 0


{-| Add only the y component of a to b
-}
addYof : Vec2 -> Vec2 -> Vec2
addYof =
    add << setX 0


vec2uni : Float -> Vec2
vec2uni u =
    vec2 u u


zero : Vec2
zero =
    vec2uni 0


vec2lt : Vec2 -> Vec2 -> Bool
vec2lt =
    vec2order LT


vec2order : Order -> Vec2 -> Vec2 -> Bool
vec2order order a b =
    (compare (getX a) (getX b) == order) && (compare (getY a) (getY b) == order)
