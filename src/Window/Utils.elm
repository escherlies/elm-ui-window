module Window.Utils exposing (..)

import Math.Vector2 exposing (Vec2, add, getX, getY, setX, setY, vec2)



-- Vector helpers


{-|

    setX (getX a) b

-}
setXof : Vec2 -> Vec2 -> Vec2
setXof =
    setX << getX


{-|

    setX (getX a) b

-}
setYof : Vec2 -> Vec2 -> Vec2
setYof =
    setY << getY


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
vec2uni =
    callTwice vec2


zero : Vec2
zero =
    vec2uni 0


vec2lt : Vec2 -> Vec2 -> Bool
vec2lt =
    vec2order LT


vec2order : Order -> Vec2 -> Vec2 -> Bool
vec2order order a b =
    (compare (getX a) (getX b) == order) && (compare (getY a) (getY b) == order)



-- Combinators


flip : a -> (a -> b) -> b
flip a b =
    b a


{-| Call a function `f` twice with an argument `x`.

Also known as Warbler (`λab.abb`)

-}
callTwice : (a -> a -> b) -> a -> b
callTwice a b =
    a b b


{-| Compose 1˚ <- 2˚, also known as B1 or Blackbird combinator
-}
compose12L : (a -> b) -> (c -> d -> a) -> c -> d -> b
compose12L =
    (<<) << (<<)
