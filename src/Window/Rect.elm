module Window.Rect exposing (..)

import Math.Vector2 exposing (Vec, add, scale, setX, setY, sub, vec2)
import Window.Utils exposing (compose12L, setXof, setYof, vec2lt)


type alias Rect =
    { position : Vec Float
    , size : Vec Float
    }


isOnRect : Rect -> Vec Float -> Bool
isOnRect { position, size } v =
    vec2lt position v && vec2lt v (add position size)


move : Vec Float -> Rect -> Rect
move delta p =
    { p | position = add delta p.position }


moveTo : Vec Float -> Rect -> Rect
moveTo pos p =
    { p | position = pos }


setPosition : Vec Float -> Rect -> Rect
setPosition =
    moveTo


setSize : Rect -> Vec Float -> Rect
setSize p size =
    { p | size = size }



-- Positioning


centerX : Vec Float -> Rect -> Rect
centerX viewport p =
    { p | position = setXof (centerOffset viewport p.size) p.position }


centerY : Vec Float -> Rect -> Rect
centerY viewport p =
    { p | position = setYof (centerOffset viewport p.size) p.position }


center : Vec Float -> Rect -> Rect
center viewport p =
    { p | position = centerOffset viewport p.size }



--


top : Rect -> Rect
top p =
    { p | position = setY 0 p.position }


left : Rect -> Rect
left p =
    { p | position = setX 0 p.position }


bottom : Vec Float -> Rect -> Rect
bottom viewport p =
    { p | position = setYof (sub viewport p.size) p.position }


right : Vec Float -> Rect -> Rect
right viewport p =
    { p | position = setXof (sub viewport p.size) p.position }



--


bottomRight : Vec Float -> Rect -> Rect
bottomRight viewport p =
    { p | position = sub viewport p.size }


bottomLeft : Vec Float -> Rect -> Rect
bottomLeft =
    compose12L left bottom


topRight : Vec Float -> Rect -> Rect
topRight =
    compose12L top right


topLeft : { a | position : Vec Float } -> { a | position : Vec Float }
topLeft p =
    { p | position = vec2 0 0 }



-- Helpers


centerOffset : Vec Float -> Vec Float -> Vec Float
centerOffset viewport rect =
    scale 0.5 (sub viewport rect)



-- Default


default : Rect
default =
    { position = vec2 0 0, size = vec2 0 0 }
