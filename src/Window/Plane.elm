module Window.Plane exposing (..)

import Math.Vector2 exposing (Vec2, add, scale, setX, setY, sub, vec2)
import Window.Utils exposing (compose12L, setXof, setYof, vec2lt)


type alias Plane =
    { position : Vec2
    , size : Vec2
    }


isOnPlane : Plane -> Vec2 -> Bool
isOnPlane { position, size } v =
    vec2lt position v && vec2lt v (add position size)


move : Vec2 -> Plane -> Plane
move delta p =
    { p | position = add delta p.position }


moveTo : Vec2 -> Plane -> Plane
moveTo pos p =
    { p | position = pos }


setPosition : Vec2 -> Plane -> Plane
setPosition =
    moveTo


setSize : Plane -> Vec2 -> Plane
setSize p size =
    { p | size = size }



-- Positioning


centerX : Vec2 -> Plane -> Plane
centerX viewport p =
    { p | position = setXof (centerOffset viewport p.size) p.position }


centerY : Vec2 -> Plane -> Plane
centerY viewport p =
    { p | position = setYof (centerOffset viewport p.size) p.position }


center : Vec2 -> Plane -> Plane
center viewport p =
    { p | position = centerOffset viewport p.size }



--


top : Plane -> Plane
top p =
    { p | position = setY 0 p.position }


left : Plane -> Plane
left p =
    { p | position = setX 0 p.position }


bottom : Vec2 -> Plane -> Plane
bottom viewport p =
    { p | position = setYof (sub viewport p.size) p.position }


right : Vec2 -> Plane -> Plane
right viewport p =
    { p | position = setXof (sub viewport p.size) p.position }



--


bottomRight : Vec2 -> Plane -> Plane
bottomRight viewport p =
    { p | position = sub viewport p.size }


bottomLeft : Vec2 -> Plane -> Plane
bottomLeft =
    compose12L left bottom


topRight : Vec2 -> Plane -> Plane
topRight =
    compose12L top right


topLeft : { a | position : Vec2 } -> { a | position : Vec2 }
topLeft p =
    { p | position = vec2 0 0 }



-- Helpers


centerOffset : Vec2 -> Vec2 -> Vec2
centerOffset viewport plane =
    scale 0.5 (sub viewport plane)



-- Default


default : Plane
default =
    { position = vec2 0 0, size = vec2 0 0 }
