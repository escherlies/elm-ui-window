module Window.Plane exposing (..)

import Math.Vector2 exposing (Vec, add, scale, setX, setY, sub, vec2)
import Window.Utils exposing (compose12L, setXof, setYof, vec2lt)


type alias Plane =
    { position : Vec Float
    , size : Vec Float
    }


isOnPlane : Plane -> Vec Float -> Bool
isOnPlane { position, size } v =
    vec2lt position v && vec2lt v (add position size)


move : Vec Float -> Plane -> Plane
move delta p =
    { p | position = add delta p.position }


moveTo : Vec Float -> Plane -> Plane
moveTo pos p =
    { p | position = pos }


setPosition : Vec Float -> Plane -> Plane
setPosition =
    moveTo


setSize : Plane -> Vec Float -> Plane
setSize p size =
    { p | size = size }



-- Positioning


centerX : Vec Float -> Plane -> Plane
centerX viewport p =
    { p | position = setXof (centerOffset viewport p.size) p.position }


centerY : Vec Float -> Plane -> Plane
centerY viewport p =
    { p | position = setYof (centerOffset viewport p.size) p.position }


center : Vec Float -> Plane -> Plane
center viewport p =
    { p | position = centerOffset viewport p.size }



--


top : Plane -> Plane
top p =
    { p | position = setY 0 p.position }


left : Plane -> Plane
left p =
    { p | position = setX 0 p.position }


bottom : Vec Float -> Plane -> Plane
bottom viewport p =
    { p | position = setYof (sub viewport p.size) p.position }


right : Vec Float -> Plane -> Plane
right viewport p =
    { p | position = setXof (sub viewport p.size) p.position }



--


bottomRight : Vec Float -> Plane -> Plane
bottomRight viewport p =
    { p | position = sub viewport p.size }


bottomLeft : Vec Float -> Plane -> Plane
bottomLeft =
    compose12L left bottom


topRight : Vec Float -> Plane -> Plane
topRight =
    compose12L top right


topLeft : { a | position : Vec Float } -> { a | position : Vec Float }
topLeft p =
    { p | position = vec2 0 0 }



-- Helpers


centerOffset : Vec Float -> Vec Float -> Vec Float
centerOffset viewport plane =
    scale 0.5 (sub viewport plane)



-- Default


default : Plane
default =
    { position = vec2 0 0, size = vec2 0 0 }
