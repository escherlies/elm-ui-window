module Window.Plane exposing (..)

import Math.Vector2 exposing (Vec2, add, getX, getY, scale, setX, setY, sub, vec2)
import Window.Utils exposing (vec2lt)


type alias Plane =
    { position : Vec2
    , size : Vec2
    }


isOnPlane : Plane -> Vec2 -> Bool
isOnPlane { position, size } v =
    vec2lt position v && vec2lt v (add position size)



--


centerOffset : Vec2 -> Vec2 -> Vec2
centerOffset viewport window =
    scale 0.5 (sub viewport window)


centerX : Vec2 -> Plane -> Plane
centerX viewport window =
    { window | position = setX (getX (centerOffset viewport window.size)) window.position }


centerY : Vec2 -> Plane -> Plane
centerY viewport window =
    { window | position = setY (getY (centerOffset viewport window.size)) window.position }


center : Vec2 -> Plane -> Plane
center viewport window =
    { window | position = centerOffset viewport window.size }


bottomRight : Vec2 -> Plane -> Plane
bottomRight viewport window =
    { window | position = sub viewport window.size }


bottom : Vec2 -> Plane -> Plane
bottom viewport window =
    { window | position = vec2 (getX window.position) (getY (sub viewport window.size)) }


move : Vec2 -> Plane -> Plane
move v window =
    { window | position = add v window.position }
