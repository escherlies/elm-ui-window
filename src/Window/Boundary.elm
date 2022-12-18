module Window.Boundary exposing (..)

import Math.Vector2 exposing (Vec2, add, getX, getY, scale, setX, setY, sub)
import Window.Plane exposing (Plane, isOnPlane)
import Window.Utils exposing (addXof, addYof, vec2uni)


type Boundary
    = Top
    | Right
    | Bottom
    | Left
    | BottomRight
    | BottomLeft
    | TopRight
    | TopLeft



-- Hit


type Hit
    = HitArea
    | HitBoundary Boundary


getHit : Vec2 -> Vec2 -> Plane -> Maybe Hit
getHit tol mp w =
    if isOnPlane (withTolerance tol w) mp then
        Maybe.map HitBoundary (hitBoundary w tol mp)
            |> Maybe.withDefault HitArea
            |> Just

    else
        Nothing



-- Check boundary


hitBoundary : Plane -> Vec2 -> Vec2 -> Maybe Boundary
hitBoundary w tol mp =
    let
        isAt areaFn =
            isOnPlane (areaFn w tol) mp
    in
    if isAt topLeft then
        Just TopLeft

    else if isAt topRight then
        Just TopRight

    else if isAt bottomLeft then
        Just BottomLeft

    else if isAt bottomRight then
        Just BottomRight

    else if isAt top then
        Just Top

    else if isAt left then
        Just Left

    else if isAt right then
        Just Right

    else if isAt bottom then
        Just Bottom

    else
        Nothing



--


withTolerance : Vec2 -> Plane -> Plane
withTolerance tol { position, size } =
    { position = sub position tol
    , size = add size (cornerSize tol)
    }


defaultTolerance : Vec2
defaultTolerance =
    vec2uni 6


cornerSize : Vec2 -> Vec2
cornerSize tol =
    scale 2 tol



-- Corners


topLeft : Plane -> Vec2 -> Plane
topLeft w tol =
    { position = sub w.position tol
    , size = cornerSize tol
    }


topRight : Plane -> Vec2 -> Plane
topRight w tol =
    { position = sub w.position tol |> addXof w.size
    , size = cornerSize tol
    }


bottomLeft : Plane -> Vec2 -> Plane
bottomLeft w tol =
    { position = sub w.position tol |> addYof w.size
    , size = cornerSize tol
    }


bottomRight : Plane -> Vec2 -> Plane
bottomRight w tol =
    { position = sub w.position tol |> add w.size
    , size = cornerSize tol
    }



-- Edges


top : Plane -> Vec2 -> Plane
top w tol =
    { position = sub w.position tol
    , size = cornerSize tol |> addXof w.size
    }


bottom : Plane -> Vec2 -> Plane
bottom w tol =
    { position = sub w.position tol |> addYof w.size
    , size = cornerSize tol |> addXof w.size
    }


left : Plane -> Vec2 -> Plane
left w tol =
    { position = sub w.position tol
    , size = cornerSize tol |> addYof w.size
    }


right : Plane -> Vec2 -> Plane
right w tol =
    { position = sub w.position tol |> addXof w.size
    , size = cornerSize tol |> addYof w.size
    }



--


{-| Get all boundaries for a given window

Maybe helpful to display boundaries for debugging

-}
getBoundaries : Plane -> Vec2 -> List Plane
getBoundaries w tol =
    [ top w tol
    , bottom w tol
    , left w tol
    , right w tol
    , topLeft w tol
    , topRight w tol
    , bottomLeft w tol
    , bottomRight w tol
    ]



--


{-| Add the delta of the movement to the window
-}
handleRezise : Plane -> Boundary -> Vec2 -> Plane
handleRezise wp corner delta =
    (case corner of
        Bottom ->
            { wp
                | size = add wp.size (setX 0 delta)
            }

        Top ->
            { wp
                | size = add wp.size (setX 0 delta |> scale -1)
                , position = add wp.position (setX 0 delta)
            }

        Right ->
            { wp
                | size = add wp.size (setY 0 delta)
            }

        Left ->
            { wp
                | size = add wp.size (setY 0 delta |> scale -1)
                , position = add wp.position (setY 0 delta)
            }

        BottomRight ->
            { wp
                | size = add wp.size delta
            }

        BottomLeft ->
            { wp
                | size =
                    add wp.size (setY 0 delta |> scale -1)
                        |> (\size -> add size (setX 0 delta))
                , position = add wp.position (setY 0 delta)
            }

        TopRight ->
            { wp
                | size =
                    add wp.size (setX 0 delta |> scale -1)
                        |> (\size -> add size (setY 0 delta))
                , position = add wp.position (setX 0 delta)
            }

        TopLeft ->
            { wp
                | size = add wp.size (delta |> scale -1)
                , position = add wp.position delta
            }
    )
        |> (\w ->
                if getX w.size < 100 then
                    { w
                        | size = setX 100 w.size
                        , position = setX (getX wp.position) w.position
                    }

                else
                    w
           )
        |> (\w ->
                if getY w.size < 100 then
                    { w
                        | size = setY 100 w.size
                        , position = setY (getY wp.position) w.position
                    }

                else
                    w
           )



-- Debug view
