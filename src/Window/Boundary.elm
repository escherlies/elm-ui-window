module Window.Boundary exposing (..)

import Math.Vector2 exposing (Vec, add, getX, getY, scale, setX, setY, sub)
import Window.Plane exposing (Plane, isOnPlane)
import Window.Utils exposing (addXof, addYof, flip, setXof, setYof, vec2uni)


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


getHit : Vec Float -> Vec Float -> Plane -> Maybe Hit
getHit tol mp w =
    if isOnPlane (withTolerance tol w) mp then
        Maybe.map HitBoundary (hitBoundary w tol mp)
            |> Maybe.withDefault HitArea
            |> Just

    else
        Nothing



-- Check boundary


hitBoundary : Plane -> Vec Float -> Vec Float -> Maybe Boundary
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


withTolerance : Vec Float -> Plane -> Plane
withTolerance tol { position, size } =
    { position = sub position tol
    , size = add size (cornerSize tol)
    }


defaultTolerance : Vec Float
defaultTolerance =
    vec2uni 6


cornerSize : Vec Float -> Vec Float
cornerSize tol =
    scale 2 tol



-- Corners


topLeft : Plane -> Vec Float -> Plane
topLeft w tol =
    { position = sub w.position tol
    , size = cornerSize tol
    }


topRight : Plane -> Vec Float -> Plane
topRight w tol =
    { position = sub w.position tol |> addXof w.size
    , size = cornerSize tol
    }


bottomLeft : Plane -> Vec Float -> Plane
bottomLeft w tol =
    { position = sub w.position tol |> addYof w.size
    , size = cornerSize tol
    }


bottomRight : Plane -> Vec Float -> Plane
bottomRight w tol =
    { position = sub w.position tol |> add w.size
    , size = cornerSize tol
    }



-- Edges


top : Plane -> Vec Float -> Plane
top w tol =
    { position = sub w.position tol
    , size = cornerSize tol |> addXof w.size
    }


bottom : Plane -> Vec Float -> Plane
bottom w tol =
    { position = sub w.position tol |> addYof w.size
    , size = cornerSize tol |> addXof w.size
    }


left : Plane -> Vec Float -> Plane
left w tol =
    { position = sub w.position tol
    , size = cornerSize tol |> addYof w.size
    }


right : Plane -> Vec Float -> Plane
right w tol =
    { position = sub w.position tol |> addXof w.size
    , size = cornerSize tol |> addYof w.size
    }



-- Resize helpers


half : Vec Float -> Vec Float
half =
    scale (1 / 2)


topCenter : Plane -> Vec Float -> Plane
topCenter w tol =
    { position = setXof (sub (half w.size) tol |> add w.position) (sub w.position tol)
    , size = cornerSize tol
    }


leftCenter : Plane -> Vec Float -> Plane
leftCenter w tol =
    { position = setYof (sub (half w.size) tol |> add w.position) (sub w.position tol)
    , size = cornerSize tol
    }


bottomCenter : Plane -> Vec Float -> Plane
bottomCenter w tol =
    { position = setXof (sub (half w.size) tol |> add w.position) (add w.position w.size |> flip sub tol)
    , size = cornerSize tol
    }


rightCenter : Plane -> Vec Float -> Plane
rightCenter w tol =
    { position = setYof (sub (half w.size) tol |> add w.position) (add w.position w.size |> flip sub tol)
    , size = cornerSize tol
    }



--


{-| Get all boundaries for a given window

Maybe helpful to display boundaries for debugging

-}
getBoundaries : Plane -> Vec Float -> List Plane
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


{-| Get all anchor points for a given window
-}
getAnchorPoints : Plane -> Vec Float -> List Plane
getAnchorPoints w tol =
    [ topCenter w tol
    , bottomCenter w tol
    , leftCenter w tol
    , rightCenter w tol
    , topLeft w tol
    , topRight w tol
    , bottomLeft w tol
    , bottomRight w tol
    ]



--


min : number
min =
    20


{-| Add the delta of the movement to the window
-}
handleRezise : Plane -> Boundary -> Vec Float -> Plane
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
                if getX w.size < min then
                    { w
                        | size = setX min w.size
                        , position = setX (getX wp.position) w.position
                    }

                else
                    w
           )
        |> (\w ->
                if getY w.size < min then
                    { w
                        | size = setY min w.size
                        , position = setY (getY wp.position) w.position
                    }

                else
                    w
           )



-- Debug view
