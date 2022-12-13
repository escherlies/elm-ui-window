module Area.Boundary exposing (..)

import Area exposing (Area, addXof, addYof, isInArea, vec2uni)
import Math.Vector2 exposing (Vec2, add, getX, getY, scale, setX, setY, sub)


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


getHit : Vec2 -> Vec2 -> Area -> Maybe Hit
getHit tol mp w =
    if isInArea (withTolerance tol w) mp then
        Maybe.map HitBoundary (hitBoundary w tol mp)
            |> Maybe.withDefault HitArea
            |> Just

    else
        Nothing



-- Check boundary


hitBoundary : Area -> Vec2 -> Vec2 -> Maybe Boundary
hitBoundary w tol mp =
    let
        isAt areaFn =
            isInArea (areaFn w tol) mp
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


withTolerance : Vec2 -> Area -> Area
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


topLeft : Area -> Vec2 -> Area
topLeft w tol =
    { position = sub w.position tol
    , size = cornerSize tol
    }


topRight : Area -> Vec2 -> Area
topRight w tol =
    { position = sub w.position tol |> addXof w.size
    , size = cornerSize tol
    }


bottomLeft : Area -> Vec2 -> Area
bottomLeft w tol =
    { position = sub w.position tol |> addYof w.size
    , size = cornerSize tol
    }


bottomRight : Area -> Vec2 -> Area
bottomRight w tol =
    { position = sub w.position tol |> add w.size
    , size = cornerSize tol
    }



-- Edges


top : Area -> Vec2 -> Area
top w tol =
    { position = sub w.position tol
    , size = cornerSize tol |> addXof w.size
    }


bottom : Area -> Vec2 -> Area
bottom w tol =
    { position = sub w.position tol |> addYof w.size
    , size = cornerSize tol |> addXof w.size
    }


left : Area -> Vec2 -> Area
left w tol =
    { position = sub w.position tol
    , size = cornerSize tol |> addYof w.size
    }


right : Area -> Vec2 -> Area
right w tol =
    { position = sub w.position tol |> addXof w.size
    , size = cornerSize tol |> addYof w.size
    }



--


{-| Get all boundaries for a given window

Maybe helpful to display boundaries for debugging

-}
getBoundaries : Area -> Vec2 -> List Area
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
handleRezise : Area -> Boundary -> Vec2 -> Area
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
