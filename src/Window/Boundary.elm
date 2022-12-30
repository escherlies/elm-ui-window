module Window.Boundary exposing (..)

import Math.Vector2 exposing (Vec, add, getX, getY, scale, setX, setY, sub)
import Window.Rect exposing (Rect, isOnRect)
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


type Resize
    = ShowAnchorPoints
    | HideAnchorPoints
    | DisableResize



-- Hit


type Hit
    = HitArea
    | HitBoundary Boundary


getHit : Vec Float -> Vec Float -> Rect -> Maybe Hit
getHit tol mp w =
    if isOnRect (withTolerance tol w) mp then
        Maybe.map HitBoundary (hitBoundary w tol mp)
            |> Maybe.withDefault HitArea
            |> Just

    else
        Nothing



-- Check boundary


hitBoundary : Rect -> Vec Float -> Vec Float -> Maybe Boundary
hitBoundary w tol mp =
    let
        isAt areaFn =
            isOnRect (areaFn w tol) mp
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


withTolerance : Vec Float -> Rect -> Rect
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


topLeft : Rect -> Vec Float -> Rect
topLeft w tol =
    { position = sub w.position tol
    , size = cornerSize tol
    }


topRight : Rect -> Vec Float -> Rect
topRight w tol =
    { position = sub w.position tol |> addXof w.size
    , size = cornerSize tol
    }


bottomLeft : Rect -> Vec Float -> Rect
bottomLeft w tol =
    { position = sub w.position tol |> addYof w.size
    , size = cornerSize tol
    }


bottomRight : Rect -> Vec Float -> Rect
bottomRight w tol =
    { position = sub w.position tol |> add w.size
    , size = cornerSize tol
    }



-- Edges


top : Rect -> Vec Float -> Rect
top w tol =
    { position = sub w.position tol
    , size = cornerSize tol |> addXof w.size
    }


bottom : Rect -> Vec Float -> Rect
bottom w tol =
    { position = sub w.position tol |> addYof w.size
    , size = cornerSize tol |> addXof w.size
    }


left : Rect -> Vec Float -> Rect
left w tol =
    { position = sub w.position tol
    , size = cornerSize tol |> addYof w.size
    }


right : Rect -> Vec Float -> Rect
right w tol =
    { position = sub w.position tol |> addXof w.size
    , size = cornerSize tol |> addYof w.size
    }



-- Resize helpers


half : Vec Float -> Vec Float
half =
    scale (1 / 2)


topCenter : Rect -> Vec Float -> Rect
topCenter w tol =
    { position = setXof (sub (half w.size) tol |> add w.position) (sub w.position tol)
    , size = cornerSize tol
    }


leftCenter : Rect -> Vec Float -> Rect
leftCenter w tol =
    { position = setYof (sub (half w.size) tol |> add w.position) (sub w.position tol)
    , size = cornerSize tol
    }


bottomCenter : Rect -> Vec Float -> Rect
bottomCenter w tol =
    { position = setXof (sub (half w.size) tol |> add w.position) (add w.position w.size |> flip sub tol)
    , size = cornerSize tol
    }


rightCenter : Rect -> Vec Float -> Rect
rightCenter w tol =
    { position = setYof (sub (half w.size) tol |> add w.position) (add w.position w.size |> flip sub tol)
    , size = cornerSize tol
    }



--


{-| Get all boundaries for a given window

Maybe helpful to display boundaries for debugging

-}
getBoundaries : Rect -> Vec Float -> List Rect
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
getAnchorPoints : Rect -> Vec Float -> List Rect
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
handleRezise : Rect -> Boundary -> Vec Float -> Rect
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
