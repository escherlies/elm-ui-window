module Window.Resize exposing (..)

import Math.Vector2 exposing (Vec2, add, getX, getY, scale, setX, setY, sub)
import Window.Internal exposing (Window)
import Window.Vec2 exposing (addXof, addYof, isInArea, vec2uni)


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
    = Hit Int (Maybe Boundary)


hasHitWindow : Vec2 -> Window -> Maybe Hit
hasHitWindow mp w =
    let
        tol =
            vec2uni 5
    in
    if isInArea (withTolerance tol w) mp then
        hitBoundary w tol mp

    else
        Nothing



-- Check boundary


hitBoundary : { position : Vec2, size : Vec2 } -> Vec2 -> Vec2 -> Maybe Hit
hitBoundary w tol mp =
    let
        isIn areaFn =
            isInArea (areaFn w tol) mp
    in
    if isIn topLeftCorner then
        Just (Hit 0 (Just TopLeft))

    else if isIn topRightCorner then
        Just (Hit 0 (Just TopRight))

    else if isIn bottomLeftCorner then
        Just (Hit 0 (Just BottomLeft))

    else if isIn bottomRightCorner then
        Just (Hit 0 (Just BottomRight))

    else if isIn topEdge then
        Just (Hit 0 (Just Top))

    else if isIn leftEdge then
        Just (Hit 0 (Just Left))

    else if isIn rightEdge then
        Just (Hit 0 (Just Right))

    else if isIn bottomEdge then
        Just (Hit 0 (Just Bottom))

    else
        Nothing



--


withTolerance : Vec2 -> { a | position : Vec2, size : Vec2 } -> { position : Vec2, size : Vec2 }
withTolerance tol { position, size } =
    { position = sub position tol, size = add size tol }


cornerSize : Vec2 -> Vec2
cornerSize tol =
    scale 2 tol



-- Corners


topLeftCorner : { a | position : Vec2 } -> Vec2 -> { position : Vec2, size : Vec2 }
topLeftCorner w tol =
    { position = sub w.position tol
    , size = cornerSize tol
    }


topRightCorner : { a | position : Vec2, size : Vec2 } -> Vec2 -> { position : Vec2, size : Vec2 }
topRightCorner w tol =
    { position = sub w.position tol |> addXof w.size
    , size = cornerSize tol
    }


bottomLeftCorner : { a | position : Vec2, size : Vec2 } -> Vec2 -> { position : Vec2, size : Vec2 }
bottomLeftCorner w tol =
    { position = sub w.position tol |> addYof w.size
    , size = cornerSize tol
    }


bottomRightCorner : { a | position : Vec2, size : Vec2 } -> Vec2 -> { position : Vec2, size : Vec2 }
bottomRightCorner w tol =
    { position = sub w.position tol |> add w.size
    , size = cornerSize tol
    }



-- Edges


topEdge : { a | position : Vec2, size : Vec2 } -> Vec2 -> { position : Vec2, size : Vec2 }
topEdge w tol =
    { position = sub w.position tol
    , size = cornerSize tol |> addXof (add w.size (cornerSize tol))
    }


bottomEdge : { a | position : Vec2, size : Vec2 } -> Vec2 -> { position : Vec2, size : Vec2 }
bottomEdge w tol =
    { position = sub w.position tol |> addYof w.size
    , size = cornerSize tol |> addXof (add w.size (cornerSize tol))
    }


leftEdge : { a | position : Vec2, size : Vec2 } -> Vec2 -> { position : Vec2, size : Vec2 }
leftEdge w tol =
    { position = sub w.position tol
    , size = cornerSize tol |> addYof (add w.size (cornerSize tol))
    }


rightEdge : { a | position : Vec2, size : Vec2 } -> Vec2 -> { position : Vec2, size : Vec2 }
rightEdge w tol =
    { position = sub w.position tol |> addXof w.size
    , size = cornerSize tol |> addYof (add w.size (cornerSize tol))
    }



--


{-| Add the delta of the movement to the window
-}
handleRezise : Window -> Boundary -> Vec2 -> Window
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
