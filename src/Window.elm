module Window exposing (..)

import Array exposing (Array, toList)
import Element exposing (Attribute, Element, clip, el, fill, height, htmlAttribute, px, rgb, width)
import Element.Border
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Json.Decode as D
import List.Extra
import Math.Vector2 exposing (Vec2, add, getX, getY, scale, setX, setY, sub, vec2)
import Maybe.Extra exposing (unwrap)
import Window.Area exposing (Area)
import Window.Boundary exposing (Boundary(..), Hit(..), defaultTolerance, getBoundaries, handleRezise, hasHitWindow)


type alias Window =
    Window.Area.Area


type Drag
    = None
    | Reszie Int Boundary
    | Move Int


type alias Model =
    { windows : Array Window
    , order : List Int
    , drag : Drag
    , mousePosition : Vec2
    , mouseOffset : Vec2
    }


init : List Window -> Model
init windowElements =
    { windows = Array.fromList windowElements
    , order = List.range 0 (List.length windowElements - 1)
    , drag = None
    , mousePosition = vec2 0 0
    , mouseOffset = vec2 0 0
    }


empty : Model
empty =
    { windows = Array.empty
    , order = []
    , drag = None
    , mousePosition = vec2 0 0
    , mouseOffset = vec2 0 0
    }


type Msg
    = TrackWindow Int Vec2
    | ResizeWindow Int Boundary
    | StopTrackWindow
    | MouseMove Vec2
    | Focus Int


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ResizeWindow ix dir ->
            ( { model | drag = Reszie ix dir }, Cmd.none )

        TrackWindow ix mp ->
            ( { model
                | drag = Move ix
                , mouseOffset =
                    unwrap (vec2 0 0)
                        (\w -> sub w.position mp)
                        (Array.get ix model.windows)
              }
            , Cmd.none
            )

        StopTrackWindow ->
            ( { model | drag = None }, Cmd.none )

        MouseMove mp ->
            ( { model
                | mousePosition = mp
                , windows = updateWindows model mp
              }
            , Cmd.none
            )

        Focus ix ->
            ( { model
                | order =
                    model.order
                        -- Add selected item to end of stack
                        |> (\zis -> zis ++ [ ix ])
                        -- Uniq
                        |> List.Extra.remove ix
              }
            , Cmd.none
            )



-- Handle moving and resizing


updateWindows : Model -> Vec2 -> Array Window
updateWindows model mp =
    case model.drag of
        None ->
            model.windows

        Move ix ->
            let
                targetWindow =
                    Array.get ix model.windows
            in
            case targetWindow of
                Just wp ->
                    Array.set ix
                        { wp | position = add mp model.mouseOffset }
                        model.windows

                Nothing ->
                    -- Should never happen
                    model.windows

        Reszie ix corner ->
            let
                targetWindow =
                    Array.get ix model.windows

                delta =
                    sub mp model.mousePosition
            in
            case targetWindow of
                Just wp ->
                    handleRezise wp corner delta
                        |> (\w -> Array.set ix w model.windows)

                Nothing ->
                    -- Should never happen
                    model.windows


resizer : (Msg -> msg) -> Boundary -> List (Attribute msg) -> String -> Int -> Element msg
resizer toMsg corner attrs c ix =
    el
        ([ htmlAttribute <| on "pointerdown" (D.succeed (toMsg <| ResizeWindow ix corner))
         , cursor c
         ]
            ++ attrs
        )
        Element.none



--


cursor : String -> Attribute msg
cursor c =
    htmlAttribute (Html.Attributes.style "cursor" c)


pointerEventsNone : Attribute msg
pointerEventsNone =
    htmlAttribute (Html.Attributes.style "pointer-events" "none")


pointerEventsAuto : Attribute msg
pointerEventsAuto =
    htmlAttribute (Html.Attributes.style "pointer-events" "auto")


userSelect : Bool -> List (Element.Attribute msg)
userSelect val =
    if val then
        []

    else
        [ Element.htmlAttribute (style "user-select" "none")
        , Element.htmlAttribute (style "-ms-user-select" "none")
        , Element.htmlAttribute (style "-moz-user-select" "none")
        , Element.htmlAttribute (style "-webkit-user-select" "none")
        , Element.htmlAttribute (style "-webkit-touch-callout" "none")
        ]



-- Helpers


centerOffset : Vec2 -> Vec2 -> Vec2
centerOffset browserWindow window =
    scale 0.5 (sub browserWindow window)


centerX : Vec2 -> Window -> Window
centerX browserWindow window =
    { window | position = setX (getX (centerOffset browserWindow window.size)) window.position }


centerY : Vec2 -> Window -> Window
centerY browserWindow window =
    { window | position = setY (getY (centerOffset browserWindow window.size)) window.position }


center : Vec2 -> Window -> Window
center browserWindow window =
    { window | position = centerOffset browserWindow window.size }


bottomRight : Vec2 -> Window -> Window
bottomRight browserWindow window =
    { window | position = sub browserWindow window.size }


bottom : Vec2 -> Window -> Window
bottom browserWindow window =
    { window
        | position =
            vec2 (getX window.position)
                (getY (sub browserWindow window.size))
    }


move : Vec2 -> Window -> Window
move v window =
    { window
        | position = add v window.position
    }


getWindowHits : Model -> Maybe Hit
getWindowHits model =
    Array.toList model.windows
        |> List.map (hasHitWindow defaultTolerance model.mousePosition)
        |> List.Extra.findMap identity



--


getCursor : Maybe Hit -> String
getCursor mh =
    case mh of
        Nothing ->
            "auto"

        Just (Hit _ mc) ->
            case mc of
                Nothing ->
                    "resize"

                Just c ->
                    case c of
                        Top ->
                            "ns-resize"

                        Right ->
                            "ew-resize"

                        Bottom ->
                            "ns-resize"

                        Left ->
                            "ew-resize"

                        BottomRight ->
                            "se-resize"

                        BottomLeft ->
                            "sw-resize"

                        TopRight ->
                            "ne-resize"

                        TopLeft ->
                            "nw-resize"



-- View


view : (Msg -> msg) -> Model -> List (Int -> Window -> Element msg) -> Element msg
view toMsg model windowElements =
    el
        ([ width fill
         , height fill
         , clip
         , htmlAttribute
            (Html.Events.on "pointerup"
                (D.succeed (toMsg StopTrackWindow))
            )
         , htmlAttribute
            (Html.Events.on "pointermove"
                (D.map2 vec2
                    (D.field "clientX" D.float)
                    (D.field "clientY" D.float)
                    |> D.map (toMsg << MouseMove)
                )
            )
         , htmlAttribute (Html.Attributes.style "touch-action" "none")
         , cursor <| getCursor (getWindowHits model)
         ]
            ++ renderWindows toMsg model windowElements
         -- -- Debug
         -- ++ (withOrder model
         --         |> List.map (uncurry (showBoundaries defaultTol))
         --         |> List.concat
         --    )
        )
        Element.none


renderWindows : (Msg -> msg) -> Model -> List (Int -> Window -> Element msg) -> List (Attribute msg)
renderWindows toMsg model windowElements =
    let
        zipped =
            List.map2
                Tuple.pair
                (Array.toList model.windows
                    |> List.map2 Tuple.pair (getOrder model.order)
                )
                windowElements

        focusedIndex =
            Maybe.withDefault 0 (List.Extra.last model.order)
    in
    List.indexedMap (viewElement toMsg model focusedIndex) zipped


viewElement :
    (Msg -> msg)
    -> Model
    -> Int -- Focused element
    -> Int
    -> ( ( Int, Window ), Int -> Window -> Element msg )
    -> Element.Attribute msg
viewElement toMsg model focusedIndex ix ( ( zindex, window ), renderElement ) =
    Element.inFront
        (el
            ([ Element.moveRight (getX window.position)
             , Element.moveDown (getY window.position)
             , height (px <| round <| getY window.size)
             , width (px <| round <| getX window.size)
             , htmlAttribute
                (Html.Events.on "pointerdown"
                    (D.succeed (toMsg (Focus ix)))
                )
             , htmlAttribute (Html.Attributes.style "z-index" (String.fromInt <| zindex * 10))
             ]
                ++ userSelect (model.drag == None && focusedIndex == ix)
            )
         <|
            renderElement ix window
        )


showBoundaries : Vec2 -> Int -> Area -> List (Attribute msg)
showBoundaries tol zindex window =
    List.indexedMap
        (\ix b ->
            Element.inFront
                (el
                    [ Element.moveRight (getX b.position)
                    , Element.moveDown (getY b.position)
                    , width (px <| round (getX b.size))
                    , height (px <| round (getY b.size))
                    , Element.Border.width 1
                    , Element.Border.color
                        (if ix < 2 then
                            rgb 1 0 0

                         else if ix < 4 then
                            rgb 0 1 0

                         else
                            rgb 0 0 1
                        )
                    , htmlAttribute (Html.Attributes.style "z-index" (String.fromInt <| zindex * 10 + 1))
                    ]
                    Element.none
                )
        )
        (getBoundaries window tol)


getOrder : List Int -> List Int
getOrder listOfIndex =
    listOfIndex
        |> List.indexedMap (\position index -> ( position, index ))
        -- Sort by index so we can zip this with our window elements again
        |> List.sortBy Tuple.second
        -- Get position
        |> List.map Tuple.first


withOrder : Model -> List ( Int, Window )
withOrder m =
    List.Extra.zip (getOrder m.order) (toList m.windows)


sortedByOrder : Model -> List Window
sortedByOrder m =
    withOrder m
        |> List.sortBy Tuple.first
        |> List.map Tuple.second


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry fn ( a, b ) =
    fn a b
