module Window exposing (..)

import Array exposing (Array)
import Element exposing (Attribute, Element, clip, el, fill, height, htmlAttribute, moveDown, moveLeft, moveRight, moveUp, px, row, width)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Json.Decode as D
import List.Extra
import Math.Vector2 exposing (Vec2, add, getX, getY, scale, setX, setY, sub, vec2)
import Maybe.Extra exposing (unwrap)


type alias Window =
    { position : Vec2
    , size : Vec2
    }


type Corner
    = Top
    | Right
    | Bottom
    | Left
    | BottomRight
    | BottomLeft
    | TopRight
    | TopLeft


type Drag
    = None
    | Reszie Int Corner
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
    | ResizeWindow Int Corner
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


handleRezise : Window -> Corner -> Vec2 -> Window
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


resizer : (Msg -> msg) -> Corner -> List (Attribute msg) -> String -> Int -> Element msg
resizer toMsg corner attrs c ix =
    el
        ([ htmlAttribute <|
            on "pointerdown"
                (D.succeed (toMsg <| ResizeWindow ix corner))
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


zero : Vec2
zero =
    vec2 0 0



-- View


viewElement :
    (Msg -> msg)
    -> Model
    -> Int -- Focused element
    -> Int
    -> ( ( Int, Window ), Element msg )
    -> Element.Attribute msg
viewElement toMsg model focusedIndex ix ( ( zindex, position ), content ) =
    let
        bw =
            3

        overhang =
            0

        rs =
            bw
    in
    Element.inFront
        (el
            ([ Element.moveRight (getX position.position)
             , Element.moveDown (getY position.position)
             , height (px <| round <| getY position.size)
             , width (px <| round <| getX position.size)
             , Element.onLeft
                (resizer toMsg
                    Left
                    [ height fill
                    , width (px rs)
                    , moveRight rs
                    ]
                    "ew-resize"
                    ix
                )
             , Element.onRight
                (resizer toMsg
                    Right
                    [ height fill
                    , width (px rs)
                    , moveLeft rs
                    ]
                    "ew-resize"
                    ix
                )
             , Element.above
                (row [ width fill, moveDown (bw + overhang) ]
                    [ resizer toMsg TopLeft [ height (px rs), width (px rs) ] "nw-resize" ix
                    , resizer toMsg Top [ height (px rs), width fill ] "ns-resize" ix
                    , resizer toMsg TopRight [ height (px rs), width (px rs) ] "ne-resize" ix
                    ]
                )
             , Element.below
                (row [ width fill, moveUp (bw + overhang) ]
                    [ resizer toMsg BottomLeft [ height (px rs), width (px rs) ] "sw-resize" ix
                    , resizer toMsg Bottom [ height (px rs), width fill ] "ns-resize" ix
                    , resizer toMsg BottomRight [ height (px rs), width (px rs) ] "se-resize" ix
                    ]
                )
             , htmlAttribute
                (Html.Events.on "pointerdown"
                    (D.succeed (toMsg (Focus ix)))
                )
             , htmlAttribute (Html.Attributes.style "z-index" (String.fromInt <| zindex * 10))
             ]
                ++ userSelect (model.drag == None && focusedIndex == ix)
            )
         <|
            content
        )


view : (Msg -> msg) -> Model -> List ( c, Element msg ) -> Element msg
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
         ]
            ++ renderWindows toMsg model windowElements
        )
        Element.none


renderWindows : (Msg -> msg) -> Model -> List ( c, Element msg ) -> List (Attribute msg)
renderWindows toMsg model windowElements =
    let
        zipped =
            List.map2
                Tuple.pair
                (Array.toList model.windows
                    |> List.map2 Tuple.pair (getOrder model.order)
                )
                (List.map Tuple.second windowElements)

        focusedIndex =
            Maybe.withDefault 0 (List.Extra.last model.order)
    in
    List.indexedMap (viewElement toMsg model focusedIndex) zipped


getOrder : List Int -> List Int
getOrder listOfIndex =
    listOfIndex
        |> List.indexedMap (\position index -> ( position, index ))
        -- Sort by index so we can zip this with our window elements again
        |> List.sortBy Tuple.second
        -- Get position
        |> List.map Tuple.first
