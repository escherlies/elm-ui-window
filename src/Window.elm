module Window exposing (..)

import Array exposing (Array, toList)
import Element exposing (Attribute, Element, clip, el, fill, height, htmlAttribute, px, rgb, width)
import Element.Border
import Html.Attributes exposing (style)
import Html.Events
import Json.Decode as D exposing (index)
import List.Extra
import Math.Vector2 exposing (Vec2, add, getX, getY, scale, setX, setY, sub, vec2)
import Maybe.Extra exposing (unwrap)
import Window.Boundary exposing (Boundary(..), Hit(..), defaultTolerance, getBoundaries, getHit, handleRezise)
import Window.Plane exposing (Plane)


type alias Window =
    Window.Plane.Plane


type Index
    = Index Int


type ZIndex
    = ZIndex Int


type Drag
    = None
    | Reszie Index Boundary
    | Move Index


type alias Model =
    { windows : Array Window
    , order : List Index
    , drag : Drag
    , mousePosition : Vec2
    , mouseOffset : Vec2
    }


init : Model
init =
    { windows = Array.empty
    , order = []
    , drag = None
    , mousePosition = vec2 0 0
    , mouseOffset = vec2 0 0
    }


initWith : List Window -> Model
initWith windowElements =
    { init
        | windows = Array.fromList windowElements
        , order = List.map Index <| List.range 0 (List.length windowElements - 1)
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
    = TrackWindow Index Vec2
    | StopTrackWindow
    | PointerDown
    | MouseMove Vec2


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        PointerDown ->
            handlePointerDown model

        TrackWindow ix mp ->
            ( { model
                | drag = Move ix
                , mouseOffset =
                    unwrap (vec2 0 0)
                        (\w -> sub w.position mp)
                        (Array.get (unwrapIndex ix) model.windows)
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


takeAndAppend : a -> List a -> List a
takeAndAppend x xs =
    xs
        -- Remove x from the list
        |> List.Extra.remove x
        -- Append x to the end
        |> i_ (++) [ x ]


i_ : (c -> b -> a) -> b -> c -> a
i_ fn a b =
    fn b a


handlePointerDown : Model -> ( Model, Cmd msg )
handlePointerDown model =
    getWindowHitsIx model
        |> Maybe.map
            (\( ix, h ) ->
                if model.drag == None then
                    { model
                        | drag =
                            case h of
                                HitArea ->
                                    None

                                HitBoundary b ->
                                    Reszie ix b
                        , order = takeAndAppend ix model.order
                    }

                else
                    { model
                        | order =
                            takeAndAppend ix model.order
                    }
            )
        |> Maybe.map (\m -> ( m, Cmd.none ))
        |> Maybe.withDefault ( model, Cmd.none )



-- Handle moving and resizing


updateWindows : Model -> Vec2 -> Array Window
updateWindows model mp =
    case model.drag of
        None ->
            model.windows

        Move (Index ix) ->
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

        Reszie (Index ix) corner ->
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
    sortedByOrder model
        |> List.map Tuple.second
        |> List.map (getHit defaultTolerance model.mousePosition)
        |> List.Extra.findMap identity


getWindowHitsIx : Model -> Maybe ( Index, Hit )
getWindowHitsIx model =
    sortedByOrder model
        |> List.Extra.findMap
            (\( ix, w ) ->
                Maybe.map (Tuple.pair ix) <| getHit defaultTolerance model.mousePosition w
            )



--


getCursor : Maybe Hit -> String
getCursor mh =
    case mh of
        Nothing ->
            "auto"

        Just h ->
            case h of
                HitArea ->
                    "auto"

                HitBoundary b ->
                    case b of
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
view toMsg model windowsContent =
    el
        ([ width fill
         , height fill
         , clip
         , htmlAttribute
            (Html.Events.stopPropagationOn "pointerdown"
                (D.succeed ( toMsg PointerDown, True ))
            )
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
            ++ renderWindows model windowsContent
         -- -- Debug
         -- ++ (withOrder model
         --         |> List.map (uncurry (showBoundaries defaultTolerance))
         --         |> List.concat
         --    )
        )
        Element.none


renderWindows : Model -> List (Int -> Window -> Element msg) -> List (Attribute msg)
renderWindows model elements =
    let
        focusedIndex =
            Maybe.withDefault (Index 0) (List.Extra.last model.order)

        window =
            List.map2 (getRenderElement focusedIndex) (withOrderIndexed model) elements
    in
    List.map (viewWindow model) window


getRenderElement : Index -> ( Index, ZIndex, Window ) -> (Int -> Window -> Element msg) -> RenderWindow msg
getRenderElement focusedIndex ( index, zindex, window ) render =
    { index = index
    , zIndex = zindex
    , window = window
    , isFocused = focusedIndex == index
    , render = render
    }


type alias RenderWindow msg =
    { index : Index
    , zIndex : ZIndex
    , window : Window
    , isFocused : Bool
    , render : Int -> Window -> Element msg
    }


viewWindow :
    Model
    -> RenderWindow msg
    -> Element.Attribute msg
viewWindow model { index, zIndex, window, isFocused, render } =
    Element.inFront
        (el
            ([ Element.moveRight (getX window.position)
             , Element.moveDown (getY window.position)
             , height (px <| round <| getY window.size)
             , width (px <| round <| getX window.size)
             , htmlAttribute (Html.Attributes.style "z-index" (String.fromInt <| unwrapZindex zIndex * 10))
             ]
                ++ userSelect (model.drag == None && isFocused)
            )
         <|
            render (unwrapIndex index) window
        )


showBoundaries : Vec2 -> ZIndex -> Plane -> List (Attribute msg)
showBoundaries tol (ZIndex zindex) window =
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



-- Helpers


isFocusedIndex : Index -> Index -> Bool
isFocusedIndex (Index a) (Index b) =
    a == b


curry : ( a, b ) -> (a -> b -> c) -> c
curry ( a, b ) fn =
    fn a b


composey : (a -> a -> b) -> (c -> a) -> c -> c -> b
composey f g x y =
    f (g x) (g y)


unwrapIndex : Index -> Int
unwrapIndex (Index ix) =
    ix


unwrapZindex : ZIndex -> Int
unwrapZindex (ZIndex zix) =
    zix


getOrder : List Index -> List ZIndex
getOrder listOfIndex =
    listOfIndex
        -- The index of this list is the zIndex
        |> List.indexedMap Tuple.pair
        |> List.map (Tuple.mapFirst ZIndex)
        -- Sort by index so we can zip this with our window elements again
        |> List.sortBy (unwrapIndex << Tuple.second)
        -- Get z index
        |> List.map Tuple.first


withOrder : Model -> List ( ZIndex, Window )
withOrder m =
    List.Extra.zip
        (getOrder m.order)
        (toList m.windows)


withOrderIndexed : Model -> List ( Index, ZIndex, Window )
withOrderIndexed m =
    List.Extra.zip
        (getOrder m.order)
        (toList m.windows)
        |> List.indexedMap (\ix ( zindex, window ) -> ( Index ix, zindex, window ))


sortedByOrder : Model -> List ( Index, Window )
sortedByOrder m =
    withOrder m
        |> List.indexedMap (\ix ( zix, w ) -> ( Index ix, zix, w ))
        |> List.sortBy (\( _, ZIndex zix, _ ) -> zix)
        |> List.map (\( ix, _, w ) -> ( ix, w ))
        |> List.reverse


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry fn ( a, b ) =
    fn a b
