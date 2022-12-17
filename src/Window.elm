module Window exposing (..)

import Array exposing (Array, toList)
import Element exposing (Attribute, Element, clip, el, fill, height, htmlAttribute, px, rgb, width)
import Element.Border
import Html.Attributes exposing (style)
import Html.Events
import Json.Decode as D exposing (index)
import List.Extra
import Math.Vector2 exposing (Vec2, add, getX, getY, sub, vec2)
import Maybe.Extra exposing (unwrap)
import Window.Boundary exposing (Boundary(..), Hit(..), defaultTolerance, getBoundaries, getHit, handleRezise)
import Window.Plane exposing (Plane)


type alias Window msg =
    { plane : Plane
    , render : Int -> Plane -> Element msg
    }


type Index
    = Index Int


type ZIndex
    = ZIndex Int


type Drag
    = None
    | Reszie Index Boundary
    | Move Index


type alias Model =
    { planes : Array Plane
    , order : List Index
    , drag : Drag
    , mousePosition : Vec2
    , mouseOffset : Vec2
    }


init : Model
init =
    { planes = Array.empty
    , order = []
    , drag = None
    , mousePosition = vec2 0 0
    , mouseOffset = vec2 0 0
    }


initWith : List (Window msg) -> Model
initWith windowElements =
    { init
        | planes = Array.fromList <| List.map .plane windowElements
        , order = List.map Index <| List.range 0 (List.length windowElements - 1)
    }


empty : Model
empty =
    { planes = Array.empty
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
                        (Array.get (unwrapIndex ix) model.planes)
              }
            , Cmd.none
            )

        StopTrackWindow ->
            ( { model | drag = None }, Cmd.none )

        MouseMove mp ->
            ( { model
                | mousePosition = mp
                , planes = updatePlanes model mp
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
    getPlaneHitsIx model
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


updatePlanes : Model -> Vec2 -> Array Plane
updatePlanes model mp =
    case model.drag of
        None ->
            model.planes

        Move (Index ix) ->
            let
                targetWindow =
                    Array.get ix model.planes
            in
            case targetWindow of
                Just wp ->
                    Array.set ix
                        { wp | position = add mp model.mouseOffset }
                        model.planes

                Nothing ->
                    -- Should never happen
                    model.planes

        Reszie (Index ix) corner ->
            let
                targetWindow =
                    Array.get ix model.planes

                delta =
                    sub mp model.mousePosition
            in
            case targetWindow of
                Just wp ->
                    handleRezise wp corner delta
                        |> (\w -> Array.set ix w model.planes)

                Nothing ->
                    -- Should never happen
                    model.planes



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


getPlaneHits : Model -> Maybe Hit
getPlaneHits model =
    sortedByOrder model
        |> List.map Tuple.second
        |> List.map (getHit defaultTolerance model.mousePosition)
        |> List.Extra.findMap identity


getPlaneHitsIx : Model -> Maybe ( Index, Hit )
getPlaneHitsIx model =
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


view : (Msg -> msg) -> Model -> List (Int -> Plane -> Element msg) -> Element msg
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
         , cursor <| getCursor (getPlaneHits model)
         ]
            ++ renderWindows model windowsContent
         -- -- Debug
         -- ++ (withOrder model
         --         |> List.map (uncurry (showBoundaries defaultTolerance))
         --         |> List.concat
         --    )
        )
        Element.none


renderWindows : Model -> List (Int -> Plane -> Element msg) -> List (Attribute msg)
renderWindows model elements =
    let
        focusedIndex =
            Maybe.withDefault (Index 0) (List.Extra.last model.order)

        window =
            List.map2 (getRenderElement focusedIndex) (withOrderIndexed model) elements
    in
    List.map (viewWindow model) window



-- Window Elements


type alias WindowRender msg =
    { index : Index
    , zIndex : ZIndex
    , window : Plane
    , isFocused : Bool
    , render : Int -> Plane -> Element msg
    }


getRenderElement : Index -> ( Index, ZIndex, Plane ) -> (Int -> Plane -> Element msg) -> WindowRender msg
getRenderElement focusedIndex ( index, zindex, window ) render =
    { index = index
    , zIndex = zindex
    , window = window
    , isFocused = focusedIndex == index
    , render = render
    }


viewWindow :
    Model
    -> WindowRender msg
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


withOrder : Model -> List ( ZIndex, Plane )
withOrder m =
    List.Extra.zip (getOrder m.order) (toList m.planes)


withOrderIndexed : Model -> List ( Index, ZIndex, Plane )
withOrderIndexed m =
    List.Extra.zip (getOrder m.order) (toList m.planes)
        |> List.indexedMap (\ix ( zindex, window ) -> ( Index ix, zindex, window ))


sortedByOrder : Model -> List ( Index, Plane )
sortedByOrder m =
    withOrder m
        |> List.indexedMap (\ix ( zix, w ) -> ( Index ix, zix, w ))
        |> List.sortBy (\( _, ZIndex zix, _ ) -> zix)
        |> List.map (\( ix, _, w ) -> ( ix, w ))
        |> List.reverse
