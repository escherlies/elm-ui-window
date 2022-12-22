module Window exposing (Model, Msg, Window, init, initWith, mapPlane, onDrag, update, updatePlanes, view)

import Array exposing (Array, toList)
import Element exposing (Attribute, Element, clip, el, fill, height, htmlAttribute, px, width)
import Html.Attributes
import Html.Events
import Json.Decode as D exposing (Decoder, index)
import List.Extra
import Math.Vector2 exposing (Vec2, add, getX, getY, sub, vec2)
import Maybe.Extra exposing (unwrap)
import Window.Boundary exposing (Boundary(..), Hit(..), defaultTolerance, getHit, handleRezise)
import Window.Elements exposing (cursor, showAnchorPoint, userSelect)
import Window.Plane exposing (Plane)
import Window.Utils exposing (apply, takeAndAppend, uncurry)


type alias Window msg =
    { plane : Plane
    , render : (Msg -> msg) -> Int -> Plane -> Element msg
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
initWith =
    handleUpdatePlanes init << List.map .plane


{-| Update your windows if you need to. Use it like

    updatePlanes (List.map .plane windows)

-}
updatePlanes : List Plane -> Msg
updatePlanes =
    UpdatePlanes


type Msg
    = TrackWindow Index Vec2
    | StopTrackWindow
    | PointerDown Vec2
    | MouseMove Vec2
    | UpdatePlanes (List Plane)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        UpdatePlanes ws ->
            ( handleUpdatePlanes model ws
            , Cmd.none
            )

        PointerDown mp ->
            handlePointerDown { model | mousePosition = mp }

        TrackWindow ix mp ->
            ( { model
                | drag = Move ix
                , mouseOffset =
                    unwrap (vec2 0 0)
                        (\w -> sub w.position mp)
                        (Array.get (unwrapIndex ix) model.planes)
                , order = takeAndAppend ix model.order
              }
            , Cmd.none
            )

        StopTrackWindow ->
            ( { model | drag = None }, Cmd.none )

        MouseMove mp ->
            ( { model
                | mousePosition = mp
                , planes = manipulatePlanes model mp
              }
            , Cmd.none
            )


handleUpdatePlanes : Model -> List Plane -> Model
handleUpdatePlanes model planes =
    { model
        | planes = Array.fromList planes
        , order = List.map Index <| List.range 0 (List.length planes - 1)
    }


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


manipulatePlanes : Model -> Vec2 -> Array Plane
manipulatePlanes model mp =
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


mapPointerPosition : (Vec2 -> value) -> Decoder value
mapPointerPosition msg =
    D.map2 vec2
        (D.field "clientX" D.float)
        (D.field "clientY" D.float)
        |> D.map msg


view : (Msg -> msg) -> { a | showAnchorPoints : Bool } -> Model -> List (Window msg) -> Element msg
view toMsg opts model windows =
    el
        ([ width fill
         , height fill
         , clip
         , htmlAttribute
            (Html.Events.on "pointerdown"
                (mapPointerPosition (toMsg << PointerDown))
            )
         , htmlAttribute
            (Html.Events.on "pointerup"
                (D.succeed (toMsg StopTrackWindow))
            )
         , htmlAttribute
            (Html.Events.on "pointermove"
                (mapPointerPosition (toMsg << MouseMove))
            )
         , htmlAttribute (Html.Attributes.style "touch-action" "none")
         , cursor <| getCursor (getPlaneHits model)
         ]
            ++ renderWindows model (List.map (apply toMsg << .render) windows)
            -- Show anchor points
            ++ (if opts.showAnchorPoints then
                    withOrder model
                        |> List.map (Tuple.mapFirst unwrapZindex)
                        |> List.map (uncurry (showAnchorPoint defaultTolerance))
                        |> List.concat

                else
                    []
               )
        )
        Element.none


trackWindow : (Msg -> msg) -> Int -> Vec2 -> msg
trackWindow toMsg ix =
    toMsg << TrackWindow (Index ix)


{-| Use this in you render element to allow for dragging.

For example, use this as an attribute to your window header/title bar to drag the windows around.

-}
onDrag : (Msg -> msg) -> Int -> Attribute msg
onDrag toMsg ix =
    htmlAttribute
        (Html.Events.on "pointerdown"
            (D.map (trackWindow toMsg ix)
                (D.map2 vec2
                    (D.field "clientX" D.float)
                    (D.field "clientY" D.float)
                )
            )
        )


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
    , plane : Plane
    , isFocused : Bool
    , render : Int -> Plane -> Element msg
    }


getRenderElement : Index -> ( Index, ZIndex, Plane ) -> (Int -> Plane -> Element msg) -> WindowRender msg
getRenderElement focusedIndex ( index, zindex, window ) render =
    { index = index
    , zIndex = zindex
    , plane = window
    , isFocused = focusedIndex == index
    , render = render
    }


viewWindow :
    Model
    -> WindowRender msg
    -> Element.Attribute msg
viewWindow model { index, zIndex, plane, isFocused, render } =
    Element.inFront
        (el
            ([ Element.moveRight (getX plane.position)
             , Element.moveDown (getY plane.position)
             , height (px <| round <| getY plane.size)
             , width (px <| round <| getX plane.size)
             , htmlAttribute (Html.Attributes.style "z-index" (String.fromInt <| unwrapZindex zIndex * 10))
             ]
                ++ userSelect (model.drag == None && isFocused)
             -- TODO: Prevent opening a link clicked in a background window
             -- Adding pointerEventsNone to `userSelect False` has currently no effect
             -- since pointer down immediatly focuses the element
             -- which sets thes isFocusedState to true
            )
         <|
            render (unwrapIndex index) plane
        )



-- Helpers


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


mapPlane : (Plane -> Plane) -> Window msg -> Window msg
mapPlane fn w =
    { w | plane = fn w.plane }
