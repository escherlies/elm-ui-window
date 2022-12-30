module Window exposing (Model, Msg, Window, fromScreen, fromScreenPosition, init, initWith, mapRect, onDrag, toScreen, toScreenPosition, update, updateRects, view)

import Array exposing (Array, toList)
import Element exposing (Attribute, Element, clip, el, fill, height, htmlAttribute, px, width)
import Html.Attributes
import Html.Events
import Json.Decode as D exposing (Decoder, index)
import List.Extra
import Math.Vector2 exposing (Vec, add, getX, getY, scale, sub, vec2)
import Maybe.Extra exposing (unwrap)
import String
import Window.Boundary exposing (Boundary(..), Hit(..), defaultTolerance, getHit, handleRezise)
import Window.Elements exposing (cursor, showAnchorPoint, userSelect)
import Window.Rect exposing (Rect)
import Window.Utils exposing (apply, takeAndAppend, uncurry)


type alias Window msg =
    { rect : Rect
    , render : (Msg -> msg) -> Int -> Rect -> Element msg
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
    { rects : Array Rect
    , order : List Index
    , drag : Drag
    , mousePosition : Vec Float
    , mouseOffset : Vec Float
    , offset : Vec Float
    , scale : Float
    }


init : Model
init =
    { rects = Array.empty
    , order = []
    , drag = None
    , mousePosition = vec2 0 0
    , mouseOffset = vec2 0 0
    , offset = vec2 0 0
    , scale = 1
    }


initWith : List (Window msg) -> Model
initWith =
    handleUpdateRects init << List.map .rect


{-| Update your windows if you need to. Use it like

    updateRects (List.map .rect windows)

-}
updateRects : List Rect -> Msg
updateRects =
    UpdateRects


type Msg
    = TrackWindow Index (Vec Float)
    | StopTrackWindow
    | PointerDown (Vec Float)
    | MouseMove (Vec Float)
    | UpdateRects (List Rect)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        UpdateRects ws ->
            ( handleUpdateRects model ws
            , Cmd.none
            )

        PointerDown mp ->
            handlePointerDown { model | mousePosition = fromScreenPosition model mp }

        TrackWindow ix mp ->
            ( { model
                | drag = Move ix
                , mouseOffset =
                    unwrap (vec2 0 0)
                        (\w -> sub w.position (fromScreenPosition model mp))
                        (Array.get (unwrapIndex ix) model.rects)
                , order = takeAndAppend ix model.order
              }
            , Cmd.none
            )

        StopTrackWindow ->
            ( { model | drag = None }, Cmd.none )

        MouseMove mp ->
            ( { model
                | mousePosition = fromScreenPosition model mp
                , rects = manipulateRects model (fromScreenPosition model mp)
              }
            , Cmd.none
            )


handleUpdateRects : Model -> List Rect -> Model
handleUpdateRects model rects =
    { model
        | rects = Array.fromList rects
        , order = List.map Index <| List.range 0 (List.length rects - 1)
    }


handlePointerDown : Model -> ( Model, Cmd msg )
handlePointerDown model =
    getRectHitsIx model
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


manipulateRects : Model -> Vec Float -> Array Rect
manipulateRects model mp =
    case model.drag of
        None ->
            model.rects

        Move (Index ix) ->
            let
                targetWindow =
                    Array.get ix model.rects
            in
            case targetWindow of
                Just wp ->
                    Array.set ix
                        { wp | position = add mp model.mouseOffset }
                        model.rects

                Nothing ->
                    -- Should never happen
                    model.rects

        Reszie (Index ix) corner ->
            let
                targetWindow =
                    Array.get ix model.rects

                delta =
                    sub mp model.mousePosition
            in
            case targetWindow of
                Just wp ->
                    handleRezise wp corner delta
                        |> (\w -> Array.set ix w model.rects)

                Nothing ->
                    -- Should never happen
                    model.rects



-- Helpers


getRectHits : Model -> Maybe Hit
getRectHits model =
    sortedByOrder model
        |> List.map Tuple.second
        |> List.map (getHit (scale (1 / model.scale) defaultTolerance) model.mousePosition)
        |> List.Extra.findMap identity


getRectHitsIx : Model -> Maybe ( Index, Hit )
getRectHitsIx model =
    sortedByOrder model
        |> List.Extra.findMap
            (\( ix, w ) ->
                Maybe.map (Tuple.pair ix) <| getHit (scale (1 / model.scale) defaultTolerance) model.mousePosition w
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


mapPointerPosition : (Vec Float -> value) -> Decoder value
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
         , cursor <| getCursor (getRectHits model)
         ]
            ++ renderWindows model (List.map (apply toMsg << .render) windows)
            -- Show anchor points
            ++ (if opts.showAnchorPoints then
                    withOrder model
                        |> List.map (Tuple.mapFirst unwrapZindex)
                        |> List.map (Tuple.mapSecond (toScreen model))
                        |> List.map (uncurry (showAnchorPoint defaultTolerance))
                        |> List.concat

                else
                    []
               )
        )
        Element.none


trackWindow : (Msg -> msg) -> Int -> Vec Float -> msg
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


renderWindows : Model -> List (Int -> Rect -> Element msg) -> List (Attribute msg)
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
    , rect : Rect
    , isFocused : Bool
    , render : Int -> Rect -> Element msg
    }


getRenderElement : Index -> ( Index, ZIndex, Rect ) -> (Int -> Rect -> Element msg) -> WindowRender msg
getRenderElement focusedIndex ( index, zindex, window ) render =
    { index = index
    , zIndex = zindex
    , rect = window
    , isFocused = focusedIndex == index
    , render = render
    }



--


toScreenPosition : { a | scale : Float, offset : Vec Float } -> Vec Float -> Vec Float
toScreenPosition model =
    scale model.scale << add model.offset


fromScreenPosition : { a | offset : Vec Float, scale : Float } -> Vec Float -> Vec Float
fromScreenPosition model =
    add (scale -1 model.offset) << scale (1 / model.scale)


toScreen : { a | offset : Vec Float, scale : Float } -> { b | position : Vec Float, size : Vec Float } -> { position : Vec Float, size : Vec Float }
toScreen model rect =
    { position = toScreenPosition model rect.position
    , size = rect.size |> scale model.scale
    }


{-| Normalize an offsetted, scaled rect back
-}
fromScreen : { a | offset : Vec Float, scale : Float } -> { b | position : Vec Float, size : Vec Float } -> { position : Vec Float, size : Vec Float }
fromScreen model rect =
    { position = fromScreenPosition model rect.position
    , size = rect.size |> scale (1 / model.scale)
    }



--


viewWindow :
    Model
    -> WindowRender msg
    -> Element.Attribute msg
viewWindow model { index, zIndex, rect, isFocused, render } =
    let
        rectOnScreen =
            toScreen model rect
    in
    Element.inFront
        (el
            ([ Element.moveRight (getX rectOnScreen.position)
             , Element.moveDown (getY rectOnScreen.position)
             , height (px <| round <| getY rectOnScreen.size)
             , width (px <| round <| getX rectOnScreen.size)
             , htmlAttribute (Html.Attributes.style "z-index" (String.fromInt <| unwrapZindex zIndex * 10))
             ]
                ++ userSelect (model.drag == None && isFocused)
             -- TODO: Prevent opening a link clicked in a background window
             -- Adding pointerEventsNone to `userSelect False` has currently no effect
             -- since pointer down immediatly focuses the element
             -- which sets thes isFocusedState to true
            )
         <|
            render (unwrapIndex index) rect
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


withOrder : Model -> List ( ZIndex, Rect )
withOrder m =
    List.Extra.zip (getOrder m.order) (toList m.rects)


withOrderIndexed : Model -> List ( Index, ZIndex, Rect )
withOrderIndexed m =
    List.Extra.zip (getOrder m.order) (toList m.rects)
        |> List.indexedMap (\ix ( zindex, window ) -> ( Index ix, zindex, window ))


sortedByOrder : Model -> List ( Index, Rect )
sortedByOrder m =
    withOrder m
        |> List.indexedMap (\ix ( zix, w ) -> ( Index ix, zix, w ))
        |> List.sortBy (\( _, ZIndex zix, _ ) -> zix)
        |> List.map (\( ix, _, w ) -> ( ix, w ))
        |> List.reverse


mapRect : (Rect -> Rect) -> Window msg -> Window msg
mapRect fn w =
    { w | rect = fn w.rect }
