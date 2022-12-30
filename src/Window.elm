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
import Window.Boundary exposing (Boundary(..), Hit(..), Resize(..), defaultTolerance, getHit, handleRezise, isBoundaryHit)
import Window.Elements exposing (cursor, showAnchorPoint, userSelect)
import Window.Rect exposing (Rect)
import Window.Utils exposing (takeAndAppend)


type alias Window msg =
    { rect : Rect
    , render : (Msg -> msg) -> Int -> Rect -> Element msg
    , resize : Resize
    }


type Index
    = Index Int


type ZIndex
    = ZIndex Int


type Drag
    = None
    | Reszie Index Boundary
    | Move Index


{-| Rho

A subset of window containing rect and resize

-}
type alias Rho =
    { rect : Rect
    , resize : Resize
    }


type alias Model =
    { rects : Array Rho
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
    handleUpdateRects init << List.map toR


toR : Window msg -> Rho
toR { rect, resize } =
    Rho rect resize


{-| Update your windows if you need to. Use it like

    updateRects (List.map .rect windows)

-}
updateRects : List Rho -> Msg
updateRects =
    UpdateRects


type Msg
    = TrackWindow Index (Vec Float)
    | StopTrackWindow
    | PointerDown (Vec Float)
    | MouseMove (Vec Float)
    | UpdateRects (List Rho)


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
                        (Array.get (unwrapIndex ix) model.rects |> Maybe.map .rect)
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


handleUpdateRects : Model -> List Rho -> Model
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


manipulateRects : Model -> Vec Float -> Array Rho
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
                        (mapRect (\rect -> { rect | position = add mp model.mouseOffset }) wp)
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
                    handleRezise wp.rect corner delta
                        |> (\w -> Array.set ix (mapRect (always w) wp) model.rects)

                Nothing ->
                    -- Should never happen
                    model.rects



-- Helpers


getRectHitsIx : Model -> Maybe ( Index, Hit )
getRectHitsIx model =
    sortedByOrder model
        |> List.Extra.findMap
            (\( ix, w ) ->
                Maybe.map (Tuple.pair ix) <| getHit (scale (1 / model.scale) defaultTolerance) model.mousePosition w
            )
        |> Maybe.andThen
            (checkResize model)


checkResize : Model -> ( Index, Hit ) -> Maybe ( Index, Hit )
checkResize model (( Index index, hit ) as foo) =
    -- ! This lookup is unnecessary
    -- TODO update sotedByOrder to include Resize and drop this lookup
    Array.get index model.rects
        |> Maybe.andThen
            (\{ resize } ->
                if resize == DisableResize && isBoundaryHit hit then
                    Nothing

                else
                    Just foo
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


view : (Msg -> msg) -> Model -> List (Window msg) -> Element msg
view toMsg model windows =
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
         , cursor <| getCursor (getRectHitsIx model |> Maybe.map Tuple.second)
         ]
            ++ renderWindows model (List.map ((|>) toMsg << .render) windows)
            ++ renderAnchorPoints model windows
        )
        Element.none


renderAnchorPoints : Model -> List (Window msg) -> List (Attribute msg)
renderAnchorPoints model windows =
    windows
        |> List.map2 (\( zindex, rect ) { resize } -> ( zindex, rect, resize )) (withOrder model)
        |> List.map
            (\( ZIndex zix, rect, resize ) ->
                if resize == ShowAnchorPoints then
                    showAnchorPoint defaultTolerance zix (toScreen model rect)

                else
                    []
            )
        |> List.concat


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
    List.Extra.zip (getOrder m.order) (List.map .rect (toList m.rects))


withOrderIndexed : Model -> List ( Index, ZIndex, Rect )
withOrderIndexed m =
    List.Extra.zip (getOrder m.order) (List.map .rect (toList m.rects))
        |> List.indexedMap (\ix ( zindex, window ) -> ( Index ix, zindex, window ))


sortedByOrder : Model -> List ( Index, Rect )
sortedByOrder m =
    withOrder m
        |> List.indexedMap (\ix ( zix, w ) -> ( Index ix, zix, w ))
        |> List.sortBy (\( _, ZIndex zix, _ ) -> zix)
        |> List.map (\( ix, _, w ) -> ( ix, w ))
        |> List.reverse


mapRect : (r -> r) -> { a | rect : r } -> { a | rect : r }
mapRect fn w =
    { w | rect = fn w.rect }
