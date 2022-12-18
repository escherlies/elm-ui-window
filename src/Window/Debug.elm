module Window.Debug exposing (..)

import Element exposing (Attribute, centerX, centerY, column, el, height, htmlAttribute, px, rgb, text, width)
import Element.Border
import Html.Attributes
import Math.Vector2 exposing (Vec2, getX, getY)
import Window exposing (onDrag)
import Window.Boundary exposing (getBoundaries)
import Window.Plane exposing (Plane)


showBoundaries : Vec2 -> Int -> Plane -> List (Attribute msg)
showBoundaries tol zindex plane =
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
        (getBoundaries plane tol)


debugWindows : { a | windowModel : { b | mousePosition : Vec2 }, window : Vec2 } -> List { plane : Plane, render : (Window.Msg -> msg) -> Int -> { c | position : Vec2, size : Vec2 } -> Element.Element msg }
debugWindows model =
    [ { plane = Window.Plane.default
      , render =
            \tw i w ->
                simpleWindow tw
                    i
                    [ text <| "ix = " ++ String.fromInt i
                    , text <| "x = " ++ String.fromFloat (getX w.position)
                    , text <| "y = " ++ String.fromFloat (getY w.position)
                    , text <| "w = " ++ String.fromFloat (getX w.size)
                    , text <| "h = " ++ String.fromFloat (getY w.size)
                    ]
      }
    , { plane = Window.Plane.default
      , render =
            \tw i _ ->
                simpleWindow tw
                    i
                    [ text <| "x = " ++ String.fromFloat (getX model.windowModel.mousePosition)
                    , text <| "y = " ++ String.fromFloat (getY model.windowModel.mousePosition)
                    , text <| "vw = " ++ String.fromFloat (getX model.window)
                    , text <| "vh = " ++ String.fromFloat (getY model.window)
                    ]
      }
    ]


simpleWindow : (Window.Msg -> msg) -> Int -> List (Element.Element msg) -> Element.Element msg
simpleWindow toMsg ix =
    column
        [ centerX
        , centerY
        , onDrag toMsg ix
        ]
