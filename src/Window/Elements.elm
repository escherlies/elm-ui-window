module Window.Elements exposing (..)

import Element exposing (Attribute, el, height, htmlAttribute, mouseOver, px, rgb, rgb255, width)
import Element.Background
import Element.Border
import Html.Attributes exposing (style)
import Math.Vector2 exposing (Vec, getX, getY)
import Window.Boundary exposing (getAnchorPoints)
import Window.Rect exposing (Rect)



--


cursor : String -> Attribute msg
cursor =
    htmlAttribute << Html.Attributes.style "cursor"


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


pointerEventsNone : Element.Attribute msg
pointerEventsNone =
    Element.htmlAttribute (Html.Attributes.style "pointer-events" "none")


pointerEventsAuto : Element.Attribute msg
pointerEventsAuto =
    Element.htmlAttribute (Html.Attributes.style "pointer-events" "auto")



--
--


showAnchorPoint : Vec Float -> Int -> Rect -> List (Attribute msg)
showAnchorPoint tol zindex rect =
    List.indexedMap
        (\_ b ->
            Element.inFront
                (el
                    [ Element.moveRight (getX b.position)
                    , Element.moveDown (getY b.position)
                    , width (px <| round (getX b.size))
                    , height (px <| round (getY b.size))
                    , Element.Border.width 1
                    , htmlAttribute (Html.Attributes.style "z-index" (String.fromInt <| zindex * 10 + 1))
                    , Element.Border.rounded (getX b.size |> round)
                    , Element.Background.color (rgb 1 1 1)
                    , Element.Border.color (rgb255 64 137 255)
                    , mouseOver
                        [ Element.Background.color (rgb255 64 137 255)
                        ]
                    ]
                    Element.none
                )
        )
        (getAnchorPoints rect tol)
