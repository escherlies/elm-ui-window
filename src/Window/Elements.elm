module Window.Elements exposing (..)

import Element exposing (Attribute, htmlAttribute)
import Html.Attributes exposing (style)



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
