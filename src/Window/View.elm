module Window.View exposing (..)

import Element exposing (Element, clip, column, el, fill, height, padding, px, row, scrollbars, width)
import Element.Background
import Element.Border
import Element.Font
import Window exposing (onDrag)
import Window.Elements exposing (cursor, userSelect)
import Window.Rect exposing (Rect)


{-| Show just a rectangle that can be dragged and resized with some content
-}
simple :
    { a
        | backgroundColor : Element.Color
        , foregroundColor : Element.Color
        , attributes : List (Element.Attribute msg)
        , content : Element msg
    }
    -> (Window.Msg -> msg)
    -> Int
    -> Rect
    -> Element msg
simple { backgroundColor, foregroundColor, content, attributes } toMsg ix _ =
    el
        ([ Element.Border.width 1
         , Element.Border.color foregroundColor
         , width fill
         , height fill
         , Element.Background.color backgroundColor
         , onDrag toMsg ix
         , cursor "move"
         , clip
         , scrollbars
         , padding 8
         ]
            ++ attributes
        )
        content


{-| A default view serving as an example. You should copy this and customize it to your likeing :)
-}
defaultWindowElement :
    { a
        | backgroundColor : Element.Color
        , foregroundColor : Element.Color
        , title : Element msg
        , content : Element msg
    }
    -> (Window.Msg -> msg)
    -> Int
    -> Rect
    -> Element msg
defaultWindowElement { backgroundColor, foregroundColor, title, content } toMsg ix _ =
    column
        [ Element.Border.width 2
        , width fill
        , height fill
        , Element.Border.shadow
            { offset = ( 3, 3 )
            , blur = 0
            , color = foregroundColor
            , size = 0
            }
        , Element.Background.color backgroundColor
        ]
        [ row
            ([ height (px 40)
             , width fill
             , Element.Border.widthEach
                { top = 0
                , left = 0
                , right = 0
                , bottom = 2
                }
             , cursor "move"
             , padding 8
             , Element.Font.semiBold

             -- This is used to handle dragging windows via the title bar
             , onDrag toMsg ix
             ]
                ++ userSelect False
            )
            [ title ]
        , el
            [ width fill
            , height fill
            , clip
            , scrollbars
            , padding 8
            ]
          <|
            content
        ]
