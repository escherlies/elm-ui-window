module Window.Example exposing (..)

import Element exposing (column, el, layout, text)
import Element.Border
import Html exposing (Html)
import Math.Vector2 exposing (getX, getY, vec2)
import Window exposing (onDrag)



{- # elm-ui-window

   Put some draggable, resizeable rectangles to your UI!

   ![](docs/Screenshot.f2a687b.png)

   # Use cases

   - A UI with window elements, i.e. [https://binaryplease.com/](https://binaryplease.com/)
   - As a core package to work with resizable elements, as used in our map customizer at [https://www.hyhyve.com/](https://www.hyhyve.com/)
   - A website builder
   - Rendering flow charts

   # Usage

   You basically have this type...

   ```elm
   type alias Window msg =
        { rect : Rect
        , render : (Msg -> msg) -> Int -> Rect -> Element msg
        }
    ```

   ...that you can use with three familiar functions: `initWith`, `update` and `view`.

   That's it!

   # Example

   A fully working example would be:
-}


type alias Model =
    { windowModel : Window.Model
    }


init : Model
init =
    { windowModel = Window.initWith windows
    }


type Msg
    = WindowMsg Window.Msg


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        WindowMsg windowMsg ->
            let
                ( windowModel, windowCmds ) =
                    Window.update windowMsg model.windowModel
            in
            ( { windowModel = windowModel }, windowCmds )


view : Model -> Html Msg
view model =
    layout []
        -- Mount the view wherever you want
        (Window.view WindowMsg { showAnchorPoints = False } model.windowModel windows)


windows : List (Window.Window msg)
windows =
    [ -- A window is just a rect in xy space and some render fn to render the content inside that rect. That's it!
      { rect =
            -- A rect is a rectangle with a position and size
            { position = vec2 0 0
            , size = vec2 100 100
            }
      , render =
            -- A simple view render function. You don't have to use the `_ _ _` params, hence the `_`.
            \_ _ _ ->
                -- Just give it some border and you are good to go!
                el [ Element.Border.width 3 ]
                    (text "Hello, World!")
      }
    ]



{- # Advanced usage

   In the simple example we ignored three params of the render function.
   These three params actually are: `toMsg`, `index`, and `rect`. Let's see
   how we can use them to create advanced views:
-}


windows2 : List (Window.Window msg)
windows2 =
    [ { rect =
            { position = vec2 0 0
            , size = vec2 100 100
            }
      , render =
            \toMsg ix rect ->
                column
                    [ Element.Border.width 3

                    -- Use the toMsg to use the `onDrag` function to allow for you window to be dragged.
                    -- This can be whereever you like, either everywhere oder at dedicated area like a window title bar.
                    , onDrag toMsg ix
                    ]
                    -- Use the index and current rect state if you want to use them
                    [ text <| "index  = " ++ String.fromInt ix
                    , text <| "x      = " ++ String.fromFloat (getX rect.position)
                    , text <| "y      = " ++ String.fromFloat (getY rect.position)
                    , text <| "width  = " ++ String.fromFloat (getX rect.size)
                    , text <| "height = " ++ String.fromFloat (getY rect.size)
                    ]
      }
    ]



{- # Notes

   You can find the example in `src/Window/Example.elm`.

   This is work in progress and therfore not yet published on elm packages.

-}
