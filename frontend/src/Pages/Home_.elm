module Pages.Home_ exposing (view)

import Html
import View exposing (View)
import Layout exposing (layout)


view : View msg
view =
    { title = "Homepage"
    , body = layout [ Html.text "Welcome to Pluto" ]
    }
