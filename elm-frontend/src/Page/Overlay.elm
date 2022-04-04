module Page.Overlay exposing (view)

import Html as H exposing (Html)


view : List (Html msg)
view =
    [ H.h1 [] [ H.text "Overlay" ]
    , H.section [] [ H.text "overlay page" ]
    ]
