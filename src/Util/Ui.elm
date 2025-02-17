module Util.Ui exposing (button, para, paraH, link)

import Html as H
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)

link : String -> String -> H.Html msg
link text url =
    H.a [
        class "text-blue-600 underline hover:no-underline hover:text-blue-700"
        , target "_blank"
        , href url
    ] [
        H.text text
    ]


para : String -> H.Html msg
para text =
    paraH [ H.text text ]

paraH : List (H.Html msg) -> H.Html msg
paraH text =
    H.p [ class "text-gray-600" ] text

button : msg -> String -> H.Html msg
button msg label =
    H.button 
        [ onClick msg
        , class "px-4 py-2 bg-blue-600 text-white rounded-md hover:bg-blue-700 transition-colors duration-200 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2"
        ] 
        [ H.text label ]
