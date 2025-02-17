module NotFound exposing (view)

import Browser as B
import Html as H

view : B.Document msg
view =
    { title = "NotFound"
    , body = [contents]
    }

contents : H.Html msg
contents =
    H.text "Not Found"

