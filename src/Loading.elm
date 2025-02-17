module Loading exposing (view)

import Browser as B
import Html as H

view : B.Document msg
view =
    { title = "Loading..."
    , body = [contents]
    }

contents : H.Html msg
contents =
    H.text "Loading..."

