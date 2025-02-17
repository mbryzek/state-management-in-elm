module NotAuthorized exposing (view)

import Browser as B
import Html as H

view : B.Document msg
view =
    { title = "Not Authorized"
    , body = [contents]
    }

contents : H.Html msg
contents =
    H.text "Not Authorized"

