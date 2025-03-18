-- codegen.global.state: GlobalStateAnonymousData
module Page.Content exposing (view)

import Browser as B
import Html as H
import Html.Attributes exposing (class)
import Templates.Shell as Shell
import Util.Ui exposing (..)

view : Shell.ViewProps mainMsg -> B.Document mainMsg
view shellProps =
    { title = "Example content page"
    , body = Shell.view shellProps (contents)
    }


contents : H.Html mainMsg
contents =
    H.div [ class "flex flex-col gap-y-4" ]
        [ para "Example content page"
        , para "This page is available to all users - logged in or not."
        , para "lorem ipsum dolor sit amet"
        ]
