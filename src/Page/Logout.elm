-- codegen.global.state: GlobalStateAnonymousData
module Page.Logout exposing (Msg, init, update, view)

import Browser as B
import Browser.Navigation as Nav
import Global exposing (GlobalState, MainViewProps, MainUpdatePropsWithLoggedOut)
import Html as H
import Html.Attributes exposing (class)
import Templates.Shell as Shell
import Util.Task as Task
import Util.Ui exposing (..)

type Msg
    = RedirectTo String
    | Logout


init : Cmd Msg
init =
    Task.dispatch (Logout)

update : GlobalState -> MainUpdatePropsWithLoggedOut Msg mainMsg -> Msg -> Cmd mainMsg
update global { onLoggedOut, msgMap } msg =
    case msg of
        RedirectTo url ->
            Nav.pushUrl (Global.getNavKey global) url |> Cmd.map msgMap

        Logout ->
            Task.dispatch (onLoggedOut)


view : MainViewProps Msg mainMsg -> Shell.ViewProps mainMsg -> B.Document mainMsg
view props shellProps =
    { title = "Your are logged out"
    , body = Shell.view shellProps (contents |> H.map props.msgMap)
    }


contents : H.Html Msg
contents =
    H.div [class "flex flex-col gap-y-4 items-start"]
    [ para "You are now logged out"
    , button (RedirectTo "/login") "Log back in"
    ]
            
