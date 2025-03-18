-- codegen.global.state: GlobalStateAnonymousData
module Page.Index exposing (Msg, init, update, view)

import Browser as B
import Browser.Navigation as Nav
import Global exposing (GlobalState, MainUpdatePropsWithSession, MainViewProps, Session, User)
import Html as H
import Html.Attributes exposing (class)
import Templates.Shell as Shell
import Util.Browser.Dom as BD
import Util.Task as Task
import Util.Ui exposing (..)

type Msg
    = FocusedEmail
    | ChangeUserName
    | RedirectTo String


init : Cmd Msg
init =
    BD.focus "email" FocusedEmail


update : GlobalState -> MainUpdatePropsWithSession Msg mainMsg -> Msg -> Cmd mainMsg
update global { onSessionUpdate } msg =
    case msg of
        FocusedEmail ->
            Cmd.none

        RedirectTo url ->
            Nav.pushUrl (Global.getNavKey global) url

        ChangeUserName ->
            let
                newSession : Session
                newSession =
                    { id = "123-test-session-new-id", user = { name = "Jerry" } }
            in
            Task.dispatch (onSessionUpdate { redirectUrl = Nothing, session = newSession })


view : MainViewProps Msg mainMsg -> Shell.ViewProps mainMsg -> B.Document mainMsg
view props shellProps =
    { title = "Elm App"
    , body = Shell.view shellProps (contents props.global |> H.map props.msgMap)
    }


contents : GlobalState -> H.Html Msg
contents global =
    H.div [ class "flex flex-col items-start gap-y-4" ] (
        case Global.getUser global of
            Just user ->
                contentsLoggedIn user

            Nothing ->
                contentsAnonymous
    )

contentsLoggedIn : User -> List (H.Html Msg)
contentsLoggedIn user =
    let
        newUserName : String
        newUserName =
            "Jerry"
    in
    if user.name == newUserName then
        [ para ("Your updated user name is " ++ newUserName)
        , para "Note how this name immediately also updated in the header bar."
        ]
    else
        [ para ("You are currently logged in as a user whose name is " ++ user.name)
        , para "Note that the header now contains a page named 'Restricted' which is only accessible to logged in users."
        , para ("Try changing your user's name to " ++ newUserName ++ " which you will see immediately reflected both in this page and the header.")
        , button ChangeUserName "Change user name"
        ]


contentsAnonymous : List (H.Html Msg)
contentsAnonymous =
    [ para "Welcome Guest!"
    , para "This is an example Elm Application showing how to manage state across various pages, components and a global header."
    , para "We minimize styling and content to focus on state management across the pages and components."
    , para "Note that you can increment or decrement the counter in the header and it will persist across pages."
    , paraH
        [ H.text "This application is built using Tailwind CSS. Source code is available on "
        , link "GitHub" "https://github.com/elm-app/elm-app"
        ]
    , button (RedirectTo "/login") "Go to the login page"
    ]