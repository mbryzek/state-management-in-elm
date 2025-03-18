-- codegen.global.state: GlobalStateAnonymousData
module Page.Login exposing (Model, Msg, init, update, view)

import Browser as B
import Global exposing (GlobalState, Session, MainViewProps, MainUpdatePropsWithLoggedIn, User)
import Html as H
import Html.Attributes exposing (class)
import Util.Browser.Dom as BD
import Templates.Shell as Shell
import Util.Task as Task
import Util.Ui exposing (..)

type alias Model =
    { currentUser : Maybe User
      , sessionRequest : Maybe Session
    }


type Msg
    = FocusedEmail
    | Login


init : GlobalState -> ( Model, Cmd Msg )
init global =
    ( { currentUser = (Global.getUser global)
    , sessionRequest = Nothing
      }
    , BD.focus "email" FocusedEmail
    )

update : MainUpdatePropsWithLoggedIn Msg mainMsg -> Msg -> Model -> ( Model, Cmd mainMsg )
update { onLoggedIn } msg model =
    case msg of
        FocusedEmail ->
            ( model, Cmd.none )

        Login ->
            let
                session : Session
                session =
                    { id = "123-test-session-id", user = { name = "John Doe" } }
            in
            ( { model | sessionRequest = Just session }
            , Task.dispatch (onLoggedIn { redirectUrl = "/", session = session })
            )


view : MainViewProps Msg mainMsg -> Shell.ViewProps mainMsg -> Model -> B.Document mainMsg
view props shellProps model =
    { title = "login"
    , body = Shell.view shellProps (contents model |> H.map props.msgMap)
    }


contents : Model -> H.Html Msg
contents model =
    let
        loggedInNote : H.Html Msg
        loggedInNote =
            case model.currentUser of
                Just user ->
                    para ("You are currently logged in as " ++ user.name)
                Nothing ->
                    H.text ""

    in
    H.div [ class "flex flex-col items-start gap-y-4" ]
        [
        loggedInNote
        , para "This is a prototype login page."
        , para "When you login, you will see your name in the header and in the welcome message on the home page, a new page in the header title 'Restricted' which is availble only to logged in users, and a link to Logout."
        , button Login "Simulate login"
        ]