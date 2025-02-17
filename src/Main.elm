module Main exposing (Flags, Model, Msg, main)

import Browser
import Browser.Navigation as Nav
import Global exposing (GlobalState(..), GlobalStateAuthenticatedData, MainUpdatePropsWithLoggedIn, MainUpdatePropsWithLoggedOut, MainUpdatePropsWithSession, MainViewProps, Session, UpdateWithLoggedInProps, UpdateWithSessionProps)
import Loading
import NotAuthorized
import NotFound
import Route exposing (Route)
import Templates.Shell as Shell
import Url
import Util.Task as Task
import Page.Content as PageContent
import Page.Index as PageIndex
import Page.Login as PageLogin
import Page.Logout as PageLogout
import Page.Restricted as PageRestricted


type alias Flags =
    { sessionId : Maybe String }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type Model
    = Init InitModel
    | Ready ReadyModel


type alias InitModel =
    { sessionId : String
    , url : Url.Url
    , key : Nav.Key
    }


type alias ReadyModel =
    { global : GlobalState
    , shellModel : Shell.Model
    , page : Page
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case flags.sessionId of
        Just sessionId ->
            ( Init { sessionId = sessionId, url = url, key = key }
            , Task.dispatch (InitMsg (SimluateCallToLogin sessionId))
            )

        Nothing ->
            initWithGlobal (toGlobalState key Nothing) url


initWithGlobal : GlobalState -> Url.Url -> ( Model, Cmd Msg )
initWithGlobal global url =
    let
        ( shellModel, shellCmd ) =
            Shell.init global

        ( page, cmd ) =
            Route.fromUrl url
                |> getPageFromRoute global
    in
    ( Ready
        { global = global
        , shellModel = shellModel
        , page = page
        }
    , Cmd.batch
        [ Cmd.map (ReadyMsg << ChangedPage) cmd
        , Cmd.map (ReadyMsg << ChangedInternal << ShellMsg) shellCmd
        ]
    )


type Msg
    = InitMsg InitMsg
    | ReadyMsg ReadyMsg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


type ReadyMsg
    = ChangedInternal InternalMsg
    | ChangedPage PageMsg


type InitMsg
    = SimluateCallToLogin String


type InternalMsg
    = ShellMsg Shell.Msg
    | LoggedInMsg UpdateWithLoggedInProps
    | LoggedOutMsg
    | SessionUpdatedMsg UpdateWithSessionProps


redirectTo : Model -> String -> Cmd Msg
redirectTo model url =
    let
        key : Nav.Key
        key =
            case model of
                Init data ->
                    data.key

                Ready rm ->
                    Global.getNavKey rm.global
    in
    Nav.pushUrl key url


updateSession : ReadyModel -> Maybe Session -> ReadyModel
updateSession model session =
    updateShell { model | global = toGlobalState (Global.getNavKey model.global) session }


toGlobalState : Nav.Key -> Maybe Session -> GlobalState
toGlobalState key maybeSession =
    case maybeSession of
        Just u ->
            GlobalStateAuthenticated { navKey = key, session = u }

        Nothing ->
            GlobalStateAnonymous { navKey = key }


updateShell : ReadyModel -> ReadyModel
updateShell model =
    let
        shellModel : Shell.Model
        shellModel =
            model.shellModel
    in
    { model | shellModel = { shellModel | global = model.global } }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, redirectTo model (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, Ready readyModel ) ->
            let
                ( page, cmd ) =
                    Route.fromUrl url
                        |> getPageFromRoute readyModel.global
            in
            ( Ready { readyModel | page = page }
            , Cmd.map (ReadyMsg << ChangedPage) cmd
            )

        ( UrlChanged url, Init data ) ->
            ( Init { data | url = url }, Cmd.none )

        ( InitMsg initMsg, Init initModel ) ->
            updateInit initMsg initModel

        ( InitMsg _, Ready _ ) ->
            ( model, Cmd.none )

        ( ReadyMsg readyMsg, Ready readyModel ) ->
            updateReady readyMsg readyModel |> Tuple.mapFirst (\m -> Ready m)

        ( ReadyMsg _, Init _ ) ->
            ( model, Cmd.none )


updateInit : InitMsg -> InitModel -> ( Model, Cmd Msg )
updateInit msg model =
    case msg of
        SimluateCallToLogin sessionId ->
            let
                newSession : Session
                newSession =
                    { id = sessionId, user = { name = "John Doe" } }
            in
            initWithGlobal (toGlobalState model.key (Just newSession)) model.url


updateReady : ReadyMsg -> ReadyModel -> ( ReadyModel, Cmd Msg )
updateReady msg model =
    case msg of
        ChangedInternal internalMsg ->
            updateInternal internalMsg model

        ChangedPage pageMsg ->
            updatePage model pageMsg
                |> Tuple.mapFirst (\page -> { model | page = page })


updateInternal : InternalMsg -> ReadyModel -> ( ReadyModel, Cmd Msg )
updateInternal msg model =
    case msg of
        ShellMsg shellMsg ->
            Shell.update shellMsg model.shellModel
                |> Tuple.mapFirst (\m -> { model | shellModel = m })
                |> Tuple.mapSecond (Cmd.map (ReadyMsg << ChangedInternal << ShellMsg))

        LoggedInMsg { redirectUrl, session } ->
            ( updateSession model (Just session), redirectTo (Ready model) redirectUrl )

        SessionUpdatedMsg { redirectUrl, session } ->
            let
                redirectCmd : Cmd Msg
                redirectCmd =
                    case redirectUrl of
                        Just url ->
                            redirectTo (Ready model) url

                        Nothing ->
                            Cmd.none
            in
            ( updateSession model (Just session), redirectCmd )

        LoggedOutMsg ->
            ( updateSession model Nothing, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    case model of
        Init _ ->
            Loading.view

        Ready readyModel ->
            viewReady readyModel


updatePropsWithLoggedIn : (a -> PageMsg) -> MainUpdatePropsWithLoggedIn a Msg
updatePropsWithLoggedIn pageMsg =
    { onLoggedIn = ReadyMsg << ChangedInternal << LoggedInMsg
    , msgMap = ReadyMsg << ChangedPage << pageMsg
    }


updatePropsWithLoggedOut : (a -> PageMsg) -> MainUpdatePropsWithLoggedOut a Msg
updatePropsWithLoggedOut pageMsg =
    { onLoggedOut = ReadyMsg (ChangedInternal LoggedOutMsg), msgMap = ReadyMsg << ChangedPage << pageMsg }


updatePropsWithSessionUpdate : (a -> PageMsg) -> MainUpdatePropsWithSession a Msg
updatePropsWithSessionUpdate pageMsg =
    { onSessionUpdate = ReadyMsg << ChangedInternal << SessionUpdatedMsg
    , msgMap = ReadyMsg << ChangedPage << pageMsg
    }


shellViewProps : ReadyModel -> Shell.ViewProps Msg
shellViewProps model =
    { shellModel = model.shellModel
    , onShellMsg = ReadyMsg << ChangedInternal << ShellMsg
    }


mainViewProps : GlobalState -> (a -> PageMsg) -> MainViewProps a Msg
mainViewProps global pageMsg =
    { global = global, msgMap = ReadyMsg << ChangedPage << pageMsg }


pageAuthenticatedData : GlobalState -> (GlobalStateAuthenticatedData -> ( Page, Cmd PageMsg )) -> ( Page, Cmd PageMsg )
pageAuthenticatedData global f =
    case global of
        GlobalStateAuthenticated data ->
            f data

        _ ->
            ( PageNotAuthorized, Nav.pushUrl (Global.getNavKey global) "/login" )


subscriptions : Model -> Sub Msg
subscriptions _ =
    pageSubscriptions |> Sub.map (ReadyMsg << ChangedPage)



-- Organized so the below can be code generated
-- CODEGEN START
pageSubscriptions : Sub PageMsg
pageSubscriptions =
    Sub.none


type Page
    = PageContent
    | PageIndex
    | PageLogin PageLogin.Model
    | PageLogout
    | PageRestricted PageRestricted.Model
    | PageNotFound
    | PageNotAuthorized


type PageMsg
    = PageIndexMsg PageIndex.Msg
    | PageLoginMsg PageLogin.Msg
    | PageLogoutMsg PageLogout.Msg


getPageFromRoute : GlobalState -> Maybe Route -> ( Page, Cmd PageMsg )
getPageFromRoute global maybeRoute =
    case maybeRoute of
        Just Route.RouteIndex ->
            ( PageIndex, PageIndex.init |> Cmd.map PageIndexMsg)

        Just Route.RouteLogin ->
            PageLogin.init global
                |> Tuple.mapFirst PageLogin
                |> Tuple.mapSecond (Cmd.map PageLoginMsg)

        Just Route.RouteLogout ->
            ( PageLogout, PageLogout.init |> Cmd.map PageLogoutMsg)

        Just Route.RouteRestricted ->
            pageAuthenticatedData global (\g ->
                ( PageRestricted.init g |> PageRestricted, Cmd.none)
            )
        Just Route.RouteContent ->
            ( PageContent, Cmd.none )

        Nothing ->
            ( PageNotFound, Cmd.none )


viewReady : ReadyModel -> Browser.Document Msg
viewReady model =
    case model.page of
        PageContent ->
            PageContent.view (shellViewProps model)

        PageIndex ->
            PageIndex.view (mainViewProps model.global PageIndexMsg) (shellViewProps model)

        PageLogin pageModel ->
            PageLogin.view (mainViewProps model.global PageLoginMsg) (shellViewProps model) pageModel

        PageLogout ->
            PageLogout.view (mainViewProps model.global PageLogoutMsg) (shellViewProps model)

        PageRestricted pageModel ->
            PageRestricted.view (shellViewProps model) pageModel

        PageNotFound ->
            NotFound.view

        PageNotAuthorized ->
            NotAuthorized.view


updatePage : ReadyModel -> PageMsg -> ( Page, Cmd Msg )
updatePage model msg =
    case ( model.page, msg ) of
        ( PageIndex, PageIndexMsg pageMsg ) ->
            PageIndex.update model.global (updatePropsWithSessionUpdate PageIndexMsg) pageMsg
                |> \c -> (model.page, c)

        ( PageLogin pageModel, PageLoginMsg pageMsg ) ->
            PageLogin.update (updatePropsWithLoggedIn PageLoginMsg) pageMsg pageModel
                |> Tuple.mapFirst PageLogin

        ( PageLogout, PageLogoutMsg pageMsg ) ->
            PageLogout.update model.global (updatePropsWithLoggedOut PageLogoutMsg) pageMsg
                |> \c -> (model.page, c)

        ( page, _ ) ->
            ( page, Cmd.none )