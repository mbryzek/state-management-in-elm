module Global exposing (..)

import Browser.Navigation as Nav


type alias MainViewProps a b =
    { global : GlobalState
    , msgMap : a -> b
    }


type alias UpdateWithLoggedInProps =
    { redirectUrl : String, session : Session }


type alias MainUpdatePropsWithLoggedIn a b =
    { msgMap : a -> b
    , onLoggedIn : UpdateWithLoggedInProps -> b
    }


type alias MainUpdatePropsWithLoggedOut a b =
    { msgMap : a -> b
    , onLoggedOut : b
    }


type alias UpdateWithSessionProps =
    { redirectUrl : Maybe String, session : Session }


type alias MainUpdatePropsWithSession a b =
    { msgMap : a -> b
    , onSessionUpdate : UpdateWithSessionProps -> b
    }


type GlobalState
    = GlobalStateAnonymous GlobalStateAnonymousData
    | GlobalStateAuthenticated GlobalStateAuthenticatedData


type alias GlobalStateAnonymousData =
    { navKey : Nav.Key
    }


type alias GlobalStateAuthenticatedData =
    { navKey : Nav.Key
    , session : Session
    }


getNavKey : GlobalState -> Nav.Key
getNavKey globalState =
    case globalState of
        GlobalStateAnonymous data ->
            data.navKey

        GlobalStateAuthenticated data ->
            data.navKey


type alias Session =
    { id : String
    , user : User
    }


type alias User =
    { name : String
    }


getSession : GlobalState -> Maybe Session
getSession globalState =
    case globalState of
        GlobalStateAnonymous _ ->
            Nothing

        GlobalStateAuthenticated data ->
            Just data.session


getUser : GlobalState -> Maybe User
getUser globalState =
    Maybe.map .user (getSession globalState)
