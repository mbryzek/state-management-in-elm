module Templates.Shell exposing (Model, Msg, ViewProps, init, update, view)

import Browser.Navigation as Nav
import Date
import Global exposing (..)
import Html as H exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events exposing (onClick)
import Task
import Time
import Url exposing (Url)

type alias ViewProps a =
    { shellModel : Model
    , onShellMsg : Msg -> a
    }


type alias Model =
    { counter : Int
    , posix : Maybe Time.Posix
    }


init : ( Model, Cmd Msg )
init =
    ( { counter = 0
      , posix = Nothing
      }
    , Task.perform DefaultFrom Time.now
    )


type Msg
    = IncrementCounter
    | DecrementCounter
    | RedirectTo String
    | DefaultFrom Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DefaultFrom posix ->
            ( { model | posix = Just posix }, Cmd.none )

        IncrementCounter ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        DecrementCounter ->
            ( { model | counter = model.counter - 1 }, Cmd.none )

        RedirectTo url ->
            ( model, Nav.pushUrl (Global.getNavKey model.global) url )


view : ViewProps a -> Html a -> List (Html a)
view viewProps content =
    [ H.div [ Attr.class "min-h-screen flex flex-col pl-4 pr-4" ]
        [ viewHeader viewProps.shellModel |> H.map viewProps.onShellMsg
        , viewMain content
        , viewFooter viewProps.shellModel |> H.map viewProps.onShellMsg
        ]
    ]


viewHeader : Model -> Html Msg
viewHeader model =
    H.header [ Attr.class "bg-white shadow mb-4" ]
        [ H.div [ Attr.class "max-w-7xl mx-auto sm:px-6 lg:px-8" ]
            [ H.div [ Attr.class "flex justify-between items-center h-16" ]
                [ viewLeftSection model.global
                , viewRightSection model
                ]
            ]
        ]


viewLeftSection : GlobalState -> Html Msg
viewLeftSection global =
    H.div [ Attr.class "flex items-center" ]
        [ viewLogo
        , viewNavLinks global
        ]


viewLogo : Html Msg
viewLogo =
    H.div [ Attr.class "flex-shrink-0 pr-8" ]
        [ H.div
            [ Attr.class "flex items-center" ]
            [ H.span
                [ Attr.class "text-2xl font-bold" ]
                [ H.span [ Attr.class "text-blue-600" ] [ H.text "Elm" ]
                , H.span [ Attr.class "text-gray-700" ] [ H.text "App" ]
                ]
            ]
        ]


viewNavLinks : GlobalState -> Html Msg
viewNavLinks global =
    H.nav [ Attr.class "ml-10 flex space-x-4" ]
        (List.map (viewNavLink (Global.getCurrentUrl global)) (getLinks global))


getLinks : GlobalState -> List ( String, String )
getLinks global =
    let
        allUsers : List ( String, String )
        allUsers =
            [ ( "Home", "/" )
            , ( "Content", "/content" )
            ]

    in
    case global of
        GlobalStateAnonymous _ ->
            allUsers ++ [ ( "Login", "/login" ) ]

        GlobalStateAuthenticated _ ->
            allUsers ++ [ ( "Restricted", "/restricted" ), ( "Logout", "/logout" ) ]

viewNavLink : Url.Url -> ( String, String ) -> Html Msg
viewNavLink currentUrl ( label, href ) =
    let
        isActive : Bool
        isActive =
            currentUrl.path == href

        linkClasses : String
        linkClasses =
            String.join " "
                [ "px-3"
                , "py-2"
                , "rounded-md"
                , "text-sm"
                , "font-medium"
                , if isActive then
                    "bg-gray-100 text-gray-900"
                  else
                    "text-gray-500 hover:bg-gray-50 hover:text-gray-900"
                ]
    in
    H.button
        [ onClick (RedirectTo href)
        , Attr.class linkClasses
        ]
        [ H.text label ]



viewRightSection : Model -> Html Msg
viewRightSection model =
    H.div [ Attr.class "flex items-center space-x-4 gap-x-4" ]
        [ viewCounter model
        , viewSessionName model
        ]


viewCounter : Model -> Html Msg
viewCounter model =
    H.div [ Attr.class "flex items-center space-x-2 bg-gray-100 rounded-lg px-3 py-1" ]
        [ H.button
            [ Events.onClick DecrementCounter
            , Attr.class counterButtonClasses
            ]
            [ H.text "-" ]
        , H.span [ Attr.class "text-gray-700 w-8 text-center" ]
            [ H.text (String.fromInt model.counter) ]
        , H.button
            [ Events.onClick IncrementCounter
            , Attr.class counterButtonClasses
            ]
            [ H.text "+" ]
        ]


counterButtonClasses : String
counterButtonClasses =
    String.join " "
        [ "bg-white"
        , "rounded-md"
        , "w-6"
        , "h-6"
        , "flex"
        , "items-center"
        , "justify-center"
        , "hover:bg-gray-200"
        , "transition-colors"
        ]


viewSessionName : Model -> Html Msg
viewSessionName model =
    let
        name : String
        name =
            getUser model.global
                |> Maybe.map .name
                |> Maybe.withDefault "Welcome"
    in
    H.div [ Attr.class "text-sm text-gray-700" ]
        [ H.text name ]


viewMain : Html msg -> Html msg
viewMain content =
    H.main_
        [ Attr.class "max-w-7xl flex-grow container mx-auto sm:px-6 lg:px-8" ]
        [ content ]


viewFooter : Model -> Html Msg
viewFooter model =
    let
        year : String
        year =
            model.posix
                |> Maybe.map (Date.year << Date.fromPosix Time.utc)
                |> Maybe.map (\y -> " " ++ String.fromInt y)
                |> Maybe.withDefault ""
    in
    H.div [ Attr.class "max-w-7xl mx-auto py-6 px-4 sm:px-6 lg:px-8" ]
        [ H.p [ Attr.class "text-center text-gray-500 text-sm" ]
            [ H.text ("©" ++ year ++ " Your Company.") ]
        ]
