module Route exposing
    ( Route(..)
    , fromUrl
    )

import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)


type Route
    = RouteIndex
    | RouteLogin
    | RouteLogout
    | RouteContent
    | RouteRestricted

fromUrl : Url -> Maybe Route
fromUrl =
    parse routeParser


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map RouteIndex top
        , map RouteLogin (s "login")
        , map RouteLogout (s "logout")
        , map RouteContent (s "content")
        , map RouteRestricted (s "restricted")
        ]
