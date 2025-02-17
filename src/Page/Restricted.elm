module Page.Restricted exposing (Model, Article, init, view)

import Browser as B
import Html as H
import Html.Attributes exposing (class)
import Global exposing (GlobalStateAuthenticatedData)
import Templates.Shell as Shell
import Util.Ui exposing (..)


type alias Model =
    { fetchArticle : Maybe Article }

type alias Article =
    { title : String
    , content : List String
    }



init : GlobalStateAuthenticatedData -> Model
init _ =
    { fetchArticle = Just {
        title = "Example article"
        , content = contents
        }
    }


view : Shell.ViewProps mainMsg -> Model -> B.Document mainMsg
view shellProps model =
    { title = "Example restricted page"
    , body = Shell.view shellProps (renderArticle model.fetchArticle)
    }

renderArticle : Maybe Article -> H.Html mainMsg
renderArticle article =
    case article of
        Just a ->
            H.div [ class "flex flex-col gap-y-4 items-start" ]
                (List.append [ para a.title ] (List.map para a.content))
        
        Nothing ->
            H.text "Loading..."

contents : List String
contents =
    [ "This is an example of a page that is restricted to authenticated users."
    , "By declaring the init method to take a GlobalStateAuthenticatedData, we can ensure that the page is only rendered if the user is authenticated."
    , "If the user is not authenticated, they will be redirected to the login page - this logic is handled in Main."
    , "You can test this by logging out and then coming back to this URL."
    ]
