module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Dict exposing (Dict)
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string)


-- MAIN


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL


type alias Model =
  { key : Nav.Key
  , url : Url.Url
  }


type Page
  = Main
  | Redshift
  | CV

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url, Cmd.none )



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

pageMap: Dict String Page
pageMap =
  Dict.fromList
  [("", Main), ("main", Main), ("redshift", Redshift)]

routeParser : Parser (Page -> a) a
routeParser =
  oneOf
    [ Url.Parser.map Main (Url.Parser.s ("main"))
    , Url.Parser.map CV (Url.Parser.s ("CV"))
    , Url.Parser.map Redshift (Url.Parser.s ("https://pf-physics.github.io/Tests/main"))
    ]

view : Model -> Browser.Document Msg
view model =
  let
    title = Url.toString model.url

    route = Url.Parser.parse routeParser model.url
    test = model.url.host -- THIS ONE I believe

    page = Dict.get title pageMap
  in
    { title = title
    , body =
        [ text "Cry "
        , b [] [ text title ]
        , p [] [ text model.url.path ]
        , b [] [ text test ]
        , ul []
            [ viewLink "main"
            , viewLink "redshift"
            , viewLink "CV"
            ]
        , case route of
            Just p ->
              case p of
                Main -> text "This is the main page. Under construction!"
                Redshift -> text "This will be the redshift page... later"
                CV -> text "CV page... so much experience wow"

            Nothing -> text "This isn't a real page so I'll just remind you that snails are not slugs and cannot be removed from their shells!"
        ]
    }

-- The Repo name, will change eventually
websiteTitle = "Tests/"

viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]