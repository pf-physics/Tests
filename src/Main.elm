module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Dict exposing (Dict)
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string)
import MainPage

-- MAIN

-- .init({flags: window.location.href})
main : Program String Model Msg
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
  , mainModel : (Maybe MainPage.Model)
  , message : Maybe String
  }


type Page
  = Main
  | Redshift
  | CV
  | APITest

init : String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let
    ( mdl, msg ) = MainPage.init () -- whichever page is init handled by url, here or in update?
  in
  ( Model key url (Just mdl) (Just flags) , Cmd.map IndexPage msg )



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | IndexPage MainPage.Msg


type PageMsg
  = Froot

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url -> -- PATTERN MATCH on the page YAH TODO, also only if doesn't already exist, do init
      let
         (m, c) = MainPage.init ()
      in
         ({ model | url = url, mainModel = Just m }, Cmd.map IndexPage c)

    IndexPage b -> case model.mainModel of -- pattern match here!
                    Just a ->
                      let
                        (m, c) = MainPage.update b a
                      in
                      ({ model | mainModel = Just m }, Cmd.map IndexPage c)

                    Nothing ->
                      let
                        (m, c) = MainPage.init ()
                      in
                      ({ model | mainModel = Just m }, Cmd.map IndexPage c)


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
    , Url.Parser.map Main (Url.Parser.s ("index.html"))
    , Url.Parser.map Redshift (Url.Parser.s ("redshift"))
    , Url.Parser.map APITest (Url.Parser.s ("apitest"))
    ]

view : Model -> Browser.Document Msg
view model =
  let
    title = Url.toString model.url

    route = Url.Parser.parse routeParser model.url
    test = model.url.host

    page = Dict.get title pageMap

  in
    { title = title
    , body =
        [
        div [ style "background" "black", style "color" "#90e"] [
        text (Maybe.withDefault "Why not?" model.message)
        , b [] [ text title ]
        , p [] [ text model.url.path ]
        , b [] [ text test ]
        , ul []
            [ viewLink "main"
            , viewLink "redshift"
            , viewLink "CV"
            , viewLink "apitest"
            ]
        , case route of
            Just p ->
              case p of
                Redshift -> text "This will be the redshift page... later"
                CV -> text "CV page... so much experience wow"
                APITest -> text "Api ? I guess"
                Main -> case model.mainModel of
                                            Just mmd -> MainPage.view mmd |> Html.map (IndexPage)
                                            Nothing -> text "Page load fail"

            Nothing -> text "This isn't a real page so I'll just remind you that snails are not slugs and cannot be removed from their shells!"
        ]
        ]
    }

-- The Repo name, will change eventually
websiteTitle = "Tests"

viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]