module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Styled exposing (toUnstyled, fromUnstyled)
import Html.Styled.Events exposing (..)
import Css exposing (..)
import Css.Global
import Url
import Dict exposing (Dict)
import Url.Parser exposing (Parser, (</>), int, map, oneOf, s, string)
import MainPage
import Navigation exposing (..)
import APIPage
import CVPage

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
  , mainModel : (Maybe MainPage.Model)
  , apiModel : (Maybe APIPage.Model )
  , message : Maybe String
  , currentPage : Page
  }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let
    -- ( mdl, msg ) = MainPage.init ()
    ( mdl, msg ) = update (UrlChanged url) (Model key url Nothing Nothing Nothing Main )
  in
  ( mdl , msg ) -- Cmd.map IndexPage msg



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | MainMsg MainPage.Msg
  | APIMsg APIPage.Msg
  | CVMsg CVPage.Msg


-- VIEW


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
        -- This is only being done to allow # routing, normally would use parser
        urlString = Maybe.map (\v -> "#" ++ v) (List.head (List.reverse (String.split "#" (Url.toString url))))
        route = Dict.get (Maybe.withDefault "" urlString) pageMap
        -- route = Url.Parser.parse routeParser url
      in
        case route of
          Just Main ->
            let
              (m, c) = MainPage.init ()
            in
              ({ model | url = url, mainModel = Just m, currentPage = Main }, Cmd.map MainMsg c)

          Just APITest ->
            let
              (m,c) = APIPage.init ()
            in
              ({ model | url = url, apiModel = Just m, currentPage = APITest }, Cmd.map APIMsg c)


          Just Redshift ->
            ({ model | url = url, currentPage = Redshift }, Cmd.none )

          Just CV ->
            ({ model | url = url, currentPage = CV }, Cmd.none )

          _ ->
            let
              (m, c) = MainPage.init ()
            in
              ({ model | url = url, mainModel = Just m, currentPage = Main }, Cmd.map MainMsg c) -- Default is main

    MainMsg b -> case model.mainModel of
                    Just a ->
                      let
                        (m, c) = MainPage.update b a
                      in
                      ({ model | mainModel = Just m }, Cmd.map MainMsg c)

                    Nothing ->
                      ( model, Cmd.none )

    APIMsg b -> case model.apiModel of
                    Just a ->
                      let
                        (m, c) = APIPage.update b a
                      in
                      ({ model | apiModel = Just m }, Cmd.map APIMsg c)

                    Nothing ->
                      ( model, Cmd.none )

    CVMsg c ->
          ( model, Cmd.map CVMsg (CVPage.update c) )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  let
    title = Url.toString model.url

  in
    { title = "Soup time"
    , body =
        [ Css.Global.global
            [ Css.Global.body
              [ Css.backgroundColor (hex "#121212")
              , Css.color (hex "#c967ff")
              , Css.property "font-family" "arial"
              ]
            ]
        , viewTabs model.currentPage
        , fromUnstyled (text (Maybe.withDefault "" model.message))
        , fromUnstyled (case model.currentPage of
              Redshift -> text "This will be the redshift page... later"

              APITest ->
                case model.apiModel of
                  Just mmd -> toUnstyled (APIPage.view mmd) |> Html.map (APIMsg)
                  Nothing -> text "Page load fail"

              CV ->
                (toUnstyled CVPage.view |> Html.map CVMsg)

              Main ->
                case model.mainModel of
                  Just mmd -> toUnstyled (MainPage.view mmd) |> Html.map (MainMsg)
                  Nothing -> text "Page load fail"
            )
        ] |> List.map toUnstyled
    }

-- The Repo name, will change eventually
websiteTitle = "Tests"

viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [ text path ] ]