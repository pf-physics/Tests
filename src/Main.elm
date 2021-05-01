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
  }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let
    -- ( mdl, msg ) = MainPage.init ()
    ( mdl, msg ) = update (UrlChanged url) (Model key url Nothing Nothing Nothing)
  in
  ( mdl , msg ) -- Cmd.map IndexPage msg



-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | MainMsg MainPage.Msg
  | APIMsg APIPage.Msg


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
      let             -- Save current page so only parse route once!
         route = Url.Parser.parse routeParser url
      in
        case route of
          Just Main ->
            let
              (m, c) = MainPage.init ()
            in
              ({ model | url = url, mainModel = Just m }, Cmd.map MainMsg c)

          Just APITest ->
            let
              (m,c) = APIPage.init ()
            in
              ({ model | url = url, apiModel = Just m }, Cmd.map APIMsg c)

          _ ->
            ({ model | url = url }, Cmd.none)

    MainMsg b -> case model.mainModel of -- sooo redundant.... I should be able to assume it exists because of url change, if no exist, do nothing
                    Just a ->
                      let
                        (m, c) = MainPage.update b a
                      in
                      ({ model | mainModel = Just m }, Cmd.map MainMsg c)

                    Nothing ->
                      let
                        (m, c) = MainPage.init ()
                      in
                      ({ model | mainModel = Just m }, Cmd.map MainMsg c)

    APIMsg b -> case model.apiModel of
                    Just a ->
                      let
                        (m, c) = APIPage.update b a
                      in
                      ({ model | apiModel = Just m }, Cmd.map APIMsg c)

                    Nothing ->
                      let
                        (m, c) = APIPage.init ()
                      in
                      ({ model | apiModel = Just m }, Cmd.map APIMsg c)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  let
    title = Url.toString model.url
    route = Url.Parser.parse routeParser model.url

  in
    { title = title
    , body =
        [ Css.Global.global
            [ Css.Global.body
              [ Css.backgroundColor (rgb 0 0 0)
              , Css.color (hex "#a400ff")
              ]
            , Css.Global.typeSelector
                "::selection" -- delete this I guess
                [ Css.backgroundColor (rgb 0 0 0)
                ]
            ]
        , viewTabs (Maybe.withDefault Main route)
        , fromUnstyled (text (Maybe.withDefault "" model.message))
        , fromUnstyled (case route of
            Just p ->
              case p of
                Redshift -> text "This will be the redshift page... later"
                CV -> text "CV page... so much experience wow"

                APITest ->
                  case model.apiModel of
                    Just mmd -> toUnstyled (APIPage.view mmd) |> Html.map (APIMsg)
                    Nothing -> text "Page load fail"

                Main ->
                  case model.mainModel of
                    Just mmd -> toUnstyled (MainPage.view mmd) |> Html.map (MainMsg)
                    Nothing -> text "Page load fail"

            Nothing ->
              case model.mainModel of -- THIS IS DEFAULT NOW
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