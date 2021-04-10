module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text, form, h1, h3, input, text, img)
import Html.Events exposing (onClick)
import Html.Attributes exposing (id, style, src, type_)
import Browser
import Http
import Json.Decode exposing (Decoder, field, string)

-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- MODEL
type Model
  = Failure
  | Loading
  | Success String


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = "https://api.nasa.gov/planetary/apod?api_key=E5I1roOsvZvSKbJFALNAnoHqD12FNcmL8uoARAd3"
      , expect = Http.expectJson GotImage imageDecoder
      }
  )



-- UPDATE

type Msg
  = GotImage (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    GotImage result ->
      case result of
        Ok image ->
          (Success image, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)


-- VIEW


imageDecoder: Decoder String
imageDecoder = field "url" string


view : Model -> Html Msg
view model =
  div [style "background" "black", style "height" "2000px"] [
  div [style "text-align" "center", style "color" "#90e"]
    [ h1 [] [ text "Main page" ]
    , h3 [] [ text "Nasa Daily Image" ]
    , (case model of
    Failure ->
      text "Error loading image"

    Loading ->
      text "Loading..."

    Success image ->
        img [src (image)] []
      )
    ]
  ]