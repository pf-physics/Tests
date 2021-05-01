module APIPage exposing (..)

import Browser
import Html.Events exposing (onClick)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Browser
import Http
import Json.Decode exposing (Decoder, field, string)

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


update : Msg -> Model -> (Model, Cmd Msg)
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
    [ h1 [] [ text "API page" ]
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