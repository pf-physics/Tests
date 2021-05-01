module MainPage exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Css exposing (..)
import Css.Global

-- MODEL
type alias Model =
  { test : String
  , message : Maybe String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model "Starting it up" Nothing
  , Cmd.none
  )



-- UPDATE

type Msg
  = Something String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Something s -> ( model, Cmd.none )


-- VIEW


view : Model -> Html Msg
view model =
  div [ css [padding (px 30)] ]
  [ h1 [css [displayFlex, justifyContent center] ] [ text "Main page" ]
  , h2 [css [displayFlex, justifyContent center] ] [ text "Welcome" ]
  , div [ css [ displayFlex, justifyContent center ] ]
    [ img [ src "Files/placeHolder.gif"] []
    ]
  ]
