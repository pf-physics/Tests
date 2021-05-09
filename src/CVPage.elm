module CVPage exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Css exposing (..)
import File.Download as Download
import Bytes exposing (Bytes)
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


downloadCV : Cmd msg
downloadCV =
  Download.url "Files/CV.pdf"


saveFrog : Cmd msg
saveFrog =
  Download.url "Files/frog.jpg"

-- UPDATE

type Msg
  = FileRequested
  | DownloadFrog
  | DownloadFile


update : Msg -> Cmd Msg
update msg =
  case msg of
    DownloadFile ->
      downloadCV
    DownloadFrog ->
      saveFrog
    _ ->
      Cmd.none 



buttonStyles : List Style
buttonStyles =
  [ padding2 (px 10) (px 25)
  , fontSize (px 20)
  , fontWeight bold
  , Css.color (hex "#ffffff")
  , backgroundColor (hex "#530082")
  , borderRadius (px 6)
  , border3 (px 1) solid (hex "#3f0e48")
  , Css.property "font-family" "calibri"
  , cursor pointer
  , hover
    [ Css.backgroundColor (hex "#380057")
    , borderRadius (px 10)
    ]
  ]

-- VIEW


view : Html Msg
view =
  div [ css [padding (px 30)] ]
  [ h1 [css [displayFlex, justifyContent center] ] [ text "CV" ]
  , h2 [css [displayFlex, justifyContent center] ] [ text "I do things sometimes" ]
  --, p [] [ text mdl.test ]
  , div [ css [ displayFlex, justifyContent center ] ]
    [ button [ onClick DownloadFile, css buttonStyles ] [ text "Download CV" ]
    ]
  , button [ onClick DownloadFrog, css buttonStyles ] [ text "Download frog" ]
  ]
