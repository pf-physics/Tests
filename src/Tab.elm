module Tab exposing (..)

import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)


activeCss: Attribute msg
activeCss =
    css
        [ border3 (px 3) solid (hex "efefef")
        , borderBottomColor transparent
        , borderTopLeftRadius <| px 2
        , borderTopRightRadius <| px 2
        , padding (px 10)
        , Css.width (pct 100)
        , displayFlex
        , justifyContent center
        ]

inactiveCss : Attribute msg
inactiveCss =
    css
        [ borderBottom3 (px 3) solid (hex "efefef")
        , padding (px 10)
        , Css.width (pct 100)
        , displayFlex
        , justifyContent center
        ]


{--
taken from:
https://elmcsspatterns.io/navigation/tab
--}
tabsWrapper : List (Html msg) -> Html msg
tabsWrapper tabs =
    div
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent spaceBetween
            ]
        ]
        tabs