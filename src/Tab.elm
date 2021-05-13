module Tab exposing (..)

import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)


activeCss: Attribute msg
activeCss =
    css
        [ Css.width (px 120)
        -- , borderLeft3 (px 3) solid (hex "efefef")
        -- , borderRight3 (px 3) solid (hex "efefef")
        --, borderTopLeftRadius <| px 2
        --, borderTopRightRadius <| px 2
        --, borderRadius <| px 2
        --, padding (px 10)
        , displayFlex
        , justifyContent center
        , Css.property "color" "white"
        , textDecoration none
        , backgroundColor (hex "#616161")
        ]

inactiveCss : Attribute msg
inactiveCss =
    css
        [ Css.width (px 120)
        -- borderBottom3 (px 3) solid (hex "efefef")
        --, borderTop3 (px 3) solid (hex "efefef")
        --, padding (px 10)
        , displayFlex
        , justifyContent center
        , Css.property "color" "white"
        , textDecoration none
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
            , justifyContent left
            --, border3 (px 3) solid (hex "efefef")
            , backgroundColor (hex "#424242")
            ]
        ]
        tabs