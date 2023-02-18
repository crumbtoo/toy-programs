module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    {
        tabno : Int
    }


init : Model
init = Model 0



-- UPDATE


type Msg
    = TabChange Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        TabChange tabno ->
            { model | tabno = tabno }



-- VIEW


view : Model -> Html Msg
view model =
    div []
    [
        viewTabButton 0,
        viewTabButton 1,
        viewTabButton 2,
        viewTabButton 3,
        viewTab model.tabno
    ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewTabButton : Int -> Html Msg
viewTabButton newtabno =
    button [ onClick (TabChange newtabno), value (String.fromInt newtabno) ] [ text (String.fromInt newtabno) ]

viewTab : Int -> Html msg
viewTab tabno =
    case tabno of
        0 -> div [] [ text "hi" ]
        1 -> div [] [ text "hello" ]
        2 -> div [] [ text "frrrm" ]
        _ -> div [] [ text "naauurr" ]


-- viewValidation : Model -> Html msg
-- viewValidation model =
--     if model.password == model.passwordAgain then
--         div [ style "color" "green" ] [ text "OK" ]
--     else
--         div [ style "color" "red" ] [ text "Passwords do not match!" ]
