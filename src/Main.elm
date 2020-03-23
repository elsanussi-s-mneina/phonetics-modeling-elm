module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = String


init : Model
init =
  "blank slate"



-- UPDATE


type Msg
  = 
  InsertText String


update : Msg -> Model -> Model
update msg model =
  case msg of
    InsertText str -> 
      model ++ str



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick (InsertText "p") ] [ text "p" ] 
    , div [] [ text (model) ]
    ]
