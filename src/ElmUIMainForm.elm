module ElmUIMainForm exposing (..)

{-| -}

-- Note: This code started by copying the sample 
-- at https://github.com/mdgriffith/elm-ui/blob/master/examples/Form.elm
-- and deleting various things, and making more and more modifications.

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region


white =
    Element.rgb 1 1 1


grey =
    Element.rgb 0.9 0.9 0.9


blue =
    Element.rgb 0 0 0.8


red =
    Element.rgb 0.8 0 0


darkBlue =
    Element.rgb 0 0 0.9


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init =
    { username = ""
    , password = ""
    , agreeTOS = False
    , comment = ""
    , lunch = Gyro
    , spiciness = 2
    }


type alias Form =
    { username : String
    , password : String
    , agreeTOS : Bool
    , comment : String
    , lunch : Lunch
    , spiciness : Float
    }


type Msg
    = Update Form


update msg model =
    case Debug.log "msg" msg of
        Update new ->
            new


type Lunch
    = Burrito
    | Taco
    | Gyro



typingButton model theText = 
    Input.button
        [ Background.color blue
        , Font.color white
        , Border.color darkBlue
        , paddingXY 32 16
        , Border.rounded 3
        , width fill
        ]
        { onPress = Just (Update { model | comment = model.comment ++ theText })
        , label = Element.text theText
        }


view model =
    Element.layout
        [ Font.size 20
        ]
    <|
        Element.column [ width (px 800), height shrink, centerY, centerX, spacing 36, padding 10, explain Debug.todo ]
            [ Input.multiline
                [ height shrink
                , spacing 12

                -- , padding 6
                ]
                { text = model.comment
                , placeholder = Just (Input.placeholder [] (text ""))
                , onChange = \new -> Update { model | comment = new }
                , label = Input.labelAbove [ Font.size 14 ] (text "")
                , spellcheck = False
                }
            , el
                [ Region.heading 1
                , alignLeft
                , Font.size 36
                ]
                (text "International Phonetic Alphabet")
            , Element.row []
                [ typingButton model "p"
                , typingButton model "b"
                ]
            ]