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


plosivePulmonic            = [ 'p', 'b', ' ', ' ', ' ', ' ', 't', 'd', ' ', ' ', 'ʈ', 'ɖ', 'c', 'ɟ', 'k', 'g', 'q', 'ɢ', ' ', ' ', 'ʔ', ' '] -- Plosive
nasalPulmonic              = [ ' ', 'm', ' ', 'ɱ', ' ', ' ', ' ', 'n', ' ', ' ', ' ', 'ɳ', ' ', 'ɲ', ' ', 'ŋ', ' ', 'ɴ', ' ', ' ', ' ', ' '] -- Nasal
trillPulmonic              = [ ' ', 'ʙ', ' ', ' ', ' ', ' ', ' ', 'r', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 'ʀ', ' ', ' ', ' ', ' '] -- Trill
tapOrFlapPulmonic          = [ ' ', ' ', ' ', 'ⱱ', ' ', ' ', ' ', 'ɾ', ' ', ' ', ' ', 'ɽ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '] -- Tap or Flap
fricativePulmonic          = [ 'ɸ', 'β', 'f', 'v', 'θ', 'ð', 's', 'z', 'ʃ', 'ʒ', 'ʂ', 'ʐ', 'ç', 'ʝ', 'x', 'ɣ', 'χ', 'ʁ', 'ħ', 'ʕ', 'h', 'ɦ']  -- Fricative
lateralFricativePulmonic   = [ ' ', ' ', ' ', ' ', ' ', ' ', 'ɬ', 'ɮ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '] -- Lateral fricative
approximantPulmonic        = [ ' ', ' ', ' ', 'ʋ', ' ', ' ', ' ', 'ɹ', ' ', ' ', ' ', 'ɻ', ' ', 'j', ' ', 'ɰ', ' ', ' ', ' ', ' ', ' ', ' '] -- Approximant
lateralApproximantPulmonic = [ ' ', ' ', ' ', ' ', ' ', ' ', ' ', 'l', ' ', ' ', ' ', 'ɭ', ' ', 'ʎ', ' ', 'ʟ', ' ', ' ', ' ', ' ', ' ', ' '] -- Lateral approximant


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
        , width (px 50)
        ]
        { onPress = Just (Update { model | comment = model.comment ++ theText })
        , label = Element.text theText
        }
emptyButtonSpace =  
    Input.button
        [ Background.color grey
        , Font.color white
        , Border.color darkBlue
        , paddingXY 32 16
        , Border.rounded 3
        , width (px 50)
        ]
        { onPress = Nothing
        , label = Element.text ""
        }


createRowOfIPATable model listOfChars =
    let typingButtonOrSpace aChar = 
           case aChar of
             ' ' -> emptyButtonSpace
             x   -> typingButton model (String.fromChar x)
    in  Element.row [spacing 10] (List.map typingButtonOrSpace listOfChars)


view model =
    Element.layout
        [ Font.size 20
        ]
    <|
        Element.column [ width (px 1800), height shrink, centerY, centerX, spacing 36, padding 10, explain Debug.todo ]
            [ Input.multiline
                [ height shrink
                , spacing 12

                -- , padding 6
                ]
                { text = model.comment
                , placeholder = Just (Input.placeholder [] (text ""))
                , onChange = \new -> Update { model | comment = new }
                , label = Input.labelBelow [ Font.size 14 ] (text "Press buttons below to write text here.")
                , spellcheck = False
                }
            , el
                [ Region.heading 1
                , alignLeft
                , Font.size 36
                ]
                (text "International Phonetic Alphabet")
            , el
                [ Region.heading 2
                , alignLeft
                , Font.size 30
                ]
                (text "Consonants (Pulmonic)")

            , Element.column [ width (px 1800), height shrink, centerY, centerX, spacing 10, padding 10, explain Debug.todo ]
                [ (createRowOfIPATable model plosivePulmonic           )
                , (createRowOfIPATable model nasalPulmonic             )              
                , (createRowOfIPATable model trillPulmonic             )              
                , (createRowOfIPATable model tapOrFlapPulmonic         )          
                , (createRowOfIPATable model fricativePulmonic         )          
                , (createRowOfIPATable model lateralFricativePulmonic  )   
                , (createRowOfIPATable model approximantPulmonic       )        
                , (createRowOfIPATable model lateralApproximantPulmonic) 
                ]
            ]