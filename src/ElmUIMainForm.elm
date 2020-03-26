module ElmUIMainForm exposing (main)

import Browser
import Element exposing (Element, Color, 
    width, height, px, shrink, centerX, centerY, spacing, padding, text, 
    el, alignLeft, alignRight, alignTop)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Lib exposing (showPhonet)
import Grapheme.InternationalPhoneticAlphabet exposing 
               ( analyzeIPA
               , plosivePulmonic           
               , nasalPulmonic             
               , trillPulmonic             
               , tapOrFlapPulmonic         
               , fricativePulmonic         
               , lateralFricativePulmonic  
               , approximantPulmonic       
               , lateralApproximantPulmonic
               , closeVowels    
               , nearCloseVowels
               , closeMidVowels 
               , midVowels      
               , openMidVowels  
               , nearOpenVowels 
               , openVowels  
               , otherSymbols
               , suprasegmentals
               , diacriticsAndSuprasegmentals
               , toneAndWordAccents
               , consonantsNonPulmonicRow1
               , consonantsNonPulmonicRow2
               , consonantsNonPulmonicRow3
               , consonantsNonPulmonicRow4
               , consonantsNonPulmonicRow5
               )

white : Color
white =
    Element.rgb 1 1 1


grey : Color
grey =
    Element.rgb 0.9 0.9 0.9

darkGrey : Color
darkGrey =
    Element.rgb 0.2 0.2 0.2

charcoal : Color
charcoal =
    Element.rgb 0.1 0.1 0.1


blue : Color
blue =
    Element.rgb 0 0 0.8


red : Color
red =
    Element.rgb 0.8 0 0



darkBlue : Color
darkBlue =
    Element.rgb 0 0 0.9


isEven : Int -> Bool
isEven x = modBy 2 x == 0

voicedMask : List Bool
voicedMask = List.map isEven (List.range 1 (List.length plosivePulmonic))


roundedVowels : List String
roundedVowels = ["y", "ʉ", "u", "ʏ",            "ʊ", "ø", "ɵ", "o", "œ", "ɞ", "ɔ",  "ɶ", "ɒ"]


main : Program () Model  Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    { phonologyText = ""
    , glossText = "Press buttons below to write text here."  -- This should contain what we call the phonemes in English
    , currentUserInput = Nothing
    }


type alias Model =
    { phonologyText : String
    , glossText : String
    , currentUserInput : Maybe String
    }


type Msg
    = Update Model

update : Msg -> Model -> Model
update msg model =
--    case Debug.log "msg" msg of -- Uncomment left side of this line for debugging
      case msg of
        Update new ->
            case new.currentUserInput of
                Just userInput -> { new | phonologyText = model.phonologyText ++ userInput, glossText = showPhonet (analyzeIPA userInput)
                                          , currentUserInput = Nothing}
                Nothing        -> new
            


typingButtonVoiced : Model -> String -> Element Msg
typingButtonVoiced model theText =
    Input.button
        [ Background.color grey
        , Font.color charcoal
        , Border.color darkBlue
        , padding 10
        , Border.roundEach 
            { topLeft     = 0
            , topRight    = 20
            , bottomLeft  = 0
            , bottomRight = 0
            }
        , width (px 50)
        , Font.center
        ]
        { onPress = Just (Update { model | currentUserInput = Just theText })
        , label = Element.text theText
        }


typingButtonVoiceless : Model -> String -> Element Msg 
typingButtonVoiceless model theText =
    Input.button
        [ Background.color grey
        , Font.color darkGrey
        , Border.color darkBlue
        , padding 10
        , Border.roundEach 
            { topLeft     = 20
            , topRight    = 0
            , bottomLeft  = 0
            , bottomRight = 0
            }
        , width (px 50)
        , Font.center
        ]
        { onPress = Just (Update { model | currentUserInput = Just theText })
        , label = Element.text theText
        }

typingButtonRoundedVowel : Model -> String -> Element Msg
typingButtonRoundedVowel model theText =
    Input.button
        [ Background.color grey
        , Font.color charcoal
        , Border.color darkBlue
        , padding 10
        , Border.roundEach 
            { topLeft     = 50
            , topRight    = 50
            , bottomLeft  = 50
            , bottomRight = 50
            }
        , width (px 50)
        , Font.center
        ]
        { onPress = Just (Update { model | currentUserInput = Just theText })
        , label = Element.text theText
        }

typingButtonVowel : Model -> String -> Element Msg 
typingButtonVowel model theText = 
    Input.button
        [ Background.color grey
        , Font.color charcoal
        , Border.color darkBlue
        , padding 10
        , Border.roundEach 
            { topLeft     = 0
            , topRight    = 20
            , bottomLeft  = 0
            , bottomRight = 20
            }
        , width (px 50)
        , Font.center
        ]
        { onPress = Just (Update { model |  currentUserInput = Just theText })
        , label = Element.text theText
        }

typingButton : Model -> String -> Element Msg
typingButton model theText = 
    Input.button
        [ Background.color grey
        , Font.color charcoal
        , Border.color darkBlue
        , padding 10
        , Border.rounded 4 
        , width (px 50)
        , Font.center
        ]
        { onPress = Just (Update { model | currentUserInput = Just theText })
        , label = Element.text theText
        }
emptyButtonSpace : Element Msg  
emptyButtonSpace =  
    Input.button
        [ Background.color grey
        , Font.color white
        , Border.color darkBlue
        , padding 10
        , Border.rounded 0
        , width (px 50)
        ]
        { onPress = Nothing
        , label = Element.text ""
        }


createRowOfIPATable : Model -> List String -> Element Msg
createRowOfIPATable model graphemes =
    let typingButtonOrSpace aChar voiced = 
           case aChar of
             " " -> emptyButtonSpace
             x   -> 
                if voiced
                    then typingButtonVoiced model x
                    else typingButtonVoiceless model x
    in  Element.row [spacing 10] (List.map2 typingButtonOrSpace graphemes voicedMask)

createRowOfVowels : Model -> List String -> Element Msg
createRowOfVowels model graphemes =
    let typingButtonOrSpace aChar =
           case aChar of
             " " -> emptyButtonSpace
             x   -> 
                if List.member aChar roundedVowels
                    then typingButtonRoundedVowel model x
                    else typingButtonVowel model x
    in  Element.row [spacing 10, alignRight] (List.map typingButtonOrSpace graphemes)

createRowOfKeys : Model -> List String -> Element Msg
createRowOfKeys model graphemes =
    let typingButtonOrSpace aChar =
           case aChar of
             " " -> emptyButtonSpace
             x   -> typingButton model x
    in  Element.row [spacing 10, alignRight] (List.map typingButtonOrSpace graphemes)

createColumnOfKeys : String -> Model -> List String -> Element Msg
createColumnOfKeys subHeadingText model graphemes =
    let typingButtonOrSpace aChar =
           case aChar of
             " " -> emptyButtonSpace
             x   -> typingButton model x
    in Element.column [ height shrink, alignTop, centerX, spacing 10, padding 10, Border.rounded 20, Border.color charcoal, Border.width 3]
        (  subKeyboardHeading subHeadingText
           :: List.map typingButtonOrSpace graphemes
        )

subKeyboardHeading : String -> Element Msg
subKeyboardHeading userFacingText = 
    el
        [ Region.heading 2
        , alignLeft
        , Font.size 30
        ]
        (text userFacingText)

view : Model -> Html.Html Msg
view model =
    Element.layout
        [ Font.size 20
        ]
    <|
        Element.column [ width (px 1800), height shrink, centerY, centerX, spacing 36, padding 10]
            [ Input.multiline
                [ height shrink
                , width (px 1200)
                , height (px 200)
                , spacing 12
                , padding 10
                ]
                { text = model.phonologyText
                , placeholder = Just (Input.placeholder [] (text ""))
                , onChange = \new -> Update { model | phonologyText = new }
                , label = Input.labelBelow [ Font.size 14 ] (text model.glossText)
                , spellcheck = False
                }
            , el
                [ Region.heading 1
                , alignLeft
                , Font.size 36
                ]
                (text "International Phonetic Alphabet")
            , Element.column [ height shrink, centerY, alignLeft, spacing 10, padding 10, Border.rounded 20, Border.color charcoal, Border.width 3]
                [ subKeyboardHeading "Consonants (Pulmonic)"
                , createRowOfIPATable model plosivePulmonic           
                , createRowOfIPATable model nasalPulmonic                           
                , createRowOfIPATable model trillPulmonic                           
                , createRowOfIPATable model tapOrFlapPulmonic                   
                , createRowOfIPATable model fricativePulmonic                   
                , createRowOfIPATable model lateralFricativePulmonic     
                , createRowOfIPATable model approximantPulmonic               
                , createRowOfIPATable model lateralApproximantPulmonic 
                ]
            , Element.row []
              [
                Element.column []
                [
                    Element.column [height shrink, centerY, alignLeft, spacing 10, padding 10, Border.rounded 20, Border.color charcoal, Border.width 3]
                        [ subKeyboardHeading "Consonants (Non-Pulmonic)"
                        , createRowOfKeys model consonantsNonPulmonicRow1
                        , createRowOfKeys model consonantsNonPulmonicRow2
                        , createRowOfKeys model consonantsNonPulmonicRow3
                        , createRowOfKeys model consonantsNonPulmonicRow4
                        , createRowOfKeys model consonantsNonPulmonicRow5
                        ]
                    , Element.column [ height shrink, centerY, alignLeft, spacing 10, padding 10, Border.rounded 20, Border.color charcoal, Border.width 3]
                        [ subKeyboardHeading "Other Symbols"
                        , createRowOfKeys model otherSymbols
                        ]
                ]
                , Element.column [ width (px 500), height shrink, centerY, centerX, spacing 10, padding 10, Border.rounded 20, Border.color charcoal, Border.width 3]
                    [ subKeyboardHeading "Vowels"
                    ,  createRowOfVowels model closeVowels    
                    ,  createRowOfVowels model nearCloseVowels
                    ,  createRowOfVowels model closeMidVowels 
                    ,  createRowOfVowels model midVowels      
                    ,  createRowOfVowels model openMidVowels  
                    ,  createRowOfVowels model nearOpenVowels 
                    ,  createRowOfVowels model openVowels     
                    ]
              ]
            , Element.row []
                [ createColumnOfKeys "Diacritics" model diacriticsAndSuprasegmentals
                , createColumnOfKeys "Suprasegmentals" model suprasegmentals
                , createColumnOfKeys "Tones and Word Accents" model toneAndWordAccents
                ]
    
            ]

