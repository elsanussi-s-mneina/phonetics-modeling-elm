module Main exposing (main)

import Browser
import Element exposing (Element, Color, 
    width, height, px, shrink, centerX, centerY, spacing, padding, text, 
    el, alignLeft, alignRight, alignTop)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region exposing (announce, description)
import Html exposing (Html)
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
                Just userInput -> { new | phonologyText = model.phonologyText ++ userInput, glossText = "The most recently entered character is [ " ++ userInput ++ " ] which is " ++ showPhonet (analyzeIPA userInput)
                                          , currentUserInput = Nothing}
                Nothing        -> new
            


typingButtonVoiced : Model -> String -> Element Msg
typingButtonVoiced model theText =
    Input.button
        [ Background.color grey
        , Font.color charcoal
        , Border.color darkBlue
        , padding 10
        , Font.center
        ]
        { onPress = Just (Update { model | currentUserInput = Just theText })
        , label = labelFromGrapheme theText
        }


typingButtonVoiceless : Model -> String -> Element Msg 
typingButtonVoiceless model theText =
    Input.button
        [ Background.color grey
        , Font.color darkGrey
        , Border.color darkBlue
        , padding 10
        , Font.center
        ]
        { onPress = Just (Update { model | currentUserInput = Just theText })
        , label = labelFromGrapheme theText
        }

typingButtonRoundedVowel : Model -> String -> Element Msg
typingButtonRoundedVowel model theText =
    Input.button
        [ Background.color grey
        , Font.color charcoal
        , Border.color darkBlue
        , padding 10
        , Font.center
        ]
        { onPress = Just (Update { model | currentUserInput = Just theText })
        , label = labelFromGrapheme theText
        }


textModeVerbose : Bool
textModeVerbose = True

labelFromGrapheme : String -> Element Msg
labelFromGrapheme grapheme =
    if textModeVerbose
      then Element.text ( grapheme ++ " " ++ describeIPA grapheme)
      else Element.text grapheme

describeIPA : String -> String
describeIPA x =
  case x of 
   "ˈ"   -> "Primary stress"
   "ˌ"   -> "Secondary stress"
   "ː"   -> "Long"
   "ˑ"   -> "Half long" 
   "̆"    -> "Extra short"
   "|"   -> "Minor (foot) group" 
   "‖"   -> "Major (intonation) group"
   "."   -> "Syllable break"
   "‿"   -> "Linking (absence of a break)"
   "˥"   -> "Extra high"
   "̋"    -> "Extra high (2)"
   "˦"  -> "High"
   "́"  -> "High (2)"
   "˧"  -> "Mid"
   "̄"  -> "Mid (2)"
   "˨"  -> "Low"
   "̀"  -> "Low (2)"
   "˩"  -> "Extra low"
   "̏"  -> "Extra low (2)"
   "ꜜ"  -> "Downstep"
   "ꜛ"  -> "Upstep"

   {- Countour -}
   "̌" -> "Rising"
   "̂" -> "Falling"
   "᷄" -> "High rising"
   "᷅" -> "Low rising"
   "᷈" -> "Rising-falling"
   "↗" -> "Global rise"
   "↘" -> "Global fall"
   "ʰ"  -> "Aspirated"
   "ʷ"  -> "Labialised"
   "ʲ"  -> "Palatalised"
   "ˠ"  -> "Velarised"
   "ˤ"  -> "Pharyngealised"
   "ⁿ"  -> "Pre/post nasalised"
   "ˡ"  -> "Lateral release"
   "˞"  -> "Rhoticity"
   "ʼ"  -> "Ejective"
   "̚"   -> "No audible release"
   "̩"   -> "Syllabic"
   "̯"   -> "Non-syllabic"
   "̰"   -> "Creaky voiced"
   "̥"   -> "Voiceless"
   "̬"   -> "Voiced"
   "̤"   -> "Breathy voiced"
   "̊"   -> "Voiceless (diacritic placed above symbol with descender)"
   "̍"   -> "Syllabic (diacritic placed above)"
   "̪"   -> "Dental"
   "̺"   -> "Apical"
   "̻"   -> "Laminal"
   "̼"   -> "Linguolabial"
   "̣"   -> "Closer variety/Fricative"
   "̃"   -> "Nasalised"
   "̈"   -> "Centralised"
   "̽"   -> "Mid centralised"
   "̇"   -> "Palatalization/Centralization"
   _     -> showPhonet (analyzeIPA x)

typingButton : Model -> String -> Element Msg
typingButton model theText = 
    Input.button
        [ Background.color grey
        , Font.color charcoal
        , Border.color darkBlue
        , padding 10
        , Font.center
        ]
        { onPress = Just (Update { model | currentUserInput = Just theText })
        , label = labelFromGrapheme theText
        }

noBlank : List String -> List String
noBlank = List.filter (\x -> x /= " ") 


createRowOfGraphemes : Model -> List String -> String -> Element Msg
createRowOfGraphemes model graphemes heading =
    let graphemesWithoutBlanks = noBlank graphemes
    in Element.wrappedRow [spacing 10] 
       (subsubKeyboardHeading heading :: List.map (typingButton model) graphemesWithoutBlanks)


createRowOfKeys : Model -> List String -> Element Msg
createRowOfKeys model graphemes =
    Element.wrappedRow [spacing 10, alignRight] (List.map (typingButton model) graphemes)


subKeyboardHeading : String -> Element Msg
subKeyboardHeading userFacingText = 
    el
        [ Region.heading 2
        , alignLeft
        , Font.size 30
        ]
        (text userFacingText)

subsubKeyboardHeading : String -> Element Msg
subsubKeyboardHeading userFacingText = 
    el
        [ Region.heading 3
        , alignLeft
        , Font.size 25
        ]
        (text userFacingText)

view : Model -> Html Msg
view model =
    Element.layout
        [ Font.size 20
        ]
    <|
        Element.column [ height shrink, centerY, centerX, spacing 36, padding 10]
            [ Input.multiline
                [ height shrink
                , height (px 200)
                , spacing 12
                , padding 10
                , Font.size 33
                ]
                { text = model.phonologyText
                , placeholder = Just (Input.placeholder [] (text ""))
                , onChange = \new -> Update { model | phonologyText = new }
                , label = Input.labelBelow [ Font.size 23 ] (text model.glossText)
                , spellcheck = False
                }
            , el
                [ Region.heading 1
                , alignLeft
                , Font.size 36
                ]
                (text "International Phonetic Alphabet")
            , Element.column [ height shrink, centerY, spacing 10, padding 10, Border.rounded 20, Border.color charcoal, Border.width 3]
                [ subKeyboardHeading "Consonants (Pulmonic)"
                , createRowOfGraphemes model plosivePulmonic         "plosives"       
                , createRowOfGraphemes model nasalPulmonic            "nasals"              
                , createRowOfGraphemes model trillPulmonic            "trills"                  
                , createRowOfGraphemes model tapOrFlapPulmonic        "taps or flaps"              
                , createRowOfGraphemes model fricativePulmonic        "fricative"     
                , createRowOfGraphemes model lateralFricativePulmonic "lateral fricative"
                , createRowOfGraphemes model approximantPulmonic      "approximant"
                , createRowOfGraphemes model lateralApproximantPulmonic "lateral approximant"
                ]
            , Element.wrappedRow []
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
                , Element.column [ height shrink, centerY, centerX, spacing 10, padding 10, Border.rounded 20, Border.color charcoal, Border.width 3]
                    [ subKeyboardHeading "Vowels"
                    ,  createRowOfGraphemes model closeVowels  "close"  
                    ,  createRowOfGraphemes model nearCloseVowels "near close"
                    ,  createRowOfGraphemes model closeMidVowels  "close-mid"
                    ,  createRowOfGraphemes model midVowels      "mid"
                    ,  createRowOfGraphemes model openMidVowels   "open-mid"
                    ,  createRowOfGraphemes model nearOpenVowels "near open"
                    ,  createRowOfGraphemes model openVowels     "open"
                    ]
              ]
            , Element.wrappedRow []
                [ createRowOfGraphemes model diacriticsAndSuprasegmentals "Diacritics" 
                , createRowOfGraphemes model suprasegmentals "Suprasegmentals"
                , createRowOfGraphemes model toneAndWordAccents "Tones and Word Accents"
                ]
    
            ]

