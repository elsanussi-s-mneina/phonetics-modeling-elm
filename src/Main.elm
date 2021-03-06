module Main exposing (main)

import Browser
import Element exposing (Element, Color, 
    width, height, px, shrink, centerX, centerY, spacing, padding, text, 
    el, alignLeft)
import Element.Region as Region
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
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
               , consonantsNonPulmonic
               , graphemesOfIPA
               , voicedIPA
               , devoicedIPA
               )


grey : Color
grey =
    Element.rgb 0.9 0.9 0.9

charcoal : Color
charcoal =
    Element.rgb 0.1 0.1 0.1

darkBlue : Color
darkBlue =
    Element.rgb 0 0 0.9



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
    , showDescriptionOnButtons = False
    , categorizeButtons = False
    , currentUserInput = Nothing
    , workspaceText = ""
    , workspaceResult = ""
    }


type alias Model =
    { phonologyText : String
    , glossText : String
    , showDescriptionOnButtons : Bool
    , categorizeButtons : Bool
    , currentUserInput : Maybe String
    , workspaceText : String
    , workspaceResult : String
    }


type Msg
    = Update Model
    | NoMsg

update : Msg -> Model -> Model
update msg model =
--    case Debug.log "msg" msg of -- Uncomment left side of this line for debugging
      case msg of
        Update new ->
            case new.currentUserInput of
                Just userInput -> { new | phonologyText = model.phonologyText ++ userInput, glossText = "The most recently entered character is [ " ++ userInput ++ " ] which represents a " ++ showPhonet (analyzeIPA userInput) ++ "."
                                          , currentUserInput = Nothing}
                Nothing        -> new
        NoMsg -> model

labelFromGrapheme : String -> Bool -> Element Msg
labelFromGrapheme grapheme textModeVerbose =
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
        , Border.color darkBlue
        , padding 10
        , Font.center
        ]
        { onPress = Just (Update { model | currentUserInput = Just theText })
        , label = labelFromGrapheme theText model.showDescriptionOnButtons
        }

noBlank : List String -> List String
noBlank = List.filter (\x -> x /= " ") 


createRowOfGraphemes : Model -> List String -> Element Msg
createRowOfGraphemes model graphemes =
    let graphemesWithoutBlanks = noBlank graphemes
    in Element.wrappedRow [spacing 10] 
       (List.map (typingButton model) graphemesWithoutBlanks)


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
        , padding 20
        , Font.size 25
        ]
        (text userFacingText)

borderedColumn : List (Element Msg) -> Element Msg
borderedColumn = Element.column [ height shrink, centerY, spacing 10, padding 10, Border.rounded 20, Border.color charcoal, Border.width 3]
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
            , Element.row []
            [ Input.text []
              { label = Input.labelLeft [] (text "Workspace:")
              , onChange = \new -> Update {model | workspaceText = new}
              , placeholder = Nothing
              , text = model.workspaceText
              }
            , Input.button
              [ Background.color grey
              , Border.color darkBlue
              , padding 10
              , Font.center
              ]
              { onPress = Just (Update { model | workspaceResult = voicedIPA model.workspaceText })
              , label = Element.text "voice it"
              }
            , Input.button
              [ Background.color grey
              , Border.color darkBlue
              , padding 10
              , Font.center
              ]
              { onPress = Just (Update { model | workspaceResult = devoicedIPA model.workspaceText })
              , label = Element.text "devoice it"
              }
            , Input.text []
              { label = Input.labelLeft [] (text "Result:")
              , onChange = \textInsideTextbox -> NoMsg
              , placeholder = Nothing
              , text = model.workspaceResult
              }
            ]
            , Input.checkbox []
                              { checked = model.showDescriptionOnButtons
                              , onChange = \new -> Update { model | showDescriptionOnButtons = new }
                              , icon = Input.defaultCheckbox
                              , label = Input.labelRight [] (text "Show description of phonemes on buttons.")
                              }
            , Input.checkbox []
                              { checked = model.categorizeButtons
                              , onChange = \new -> Update { model | categorizeButtons = new }
                              , icon = Input.defaultCheckbox
                              , label = Input.labelRight [] (text "Organize buttons in categories.")
                              }
            , borderedColumn
                (
                if model.categorizeButtons
                  then
                    [ borderedColumn
                      [ subKeyboardHeading "Consonants (Pulmonic)"
                      , subsubKeyboardHeading "plosives"
                      , createRowOfGraphemes model plosivePulmonic
                      , subsubKeyboardHeading "nasals"
                      , createRowOfGraphemes model nasalPulmonic
                      , subsubKeyboardHeading "trills"
                      , createRowOfGraphemes model trillPulmonic
                      , subsubKeyboardHeading "taps or flaps"
                      , createRowOfGraphemes model tapOrFlapPulmonic
                      , subsubKeyboardHeading "fricative"
                      , createRowOfGraphemes model fricativePulmonic
                      , subsubKeyboardHeading "lateral fricative"
                      , createRowOfGraphemes model lateralFricativePulmonic
                      , subsubKeyboardHeading "approximant"
                      , createRowOfGraphemes model approximantPulmonic
                      , subsubKeyboardHeading "lateral approximant"
                      , createRowOfGraphemes model lateralApproximantPulmonic
                      ]
                    , borderedColumn
                        [ subKeyboardHeading "Vowels"
                        , subsubKeyboardHeading "close"
                        , createRowOfGraphemes model closeVowels
                        , subsubKeyboardHeading "near close"
                        , createRowOfGraphemes model nearCloseVowels
                        , subsubKeyboardHeading "close-mid"
                        , createRowOfGraphemes model closeMidVowels
                        , subsubKeyboardHeading "mid"
                        , createRowOfGraphemes model midVowels
                        , subsubKeyboardHeading "open-mid"
                        , createRowOfGraphemes model openMidVowels
                        , subsubKeyboardHeading "near open"
                        , createRowOfGraphemes model nearOpenVowels
                        , subsubKeyboardHeading "open"
                        , createRowOfGraphemes model openVowels
                        ]
                    , borderedColumn
                        [ subKeyboardHeading "Miscellaneous"
                        , subsubKeyboardHeading "Consonants (Non-Pulmonic)"
                        , createRowOfGraphemes model consonantsNonPulmonic
                        , subsubKeyboardHeading "Other Symbols"
                        , createRowOfGraphemes model otherSymbols
                        , subsubKeyboardHeading "Diacritics"
                        , createRowOfGraphemes model diacriticsAndSuprasegmentals
                        , subsubKeyboardHeading "Suprasegmentals"
                        , createRowOfGraphemes model suprasegmentals
                        , subsubKeyboardHeading "Tones and Word Accents"
                        , createRowOfGraphemes model toneAndWordAccents
                        ]
                    ]
                  else
                    [ subKeyboardHeading "IPA characters"
                    , createRowOfGraphemes model graphemesOfIPA
                    ]
                )
            ]

