module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text, textarea, b, h1, h2, table, tr, th, td, br, span)
import Html.Attributes exposing (attribute, id, class)
import Html.Events exposing (onClick)
import Lib exposing (englishDescription, Phonet(..), VocalFolds(..), Place(..), Manner(..), Airstream(..))



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = String


init : Model
init =
  ""  -- Start with nothing in the textbox.



-- UPDATE


type Msg
  = InsertText String
  | Describe


update : Msg -> Model -> Model
update msg model =
  case msg of
    InsertText str -> 
      model ++ str
    Describe ->
        model ++ englishDescription (Consonant Voiceless Velar Fricative PulmonicEgressive)


typingButton theText = 
    button [ onClick (InsertText theText) ]      [ text theText ]

typingButtonWithClass theClass theText =
    button [ class theClass, onClick (InsertText theText) ]      [ text theText ]

typingButtonVoiceless theText = typingButtonWithClass "voiceless" theText
typingButtonVoiced theText = typingButtonWithClass "voiced" theText

typingButtonVowel theLeftMargin theText  =
    button [ class "vowel", attribute "style" ("margin-left:" ++ theLeftMargin ++ "px"), onClick (InsertText theText) ]
                                [ text theText ]

typingButtonVowelNoMargin theText  =
    button [ class "vowel", onClick (InsertText theText) ]
                                [ text theText ]

typingButtonRoundedVowel theLeftMargin theText  = 
    button [ class "vowel rounded", attribute "style" ("margin-left:" ++ theLeftMargin ++ "px"), onClick (InsertText theText) ]
                                [ text theText ]

typingButtonRoundedVowelNoMargin theText  = 
    button [ class "vowel rounded", onClick (InsertText theText) ]
                                [ text theText ]

emptyButtonSpace = button [ attribute "style" "visibility: hidden;" ] []


-- VIEW


view : Model -> Html Msg
view model =
  div []
            [ button [ class "voiceless", onClick (Describe) ]
                [ text "describe" ] 
            , textarea [ id "screen"] 
                [text (model) ]
            , b []
                [ text "Press buttons below to write text here." ]
            , h1 []
                [ text "International Phonetic Alphabet" ]
            , h2 []
                [ text "Consonants (Pulmonic)" ]
            , table [ attribute "style" "border: 1px solid black;" ]
                [ tr []
                    [ th []
                        []
                    , th []
                        [ text "Bilabial        " ]
                    , th []
                        [ text "Labio-dental        " ]
                    , th []
                        [ text "Dental        " ]
                    , th []
                        [ text "Alveolar        " ]
                    , th []
                        [ text "Post-alveolar        " ]
                    , th []
                        [ text "Retroflex        " ]
                    , th []
                        [ text "Palatal        " ]
                    , th []
                        [ text "Velar        " ]
                    , th []
                        [ text "Uvular        " ]
                    , th []
                        [ text "Pharyngeal        " ]
                    , th []
                        [ text "Glottal        " ]
                    ]
                , tr []
                    [ th []
                        [ text "Plosive" ]
                    , td []
                        [ typingButtonVoiceless "p"
                        , typingButtonVoiced "b"
                        ]
                    , td []
                        []
                    , td []
                        []
                    , td []
                        [ typingButtonVoiceless "t"
                        , typingButtonVoiced "d"
                        ]
                    , td []
                        []
                    , td []
                        [ typingButtonVoiceless "ʈ"
                        , typingButtonVoiced "ɖ"
                        ]
                    , td []
                        [ typingButtonVoiceless "c"
                        , typingButtonVoiced "ɟ"
                        ]
                    , td []
                        [ typingButtonVoiceless "k"
                        , typingButtonVoiced "g"
                        ]
                    , td []
                        [ typingButtonVoiceless "q"
                        , typingButtonVoiced "ɢ"
                        ]
                    , td []
                        []
                    , td []
                        [ typingButtonVoiceless "ʔ"
                        , emptyButtonSpace
                        ]
                    ]
                , tr []
                    [ th []
                        [ text "Nasal" ]
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "m"
                        ]
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "ɱ"
                        ]
                    , td []
                        []
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "n"
                        ]
                    , td []
                        []
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "ɳ"
                        ]
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "ɲ"
                        ]
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "ɴ"
                        ]
                    , td []
                        []
                    , td []
                        []
                    , td []
                        []
                    ]
                , tr []
                    [ th []
                        [ text "Trill" ]
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "ʙ"
                        ]
                    , td []
                        []
                    , td []
                        []
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "r"
                        ]
                    , td []
                        []
                    , td []
                        []
                    , td []
                        []
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "ʀ"
                        ]
                    , td []
                        []
                    , td []
                        []
                    , td []
                        []
                    ]
                , tr []
                    [ th []
                        [ text "Tap or Flap" ]
                    , td []
                        []
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "ⱱ"
                        ]
                    , td []
                        []
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "ɾ"
                        ]
                    , td []
                        []
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "ɽ"
                        ]
                    , td []
                        []
                    , td []
                        []
                    , td []
                        []
                    , td []
                        []
                    , td []
                        []
                    ]
                , tr []
                    [ th []
                        [ text "Fricative" ]
                    , td []
                        [ typingButtonVoiceless "ɸ"
                        , typingButtonVoiced "β"
                        ]
                    , td []
                        [ typingButtonVoiceless "f"
                        , typingButtonVoiced "v"
                        ]
                    , td []
                        [ typingButtonVoiceless "θ"
                        , typingButtonVoiced "ð"
                        ]
                    , td []
                        [ typingButtonVoiceless "s"
                        , typingButtonVoiced "z"
                        ]
                    , td []
                        [ typingButtonVoiceless "ʃ"
                        , typingButtonVoiced "ʒ"
                        ]
                    , td []
                        [ typingButtonVoiceless "ʂ"
                        , typingButtonVoiced "ʐ"
                        ]
                    , td []
                        [ typingButtonVoiceless "ç"
                        , typingButtonVoiced "ʝ"
                        ]
                    , td []
                        [ typingButtonVoiceless "x"
                        , typingButtonVoiced "ɣ"
                        ]
                    , td []
                        [ typingButtonVoiceless "χ"
                        , typingButtonVoiced "ʁ" 
                        ]
                    , td []
                        [ typingButtonVoiceless "ħ"
                        , typingButtonVoiced "ʕ"
                        ]
                    , td []
                        [ typingButtonVoiceless "h"
                        , typingButtonVoiced "ɦ"
                        ]
                    ]
                , tr []
                    [ th []
                        [ text "Lateral Fricative" ]
                    , td []
                        []
                    , td []
                        []
                    , td []
                        []
                    , td []
                        [ typingButtonVoiceless "ɬ"
                        , typingButtonVoiced "ɮ"
                        ]
                    , td []
                        []
                    , td []
                        []
                    , td []
                        []
                    , td []
                        []
                    , td []
                        []
                    , td []
                        []
                    , td []
                        []
                    ]
                , tr []
                    [ th []
                        [ text "Approximant" ]
                    , td []
                        []
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "ʋ"
                        ]
                    , td []
                        []
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "ɹ"
                        ]
                    , td []
                        []
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "ɻ"
                        ]
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "j"
                        ]
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "ɰ"
                        ]
                    , td []
                        []
                    , td []
                        []
                    , td []
                        []
                    ]
                , tr []
                    [ th []
                        [ text "Lateral Approximant" ]
                    , td []
                        []
                    , td []
                        []
                    , td []
                        []
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "l"
                        ]
                    , td []
                        []
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "ɭ"
                        ]
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "ʎ"
                        ]
                    , td []
                        [ emptyButtonSpace
                        , typingButtonVoiced "ʟ"
                        ]
                    , td []
                        []
                    , td []
                        []
                    , td []
                        []
                    ]
                ]
            , table []
                [ tr []
                    [ td [ attribute "style" "width: 600px;" ]
                        [ h2 []
                            [ text "Consonants (Non-Pulmonic)" ]
                        , table []
                            [ tr []
                                [ th []
                                    [ text "Clicks" ]
                                , th []
                                    [ text "Voiced implosives" ]
                                , th []
                                    [ text "Ejective" ]
                                ]
                            , tr []
                                [ td []
                                    [ typingButton "ʘ" 
                                    , text "Bilabial"
                                    ]
                                , td []
                                    [ typingButton "ɓ"
                                    , text "Bilabial"
                                    ]
                                , td []
                                    [ typingButton "ʼ"
                                    , text "Ejective"
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ typingButton "ǀ"
                                    , text "Dental  "
                                    ]
                                , td []
                                    [ typingButton "ɗ"
                                    , text "Dental/alveolar"
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ typingButton "ǃ"
                                    , text "(Post)alveolar "
                                    ]
                                , td []
                                    [ typingButton "ʄ"
                                    , text "Palatal"
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ typingButton "ǂ"
                                    , text "Palatoalveolar"
                                    ]
                                , td []
                                    [ typingButton "ɠ"
                                    , text "Velar"
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ typingButton "ǁ"
                                    , text "Alveolar lateral"
                                    ]
                                , td []
                                    [ typingButton "ʛ"
                                    , text "Uvular"
                                    ]
                                ]
                            ]
                        , br []
                            []
                        ]
                    , td [ attribute "style" "width: 600px;" ]
                        [ h2 []
                            [ text "Vowels" ]
                        , br []
                            []
                        , span [ attribute "style" "margin-left:30px" ]
                            [ text "Front" ]
                        , span [ attribute "style" "margin-left:170px" ]
                            [ text "Central" ]
                        , span [ attribute "style" "margin-left:170px" ]
                            [ text "Back" ]
                        , br []
                            []
                        , typingButtonVowelNoMargin "i"
                        , typingButtonRoundedVowelNoMargin "y"
                        , typingButtonVowel "100" "ɨ"
                        , typingButtonRoundedVowelNoMargin "ʉ"
                        , typingButtonVowel "100" "ɯ"
                        , typingButtonRoundedVowelNoMargin "u"
                        , text "Close            "
                        , br []
                            []
                        , typingButtonVowel "100" "ɪ"
                        , typingButtonRoundedVowelNoMargin "ʏ"
                        , typingButtonRoundedVowel "150" "ʊ"
                        , br []
                            []
                        , typingButtonVowel "50" "e"
                        , typingButtonRoundedVowelNoMargin "ø"
                        , typingButtonVowel "50" "ɘ"
                        , typingButtonRoundedVowelNoMargin "ɵ"
                        , typingButtonVowel "100" "ɤ"
                        , typingButtonRoundedVowelNoMargin "o"
                        , text "Close-mid            "
                        , br []
                            []
                        , typingButtonVowel "230" "ə"
                        , br []
                            []
                        , typingButtonVowel "80" "ɛ"
                        , typingButtonRoundedVowelNoMargin "œ"
                        , typingButtonVowel "20" "ɜ"
                        , typingButtonRoundedVowelNoMargin "ɞ"
                        , typingButtonVowel "100" "ʌ"
                        , typingButtonRoundedVowelNoMargin "ɔ"
                        , text "Open-mid            "
                        , br []
                            []
                        , typingButtonVowel "120" "æ"
                        , typingButtonVowel "60" "ɐ"
                        , br []
                            []
                        , typingButtonVowel "110" "a"
                        , typingButtonRoundedVowelNoMargin "ɶ"
                        , typingButtonVowel "200" "ɑ"
                        , typingButtonRoundedVowelNoMargin "ɒ"
                        , text "Open        "
                        ]
                    ]
                , tr []
                    [ td []
                        [ h2 []
                            [ text "Other Symbols" ]
                        , table []
                            [ tr []
                                [ td []
                                    
                                    [ typingButtonVoiceless "ʍ"
                                    , text "Voiceless labial-velar fricative"
                                    ]
                                , td []
                                    [ typingButtonVoiceless "ɕ"
                                    , typingButtonVoiced "ʑ"
                                    , text "Alveolo-palatal fricatives"
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ typingButtonVoiced "w"
                                    , text "Voiced labial-velar approximant"
                                    ]
                                , td []
                                    [ typingButtonVoiced "ɺ"
                                    , text "Voiced alveolar lateral flap"
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ typingButtonVoiced "ɥ"
                                    , text "Voiced labial-palatal approximant"
                                    ]
                                , td []
                                    [ typingButtonVoiceless "ɧ"
                                    , text "Simultaneous ʃ and x"
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ typingButtonVoiceless "ʜ"
                                    , text "Voiceless epiglottal fricative"
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ typingButtonVoiced "ʢ"
                                    , text "Voiced epiglottal fricative"
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ typingButton "ʡ"
                                    , text "Epiglottal plosive"
                                    ]
                                , td []
                                    [ typingButton "͡"
                                    , typingButton "͜"
                                    , text "Double articulation/ affricate"
                                    ]
                                ]
                            ]
                        , br []
                            []
                        , h2 []
                            [ text "Diacritics" ]
                        , table []
                            [ tr []
                                [ td []
                                    [ typingButton "̥"
                                    , typingButton "̊"
                                    , text "Voiceless "
                                    ]
                                , td []
                                    [ typingButton "̤"
                                    , text "Breathy voiced"
                                    ]
                                , td []
                                    [ typingButton "̪"
                                    , text "Dental"
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ typingButton "̬"
                                    , text "Voiced"
                                    ]
                                , td []
                                    [ typingButton "̰"
                                    , text "Creaky voiced"
                                    ]
                                , td []
                                    [ typingButton "̺"
                                    , text "Apical"
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ typingButton "ʰ"
                                    , text "Aspirated"
                                    ]
                                , td []
                                    [ typingButton "̼"
                                    , text "Linguolabial"
                                    ]
                                , td []
                                    [ typingButton "̻"
                                    , text "Laminal"
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ typingButton "̹"
                                    , text "More rounded"
                                    ]
                                , td []
                                    [ typingButton "ʷ"
                                    , text "Labialised "
                                    ]
                                , td []
                                    [ typingButton "̃"
                                    , text "Nasalised"
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ typingButton "̜"
                                    , text "Less rounded"
                                    ]
                                , td []
                                    [ typingButton "ʲ"
                                    , text "Palatalised"
                                    ]
                                , td []
                                    [ typingButton "ⁿ"
                                    , text "Pre/post nasalised"
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ typingButton "̟"
                                    , text "Advanced"
                                    ]
                                , td []
                                    [ typingButton "ˠ"
                                    , text "Velarised"
                                    ]
                                , td []
                                    [ typingButton "ˡ"
                                    , text "Lateral release"
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ typingButton "̠"
                                    , text "Retracted"
                                    ]
                                , td []
                                    [ typingButton "ˤ"
                                    , text "Pharyngealised"
                                    ]
                                , td []
                                    [ typingButton "̚"
                                    , text "No audible release"
                                    ]
                                ]
                            , tr []
                                [ td []
                                    [ typingButton "̈"
                                    , text "Centralised"
                                    ]
                                , td []
                                    []
                                ]
                            ]
                        ]
                    ]
                ]
            ]



-- Note: I used an online tool (on March 23, 2020) to translate the HTML I had previously written
-- to Elm source code.
-- You can find that tool at: https://mbylstra.github.io/html-to-elm/

-- I used the following regular expressions to add all the onClick function calls.
--  Find: (button\s*\[\s*(?:class\s*".*?")?.*?)(\s*\]\s*\n\s*\[\s*text\s*)(".*?")
--   $1, onClick (InsertText $3)$2$3
-- The result was not perfect, but worked for most of the cases.