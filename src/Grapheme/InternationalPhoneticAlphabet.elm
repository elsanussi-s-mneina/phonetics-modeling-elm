module Grapheme.InternationalPhoneticAlphabet exposing (analyzeIPA, constructIPA,
   plosivePulmonic, nasalPulmonic, trillPulmonic, tapOrFlapPulmonic, fricativePulmonic, lateralFricativePulmonic,
   approximantPulmonic, lateralApproximantPulmonic, closeVowels, nearCloseVowels, closeMidVowels, midVowels, openMidVowels, nearOpenVowels, openVowels, otherSymbols, suprasegmentals, diacriticsAndSuprasegmentals,
   toneAndWordAccents, consonantsNonPulmonicRow1, consonantsNonPulmonicRow2, consonantsNonPulmonicRow3,
   consonantsNonPulmonicRow4, consonantsNonPulmonicRow5)

import Lib exposing (PhonetInventory, Manner(..), Place(..), Phonet(..), VocalFolds(..), Airstream(..),
                     Rounding(..), Backness(..), Height(..), 
                     spirantizedPhonet, devoicedPhonet, voicedPhonet)




graphemesOfIPA : List String
graphemesOfIPA = consonantsPulmonic 
  ++ consonantsNonPulmonic
  ++ otherSymbols
  ++ vowels
  ++ suprasegmentals 
  ++ toneAndWordAccents
  ++ diacriticsAndSuprasegmentals
-- See: https://www.internationalphoneticassociation.org/sites/default/files/IPA_Kiel_2015.pdf
-- For the source of this information..

-- CONSONANTS (PULMONIC)
consonantsPulmonic : List String
consonantsPulmonic = List.concat consonantsPulmonicTable


-- Pulmonic consonants by manner of articulation
plosivePulmonic           : List String
plosivePulmonic            = [ "p", "b", " ", " ", " ", " ", "t", "d", " ", " ", "ʈ", "ɖ", "c", "ɟ", "k", "g", "q", "ɢ", " ", " ", "ʔ", " "] -- Plosive

nasalPulmonic             : List String
nasalPulmonic              = [ " ", "m", " ", "ɱ", " ", " ", " ", "n", " ", " ", " ", "ɳ", " ", "ɲ", " ", "ŋ", " ", "ɴ", " ", " ", " ", " "] -- Nasal

trillPulmonic             : List String
trillPulmonic              = [ " ", "ʙ", " ", " ", " ", " ", " ", "r", " ", " ", " ", " ", " ", " ", " ", " ", " ", "ʀ", " ", " ", " ", " "] -- Trill

tapOrFlapPulmonic         : List String
tapOrFlapPulmonic          = [ " ", " ", " ", "ⱱ", " ", " ", " ", "ɾ", " ", " ", " ", "ɽ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " "] -- Tap or Flap

fricativePulmonic         : List String
fricativePulmonic          = [ "ɸ", "β", "f", "v", "θ", "ð", "s", "z", "ʃ", "ʒ", "ʂ", "ʐ", "ç", "ʝ", "x", "ɣ", "χ", "ʁ", "ħ", "ʕ", "h", "ɦ"]  -- Fricative

lateralFricativePulmonic  : List String
lateralFricativePulmonic   = [ " ", " ", " ", " ", " ", " ", "ɬ", "ɮ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " ", " "] -- Lateral fricative

approximantPulmonic       : List String
approximantPulmonic        = [ " ", " ", " ", "ʋ", " ", " ", " ", "ɹ", " ", " ", " ", "ɻ", " ", "j", " ", "ɰ", " ", " ", " ", " ", " ", " "] -- Approximant

lateralApproximantPulmonic: List String
lateralApproximantPulmonic = [ " ", " ", " ", " ", " ", " ", " ", "l", " ", " ", " ", "ɭ", " ", "ʎ", " ", "ʟ", " ", " ", " ", " ", " ", " "] -- Lateral approximant


-- Pulmonic consonants by place of articulation:
bilabialPulmonic          : List String
bilabialPulmonic          = ["p", "b", "m", "ʙ", "ɸ", "β"]



closeVowels     : List String
closeVowels     =  ["i", "y", " ", " ",  "ɨ", "ʉ",   "ɯ", "u"]   -- Close
nearCloseVowels : List String
nearCloseVowels =  [" ", "ɪ", "ʏ", " ", " ",  "ʊ", " "]
closeMidVowels  : List String
closeMidVowels  =  ["e", "ø"," ",   "ɘ", "ɵ",   "ɤ", "o"]   -- Close-mid
midVowels       : List String
midVowels       =  [" ", " ", " ", "ə", " " , " "]
openMidVowels   : List String
openMidVowels   =  [ "ɛ", "œ",   "ɜ", "ɞ",   "ʌ", "ɔ"]  -- Open-mid
nearOpenVowels  : List String
nearOpenVowels  =  [ "æ",  " ", "ɐ", " ", " " ]
openVowels      : List String
openVowels      =  [ "a", "ɶ", " ", "ɑ", "ɒ" ]  -- Open


consonantsPulmonicTable : List (List String)
consonantsPulmonicTable =
 [ plosivePulmonic
 , nasalPulmonic
 , trillPulmonic
 , tapOrFlapPulmonic
 , fricativePulmonic
 , lateralFricativePulmonic
 , approximantPulmonic
 , lateralApproximantPulmonic
 ]

consonantsNonPulmonicRow1 : List String
                            -- Clicks   Voiced implosives
consonantsNonPulmonicRow1 = ["ʘ",     "ɓ" {- Bilabial         -}            ]
consonantsNonPulmonicRow2 : List String
consonantsNonPulmonicRow2 = ["ǀ", {- Dental -}    "ɗ" {- Dental/alveolar -} ]
consonantsNonPulmonicRow3 : List String
consonantsNonPulmonicRow3 = ["ǃ", {-  (Post)alveolar -}  "ʄ"                ]
consonantsNonPulmonicRow4 : List String
consonantsNonPulmonicRow4 = ["ǂ",  "ɠ"                                      ]
consonantsNonPulmonicRow5 : List String

consonantsNonPulmonicRow5 = ["ǁ",  "ʛ"                                      ]
consonantsNonPulmonic : List String
consonantsNonPulmonic
  =  consonantsNonPulmonicRow1
  ++ consonantsNonPulmonicRow2
  ++ consonantsNonPulmonicRow3
  ++ consonantsNonPulmonicRow4
  ++ consonantsNonPulmonicRow5


otherSymbols : List String
otherSymbols =
  ["ʍ",  "ɕ"
  ,"w",  "ʑ"
  ,"ɥ",  "ɺ"
  ,"ʜ",  "ɧ"
  ,"ʢ"
  ,"ʡ"
  ]

vowels : List String
vowels =
  ["i", "y",   "ɨ", "ʉ",   "ɯ", "u"   -- Close
  ,"ɪ", "ʏ",            "ʊ"
  ,"e", "ø",   "ɘ", "ɵ",   "ɤ", "o"   -- Close-mid
  ,               "ə"
  ,"ɛ", "œ",   "ɜ", "ɞ",   "ʌ", "ɔ"   -- Open-mid
  , "æ",           "ɐ"
  , "a", "ɶ",              "ɑ", "ɒ"  -- Open
  ]     

suprasegmentals : List String
suprasegmentals =
  [ "ˈ"   -- Primary stress
  , "ˌ"   -- Secondary stress
  , "ː"   -- Long
  , "ˑ"   -- Half long

  , "̆"    -- Extra short
  , "|"   -- Minor (foot) group 
  , "‖"   -- Major (intonation) group
  , "."   -- Syllable break
  , "‿"   -- Linking (absence of a break
  ]


toneAndWordAccents : List String
toneAndWordAccents =
{- Level -}
  [ "˥", "̋"  -- Extra high
  , "˦", "́"  -- High
  , "˧", "̄"  -- Mid
  , "˨", "̀"  -- Low
  , "˩", "̏"  -- Extra low
  ,      "ꜜ"  -- Downstep
  ,      "ꜛ"  -- Upstep

{- Countour -}
  , "̌" -- Rising
  , "̂" -- Falling
  , "᷄" -- High rising
  , "᷅" -- Low rising
  , "᷈" -- Rising-falling
  , "↗" -- Global rise
  , "↘" -- Global fall
  ]

diacriticsAndSuprasegmentals : List String
diacriticsAndSuprasegmentals =
  [ "ʰ"  -- Aspirated
  , "ʷ"  -- Labialised
  , "ʲ"  -- Palatalised
  , "ˠ"  -- Velarised
  , "ˤ"  -- Pharyngealised
  , "ⁿ"  -- Pre/post nasalised
  , "ˡ"  -- Lateral release

  , "˞"  -- Rhoticity
  , "ʼ"  -- Ejective
  , "̚"   -- No audible release

  , "̩"   -- Syllabic
  , "̯"   -- Non-syllabic
  , "̰"   -- Creaky voiced
  , "̥"   -- Voiceless
  , "̬"   -- Voiced
  , "̤"   -- Breathy voiced
  , "̊"   -- Voiceless (diacritic placed above symbol with descender)
  , "̍"   -- Syllabic (diacritic placed above)
  , "̪"   -- Dental
  , "̺"   -- Apical
  , "̻"   -- Laminal
  , "̼"   -- Linguolabial
  , "̣"  -- Closer variety/Fricative
  , "̃"   -- Nasalised
  , "̈"   -- Centralised
  , "̽"   -- Mid centralised
  , "̆"   -- Extra short
  , "̇"    -- Palatalization/Centralization
  ]

showPhonetInventory : PhonetInventory -> String
showPhonetInventory phonetes = String.concat (List.map constructIPA phonetes)

showIPA : Phonet -> String
showIPA phonete = constructIPA phonete


indexOf : List a -> Int -> a -> Int
indexOf aList index target = 
  case aList of 
    []              -> -1
    (element::rest) -> 
      if element == target
        then index
        else indexOf rest (index + 1) target

analyzeMannerIPA : String -> (Manner, Int)
analyzeMannerIPA x =
  if                    List.member x plosivePulmonic            then (Plosive, 0)
    else if             List.member x nasalPulmonic              then (Nasal, 1)
      else if           List.member x trillPulmonic              then (Trill, 2)
        else if         List.member x tapOrFlapPulmonic          then (TapOrFlap, 3)
          else if       List.member x fricativePulmonic          then (Fricative, 4)
            else if     List.member x lateralFricativePulmonic   then (LateralFricative, 5)
              else if   List.member x approximantPulmonic        then (Approximant, 6)
                else if List.member x lateralApproximantPulmonic then (LateralApproximant, 7)
                  else (LateralApproximant, 7) -- Not right, but will have to work for now. -- TODO: Fix this.

analyzePlaceIPA : Int -> Place
analyzePlaceIPA colIndex = 
  let colNames = [Bilabial, LabioDental, Dental, Alveolar, PostAlveolar, Retroflex, Palatal, Velar, Uvular, Pharyngeal, Glottal]
  in let x = getAtIndex colNames (colIndex // 2)
     in case x of 
        Just s  -> s
        Nothing ->  UnmarkedPlace

placeToHalfColIndex : Place -> Int
placeToHalfColIndex place1 = 
  let colNames = [Bilabial, LabioDental, Dental, Alveolar, PostAlveolar, Retroflex, Palatal, Velar, Uvular, Pharyngeal, Glottal]
  in indexOf colNames 0 place1

analyzeIPAv2 : String -> Phonet
analyzeIPAv2 x =
  let (manner, rowIndex) = analyzeMannerIPA x 
      colIndex = indexOf (Maybe.withDefault [] (getAtIndex consonantsPulmonicTable rowIndex)) 0 x
      voicing  = colIndexToVoicing colIndex 
      place    = analyzePlaceIPA colIndex 
  in Consonant voicing place manner PulmonicEgressive

colIndexToVoicing : Int -> VocalFolds
colIndexToVoicing colIndex = 
  if modBy 2 colIndex  == 0 then Voiceless else Voiced

voicingToColIndexOffset : VocalFolds -> Int
voicingToColIndexOffset v =
  case v of 
    Voiceless          -> 0
    Voiced             -> 1
    VoicelessAspirated -> 0
    VoicedAspirated    -> 1
    CreakyVoiced       -> 1
    UnmarkedVocalFolds -> 0


mannerToRowIndex : Manner -> Int
mannerToRowIndex manner = 
  let rowNames = [Plosive, Nasal, Trill, TapOrFlap, Fricative, LateralFricative, Approximant, LateralApproximant]
  in indexOf rowNames 0 manner

voicingAndPlaceToColIndex : VocalFolds -> Place -> Int
voicingAndPlaceToColIndex voicing place = 
      (2 * placeToHalfColIndex place) + voicingToColIndexOffset voicing
  

-- | This function will allow us to convert an IPA symbol
-- | to its analyzed form (its phonetic features)
-- Currently, only the consonants (pulmonic) in the 2005 IPA chart are included.
analyzeIPA  : String -> Phonet

-- Affricates
analyzeIPA p = 
  case p of 
    "t͡ʃ" -> Consonant  Voiceless PostAlveolar Affricate PulmonicEgressive
    "d͡ʒ" -> Consonant  Voiced    PostAlveolar Affricate PulmonicEgressive
    -- We should probably enforce use of the tie-bar underneath, otherwise
    -- it would not be deterministic to determine whether two graphemes here
    -- represent affricates or a plosive followed by a fricative.
    
    
    
    
    -- Under the Other Symbols part of the IPA chart:

    "w" -> Consonant Voiced    LabialVelar    Approximant PulmonicEgressive
    "ʍ" -> Consonant Voiceless LabialVelar    Fricative   PulmonicEgressive
    "ɥ" -> Consonant Voiced    LabialPalatal  Approximant PulmonicEgressive
    "ʜ" -> Consonant Voiceless Epiglottal     Fricative   PulmonicEgressive
    "ʢ" -> Consonant Voiced    Epiglottal     Fricative   PulmonicEgressive
    "ʡ" -> Consonant Voiceless Epiglottal     Plosive     PulmonicEgressive -- Is the epiglottal plosive voiceless? The IPA chart does not specify.
    "ɕ" -> Consonant Voiceless AlveoloPalatal Fricative   PulmonicEgressive
    "ʑ" -> Consonant Voiced    AlveoloPalatal Fricative   PulmonicEgressive
    "ɺ" -> Consonant Voiced    Alveolar       LateralFlap PulmonicEgressive

    -- We cannot handle the ɧ (simultaneous ʃ and x) because
    -- we did not define our data types to handle it yet.
    -- In any case, here is some pseudocode for it:
    --     "ɧ" -> simultaneous (    "ʃ") (    "x")

    -- Other Consonants:
    "ʘ" -> Consonant UnmarkedVocalFolds Bilabial       UnmarkedManner Click
    "ǀ" -> Consonant UnmarkedVocalFolds Dental         UnmarkedManner Click
    "ǃ" -> Consonant UnmarkedVocalFolds Alveolar       UnmarkedManner Click -- Or it could be PostAlveolar.
    "ǂ" -> Consonant UnmarkedVocalFolds PalatoAlveolar UnmarkedManner Click
    "ǁ" -> Consonant UnmarkedVocalFolds Alveolar       Lateral        Click
    "ɓ" -> Consonant Voiced             Bilabial       UnmarkedManner Implosive
    "ɗ" -> Consonant Voiced             Dental         UnmarkedManner Implosive  -- Or Alveolar
    "ʄ" -> Consonant Voiced             Palatal        UnmarkedManner Implosive
    "ɠ" -> Consonant Voiced             Velar          UnmarkedManner Implosive
    "ʛ" -> Consonant Voiced             Uvular         UnmarkedManner Implosive

    -- Close Vowels:
    "i"  -> Vowel  Close Front   Unrounded Voiced
    "y"  -> Vowel  Close Front   Rounded   Voiced
    "ɨ"  -> Vowel  Close Central Unrounded Voiced
    "ʉ"  -> Vowel  Close Central Rounded   Voiced
    "ɯ"  -> Vowel  Close Back    Unrounded Voiced
    "u"  -> Vowel  Close Back    Rounded   Voiced

    -- Near-close Vowels:
    "ɪ"  -> Vowel NearClose Front Unrounded Voiced
    "ʏ"  -> Vowel NearClose Front Rounded   Voiced
    "ʊ"  -> Vowel NearClose Back  Rounded   Voiced

    -- Close-mid Vowels:
    "e"  -> Vowel  CloseMid Front   Unrounded Voiced
    "ø"  -> Vowel  CloseMid Front   Rounded   Voiced
    "ɘ"  -> Vowel  CloseMid Central Unrounded Voiced
    "ɵ"  -> Vowel  CloseMid Central Rounded   Voiced
    "ɤ"  -> Vowel  CloseMid Back    Unrounded Voiced
    "o"  -> Vowel  CloseMid Back    Rounded   Voiced

    -- Mid Vowels:
    "ə"  -> Vowel Mid Central UnmarkedRounding Voiced


    -- Open-mid Vowels:
    "ɛ"  -> Vowel  OpenMid Front   Unrounded Voiced
    "œ"  -> Vowel  OpenMid Front   Rounded   Voiced
    "ɜ"  -> Vowel  OpenMid Central Unrounded Voiced
    "ɞ"  -> Vowel  OpenMid Central Rounded   Voiced
    "ʌ"  -> Vowel  OpenMid Back    Unrounded Voiced
    "ɔ"  -> Vowel  OpenMid Back    Rounded   Voiced

    -- Near-open
    "æ"  -> Vowel  NearOpen Front   Unrounded Voiced
    "ɐ"  -> Vowel  NearOpen Central UnmarkedRounding  Voiced

    -- Open Vowels:
    "a"  -> Vowel  Open Front Unrounded Voiced
    "ɶ"  -> Vowel  Open Front Rounded   Voiced
    "ɑ"  -> Vowel  Open Back  Unrounded Voiced
    "ɒ"  -> Vowel  Open Back  Rounded   Voiced
    x    ->
      if String.length x == 1
          then analyzeIPAv2 (String.left 1 x)
          else  -- Handle Diacritics:
                let firstChar  = String.slice 0 1 x
                    secondChar = String.slice 1 1 x
                in
                  if secondChar == "̥"
                    then 
                      let fullGrapheme = analyzeIPA firstChar
                      in case fullGrapheme of
                              Consonant _ place manner airstream    -> Consonant Voiceless place manner airstream
                              Vowel height backness rounding _      -> Vowel height backness rounding Voiceless
                    else if secondChar == "̬"
                      then let fullGrapheme = analyzeIPA firstChar
                        in case fullGrapheme of
                                Consonant _ place manner airstream    -> Consonant Voiced place manner airstream
                                Vowel height backness rounding _      -> Vowel height backness rounding Voiced

                      else if secondChar == "ʰ"
                        then 
                          let fullGrapheme = analyzeIPA firstChar
                          in case fullGrapheme of
                                  Consonant Voiced place manner airstream    -> Consonant VoicedAspirated place manner airstream
                                  Consonant Voiceless place manner airstream -> Consonant VoicelessAspirated place manner airstream
                                  Vowel height backness rounding voice       -> Vowel height backness rounding voice
                                  anythingElse                               -> anythingElse
                                  -- (About the preceding line:) It is strange but we will just do nothing if they give us an aspirated vowel.
                                  -- since we have no way to represent it in the type system. to do: determine
                                  -- if the idea of an aspirated vowel makes sense
                        else Consonant UnmarkedVocalFolds UnmarkedPlace UnmarkedManner UnmarkedAirstream -- Not recognized.

constructIPA : Phonet -> String
-- Affricates
constructIPA p =
  case p of
    (Consonant  Voiceless PostAlveolar  Affricate PulmonicEgressive) -> "t͡ʃ"
    (Consonant  Voiced    PostAlveolar  Affricate PulmonicEgressive) -> "d͡ʒ"
    (Consonant  Voiceless Bilabial      Affricate PulmonicEgressive) -> "p͡ɸ"
    (Consonant  Voiceless Alveolar      Affricate PulmonicEgressive) -> "t͜s"
    (Consonant  Voiced    Alveolar      Affricate PulmonicEgressive) -> "d͡z"
    (Consonant  Voiceless Velar         Affricate PulmonicEgressive) -> "k͡x"
    (Consonant  Voiceless Uvular        Affricate PulmonicEgressive) -> "q͡χ"
    -- The following two lines are commented out, because I am unsure about their place of articulation:
    -- (Consonant  Voiceless LabialVelar? Affricate PulmonicEgressive) -> "k͡p"
    -- (Consonant  Voiceless Palatal (or AlveolaPalatal?) Affricate PulmonicEgressive) -> "c͡ɕ"

    _ ->
      -- If it can represent it as a single character it will
      -- return the single character result (i.e. without diacritics),
      -- otherwise
      -- it will try to represent it in IPA with more than
      -- one character
      let simpleResult = constructIPA1 p
      in if simpleResult == " "
          then constructIPA2 p
          else simpleResult



-- Note to Software Developer: the reason there are three
-- functions for constructing the IPA is to prevent
-- infinite recursion. The reason is that
-- if we only had one function, it would -- for some
-- cases never halt if it could not find a character
-- to place a diacritic on.

-- | Given an analysis construct an IPA symbol
-- | This function will allow us to convert an analyzed form
-- | to its IPA symbol.
-- | Note this only returns one character without diacritics.
constructIPA1  :  Phonet -> String

-- Under the Other Symbols part of the IPA chart:

constructIPA1 p =
  case p of 
    (Consonant Voiced    LabialVelar    Approximant PulmonicEgressive) -> "w"
    (Consonant Voiceless LabialVelar    Fricative   PulmonicEgressive) -> "ʍ"
    (Consonant Voiced    LabialPalatal  Approximant PulmonicEgressive) -> "ɥ"
    (Consonant Voiceless Epiglottal     Fricative   PulmonicEgressive) -> "ʜ"
    (Consonant Voiced    Epiglottal     Fricative   PulmonicEgressive) -> "ʢ"
    (Consonant Voiceless Epiglottal     Plosive     PulmonicEgressive) -> "ʡ"
    -- Is the epiglottal plosive voiceless? The IPA chart does not specify.

    (Consonant Voiceless AlveoloPalatal Fricative   PulmonicEgressive) -> "ɕ"
    (Consonant Voiced    AlveoloPalatal Fricative   PulmonicEgressive) -> "ʑ"
    (Consonant Voiced    Alveolar       LateralFlap PulmonicEgressive) -> "ɺ"

    -- We cannot handle the ɧ (simultaneous ʃ and x) because
    -- we did not define our data types to handle it yet.
    --  (simultaneous (analyzeIPA "ʃ") (analyzeIPA "x")) -> "ɧ"

    -- Other Consonants:
    (Consonant UnmarkedVocalFolds Bilabial       UnmarkedManner Click    ) -> "ʘ"
    (Consonant UnmarkedVocalFolds Dental         UnmarkedManner Click    ) -> "ǀ"
    (Consonant UnmarkedVocalFolds Alveolar       UnmarkedManner Click    ) -> "ǃ" -- Or it could be PostAlveolar.
    (Consonant UnmarkedVocalFolds PalatoAlveolar UnmarkedManner Click    ) -> "ǂ"
    (Consonant UnmarkedVocalFolds Alveolar       Lateral        Click    ) -> "ǁ"
    (Consonant Voiced             Bilabial       UnmarkedManner Implosive) -> "ɓ"
    (Consonant Voiced             Dental         UnmarkedManner Implosive) -> "ɗ"  -- Or Alveolar
    (Consonant Voiced             Palatal        UnmarkedManner Implosive) -> "ʄ"
    (Consonant Voiced             Velar          UnmarkedManner Implosive) -> "ɠ"
    (Consonant Voiced             Uvular         UnmarkedManner Implosive) -> "ʛ"

    ((Consonant Voiced _ _ PulmonicEgressive) as c) 
      -> constructUnaspiratedPulmonicEgressive c

    ((Consonant VoicedAspirated _ _ PulmonicEgressive) as c) 
      -> constructUnaspiratedPulmonicEgressive (deaspirate c) ++ "ʰ"


    ((Consonant Voiceless _ _ PulmonicEgressive) as c) -> 
      constructUnaspiratedPulmonicEgressive c

    ((Consonant VoicelessAspirated _ _ PulmonicEgressive) as c) -> 
      constructUnaspiratedPulmonicEgressive (deaspirate c) ++ "ʰ"

    -- Close Vowels:
    (Vowel  Close Front   Unrounded Voiced) -> "i"
    (Vowel  Close Front   Rounded   Voiced) -> "y"
    (Vowel  Close Central Unrounded Voiced) -> "ɨ"
    (Vowel  Close Central Rounded   Voiced) -> "ʉ"
    (Vowel  Close Back    Unrounded Voiced) -> "ɯ"
    (Vowel  Close Back    Rounded   Voiced) -> "u"

    -- Near-close Vowels:
    (Vowel NearClose Front Unrounded Voiced) -> "ɪ"
    (Vowel NearClose Front Rounded   Voiced) -> "ʏ"
    (Vowel NearClose Back  Rounded   Voiced) -> "ʊ"

    -- Close-mid Vowels:
    (Vowel  CloseMid Front   Unrounded Voiced) -> "e"
    (Vowel  CloseMid Front   Rounded   Voiced) -> "ø"
    (Vowel  CloseMid Central Unrounded Voiced) -> "ɘ"
    (Vowel  CloseMid Central Rounded   Voiced) -> "ɵ"
    (Vowel  CloseMid Back    Unrounded Voiced) -> "ɤ"
    (Vowel  CloseMid Back    Rounded   Voiced) -> "o"

    -- Mid Vowels:
    (Vowel Mid Central UnmarkedRounding Voiced) -> "ə"


    -- Open-mid Vowels:
    (Vowel  OpenMid Front   Unrounded Voiced) -> "ɛ"
    (Vowel  OpenMid Front   Rounded   Voiced) -> "œ"
    (Vowel  OpenMid Central Unrounded Voiced) -> "ɜ"
    (Vowel  OpenMid Central Rounded   Voiced) -> "ɞ"
    (Vowel  OpenMid Back    Unrounded Voiced) -> "ʌ"
    (Vowel  OpenMid Back    Rounded   Voiced) -> "ɔ"

    -- Near-open
    (Vowel  NearOpen Front   Unrounded Voiced) -> "æ"
    (Vowel  NearOpen Central UnmarkedRounding  Voiced) -> "ɐ"

    -- Open Vowels:
    (Vowel  Open Front Unrounded Voiced) -> "a"
    (Vowel  Open Front Rounded   Voiced) -> "ɶ"
    (Vowel  Open Back  Unrounded Voiced) -> "ɑ"
    (Vowel  Open Back  Rounded   Voiced) -> "ɒ"
    _ -> " "




constructIPA2 : Phonet -> String

constructIPA2 p = 
  case p of 
    (Consonant  x PostAlveolar y z) 
      -> constructIPA1 (Consonant x Alveolar y z) ++ "̠"  -- Add the diacritic for "retracted"


    -- If there isn't a symbol, and the consonant we want is voiceless,
    -- Just take the symbol for a voiced consonant,
    -- and then put that diacritic that means voiceless after.
    -- (The following two definitions are intended to implement that)
    -- Add the small circle diacritic to consonants to make them voiceless.
    (Consonant Voiceless x y z) 
      -> constructIPA1 (Consonant Voiced x y z) ++ "̥" -- add diacritic for voiceless

    -- Add the small circle diacritic to vowels to make them voiceless.
    (Vowel x y z Voiceless) 
      -> constructIPA1 (Vowel x y z Voiced) ++ "̥"

    -- If there is no way to express a voiced consonant in a single
    -- grapheme add a diacritic to the grapheme that represents
    -- the voiceless counterpart.
    (Consonant Voiced x y z) 
      -> constructIPA1 (Consonant Voiceless x y z) ++ "̬"

    (Vowel x y z Voiced) 
      -> constructIPA1 (Vowel x y z Voiceless) ++ "̬"
    _ -> "∅" -- This return value ( a symbol representing the empty set)
              -- is not a full answer. It really means we don't have an answer.
              -- We are only using it here so that we can ignore values we have not programmed
              -- yet. We just want it to show that we do not have it.

getAtIndex : List a -> Int -> Maybe a
getAtIndex aList i =
  List.head (List.drop i aList)


constructUnaspiratedPulmonicEgressive : Phonet -> String
constructUnaspiratedPulmonicEgressive c =
  case c of 
    (Consonant voicing place manner PulmonicEgressive) 
      ->
      let rowIndex = mannerToRowIndex manner
          colIndex = voicingAndPlaceToColIndex voicing place
          row = getAtIndex consonantsPulmonicTable rowIndex
      in case row of 
           Just r   -> let valueAtPlace = getAtIndex r colIndex
                       in case valueAtPlace of 
                            Just v -> v
                            Nothing -> "∅"
           Nothing  -> "∅"
    _ ->  "∅"  -- Kind of senseless for non-consonants, so just do nothing.

deaspirate : Phonet -> Phonet
deaspirate p =
  case p of 
    (Consonant VoicedAspirated place manner airstream) 
      -> (Consonant Voiced place manner airstream)
    (Consonant VoicelessAspirated place1 manner1 airstream1)
      -> (Consonant Voiceless place1 manner1 airstream1)
    x
      -> x

voicedIPA : String -> String
voicedIPA x = constructIPA (voicedPhonet (analyzeIPA x))

devoicedIPA : String -> String
devoicedIPA x = constructIPA (devoicedPhonet (analyzeIPA x))


spirantizedIPA : String -> String
spirantizedIPA x = constructIPA (spirantizedPhonet (analyzeIPA x))

{-|
Return an english description of a phoneme,
given a phoneme's representation in the
international phonetic alphabet.
  |-}
describeIPA : String -> String
describeIPA x = showIPA (analyzeIPA x)