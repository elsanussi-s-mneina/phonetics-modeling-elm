module Lib exposing (Phonet(..), englishDescription, VocalFolds(..),
  Place(..), Manner(..), Airstream(..), PhonetInventory, Rounding(..), Height(..), Backness(..),
  spirantizedPhonet, devoicedPhonet, voicedPhonet, interVocalic, retractedPlace,
  showPhonet)



type VocalFolds = Voiced
                | Voiceless
                | VoicedAspirated
                | VoicelessAspirated
                | CreakyVoiced
                | UnmarkedVocalFolds



type Phonet = Consonant 
                VocalFolds 
                Place  -- | Place of articulation
                Manner -- | Manner of articulation
                Airstream
            | Vowel Height Backness Rounding VocalFolds


showPhonet : Phonet -> String
showPhonet phonet =
  case phonet of
    Consonant vocalFolds place manner airstream -> showVocalFolds vocalFolds ++ " " ++ showPlace place ++ " " ++ showManner manner ++ " " ++ showAirstream airstream ++ " consonant" 
    Vowel height backness rounding vocalFolds   -> showVocalFolds vocalFolds ++ " " ++ showRounding rounding ++ " " ++ showHeight height ++ " " ++ showBackness backness ++ " vowel"

type Backness = Front
              | Central
              | Back
              | UnmarkedBackness
showBackness : Backness -> String
showBackness backness =
  case backness of
    Front            -> "front"
    Central          -> "central"
    Back             -> "back"
    UnmarkedBackness -> ""

backnessStates : List Backness
backnessStates = [Front, Central, Back]

type Height = Close
            | NearClose
            | CloseMid
            | Mid
            | OpenMid
            | NearOpen
            | Open
            | UnmarkedHeight

showHeight : Height -> String
showHeight height =
  case height of 
     Close          -> "close"
     NearClose      -> "near-close"
     CloseMid       -> "close-mid"
     Mid            -> "mid"
     OpenMid        -> "open-mid"
     NearOpen       -> "near-open"
     Open           -> "open"
     UnmarkedHeight -> ""

heightStates : List Height
heightStates =
             [ Close
             , NearClose
             , CloseMid
             , Mid
             , OpenMid
             , NearOpen
             , Open
             ]


showVocalFolds : VocalFolds -> String
showVocalFolds vocalFolds =
  case vocalFolds of
    Voiced             -> "voiced"
    Voiceless          -> "voiceless"
    VoicedAspirated    -> "voiced aspirated"
    VoicelessAspirated -> "voiceless aspirated"
    CreakyVoiced       -> "creaky voiced"
    UnmarkedVocalFolds -> ""

vocalFoldStates : List VocalFolds
vocalFoldStates = [Voiceless, Voiced, VoicedAspirated, VoicelessAspirated, CreakyVoiced]

type Rounding = Rounded
              | Unrounded
              | UnmarkedRounding
showRounding : Rounding -> String
showRounding rounding =
  case rounding of
     Rounded             -> "rounded"
     Unrounded           -> "unrounded"
     UnmarkedRounding    -> ""

roundingStates : List Rounding
roundingStates = [Rounded, Unrounded]

type Place = Bilabial
           | LabioDental
           | Dental
           | Alveolar
           | PostAlveolar
           | Retroflex
           | Palatal
           | Velar
           | Uvular
           | Pharyngeal
           | Glottal
           | Epiglottal
           -- I am unsure if the following three should be counted
           -- as 6 different places of articulation, or just 3
           | LabialVelar
           | LabialPalatal
           | AlveoloPalatal
           | PalatoAlveolar  -- To do: investigate what the difference
           -- is between alveolopalatal, and palatoalveolar
           | UnmarkedPlace

showPlace : Place -> String
showPlace place =
  case place of 
    Bilabial       -> "bilabial"
    LabioDental    -> "labio-dental"
    Dental         -> "dental"
    Alveolar       -> "alveolar"
    PostAlveolar   -> "post-alveolar"
    Retroflex      -> "retroflex"
    Palatal        -> "palatal"
    Velar          -> "velar"
    Uvular         -> "uvular"
    Pharyngeal     -> "pharyngeal"
    Glottal        -> "glottal"
    Epiglottal     -> "epiglottal"
    LabialVelar    -> "labial-velar"
    LabialPalatal  -> "labial-palatal"
    AlveoloPalatal -> "alveolo-palatal"
    PalatoAlveolar -> "palato-alveolar"
    UnmarkedPlace  -> ""

placeStates : List Place
placeStates = [ Bilabial
              , LabioDental
              , Dental
              , Alveolar
              , PostAlveolar
              , Retroflex
              , Palatal
              , Velar
              , Uvular
              , Pharyngeal
              , Glottal
              , Epiglottal
              , LabialVelar
              , LabialPalatal
              , AlveoloPalatal
              , PalatoAlveolar
              ]

retractedPlace : Place -> Place
retractedPlace place =
  case place of
               Bilabial     -> LabioDental
               LabioDental  -> Dental
               Dental       -> Alveolar
               Alveolar     -> PostAlveolar
               PostAlveolar -> Retroflex
               Retroflex    -> Palatal
               Palatal      -> Velar
               Velar        -> Uvular
               Uvular       -> Pharyngeal
               Pharyngeal   -> Glottal
               Glottal      -> Epiglottal
               same         -> same


type Manner = Plosive
            | Nasal
            | Trill
            | TapOrFlap
            | Approximant
            | Fricative
            | Affricate
            | LateralFricative
            | LateralApproximant
            | LateralFlap
            | Lateral -- we need this one for the lateral click.
            | UnmarkedManner -- There are very few IPA symbols for lateral flaps

showManner : Manner -> String
showManner manner =
  case manner of
    Plosive            -> "plosive"
    Nasal              -> "nasal"
    Trill              -> "trill"
    TapOrFlap          -> "tap or flap"
    Approximant        -> "approximant"
    Fricative          -> "fricative"
    Affricate          -> "affricate"
    LateralFricative   -> "lateral fricative"
    LateralApproximant -> "lateral approximant"
    LateralFlap        -> "lateral flap"
    Lateral            -> "lateral"
    UnmarkedManner     -> ""

mannerStates : List Manner
mannerStates = [ Plosive
               , Nasal
               , Trill
               , TapOrFlap
               , Approximant
               , Fricative
               , Affricate
               , LateralFricative
               , LateralApproximant
               , LateralFlap
               , Lateral
               ]

type Airstream = PulmonicEgressive
               | Click
               | Implosive
               | UnmarkedAirstream

showAirstream : Airstream -> String
showAirstream airstream =
  case airstream of
    PulmonicEgressive -> "pulmonic egressive"
    Implosive         -> "implosive"
    Click             -> "click"
    UnmarkedAirstream -> ""

airstreamStates : List Airstream
airstreamStates = [ PulmonicEgressive
                  , Click
                  , Implosive
                  ]

type alias PhonetInventory = (List Phonet)


showPhonetInventory : PhonetInventory -> String
showPhonetInventory (phonetes) = String.concat (List.map englishDescription phonetes)


englishDescription : Phonet -> String
englishDescription x = showPhonet x


-- | A function that given an IPA symbol will convert it to the voiced equivalent.
voicedPhonet : Phonet -> Phonet
voicedPhonet phonet =
  case phonet of 
    (Consonant   VoicelessAspirated x y z) -> Consonant   VoicedAspirated x y z
    (Consonant   Voiceless          x y z) -> Consonant   Voiced x y z
    (Consonant   Voiced             x y z) -> Consonant   Voiced x y z
    (Consonant   VoicedAspirated    x y z) -> Consonant   VoicedAspirated x y z
    (Consonant   _                  x y z) -> Consonant   Voiced x y z
    (Vowel x y z _                       ) -> Vowel x y z Voiced

-- | A function that given an IPA symbol will convert it to the voiceless equivalent.
devoicedPhonet : Phonet -> Phonet
devoicedPhonet phonet =
  case phonet of 
    (Consonant   Voiced             x y z) -> Consonant   Voiceless          x y z
    (Consonant   CreakyVoiced       x y z) -> Consonant   Voiceless          x y z
    (Consonant   Voiceless          x y z) -> Consonant   Voiceless          x y z
    (Consonant   VoicedAspirated    x y z) -> Consonant   VoicelessAspirated x y z
    (Consonant   VoicelessAspirated x y z) -> Consonant   VoicelessAspirated x y z
    (Consonant   UnmarkedVocalFolds x y z) -> Consonant   UnmarkedVocalFolds x y z 
    (Vowel x y z _                       ) -> Vowel x y z Voiceless



spirantizedPhonet : Phonet -> Phonet

-- The following is inelegant, but there is no other way in the system,
-- right now. The part that is inelegant is that,
-- a [t] which is considered alveolar, when spirantized becomes [θ] which is dental.
-- So the following line implements this
-- change in place of articulation.
spirantizedPhonet phonet =
  case phonet of
   (Consonant x Alveolar Plosive z) -> Consonant x Dental Fricative z
   (Consonant x place Plosive z)    -> Consonant x place Fricative z
   other                            -> other



unmarkDifferences : Phonet -> Phonet -> Phonet
unmarkDifferences p1 p2 =
  case (p1, p2) of
    ((Consonant voice1 place1 manner1 airstream1), (Consonant voice2 place2 manner2 airstream2)) ->
      let voice3     = if voice1     == voice2     then voice1     else UnmarkedVocalFolds
          place3     = if place1     == place2     then place1     else UnmarkedPlace
          manner3    = if manner1    == manner2    then manner1    else UnmarkedManner
          airstream3 = if airstream1 == airstream2 then airstream1 else UnmarkedAirstream
      in Consonant voice3 place3 manner3 airstream3
    ((Vowel height1 backness1 rounding1 voice1), (Vowel height2 backness2 rounding2 voice2)) ->
      let voice3    = if voice1    == voice2    then voice1    else UnmarkedVocalFolds
          height3   = if height1   == height2   then height1   else UnmarkedHeight
          backness3 = if backness1 == backness2 then backness1 else UnmarkedBackness
          rounding3 = if rounding1 == rounding2 then rounding1 else UnmarkedRounding
      in Vowel height3 backness3 rounding3 voice3
    ((Vowel _ _ _ voice1), (Consonant voice2 _ _ _)) ->
      let voice3 = if voice1 == voice2 then voice1 else UnmarkedVocalFolds
      in Vowel UnmarkedHeight UnmarkedBackness UnmarkedRounding voice3
    (((Consonant _ _ _ _) as c), ((Vowel _ _ _ _) as v)) ->
      unmarkDifferences v c -- Change the order of arguments 



-- This function (I realize it is poorly named)
-- takes any unmarked attributes in the phoneme definition,
-- and returns a list with all possibilities for that attribute.
generateFromUnmarked : Phonet -> List Phonet
generateFromUnmarked phonet =
  case phonet of
    (Consonant voice place manner airstream) ->
      let voice3     = if voice     == UnmarkedVocalFolds     then vocalFoldStates else [voice]
          place3     = if place     == UnmarkedPlace          then placeStates     else [place]
          manner3    = if manner    == UnmarkedManner         then mannerStates    else [manner]
          airstream3 = if airstream == UnmarkedAirstream      then airstreamStates else [airstream]
      in 
        List.concatMap (\a -> (List.concatMap (\m -> (List.concatMap (\p -> (List.map (\v -> Consonant v p m a) (voice3))) place3)) manner3)) airstream3

    (Vowel height backness rounding voice) ->
      let voice3    = if voice    == UnmarkedVocalFolds then vocalFoldStates else [voice]
          height3   = if height   == UnmarkedHeight     then heightStates    else [height]
          backness3 = if backness == UnmarkedBackness   then backnessStates  else [backness]
          rounding3 = if rounding == UnmarkedRounding   then roundingStates  else [rounding]
      in
        List.concatMap (\v -> List.concatMap (\r -> List.concatMap  (\b -> List.map (\h -> Vowel h b r v) height3) backness3) rounding3) voice3  


-- The following function returns whether an articulation is
-- considered impossible according to the IPA (pulmonic) consonants chart.
-- Does not work for other values.
impossible : Phonet -> Bool
impossible phonet =
  case phonet of
    (Consonant Voiced          Pharyngeal  Plosive            PulmonicEgressive) -> True
    (Consonant VoicedAspirated Pharyngeal  Plosive            PulmonicEgressive) -> True
    (Consonant Voiceless       Glottal     Plosive            PulmonicEgressive) -> False  -- [ʔ] is not impossible.
    (Consonant _               Glottal     Fricative          PulmonicEgressive) -> False  -- [h] and [ɦ] are not impossible.
    (Consonant _               Glottal     _                  PulmonicEgressive) -> True   -- all other pulmonary egressive consonants are impossible..
    (Consonant _               Pharyngeal  Nasal              PulmonicEgressive) -> True
    (Consonant _               Pharyngeal  LateralFricative   PulmonicEgressive) -> True
    (Consonant _               Pharyngeal  LateralApproximant PulmonicEgressive) -> True
    (Consonant _               Velar       Trill              PulmonicEgressive) -> True
    (Consonant _               Velar       TapOrFlap          PulmonicEgressive) -> True
    (Consonant _               Bilabial    LateralFricative   PulmonicEgressive) -> True
    (Consonant _               Bilabial    LateralApproximant PulmonicEgressive) -> True
    (Consonant _               LabioDental LateralFricative   PulmonicEgressive) -> True
    (Consonant _               LabioDental LateralApproximant PulmonicEgressive) -> True
    _                                                                            -> False -- Everything else is assumed t be possible.

-- | Whether a phonet is in an intervocalic environment.
-- | This means that there is a vowel directly before it,
-- | and one after it.
interVocalic : Phonet  -- Before
             -> Phonet  -- After
             -> Bool
interVocalic p1 p2 =
  case (p1, p2) of
    ((Vowel _ _ _ _), (Vowel _ _ _ _)) -> True
    (_              , _              ) -> False

