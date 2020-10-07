-- | Handle splitting of IPA graphemes into chunks, so that
--   diacritics go with the non-diacritic characters they modify.
--
module Grapheme.GraphemeGrammar exposing (isAscender, isDescender, isDiacriticAbove, isDiacriticBelow) 


import Grapheme.PrimitiveParsers exposing (optionalParser, orParser, singleCharParser, thenParser)

{- Context free grammar of IPA (incomplete, but good enough to start)

We want something like this:

BaseDigraph -> BaseCharacter
             | BaseCharacter VoicingDiacritic
             | BaseCharacter SecondaryArticulationDiacritic
             | BaseCharacter VoicingDiacritic SecondaryArticulationDiacritic
             | Digraph
Digraph -> BaseCharacter TieCharacter BaseCharacter
TieCharacter ->  ͜  | ͡
StrictSegmentals -> a | b | c | t | d | s | ʃ | ʒ | f | r
VoicingDiacritic -> ̥ | ̊ | ̬
SuperscriptAfter -> ʰ
SecondaryArticulationDiacritic ->  ʷ | ʲ | ˤ | ˠ
-}

baseCharacters : List Char
baseCharacters = strictSegmentals

secondaryArticulationDiacritics : List Char
secondaryArticulationDiacritics = ['ʷ', 'ʲ', 'ˤ', 'ˠ']

voicingDiacritics : List Char
voicingDiacritics = ['̥', '̊', '̬']

-- | This implements the
-- rule expressed in (BNF) grammar as:
-- digraph -> baseCharacter tieBarCharacter baseCharacter
digraphParser
  : String
  -> Maybe (String, String)
digraphParser =
  thenParser baseCharacterParser (thenParser tieBarParser baseCharacterParser)

tieBarParser
  : String
  -> Maybe (String, String)
tieBarParser = singleCharParser ['͜', '͡']

voicingDiacriticParser
  : String
  -> Maybe (String, String)
voicingDiacriticParser = singleCharParser voicingDiacritics

superscriptAfterParser
  : String
  -> Maybe (String, String)
superscriptAfterParser = singleCharParser superscriptsAfter

superscriptBeforeParser
  : String
  -> Maybe (String, String)
superscriptBeforeParser = singleCharParser superscriptsBefore


-- | represents grammar rule:
--  phoneme -> [subscriptBefore] (digraph | baseCharacter) [voicingDiacritic] [subscriptAfter] [secondaryArticulation]
phonemeParser
  : String
  -> Maybe (String, String)
phonemeParser =
  let optionalSuperscriptBefore = optionalParser superscriptBeforeParser
      digraphOrBaseCharacter = orParser digraphParser baseCharacterParser
      optionalVoicingDiacritic = optionalParser voicingDiacriticParser
      optionalSuperscriptAfter = optionalParser superscriptAfterParser
      optionalSecondaryArticulation = optionalParser secondaryArticulationDiacriticParser
  in
  thenParser
    optionalSuperscriptBefore
    (thenParser
      digraphOrBaseCharacter
      (thenParser
        optionalVoicingDiacritic
        (thenParser
          optionalSuperscriptAfter
          optionalSecondaryArticulation)))

secondaryArticulationDiacriticParser
  : String
  -> Maybe (String, String)
secondaryArticulationDiacriticParser =
  singleCharParser secondaryArticulationDiacritics

baseCharacterParser
  : String
  -> Maybe (String, String)
baseCharacterParser =
  singleCharParser baseCharacters

-- | Splits text in the International Phonetic Alphabet by
--   phones. This is also called tokenization.
--
--   Note: it does not recognize affricates, unless a tie-bar
--   is provided.
splitIntoPhonemes
  : String
  -> List String
splitIntoPhonemes text = toListParser phonemeParser text

toListParser
  : (String -> Maybe (String, String))
  -> String
  -> List String
toListParser parser text =
  case parser text  of
    Just (parsed, rest) -> parsed :: toListParser parser rest
    Nothing -> []

-- | Whether the character in the string at a certain place,
--   represents a consonant.
isConsonantAt : Int  -- ^ an index within the range of 0, and the length of the string argument
              -> String -- ^ a text string
              -> Bool -- ^ true if it is a consonant
isConsonantAt = isSuchAt isConsonant


-- | Whether a character is one that is used in the
--   International Phonetic Alphabet to represent a
--   consonant.
isConsonant : Char -> Bool
isConsonant c = List.member c consonants

-- | Whether a character in some text, at a specific place
--   within the text is a "segmental" (i.e. not a diacritic or modifier).
isSegmentalAt : Int -> String -> Bool
isSegmentalAt = isSuchAt isSegmental

-- | Whether a character is one that is used in the
--   International Phonetic Alphabet to represent something
--   that is not a diacritic, and can stand on its own.
--   This means characters that can represent a
--   consonant or vowel.
isSegmental : Char -> Bool
isSegmental c = List.member c strictSegmentals

-- | Whether a character is a diacritic that can go after
--   the main character.
isSuperscriptAfterAt : Int  -- ^ a number indicating where the character is in the text
                     -> String -- ^ the text that contains the character
                     -> Bool -- ^ true if the character can be a diacritic after the main character
isSuperscriptAfterAt = isSuchAt isSuperscriptAfter

-- | Whether a character at a certain place in a string,
--   is the tie-bar diacritic.
isTieBarAt : Int  -- ^ a number telling which character in the string
           -> String -- ^ the string (some text)
           -> Bool -- ^ true if it is a tie-bar
isTieBarAt = isSuchAt isTieBar

-- | Whether a character at a string is of a certain class.
isSuchAt : (Char -> Bool) -- ^ a function
         -> Int  -- ^ a number indicating which character in the text
         -> String -- ^ a string
         -> Bool -- ^ whether it is true
isSuchAt function index text = 
  index < String.length text && 
    let charAtIndex = List.head (String.toList (String.slice index (index + 1) text))
    in case charAtIndex
         of Nothing -> False
            Just c  -> function c

-- | Whether a character is a superscript character, that
--   often goes after a full character to modify the full
--   character's meaning.
--   For example in the International Phonetic Alphabet,
--   a superscript `h` causes the phoneme represented by the
--   previous character to
--   be aspirated.
isSuperscriptAfter : Char -> Bool
isSuperscriptAfter c = List.member c superscriptsAfter

-- | Whether a character is a superscript character, that
--   often goes before a full character to modify the
--   full character's meaning.
--   For example in the International Phonetic Alphabet,
--   a superscript `n`.
isSuperscriptBefore : Char -> Bool
isSuperscriptBefore c = List.member c superscriptsBefore


-- | Whether a character is used to tie two characters in the
--   international phonetic alphabet together. The tie bar is
--   usually used to indicate an affricate, or double-articulation.
isTieBar : Char -> Bool
isTieBar x = List.member x ['͜', '͡']

-- | Count how many superscript characters occur one after another, at a
--   specific place in a text (that could modify a previous character).
countPostDiacriticsInARow : String -> Int -> Int
countPostDiacriticsInARow sText startIndex =
  if isSuperscriptAfterAt startIndex sText
    then 1 + countPostDiacriticsInARow sText (startIndex + 1)
    else 0


-- |
-- Whether an IPA character is written above the base line
-- and to the right of the previous character,
-- like how exponents of a power are written
-- in mathematical notation.
isSuperscript : Char -> Bool
isSuperscript character = List.member character superscripts


plosivePulmonic : List Char
plosivePulmonic =
    [ 'p',
      'b',
      't',
      'd',
      'ʈ',
      'ɖ',
      'c',
      'ɟ',
      'k',
      'g',
      'q',
      'ɢ',
      'ʔ'
    ]

nasalPulmonic : List Char
nasalPulmonic = ['m', 'ɱ', 'n', 'ɳ', 'ɲ', 'ŋ', 'ɴ']

trillPulmonic : List Char
trillPulmonic = ['ʙ', 'r', 'ʀ']

tapOrFlapPulmonic : List Char
tapOrFlapPulmonic = ['ⱱ', 'ɾ', 'ɽ']

fricativePulmonic : List Char
fricativePulmonic =
    [ 'ɸ',
      'β',
      'f',
      'v',
      'θ',
      'ð',
      's',
      'z',
      'ʃ',
      'ʒ',
      'ʂ',
      'ʐ',
      'ç',
      'ʝ',
      'x',
      'ɣ',
      'χ',
      'ʁ',
      'ħ',
      'ʕ',
      'h',
      'ɦ'
    ]

lateralFricativePulmonic : List Char
lateralFricativePulmonic = ['ɬ', 'ɮ']

approximantPulmonic : List Char
approximantPulmonic = ['ʋ', 'ɹ', 'ɻ', 'j', 'ɰ']

lateralApproximantPulmonic : List Char
lateralApproximantPulmonic = ['l', 'ɭ', 'ʎ', 'ʟ']


diacriticsAndSuprasegmentals : List Char
diacriticsAndSuprasegmentals =
    [ '̥', -- Voiceless
      '̊', -- Voiceless (diacritic placed above symbol with descender)
      '̤', -- Breathy voiced
      -- End of first row.
      '̬', -- Voiced
      '̰', -- Creaky voiced
      '̺', -- Apical
      -- End of second row.
      'ʰ', -- Aspirated
      '̼', -- Linguolabial
      '̻', -- Laminal
      -- End of third row.
      '̹', -- More rounded
      'ʷ', -- Labialised
      '̃', -- Nasalised
      -- End of fourth row.
      '̜', -- Less rounded
      'ʲ', -- Palatalised
      'ⁿ', -- Pre/post nasalised
      '̟', -- Advanced
      'ˠ', -- Velarised
      'ˡ', -- Lateral release
      '̠', -- Retracted
      'ˤ', -- Pharyngealised
      '̚', -- No audible release
      '̈', -- Centralised
      '̽', -- Mid centralised
      '̝', -- Raised
      '̩', -- Syllabic
      '̞', -- Lowered
      '̯', -- Non-syllabic
      '̘', -- Advanced tongue root
      '˞', -- Rhoticity
      '̙', -- Retracted tongue root
      'ʼ', -- Ejective
      '̍', -- Syllabic (diacritic placed above)
      '̪', -- Dental
      '̣', -- Closer variety/Fricative
      '̇' -- Palatalization/Centralization
    ]

-- To do: find a more suitable name than superscripts.
-- They only look like superscripts if you consider how they
-- look similar to mathematical notation for superscripts.
-- Really, they should be named something different.
superscripts : List Char
superscripts = superscriptsBefore ++ superscriptsAfter

superscriptsBefore : List Char
superscriptsBefore = ['ⁿ']

superscriptsAfter : List Char
superscriptsAfter = diacriticsAndSuprasegmentals ++  ['ː', 'ˑ', '̆']

-- |
-- Whether a character (but not a diacritic)
-- takes up space
-- below the imaginary horizontal line
-- on which it is written.
--
-- This could be useful later for determining
-- where to put diacritics so that
-- they are readable.
ascenders : List Char
ascenders =
    [ 'b',
      't',
      'd',
      'k',
      'ʔ',
      'f',
      'θ',
      'ð',
      'ħ',
      'ʕ',
      'h',
      'ɦ',
      'ɬ',
      'l',
      'ʎ',
      'ʘ',
      'ɓ',
      'ǀ',
      'ɗ',
      'ǃ',
      'ǂ',
      'ɠ',
      'ʄ',
      'ǁ',
      'ʛ',
      'ɺ',
      'ʢ',
      'ʡ',
      'ɤ',
      'ʈ',
      'ɖ',
      'ɸ',
      'β',
      'ʃ',
      'ɮ',
      'ɭ',
      'ɧ'
    ]

descenders : List Char
descenders =
    [ 'p',
      'ɟ',
      'g',
      'q',
      'ɱ',
      'ɽ',
      'ʒ',
      'ʂ',
      'ʐ',
      'ç',
      'ʝ',
      'ɣ',
      'χ',
      'ɻ',
      'j',
      'ɰ',
      'ɥ',
      'y',
      'ɳ',
      'ɲ',
      'ŋ',
      'ʈ',
      'ɖ',
      'ɸ',
      'β',
      'ʃ',
      'ɮ',
      'ɧ'
    ]
    -- We don't include the retroflex l i.e <ɭ> because, even though it is a descender,
    -- There is more free space under it than above


graphemesOfIPA : List Char
graphemesOfIPA =
  consonantsPulmonic
    ++ consonantsNonPulmonic
    ++ otherSymbols
    ++ vowels
    ++ suprasegmentals
    ++ toneAndWordAccents
    ++ diacriticsAndSuprasegmentals

-- See:
--www.internationalphoneticassociation.org/sites/default/files/IPA_Kiel_2015.pdf
-- For the source of this information..


-- CONSONANTS (PULMONIC)
consonantsPulmonic : List Char
consonantsPulmonic =
  plosivePulmonic
    ++ nasalPulmonic
    ++ trillPulmonic
    ++ tapOrFlapPulmonic
    ++ fricativePulmonic
    ++ lateralFricativePulmonic
    ++ approximantPulmonic
    ++ lateralApproximantPulmonic

consonantsNonPulmonic : List Char
consonantsNonPulmonic =
    [ 'ʘ',
      'ɓ', -- Bilabial
      'ǀ' {- Dental -},
      'ɗ', -- Dental/alveolar
      'ǃ' {-  (Post)alveolar -},
      'ʄ',
      'ǂ',
      'ɠ',
      'ǁ',
      'ʛ'
    ]

otherSymbols : List Char
otherSymbols =
    [ 'ʍ',
      'ɕ',
      'w',
      'ʑ',
      'ɥ',
      'ɺ',
      'ʜ',
      'ɧ',
      'ʢ',
      'ʡ'
    ]

consonants : List Char
consonants = consonantsPulmonic ++ consonantsNonPulmonic ++ otherSymbols

vowels : List Char
vowels =
    [ 'i',
      'y',
      'ɨ',
      'ʉ',
      'ɯ',
      'u', -- Close
      'ɪ',
      'ʏ',
      'ʊ',
      'e',
      'ø',
      'ɘ',
      'ɵ',
      'ɤ',
      'o', -- Close-mid
      'ə',
      'ɛ',
      'œ',
      'ɜ',
      'ɞ',
      'ʌ',
      'ɔ', -- Open-mid
      'æ',
      'ɐ',
      'a',
      'ɶ',
      'ɑ',
      'ɒ' -- Open
    ]


-- | IPA text that is not a semantic modifier to what is before or after it.
--   This includes vowels, and consonants. It excludes all diacritics.
strictSegmentals : List Char
strictSegmentals = consonants ++ vowels

suprasegmentals : List Char
suprasegmentals =
    [ 'ˈ', -- Primary stress
      'ˌ', -- Secondary stress
      'ː', -- Long
      'ˑ', -- Half long
      '̆', -- Extra short
      '|', -- Minor (foot) group
      '‖', -- Major (intonation) group
      '.', -- Syllable break
      '‿' -- Linking (absence of a break)
    ]

toneAndWordAccents : List Char
toneAndWordAccents =
    {- Level -}
    [ '˥',
      '̋', -- Extra high
      '˦',
      '́', -- High
      '˧',
      '̄', -- Mid
      '˨',
      '̀', -- Low
      '˩',
      '̏', -- Extra low
      'ꜜ', -- Downstep
      'ꜛ', -- Upstep

      {- Contour -}
      '̌', -- Rising
      '̂', -- Falling
      '᷄', -- High rising
      '᷅', -- Low rising
      '᷈', -- Rising-falling
      '↗', -- Global rise
      '↘' -- Global fall
    ]

isAscender : Char -> Bool
isAscender character = List.member character ascenders

-- |
-- Whether a character (but not a diacritic)
-- takes up space
-- below the imaginary horizontal line
-- on which it is written.
--
-- This could be useful later for determining
-- where to put diacritics so that
-- they are readable.
isDescender : Char -> Bool
isDescender character = List.member character descenders

-- |
-- Whether a diacritic goes above
-- the character it is placed on.
isDiacriticAbove : Char -> Bool
isDiacriticAbove c = c == '̊'
-- |
-- Whether a diacritic goes below
-- the character which it is placed on.
isDiacriticBelow : Char -> Bool
isDiacriticBelow c = c == '̥'

-- |
-- When given a diacritic that goes above,
-- replaces it with one that goes below,
-- and has the same meaning.
-- otherwise does nothing.
lowerDiacritic : Char -> Char
lowerDiacritic c =
  case c of 
    '̊'    -> '̥'
    other -> other

-- |
-- When given a diacritic that goes below,
-- replaces it with one that goes below, and
-- has the same meaning;
-- otherwise it does nothing.
raiseDiacritic : Char -> Char
raiseDiacritic c =
  case c of 
    '̥'     -> '̊'
    other  -> other