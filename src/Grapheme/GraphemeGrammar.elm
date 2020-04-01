module Grapheme.GraphemeGrammar exposing (isAscender, isDescender, isExponential, isDiacriticAbove, isDiacriticBelow) 

exponentials : List Char
exponentials = ['ʰ' , 'ʷ' , 'ʲ' , 'ˠ' , 'ˤ' , 'ⁿ' , 'ˡ']

{-|
Whether an IPA character is written above the base line
and to the right of the previous character,
like how exponents of a power are written
in mathematical notation.
|-}
isExponential : Char -> Bool
isExponential character = List.member character exponentials
{-|
Whether a diacritic goes above
the character it is placed on.
|-}
isDiacriticAbove : Char -> Bool
isDiacriticAbove c = 
  case c of 
    '̊' -> True
    _  -> False

{-|
Whether a diacritic goes below
the character which it is placed on.
|-}
isDiacriticBelow : Char -> Bool
isDiacriticBelow c =
  case c of 
    '̥' -> True
    _  -> False



{-|
Whether a character (but not a diacritic)
takes up space
below the imaginary horizontal line
on which it is written.

This could be useful later for determining
where to put diacritics so that
they are readable.
|-}
ascenders : List Char
ascenders =
  ['b', 't', 'd', 'k', 'ʔ', 'f', 'θ', 'ð', 'ħ', 'ʕ', 'h', 'ɦ', 'ɬ', 'l', 'ʎ',
  'ʘ', 'ɓ', 'ǀ', 'ɗ', 'ǃ', 'ǂ', 'ɠ', 'ʄ', 'ǁ', 'ʛ', 'ɺ', 'ʢ', 'ʡ', 'ɤ', 'ʈ', 'ɖ',
  'ɸ', 'β', 'ʃ', 'ɮ', 'ɭ', 'ɧ']


isAscender : Char -> Bool
isAscender character = List.member character ascenders

descenders : List Char
descenders =
  ['p', 'ɟ', 'g', 'q', 'ɱ', 'ɽ', 'ʒ', 'ʂ', 'ʐ', 'ç', 'ʝ', 'ɣ', 'χ', 'ɻ', 'j',
   'ɰ', 'ɥ', 'y', 'ɳ', 'ɲ', 'ʈ', 'ɖ', 'ɸ', 'β', 'ʃ', 'ɮ', 'ɭ', 'ɧ']


{-|
Whether a character (but not a diacritic)
takes up space
below the imaginary horizontal line
on which it is written.

This could be useful later for determining
where to put diacritics so that
they are readable.
|-}
isDescender : Char -> Bool
isDescender character = List.member character descenders
