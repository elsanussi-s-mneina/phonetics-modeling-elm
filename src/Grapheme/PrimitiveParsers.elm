module Grapheme.PrimitiveParsers exposing (optionalParser, orParser, singleCharParser, thenParser)


singleCharParser
  :  List Char
  -> String
  -> Maybe (String, String)
singleCharParser charList text =
  if not (String.isEmpty text) && List.member (String.left 1 text) (List.map String.fromChar charList)
    then Just (String.left 1 text, String.dropLeft 1 text)
    else Nothing


-- | Uses one parser on the text,
--   then uses the next parser on the remaining
--   text from the first parse.
thenParser
  :  (String -> Maybe (String, String))
  -> (String -> Maybe (String, String))
  -> String
  -> Maybe (String, String)
thenParser firstParser secondParser text =
  case firstParser text of
    Nothing -> Nothing
    Just (parsed, rest)
            -> case secondParser rest of
                 Nothing -> Nothing
                 Just (parsed2, rest2) -> Just (parsed ++ parsed2, rest2)


-- | Combines parsers by using one or the other.
--   The first parser that succeeds is used.
orParser
  :  (String -> Maybe (String, String))
  -> (String -> Maybe (String, String))
  -> String
  -> Maybe (String, String)
orParser firstParser secondParser text =
  case firstParser text of
    Nothing -> secondParser text
    Just result -> Just result

-- | changes a parser by repeating it an indefinite number
--   of times.
--   So a parser that parses only "a", will parse "aaaaa".
--   A parser that parses only "@", will parse "@@@@", "@@@@@" and
--   so on.
manyParser
  :  (String -> Maybe (String, String))
  -> String
  -> Maybe (String, String)
manyParser subParser text =
  case subParser text of
    Nothing -> Nothing
    Just (parsed, rest)
            -> case manyParser subParser rest of
                  Nothing -> Just (parsed, rest)
                  Just (parsed2, rest2) -> Just (parsed ++ parsed2, rest2)

-- | changes a parser by making it never return Nothing,
--   that is it makes the parser optional.
optionalParser
  :  (String -> Maybe (String, String))
  -> String
  -> Maybe (String, String)
optionalParser subParser text =
  case subParser text of
    Nothing -> Just ("", text) -- Return text unconsumed.
    Just result -> Just result
