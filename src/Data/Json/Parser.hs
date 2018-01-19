{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ApplicativeDo     #-}

module Data.Json.Parser where

import Data.Json.Types
import qualified Data.Map as Map
import Text.Parsing.Char.Parser

import Control.Applicative ((<*>), (*>), (<*), pure, optional, (<|>))
import Data.Function (($), (.), const, flip)
import Data.Functor ((<$>))
import Data.Char (Char, chr, digitToInt, isHexDigit)
import Data.Bool (Bool (True, False), (||))
import Data.Eq ((==))
import Data.Int (Int)
import Data.Maybe (Maybe (Nothing, Just), fromMaybe, isJust)
import Prelude (Double, Num, (*), (+), (/), (**), (^))
import Data.List (elem, notElem)
import Data.String (String)
import Data.Foldable (foldl, toList)

null :: Parsing m => m JSON
null = const JNull <$> string "null"

true :: Parsing m => m JSON
true = const (JBool True) <$> string "true"

false :: Parsing m => m JSON
false = const (JBool False) <$> string "false"

bool :: Parsing m => m JSON
bool = true <|> false

integral :: Parsing m => m Double
integral = foldl (\z a -> z * 10 + a) 0 <$> some digit

floating :: Parsing m => m Double
floating = do
  i <- integral
  _ <- char '.'
  -- Sum the increasingly small values.
  r <- (foldl f 0 . zipWithDivisor . toList <$> some digit)
  pure $ i + r
  where
    f z (d, n) = z + (n / d)
    zipWithDivisor = zipWithDivisor' 10 
    zipWithDivisor' _ [] = []
    zipWithDivisor' i (a:as) = (i, a) : zipWithDivisor' (i + 10) as

digit :: Num a => Parsing m => m a
digit = (const 0 <$> satisfy (== '0'))
    <|> (const 1 <$> satisfy (== '1'))
    <|> (const 2 <$> satisfy (== '2'))
    <|> (const 3 <$> satisfy (== '3'))
    <|> (const 4 <$> satisfy (== '4'))
    <|> (const 5 <$> satisfy (== '5'))
    <|> (const 6 <$> satisfy (== '6'))
    <|> (const 7 <$> satisfy (== '7'))
    <|> (const 8 <$> satisfy (== '8'))
    <|> (const 9 <$> satisfy (== '9'))

minus :: Parsing m => m Sign
minus = const Minus <$> char '-'

plus :: Parsing m => m Sign
plus = const Plus  <$> char '+'

sign :: Parsing m => m Sign
sign = minus <|> plus

number :: Parsing m => m JSON
number = buildNumber <$> neg <*> num <*> exp
  where
    -- Number building
    buildNumber :: Bool -> Double -> Maybe (Maybe Sign, Double) -> JSON
    buildNumber m n e =
      let x = applyExponent n e
      in JNumber $ if m then -x else x
    applyExponent :: Double -> Maybe (Maybe Sign, Double) -> Double
    applyExponent n e = case e of
      Nothing     -> n
      Just (s, i) -> case fromMaybe Plus s of
        Plus  -> n * (10 ** i)
        Minus -> n * (10 ** (-i))

    -- Parsing helper
    neg = isJust <$> optional minus
    num = floating <|> integral
    exp = optional $ satisfy (\c -> c == 'e' || c == 'E') *> ((,) <$> optional sign <*> integral)

quote :: Parsing m => m Char
quote = char '"'

escape :: Parsing m => m Char
escape = char '\\'

escapeCharacters :: [Char]
escapeCharacters = ['"', '\\', '/', '\b', '\f', '\n', '\r', '\t']

escaped :: Parsing m => m Char
escaped = escape *> (hex <|> satisfy (flip elem escapeCharacters))

unescaped :: Parsing m => m Char
unescaped = satisfy (flip notElem escapeCharacters)

character :: Parsing m => m Char
character = unescaped <|> escaped

hex :: Parsing m => m Char
hex = char 'u' *> hexes
  where
    -- Pull out 4 hex digits, convert into a number, lookup the code point.
    hex' = digitToInt <$> satisfy isHexDigit
    hexes = f <$> hex' <*> hex' <*> hex' <*> hex'
    b16 :: Int -> Int -> Int
    b16 i o = i * (16 ^ o)
    f h1 h2 h3 h4 = chr $ b16 h1 3 + b16 h2 2 + b16 h3 1 + b16 h4 0

jString :: Parsing m => m JSON
jString = quote *> (JString <$> many character) <* quote


--
-- Closer to the gramar of JSON
--

whitespace :: Parsing m => m ()
whitespace = const () <$> (many . satisfy $ flip elem [' ', '\t', '\n', '\r'])

array :: Parsing m => m JSON
array = JArray <$> (beginArray *> delimited valueSeparator value <* endArray)

keyValue :: Parsing m => m (String, JSON)
keyValue = (,) <$> s <* nameSeparator <*> value
  where
    s = (\(JString s') -> s') <$> jString

object :: Parsing m => m JSON
object = JObject . Map.fromList <$> (beginObject *> delimited valueSeparator keyValue <* endObject)

value :: Parsing m => m JSON
value = object <|> array <|> number <|> jString <|> bool <|> null

json :: Parsing m => m JSON
json = whitespace *> value <* whitespace <* eof

beginArray, endArray, beginObject, endObject, nameSeparator, valueSeparator :: Parsing m => m ()
beginArray     = const () <$> wrapped whitespace (char '[')
endArray       = const () <$> wrapped whitespace (char ']')
beginObject    = const () <$> wrapped whitespace (char '{')
endObject      = const () <$> wrapped whitespace (char '}')
nameSeparator  = const () <$> wrapped whitespace (char ':')
valueSeparator = const () <$> wrapped whitespace (char ',')
