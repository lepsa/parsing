{-# LANGUAGE NoImplicitPrelude #-}

module Data.Json.Parser where

import Data.Json.Types
import qualified Data.Map as Map
import Text.Parsing.Char.Parser
import Text.Parsing.Char.Types

import Control.Applicative ((<*>), (*>), (<*), pure, optional)
import Data.Function (($), (.), const, flip)
import Data.Functor ((<$>))
import Data.Char (Char, chr, digitToInt, isHexDigit, isSpace)
import Data.Bool (Bool (True, False), (||))
import Data.Eq ((==))
import Data.Int (Int)
import Data.Set (Set, fromList, member)
import Data.Maybe (Maybe (Nothing, Just), fromMaybe, isJust)
import Prelude (Double, Num, (*), (+), (/), (**), (^))
import Data.List ((++), elem, notElem)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String (String)
import Data.Foldable (foldl, toList)

null :: Parser JSON
null = const JNull <$> string "null"

true :: Parser JSON
true = const (JBool True) <$> string "true"

false :: Parser JSON
false = const (JBool False) <$> string "false"

bool :: Parser JSON
bool = true <|> false

integral :: Parser Double
integral = foldl (\z a -> z * 10 + a) 0 <$> some digit

floating :: Parser Double
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

digit :: Num a => Parser a
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

minus :: Parser Sign
minus = const Minus <$> char '-'

plus :: Parser Sign
plus = const Plus  <$> char '+'

sign :: Parser Sign
sign = minus <|> plus

number :: Parser JSON
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

quote :: Parser Char
quote = char '"'

escape :: Parser Char
escape = char '\\'

escapeCharacters :: [Char]
escapeCharacters = ['"', '\\', '/', '\b', '\f', '\n', '\r', '\t']

escaped :: Parser Char
escaped = escape *> (hex <|> satisfy (flip elem escapeCharacters))

unescaped :: Parser Char
unescaped = satisfy (flip notElem escapeCharacters)

character :: Parser Char
character = unescaped <|> escaped

hex :: Parser Char
hex = char 'u' *> hexes
  where
    -- Pull out 4 hex digits, convert into a number, lookup the code point.
    hex' = digitToInt <$> satisfy isHexDigit
    hexes = f <$> hex' <*> hex' <*> hex' <*> hex'
    b16 :: Int -> Int -> Int
    b16 i o = i * (16 ^ o)
    f h1 h2 h3 h4 = chr $ b16 h1 3 + b16 h2 2 + b16 h3 1 + b16 h4 0

jString :: Parser JSON
jString = quote *> (JString <$> many character) <* quote


--
-- Closer to the gramar of JSON
--

whitespace :: Parser ()
whitespace = const () <$> (many . satisfy $ flip elem [' ', '\t', '\n', '\r'])

array :: Parser JSON
array = do
  _ <- beginArray
  l <- delimited valueSeparator value
  _ <- endArray
  pure $ JArray l

object :: Parser JSON
object = do
  _ <- beginObject
  l <- delimited valueSeparator keyValue
  _ <- endObject
  pure . JObject $ Map.fromList l
  where
    keyValue :: Parser (String, JSON)
    keyValue = do
      k <- (\(JString s) -> s) <$> jString
      _ <- nameSeparator
      v <- value
      pure (k, v)

value, json :: Parser JSON
value = object <|> array <|> number <|> jString <|> bool <|> null
json  = whitespace *> value <* whitespace <* eof

beginArray, endArray, beginObject, endObject, nameSeparator, valueSeparator :: Parser ()
beginArray     = const () <$> wrapped whitespace (char '[')
endArray       = const () <$> wrapped whitespace (char ']')
beginObject    = const () <$> wrapped whitespace (char '{')
endObject      = const () <$> wrapped whitespace (char '}')
nameSeparator  = const () <$> wrapped whitespace (char ':')
valueSeparator = const () <$> wrapped whitespace (char ',')


(<|>) = try
