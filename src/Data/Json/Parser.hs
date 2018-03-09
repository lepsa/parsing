{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Json.Parser where

import           Data.Json.Types
import qualified Data.Map            as Map
import           Text.Parsing
import           Text.Parsing.Char

import           Control.Applicative (optional, pure, (*>), (<*), (<*>), (<|>))
import           Data.Bool           (Bool (False, True), (||))
import           Data.Char           (Char, chr, digitToInt, isHexDigit, isDigit)
import           Data.Eq             ((==))
import           Data.Foldable       (foldl, toList)
import           Data.Function       (const, flip, ($), (.))
import           Data.Functor        ((<$>))
import           Data.Int            (Int)
import           Data.List           (elem, notElem)
import           Data.Maybe          (Maybe (Just, Nothing), fromMaybe, isJust)
import           Data.String         (String)
import           Prelude             (Double, Num, fromInteger, toInteger, (*),
                                      (**), (+), (/), (^))
import Data.Json.Internal

null :: CharParsing m => m JSON
null = const JNull <$> string jNull

true :: CharParsing m => m JSON
true = const (JBool True) <$> string jTrue

false :: CharParsing m => m JSON
false = const (JBool False) <$> string jFalse

bool :: CharParsing m => m JSON
bool = true <|> false

integral :: CharParsing m => m Double
integral = foldl (\z a -> z * 10 + a) 0 <$> some digit

floating :: CharParsing m => m Double
floating = do
  i <- integral
  _ <- char '.'
  -- Sum the increasingly small values.
  r <- (foldl f 0 . zipWithDivisor . toList <$> some digit)
  pure $ i + r
  where
    f z (d, n) = z + (n / d)
    fractionalBase = 10
    zipWithDivisor = zipWithDivisor' fractionalBase
    zipWithDivisor' _ []     = []
    zipWithDivisor' i (a:as) = (i, a) : zipWithDivisor' (i + fractionalBase) as

digit :: (Num a, CharParsing m) => m a
digit = fromInteger . toInteger . digitToInt <$> satisfy isDigit

minus :: CharParsing m => m Sign
minus = const Minus <$> char '-'

plus :: CharParsing m => m Sign
plus = const Plus <$> char '+'

sign :: CharParsing m => m Sign
sign = minus <|> plus

number :: CharParsing m => m JSON
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

    -- CharParsing helper
    neg = isJust <$> optional minus
    num = floating <|> integral
    exp = optional $ satisfy (\c -> c == 'e' || c == 'E') *> ((,) <$> optional sign <*> integral)

quote :: CharParsing m => m Char
quote = char quoteChar

character :: CharParsing m => m Char
character = escaped <|> satisfy (flip notElem mandatoryEscape)

unescaped :: CharParsing m => m Char
unescaped = satisfy (flip notElem escapeCharacters)


escape :: CharParsing m => m Char
escape = char escapePrefix

escaped :: CharParsing m => m Char
escaped = escape *> (hex <|> quote <|> foldr (<|>) empty [
    , char '\\'
    , char '/'
    , const '\b' <$> char 'b'
    , const '\f' <$> char 'f'
    , const '\n' <$> char 'n'
    , const '\r' <$> char 'r'
    , const '\t' <$> char 't'
   ] 

hex :: CharParsing m => m Char
hex = char 'u' *> hexes
  where
    -- Pull out 4 hex digits, convert into a number, lookup the code point.
    hex' = digitToInt <$> satisfy isHexDigit
    hexes = f <$> hex' <*> hex' <*> hex' <*> hex'
    b16 :: Int -> Int -> Int
    b16 i o = i * (16 ^ o)
    f h1 h2 h3 h4 = chr $ b16 h1 3 + b16 h2 2 + b16 h3 1 + b16 h4 0

jString :: CharParsing m => m JSON
jString = quote *> (JString <$> many character) <* quote


--
-- Closer to the gramar of JSON
--

whitespace :: CharParsing m => m ()
whitespace = const () <$> (many . satisfy $ flip elem [' ', '\t', '\n', '\r'])

array :: CharParsing m => m JSON
array = JArray <$> (beginArray *> delimited valueSeparator value <* endArray)

keyValue :: CharParsing m => m (String, JSON)
keyValue = (,) <$> s <* nameSeparator <*> value
  where
    s = (\(JString s') -> s') <$> jString

object :: CharParsing m => m JSON
object = JObject . Map.fromList <$> (beginObject *> delimited valueSeparator keyValue <* endObject)

value :: CharParsing m => m JSON
value = object <|> array <|> number <|> jString <|> bool <|> null

json :: CharParsing m => m JSON
json = whitespace *> value <* whitespace <* eof

beginArray, endArray, beginObject, endObject, nameSeparator, valueSeparator :: CharParsing m => m ()
beginArray     = const () <$> wrapped whitespace (char beginArrayChar)
endArray       = const () <$> wrapped whitespace (char endArrayChar)
beginObject    = const () <$> wrapped whitespace (char beginObjectChar)
endObject      = const () <$> wrapped whitespace (char endObjectChar)
nameSeparator  = const () <$> wrapped whitespace (char nameSeparatorChar)
valueSeparator = const () <$> wrapped whitespace (char valueSeparatorChar)
