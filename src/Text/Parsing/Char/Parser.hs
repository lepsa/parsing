{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Parsing.Char.Parser 
  ( module Text.Parsing.Char.Parser
  , many
  ) where

import Text.Parsing.Char.Types
import Control.Applicative (pure, (<*>), (*>), (<*), (<|>), many)
import Data.Char (Char, isSpace)
import Data.Bool (Bool)
import Data.String (String)
import Data.Eq ((==), (/=))
import Data.Either (either)
import Data.Function (($), (.), const)
import Data.Functor ((<$>), fmap)
import Control.Monad.Error.Class (throwError)
import Data.Semigroup ((<>))
import Data.List.NonEmpty (NonEmpty ((:|)))

eof :: Parser ()
eof = Parser $ \s ->
  case s of
    ""    -> pure ("", ())
    (c:_) -> throwError $ UnexpectedCharacter c

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s ->
  case s of
    []     -> throwError EOF
    (c:cs) -> if f c
                then pure (cs, c)
                else throwError (UnexpectedCharacter c)

satisfy' :: (Char -> Bool) -> Parser ()
satisfy' = fmap (const ()) . satisfy

peek :: Parser Char
peek = Parser $ \s ->
  case s of
    []    -> throwError EOF
    (c:_) -> pure (s, c)

lookAhead :: Parser a -> Parser a
lookAhead p = Parser $ \s ->
  either throwError (suc s) $ runParser p s
  where
    suc s (_, a) = pure (s, a)

notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = Parser $ \s -> either (const $ pure (s, ())) (const $ throwError UnexpectedValue) $ runParser p s

char :: Char -> Parser Char
char c = satisfy (== c)

notChar :: Char -> Parser Char
notChar c = satisfy (/= c)

string :: String -> Parser String
string "" = pure ""
string (c:cs) = (:) <$> char c <*> string cs

spaces :: Parser ()
spaces = const () <$> some (satisfy isSpace)

trim :: Parser a -> Parser a
trim p = spaces *> p <* spaces

wrapped :: Parser () -> Parser a -> Parser a
wrapped w p = w *> p <* w

some :: Parser a -> Parser (NonEmpty a)
some p = (:|) <$> p <*> many p

-- 0 or more elements with the unit between them
-- 
-- Try to parse out the empty list / a single element / a single element : many $ by s *> p
delimited :: Parser () -> Parser a -> Parser [a]
delimited s p = emptyList <|> list
  where
    emptyList = pure [] <* notFollowedBy p
    list      = (:) <$> p <*> many (s *> p)

try :: Parser a -> Parser a -> Parser a
try = (<|>)
