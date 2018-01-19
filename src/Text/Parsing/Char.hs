{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Text.Parsing.Char
  ( module Text.Parsing.Char
  , module Control.Applicative
  ) where

import           Control.Applicative       (Applicative, Alternative, many, some, pure, (*>), (<*),
                                            (<*>), (<|>))
import           Data.Bool                 (Bool)
import           Data.Char                 (Char, isSpace)
import           Data.Eq                   ((/=), (==))
import           Data.Function             (const, (.))
import           Data.Functor              ((<$>))
import           Data.String               (String)
import Text.Parsing (Parsing (..))

class Parsing m => CharParsing m where
  satisfy :: (Char -> Bool) -> m Char
  peek    :: m Char

  satisfy' :: (Char -> Bool) -> m ()
  satisfy' f = const () <$> satisfy f

  char :: Char -> m Char
  char =  satisfy . (==)

  notChar :: Char -> m Char
  notChar =  satisfy . (/=)

  string :: String -> m String
  string []     = pure []
  string (c:cs) = (:) <$> char c <*> string cs

  trim :: m a -> m a
  trim p = spaces *> p <* spaces

  spaces :: m ()
  spaces = const () <$> some (satisfy isSpace)
