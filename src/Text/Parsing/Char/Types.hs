{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Parsing.Char.Types where

import Control.Applicative (Applicative, pure, (<*>), Alternative, empty, (<|>))
import Control.Monad (Monad, (>>=))
import Data.Char (Char)
import Data.Maybe ()
import Data.List.NonEmpty ()
import Control.Monad.Except ()
import Control.Monad.Error.Class (throwError, catchError)
import Text.Show (Show)
import Data.Eq (Eq)
import Data.Functor (Functor, fmap, (<$>))
import Data.String (String)
import Data.Either (Either, either)
import Data.Function (($), const, (.))

data ParseError = EOF
                | UnexpectedCharacter Char
                | UnexpectedValue
                | Empty
  deriving (Show, Eq)

-- Parse values out of a string.
newtype Parser a = Parser {
  runParser :: String -> Either ParseError (String, a)
} deriving (Functor)

instance Applicative Parser where
  pure a    = Parser $ \s -> pure (s, a)
  pF <*> pA = Parser $ \s ->
    let f = \(s', a) -> fmap a <$> runParser pA s'
    in either throwError f $ runParser pF s

instance Alternative Parser where
  empty = Parser . const $ throwError Empty
  p <|> q = Parser $ \s -> catchError (runParser p s) (const $ runParser q s)

instance Monad Parser where
  m >>= f = Parser $ \s ->
    either
      throwError
      (\(s', a) -> runParser (f a) s')
      (runParser m s)
