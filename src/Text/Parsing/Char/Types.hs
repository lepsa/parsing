{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Parsing.Char.Types where

import Control.Lens.TH (makeLenses)
import Control.Applicative (Applicative, pure, (<*>), Alternative, empty, (<|>), liftA2)
import Control.Monad (Monad, (>>=))
import Data.Char (Char)
import Data.Maybe ()
import Data.List.NonEmpty ()
import Control.Monad.Except ()
import Control.Monad.Error.Class (throwError, catchError)
import Text.Show (Show)
import Data.Maybe (Maybe)
import Data.Eq (Eq)
import Data.Functor (Functor, fmap, (<$>))
import Data.String (String)
import Data.Either (Either, either)
import Data.Function (($), const, (.))
import Data.Monoid (Monoid, mempty, mappend)
import Data.Semigroup (Semigroup, (<>))
import Prelude (Int, (+))

data ParseError = EOF -- Ran out of input
                | Foo -- ???
                | UnexpectedCharacter Char
                | UnexpectedValue -- Incorrect lookahead. Should be UnexpectedValue a
                | Empty -- The empty error for Alternative.
  deriving (Show, Eq)

-- Parse values out of a string.
newtype Parser a = Parser {
  runParser :: String -> Either ParseError (String, a)
}

instance Functor Parser where
  fmap f p = Parser $ \s -> fmap f <$> runParser p s

instance Applicative Parser where
  pure a    = Parser $ \s -> pure (s, a)
  pF <*> pA = Parser $ \s ->
    let f = \(s', a) -> fmap a <$> runParser pA s'
    in either throwError f $ runParser pF s

instance Alternative Parser where
  empty = Parser . const $ throwError Empty
  p <|> q = Parser $ \s -> catchError (runParser p s) (const $ runParser q s)

-- We don't need the Monad instance for this repo, but I'll leave it here just in case.
instance Monad Parser where
  m >>= f = Parser $ \s ->
    either
      throwError
      (\(s', a) -> runParser (f a) s')
      (runParser m s)

--
-- Newtype instances
--

-- Keep Conal Elliot happy
newtype ParserApp a = ParserApp { unParserApp :: Parser a }
  deriving (Functor, Applicative, Alternative, Monad)

instance Semigroup a => Semigroup (ParserApp a) where
  (<>) = liftA2 (<>)

instance (Semigroup a, Monoid a) => Monoid (ParserApp a) where
  mempty = pure mempty
  mappend = (<>)

-- Keep Conor McBride happy
newtype ParserAlt a = ParserAlt { unParserAlt :: Parser a }
  deriving (Functor, Applicative, Alternative, Monad)

instance Semigroup (ParserAlt a) where
  (<>) = (<|>)

instance Monoid (ParserAlt a) where
  mempty = empty
  mappend = (<>)





--
-- Fancier parsing type
--

data ParseResult a = 
  Failure { _failureError  :: ParseError
          , _failureOffset :: Int
          } |
  Success { _successResult :: a
          , _successOffset :: Int
          , _successRest   :: String
          }
  deriving (Functor, Show)
makeLenses ''ParseResult

newtype Parser' a = Parser' {
  runParser' :: String -> ParseResult a
}

instance Functor Parser' where
  fmap f p = Parser' $ \s -> case runParser' p s of
    (Failure e o)   -> Failure e o
    (Success a o r) -> Success (f a) o r

addOffset :: Int -> Parser' a -> Parser' a
addOffset n p = Parser' $ \s -> case runParser' p s of
  (Failure e o)   -> Failure e (o + n)
  (Success a o r) -> Success a (o + n) r

instance Applicative Parser' where
  pure a = Parser' $ \s -> Success { _successResult = a, _successOffset = 0, _successRest = s }
  pF <*> pA = Parser' $ \s -> case runParser' pF s of
    (Failure e o)    -> Failure e o
    (Success f o s') -> runParser' (f <$> addOffset o pA) s'

instance Alternative Parser' where
  empty   = Parser' $ \_ -> Failure Empty 0
  p <|> q = Parser' $ \s -> case runParser' p s of
    (Failure _ _) -> runParser' q s
    r             -> r
