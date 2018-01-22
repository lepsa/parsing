{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Text.Parsing.Types where

import           Control.Applicative       (Alternative, Applicative, empty,
                                            liftA2, pure, (<*>), (<|>), (*>))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad             (Monad, (>>=), ap)
import           Data.Char                 (Char)
import           Data.Either               (Either (Left), either)
import           Data.Eq                   (Eq ((==)))
import           Data.Function             (const, ($), (.), id)
import           Data.Functor              (Functor, fmap, (<$>))
import           Data.Monoid               (Monoid, mappend, mempty)
import           Data.Semigroup            (Semigroup, (<>))
import           Data.String               (String)
import           Prelude                   (Int, (+))
import           Text.Show                 (Show)
import Text.Parsing (Parsing (..))
import Text.Parsing.Char (CharParsing (..))

data ParseError = EOF -- Ran out of input
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
    in either Left f $ runParser pF s

instance Alternative Parser where
  empty = Parser . const $ Left Empty
  p <|> q = Parser $ \s -> either (const $ runParser q s) (pure . id) $ runParser p s

-- We don't need the Monad instance for this repo, but I'll leave it here just in case.
instance Monad Parser where
  m >>= f = Parser $ \s ->
    either
      Left
      (\(s', a) -> runParser (f a) s')
      (runParser m s)

instance Parsing Parser where
  eof = Parser $ \s ->
    case s of
      []    -> pure ([], ())
      (c:_) -> Left $ UnexpectedCharacter c

  lookAhead p = Parser $ \s ->
    either Left (suc s) $ runParser p s
    where
      suc s (_, a) = pure (s, a)

  notFollowedBy p = Parser $ \s ->
    either
      (const $ pure (s, ()))
      (const $ Left UnexpectedValue)
      $ runParser p s

instance CharParsing Parser where
  satisfy f = Parser $ \s ->
    case s of
      []     -> Left EOF
      (c:cs) -> if f c
                  then pure (cs, c)
                  else Left (UnexpectedCharacter c)

  peek = Parser $ \s ->
    case s of
      []    -> Left EOF
      (c:_) -> pure (s, c)


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
-- Non-backtracking parser based on trifecta.
-- You may have noticed the names.
--

data ParseResult a = EpsilonSuccess   String a
                   | EpsilonFailure   ParseError
                   | CommittedSuccess String String a
                   | CommittedFailure String ParseError
  deriving (Show)

instance Functor ParseResult where
  fmap f (EpsilonSuccess s' a)     = EpsilonSuccess s' $ f a
  fmap f (CommittedSuccess s s' a) = CommittedSuccess s s' $ f a
  fmap _ (EpsilonFailure e)        = EpsilonFailure e
  fmap _ (CommittedFailure s e)    = CommittedFailure s e

newtype Parser' a = Parser' { runParser' :: String -> ParseResult a }

instance Functor Parser' where
  fmap f p = Parser' $ \s -> f <$> runParser' p s

instance Applicative Parser' where
  pure a = Parser' $ \s -> EpsilonSuccess s a
  (<*>) = ap

instance Alternative Parser' where
  empty = Parser' . const $ EpsilonFailure Empty
  a <|> b = Parser' $ \s -> case runParser' a s of
    (EpsilonFailure _) -> runParser' b s
    r                  -> r

instance Monad Parser' where
  p >>= f = Parser' $ \i -> case runParser' p i of
    (EpsilonFailure e)        -> EpsilonFailure e
    (CommittedFailure s e)    -> CommittedFailure s e
    (EpsilonSuccess s' a)     -> runToCommitted "" s' a
    (CommittedSuccess s s' a) -> runToCommitted s s' a
    where
      runToCommitted s s' a = epsilonToCommitted s $ runParser' (f a) s'

committedToEpsilon :: ParseResult a -> ParseResult a
committedToEpsilon (CommittedFailure _ e)    = EpsilonFailure e
committedToEpsilon (CommittedSuccess _ s' a) = EpsilonSuccess s' a
committedToEpsilon r                         = r

epsilonToCommitted :: String -> ParseResult a -> ParseResult a
epsilonToCommitted s (EpsilonFailure e)    = CommittedFailure s e
epsilonToCommitted s (EpsilonSuccess s' a) = CommittedSuccess s s' a
epsilonToCommitted s (CommittedFailure s' e) = CommittedFailure (s <> s') e
epsilonToCommitted s (CommittedSuccess s' s'' a) = CommittedSuccess (s <> s') s'' a

instance Parsing Parser' where
  eof = Parser' $ \s -> EpsilonSuccess s ()
  lookAhead p = Parser' $ \s -> case runParser' p s of
    (CommittedSuccess _ _ a) -> EpsilonSuccess s a
    (EpsilonSuccess _ a)     -> EpsilonSuccess s a
    r                        -> committedToEpsilon r

  notFollowedBy p = Parser' $ \s -> case runParser' p s of
    (CommittedSuccess _ _ _) -> EpsilonFailure UnexpectedValue
    (EpsilonSuccess _ _)     -> EpsilonFailure UnexpectedValue
    _                        -> EpsilonSuccess s ()

instance CharParsing Parser' where
  satisfy f = Parser' $ \s -> case s of
    ""     -> EpsilonFailure EOF
    (c:cs) -> if f c
      then CommittedSuccess [c] cs c
      else EpsilonFailure $ UnexpectedCharacter c

  peek = Parser' $ \s -> case s of
    ""    -> EpsilonFailure EOF
    (c:_) -> EpsilonSuccess s c
