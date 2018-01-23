{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Text.Parsing.Types where

import           Control.Applicative        (Alternative, Applicative, empty,
                                             liftA2, pure, (<*>), (<|>))
import           Control.Monad              (Monad, (>>=), ap)
import           Data.Char                  (Char)
import           Data.Either                (Either (Left), either)
import           Data.Eq                    (Eq)
import           Data.Function              (const, ($), (.), id)
import           Data.Functor               (Functor, fmap, (<$>))
import           Data.Maybe                 (Maybe, maybe)
import qualified Data.List                  as List
import           Data.Monoid                (Monoid, mappend, mempty)
import           Data.Semigroup             (Semigroup, (<>))
import           Data.String                (String)
import           Text.Parsing               (Parsing (..))
import           Text.Parsing.Char          (CharParsing (..))
import           Text.Show                  (Show)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

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

-- If the first parser fails, run the second.
instance Alternative Parser where
  empty = Parser . const $ Left Empty
  p <|> q = Parser $ \s -> either 
    (const $ runParser q s)
    (pure . id)
    $ runParser p s

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

instance Monoid a => Monoid (ParserApp a) where
  mempty = pure mempty
  mappend = liftA2 mappend

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

-- How we will be accessing our streams
class (Semigroup s, Monoid s) => Cons s c where
  type ConsValue c :: *
  type ConsValue c = Char
  cons   :: c -> s -> s
  uncons :: s -> Maybe (c, s)

instance Cons [a] a where
  type ConsValue a = a
  cons   = (:)
  uncons = List.uncons

instance Cons T.Text Char where
  cons   = T.cons
  uncons = T.uncons

instance Cons TL.Text Char where
  cons   = TL.cons
  uncons = TL.uncons

instance Cons BS.ByteString Char where
  cons   = BS.cons
  uncons = BS.uncons

instance Cons BSL.ByteString Char where
  cons   = BSL.cons
  uncons = BSL.uncons

data ParseResult s a = EpsilonSuccess s a -- String is what remains
                     | EpsilonFailure  ParseError
                     -- The fields are what we have consumed so far,
                     -- what remains to parse,
                     -- and the result
                     | CommittedSuccess s s a
                     -- String is what we have consumed so far
                     | CommittedFailure s ParseError
  deriving (Functor, Show)

newtype Parser' s c a = Parser' { runParser' :: Cons s c => s -> ParseResult s a }

instance Functor (Parser' s c) where
  fmap f p = Parser' $ \s -> f <$> runParser' p s

instance Cons s c => Applicative (Parser' s c) where
  pure a = Parser' $ \s -> EpsilonSuccess s a
  (<*>) = ap

instance Cons s c => Alternative (Parser' s c) where
  empty = Parser' . const $ EpsilonFailure Empty
  a <|> b = Parser' $ \s -> case runParser' a s of
    (EpsilonFailure _) -> runParser' b s
    r                  -> r

instance Cons s c => Monad (Parser' s c) where
  p >>= f = Parser' $ \i -> case runParser' p i of
    (EpsilonFailure e)        -> EpsilonFailure e
    (CommittedFailure s e)    -> CommittedFailure s e
    (EpsilonSuccess s' a)     -> runToCommitted mempty s' a
    (CommittedSuccess s s' a) -> runToCommitted s s' a
    where
      runToCommitted s s' a = epsilonToCommitted s $ runParser' (f a) s'

-- Keep Conal Elliot happy
newtype ParserApp' s c a = ParserApp' { unParserApp' :: Parser' s c a }
  deriving (Functor, Applicative, Alternative, Monad)

instance (Cons s c, Semigroup a) => Semigroup (ParserApp' s c a) where
  (<>) = liftA2 (<>)

instance (Cons s c, Monoid a) => Monoid (ParserApp' s c a) where
  mappend = liftA2 mappend
  mempty = pure mempty

-- Keep Conor McBride happy
newtype ParserAlt' s c a = ParserAlt' { unParserAlt' :: Parser' s c a }
  deriving (Functor, Applicative, Alternative, Monad)

instance Cons s c => Semigroup (ParserAlt' s c a) where
  (<>) = (<|>)

instance Cons s c => Monoid (ParserAlt' s c a) where
  mempty = empty
  mappend = (<>)

committedToEpsilon :: ParseResult s a -> ParseResult s a
committedToEpsilon (CommittedFailure _ e)    = EpsilonFailure e
committedToEpsilon (CommittedSuccess _ s' a) = EpsilonSuccess s' a
committedToEpsilon r                         = r

epsilonToCommitted :: Semigroup s => s -> ParseResult s a -> ParseResult s a
epsilonToCommitted s (EpsilonFailure e)          = CommittedFailure s e
epsilonToCommitted s (EpsilonSuccess s' a)       = CommittedSuccess s s' a
epsilonToCommitted s (CommittedFailure s' e)     = CommittedFailure (s <> s') e
epsilonToCommitted s (CommittedSuccess s' s'' a) = CommittedSuccess (s <> s') s'' a

instance Cons s c => Parsing (Parser' s c) where
  eof = Parser' $ \s -> EpsilonSuccess s ()
  lookAhead p = Parser' $ \s -> case runParser' p s of
    (CommittedSuccess _ _ a) -> EpsilonSuccess s a
    (EpsilonSuccess _ a)     -> EpsilonSuccess s a
    (CommittedFailure _ e)   -> EpsilonFailure e
    (EpsilonFailure e)       -> EpsilonFailure e

  notFollowedBy p = Parser' $ \s -> case runParser' p s of
    (CommittedSuccess _ _ _) -> EpsilonFailure UnexpectedValue
    (EpsilonSuccess _ _)     -> EpsilonFailure UnexpectedValue
    _                        -> EpsilonSuccess s ()

instance (Cons s c, c ~ Char) => CharParsing (Parser' s c) where
  satisfy f = Parser' $ maybe (EpsilonFailure EOF) g . uncons
    where
      g (c, cs) = if f c
        then CommittedSuccess (cons c mempty) cs c
        else EpsilonFailure $ UnexpectedCharacter c

  peek = Parser' $ \s -> maybe
    (EpsilonFailure EOF)
    (\(c, _) -> EpsilonSuccess s c)
    $ uncons s
