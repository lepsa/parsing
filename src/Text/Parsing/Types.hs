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
  -- c is derivable from s, and we default it to Char
  type ConsValue c :: *
  type ConsValue c = Char
  cons   :: c -> s -> s
  uncons :: s -> Maybe (c, s)

instance Cons [a] a where
  -- In this case, we can Cons and list type.
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

newtype Consumed s = Consumed s
  deriving (Show, Semigroup, Monoid)
newtype Remaining s = Remaining s
  deriving (Show, Semigroup, Monoid)

data ParseResult s a = EpsilonSuccess (Remaining s) a
                     | EpsilonFailure ParseError
                     | CommittedSuccess (Consumed s) (Remaining s) a
                     | CommittedFailure (Consumed s) ParseError
  deriving (Functor, Show)

newtype Parser' s c a = Parser' { runParser' :: Cons s c => s -> ParseResult s a }

instance Functor (Parser' s c) where
  fmap f p = Parser' $ \s -> f <$> runParser' p s

instance Cons s c => Applicative (Parser' s c) where
  pure a = Parser' $ \s -> EpsilonSuccess (Remaining s) a
  (<*>) = ap -- Comes from Monad. Now we have circular deps.

instance Cons s c => Alternative (Parser' s c) where
  empty = Parser' . const $ EpsilonFailure Empty
  a <|> b = Parser' $ \s -> case runParser' a s of
    (EpsilonFailure _) -> runParser' b s
    r                  -> r

instance Cons s c => Monad (Parser' s c) where
  p >>= f = Parser' $ \i -> case runParser' p i of
    (EpsilonFailure e)       -> EpsilonFailure e
    (CommittedFailure c e)   -> CommittedFailure c e
    (EpsilonSuccess s a)     -> runToCommitted s a
    (CommittedSuccess c s a) -> prependConsumed c $ runToCommitted s a
    where
      runToCommitted (Remaining s) a = toCommitted (runParser' (f a) s)
      prependConsumed :: Semigroup s => Consumed s -> ParseResult s a -> ParseResult s a
      prependConsumed c r = case r of
        (CommittedSuccess c' s a) -> CommittedSuccess (c <> c') s a
        (CommittedFailure c' e)   -> CommittedFailure (c <> c') e
        _                         -> r

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

toCommitted :: Monoid s => ParseResult s a -> ParseResult s a
toCommitted (EpsilonFailure e)    = CommittedFailure mempty e
toCommitted (EpsilonSuccess s' a) = CommittedSuccess mempty s' a
toCommitted r                     = r

instance Cons s c => Parsing (Parser' s c) where
  eof = Parser' $ \s -> EpsilonSuccess (Remaining s) ()
  lookAhead p = Parser' $ \s -> case runParser' p s of
    (CommittedSuccess _ _ a) -> EpsilonSuccess (Remaining s) a
    (EpsilonSuccess _ a)     -> EpsilonSuccess (Remaining s) a
    (CommittedFailure _ e)   -> EpsilonFailure e
    (EpsilonFailure e)       -> EpsilonFailure e

  notFollowedBy p = Parser' $ \s -> case runParser' p s of
    (CommittedSuccess _ _ _) -> EpsilonFailure UnexpectedValue
    (EpsilonSuccess _ _)     -> EpsilonFailure UnexpectedValue
    _                        -> EpsilonSuccess (Remaining s) ()

instance (Cons s c, c ~ Char) => CharParsing (Parser' s c) where
  satisfy f = Parser' $ maybe (EpsilonFailure EOF) g . uncons
    where
      g (c, cs) = if f c
        then CommittedSuccess (Consumed $ cons c mempty) (Remaining cs) c
        else EpsilonFailure $ UnexpectedCharacter c

  peek = Parser' $ \s -> maybe
    (EpsilonFailure EOF)
    (\(c, _) -> EpsilonSuccess (Remaining s) c)
    $ uncons s
