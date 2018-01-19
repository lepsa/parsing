{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Text.Parsing.Types where

import           Control.Applicative       (Alternative, Applicative, empty,
                                            liftA2, pure, (<*>), (<|>), (*>))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad             (Monad, (>>=))
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

instance Parsing Parser' where
  eof = Parser' $ \s -> case s of
    []    -> Success () 0 []
    (c:_) -> Failure (UnexpectedCharacter c) 1

  lookAhead p = Parser' $ \s -> case s of
    [] -> Failure EOF 0
    _  -> f $ runParser' p s
    where
      f (Failure e o)   = Failure e o
      f (Success a _ s) = Success a 0 s

  notFollowedBy p = Parser' $ \s -> f s $ runParser' p s
    where
      f s (Failure _ _)   = Success () 0 s
      f _ (Success _ o _) = Failure UnexpectedValue o

instance CharParsing Parser' where
  -- I need to do something similar to trifecta where
  -- if my parser consumes input it will not backtrack.
  --
  -- runParser' bool "false" -- currently fails when it shouldn't.
  -- runParser' bool "tru"   -- should fail at position 3 with EOF
  -- runParser' bool "falst" -- should fail at position 5 with UnexpectedCharacter 't'
  string s = case s of
    []    -> pure []
    (c:_) -> f c *> string' s
    where
      string' []     = pure []
      string' (c:cs) = (:) <$> char c <*> string' cs
      -- Check if we need to parse through the string, or if it is skipable.
      f c = Parser' $ \s' -> case s' of
        []     -> Failure EOF 0
        (c':_) -> if c == c'
          then Success () 0 s'
          else Failure Empty 0

  peek = Parser' $ \s -> case s of
    []    -> Failure EOF 0
    (c:_) -> Success c 0 s
  
  satisfy f = Parser' $ \s -> case s of
    []     -> Failure EOF 0
    (c:cs) -> if f c
      then Success c 1 cs
      else Failure (UnexpectedCharacter c) 1
