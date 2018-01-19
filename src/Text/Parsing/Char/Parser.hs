{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Text.Parsing.Char.Parser 
  ( module Text.Parsing.Char.Parser
  , module Control.Applicative
  ) where

import Text.Parsing.Char.Types
import Control.Applicative (pure, (<*>), (*>), (<*), (<|>), many, Alternative)
import Data.Char (Char, isSpace)
import Data.Bool (Bool)
import Data.String (String)
import Data.Eq ((==), (/=))
import Data.Either (either)
import Data.Function (($), (.), const)
import Data.Functor ((<$>))
import Control.Monad.Error.Class (throwError)
import Data.List.NonEmpty (NonEmpty ((:|)))

class Alternative m => Parsing e m | m -> e where
  eof           :: m ()
  satisfy       :: (Char -> Bool) -> m Char
  peek          :: m Char
  lookAhead     :: m a -> m a
  notFollowedBy :: m a -> m ()
  error         :: e -> m a
 
  satisfy' :: (Char -> Bool) -> m ()
  satisfy' f = const () <$> satisfy f

  char :: Char -> m Char
  char =  satisfy . (==)

  notChar :: Char -> m Char
  notChar =  satisfy . (/=)

  string :: String -> m String
  string [] = pure []
  string (c:cs) = (:) <$> char c <*> string cs

  trim :: m a -> m a
  trim p = spaces *> p <* spaces
  
  wrapped :: m () -> m a -> m a
  wrapped w p = w *> p <* w
  
  some :: m a -> m (NonEmpty a)
  some p = (:|) <$> p <*> many p

  spaces :: m ()
  spaces = const () <$> some (satisfy isSpace)
  
  -- 0 or more elements with the unit between them
  delimited :: m () -> m a -> m [a]
  delimited s p = emptyList <|> list
    where
      emptyList = pure [] <* notFollowedBy p
      list      = (:) <$> p <*> many (s *> p)


instance Parsing Parser' where
  error e = Parser' . const $ Failure e 0

  eof = Parser' $ \s -> case s of
    []    -> Success () 0 []
    (c:_) -> Failure (UnexpectedCharacter c) 1

  satisfy f = Parser' $ \s -> case s of
    []     -> Failure EOF 0
    (c:cs) -> if f c
      then Success c 1 cs
      else Failure (UnexpectedCharacter c) 1

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

instance Parsing Parser where
  error = Parser . const . throwError

  eof = Parser $ \s ->
    case s of
      []    -> pure ([], ())
      (c:_) -> throwError $ UnexpectedCharacter c
  
  satisfy f = Parser $ \s ->
    case s of
      []     -> throwError EOF
      (c:cs) -> if f c
                  then pure (cs, c)
                  else throwError (UnexpectedCharacter c)
  
  peek = Parser $ \s ->
    case s of
      []    -> throwError EOF
      (c:_) -> pure (s, c)
  
  lookAhead p = Parser $ \s ->
    either throwError (suc s) $ runParser p s
    where
      suc s (_, a) = pure (s, a)
  
  notFollowedBy p = Parser $ \s ->
    either
      (const $ pure (s, ()))
      (const $ throwError UnexpectedValue)
      $ runParser p s
