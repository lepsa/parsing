module Text.Parsing where

import Control.Applicative (Alternative, (<*>), (*>), (<*), many, (<*>), (<|>))
import Data.Function (const)

{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor m => Applicative m where
  pure :: a -> m a
  (<*>) :: m (a -> b) -> m a -> m b

class Applicative m => Alternative m where
  empty :: m a
  (<|>) :: m a -> m a -> m a
-}

class Alternative m => Parsing m where
  -- we can use all of these, because the constaints say
  -- that we will have them in scope for the type m
  -- fmap :: (a -> b) -> f a -> f b
  -- pure :: a -> m a
  -- (<*>) :: m (a -> b) -> m a -> m b
  -- empty :: m a
  -- (<|>) :: m a -> m a -> m a
  
  eof           :: m ()
  lookAhead     :: m a -> m a
  notFollowedBy :: m a -> m ()
  
  wrapped :: m a -> m b -> m b
  wrapped w p = w *> p <* w
  
  delimited :: m a -> m b -> m [b]
  delimited u p = emptyList <|> list
    where
      emptyList = const [] <$> notFollowedBy p
      list      = (:) <$> p <*> many (u *> p)
