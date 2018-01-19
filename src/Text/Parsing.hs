module Text.Parsing where

import Control.Applicative (Alternative, (<*>), (*>), (<*), many, (<*>), (<|>))
import Data.Function (const)

class Alternative m => Parsing m where
  eof           :: m ()
  lookAhead     :: m a -> m a
  notFollowedBy :: m a -> m ()
  
  wrapped :: m () -> m a -> m a
  wrapped w p = w *> p <* w
  
  delimited :: m () -> m a -> m [a]
  delimited u p = emptyList <|> list
    where
      emptyList = const [] <$> notFollowedBy p
      list      = (:) <$> p <*> many (u *> p)
