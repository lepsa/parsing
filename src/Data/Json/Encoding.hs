{-# LANGUAGE NoImplicitPrelude #-}

module Data.Json.Encoding where

import Data.String (String)
import Control.Monad ((>>=))
import Control.Applicative (pure)
import Data.Function (($))
import Text.Show (show)
import Data.Functor ((<$>))
import Data.Json.Types
import Data.Json.Internal
import Data.Bool (bool)
import Data.Semigroup ((<>))
import Data.List (intercalate)
import Data.Map (toList, lookup)
import Data.Maybe (fromMaybe)

escape :: String -> String
escape s = s >>= \c -> 
  fromMaybe (pure c) $ lookup c escapeSerialise

encode :: JSON -> String
encode j = case j of
  JNull     -> jNull
  JBool b   -> bool jFalse jTrue b
  JNumber n -> show n
  JString s -> escape s
  JArray a  -> [beginArrayChar]  <> intercalate [valueSeparatorChar] (encode <$> a) <> [endArrayChar]
  JObject o -> [beginObjectChar] <> intercalate [valueSeparatorChar] (encodePair <$> toList o) <> [endObjectChar]
  where
    encodePair :: (String, JSON) -> String
    encodePair (n, v) = [quoteChar] <> n <> [quoteChar, nameSeparatorChar] <> encode v
