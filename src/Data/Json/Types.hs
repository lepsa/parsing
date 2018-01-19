{-# LANGUAGE NoImplicitPrelude #-}

module Data.Json.Types where

import           Data.Bool   (Bool)
import           Data.Eq     (Eq)
import           Data.Map    (Map)
import           Data.String (String)
import           Prelude     (Double)
import           Text.Show   (Show)

-- RFC7159-ish JSON
data JSON =
    JObject (Map String JSON)
  | JArray [JSON]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
  deriving (Show, Eq)

data Sign = Minus | Plus
