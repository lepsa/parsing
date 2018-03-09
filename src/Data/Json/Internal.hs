module Data.Json.Internal where

import Data.Map
import Data.Tuple (swap)

jNull :: String
jNull = "null"

jTrue :: String
jTrue = "true"

jFalse :: String
jFalse = "false"

quoteChar :: Char
quoteChar = '"'

escapePrefix :: Char
escapePrefix = '\\'

mandatoryEscape :: [Char]
mandatoryEscape = ['"', '\\', '/']

escapeSerialise :: Map Char String
escapeSerialise = fromList $ 
  [ ('"',  escapePrefix : "\"")
  , ('\\', escapePrefix : "\\")
  , ('/',  escapePrefix : "/")
  , ('b',  escapePrefix : "b")
  , ('f',  escapePrefix : "f")
  , ('n',  escapePrefix : "n")
  , ('r',  escapePrefix : "r")
  , ('t',  escapePrefix : "t")
  ]

escapeMap' :: Map String Char
escapeMap' = fromList $ swap <$> toList escapeMap

beginArrayChar, endArrayChar, beginObjectChar, endObjectChar, nameSeparatorChar, valueSeparatorChar :: Char
beginArrayChar     = '['
endArrayChar       = ']'
beginObjectChar    = '{'
endObjectChar      = '}'
nameSeparatorChar  = ':'
valueSeparatorChar = ','
