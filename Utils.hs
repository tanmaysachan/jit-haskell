module Utils where

import Data.ByteString.Short as SBS
import Data.ByteString.UTF8 as BS

toSBS :: String -> SBS.ShortByteString
toSBS s = SBS.toShort $ BS.fromString s
