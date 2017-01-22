{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Tail2Futhark.Prelude ( futharkPrelude ) where

import Data.FileEmbed

-- | A prelude prefixed to every generated Futhark program.
futharkPrelude :: String
futharkPrelude = $(embedStringFile "lib/prelude.fut")
