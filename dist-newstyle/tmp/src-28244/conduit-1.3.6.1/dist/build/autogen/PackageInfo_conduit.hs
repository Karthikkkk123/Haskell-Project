{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_conduit (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "conduit"
version :: Version
version = Version [1,3,6,1] []

synopsis :: String
synopsis = "Streaming data processing library."
copyright :: String
copyright = ""
homepage :: String
homepage = "http://github.com/snoyberg/conduit"
