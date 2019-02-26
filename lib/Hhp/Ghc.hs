-- | The Happy Haskell Programming library.
--   API for interactive processes

module Hhp.Ghc (
  -- * Converting the Ghc monad to the IO monad
    withGHC
  , withGHC'
  -- * Initializing DynFlags
  , initializeFlagsWithCradle
  -- * Ghc utilities
  -- * Misc
  , getSystemLibDir
  ) where

import Hhp.Check
import Hhp.GHCApi
