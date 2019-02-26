-- | The Happy Haskell Programming library in low level.

module Hhp.Internal (
  -- * Types
  CompilerOptions(..)
  -- * IO
  , getDynamicFlags
  -- * Targets
  , setTargetFiles
  -- * Logging
  , withLogger
  , setNoWaringFlags
  , setAllWaringFlags
  ) where

import Hhp.GHCApi
import Hhp.Logger
import Hhp.Types
