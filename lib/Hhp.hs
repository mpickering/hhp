-- | The Happy Haskell Programming library.
--   API for commands.

module Hhp (
  -- * Cradle
    Cradle(..)
  , findCradle
  -- * Options
  , Options(..)
  , LineSeparator(..)
  , OutputStyle(..)
  , defaultOptions
  -- * 'IO' utilities
  , checkSyntax
  , expandTemplate
  , listLanguages
  , listFlags
  , debugInfo
  , rootInfo
  , module Hhp.HIE
  ) where

import Hhp.Check
import Hhp.Cradle
import Hhp.Debug
import Hhp.Flag
import Hhp.Lang
import Hhp.Types
import Hhp.HIE
