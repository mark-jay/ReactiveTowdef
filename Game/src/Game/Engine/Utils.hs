-----------------------------------------------------------------------------
--
-- Module      :  Game.Engine.Utils
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Game.Engine.Utils (
    setHotkey
) where

import Game.Engine.GlobState ( MainCoroutine )
import Game.Engine.Input

import Control.Arrow
import Control.Coroutine.FRP

-- key bindings utils
---------------------

setHotkey :: Key -> [Mod] -> IO () -> MainCoroutine
setHotkey key mods action = arr fst >>>
  withPrevious' >>> watch pressed >>> constE action
    where
      pressed :: ( Input, Input ) -> Bool
      pressed ( new, old ) =
         isKeyDown new key &&
         not ( isKeyDown old key ) && checkMods new mods


