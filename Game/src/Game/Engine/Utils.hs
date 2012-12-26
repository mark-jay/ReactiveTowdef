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

import Game.Engine.Data ( MainCoroutineIO )
import Game.Engine.Input

import Control.Arrow
import Control.Coroutine.FRP

-- logging
import System.IO

log :: String -> IO ()
log = hPutStrLn stderr

-- key bindings utils
---------------------

setHotkey :: Key -> [Mod] -> IO () -> MainCoroutineIO
setHotkey key mods action = arr fst >>>
  withPrevious' >>> watch pressed >>> constE action
    where
      pressed :: ( Input, Input ) -> Bool
      pressed ( new, old ) =
         isKeyDown new key &&
         not ( isKeyDown old key ) && checkMods new mods


