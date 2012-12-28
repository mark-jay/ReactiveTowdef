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
  , withTexture2d
) where

import Graphics.UI.GLUT

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

fst3 (x, _, _) = x

setHotkey :: Key -> [Mod] -> IO () -> MainCoroutineIO
setHotkey key mods action = arr fst3 >>>
  withPrevious' >>> watch pressed >>> constE action
    where
      pressed :: ( Input, Input ) -> Bool
      pressed ( new, old ) =
         isKeyDown new key &&
         not ( isKeyDown old key ) && checkMods new mods

withTexture2d :: Maybe TextureObject -> IO a -> IO ()
withTexture2d texObj action = do
  texture Texture2D $= Enabled
  textureBinding Texture2D $= texObj
  action
  texture Texture2D $= Disabled
