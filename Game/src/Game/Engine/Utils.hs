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
{-# LANGUAGE Arrows #-}
module Game.Engine.Utils (
    setHotkey
  , setHotkey'
  , hotKeyEvents
  , withTexture2d
) where

import Graphics.UI.GLUT

import Game.Engine.Data ( MainCoroutineIO, MainCoroutine )
import Game.Engine.Input

import Control.Arrow
import Control.Coroutine.FRP

import qualified Game.Engine.Input as Input
import Data.Time.Clock.POSIX

import Game.Engine.Textures ( Textures )


-- logging
import System.IO

log :: String -> IO ()
log = hPutStrLn stderr

-- key bindings utils
---------------------

fst3 (x, _, _) = x

setHotkey :: Key -> [Mod] -> IO () -> MainCoroutineIO
setHotkey key mods action = setHotkey' key mods $ const action

setHotkey' :: Key -> [Mod]
           -> ((Input.Input, POSIXTime) -> IO ())
           -> MainCoroutineIO
setHotkey' key mods funAction = proc t -> do
  t' <- hotKeyEvents key mods -< t
  returnA -< map (const (funAction t)) t'

hotKeyEvents :: Key -> [Mod] -> MainCoroutine Input
hotKeyEvents key mods = proc arg@(inp,_) -> do
  events <- watch pressed <<< withPrevious' -< inp
  returnA -< map (const inp) events
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
