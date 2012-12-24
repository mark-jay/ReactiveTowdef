-----------------------------------------------------------------------------
--
-- Module      :  Game.Engine.GameLoop
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

module Game.Engine.GameLoop (
    gameLoop
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit as System
import Data.Time.Clock.POSIX
import Data.IORef

import Control.Coroutine

import qualified Game.Engine.Keyboard as KB
import qualified Game.Engine.GlobState as GS

-- consts
---------

secPerTick :: Fractional a => a
secPerTick = 0.05

maxFrameTime :: Fractional a => a
maxFrameTime = 0.05

-- entry point
--------------

gameLoop :: GS.MainCoroutine -> IO ()
gameLoop coroutine = do
  getArgsAndInitialize >> createWindow "Game"
  state <- GS.initGlobState coroutine

  let redraw = renderViewport (postRedisplay Nothing) state
      kbmouseCallback k ks mods pos =
        KB.updateKeyboard (GS.getKB state) k ks mods pos >> redraw
  keyboardMouseCallback $= Just kbmouseCallback
  displayCallback $= redraw
  mainLoop

-- rendering
------------

renderViewport redisplayFn state = do
  (kb, prev, c) <- GS.unpack state
  current <- getPOSIXTime

  let delta  = (current - prev)

  if delta < secPerTick
      then redisplayFn
      else do
        let (r', c') = runC c (kb, current)
        case c' of
            Just c' -> do
                writeIORef (GS.getCoroutine state) c'
                writeIORef (GS.getPrevCall state) current
                r'
            Nothing -> do
                exitWith ExitSuccess

  return ()
