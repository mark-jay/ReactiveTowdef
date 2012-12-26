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

import qualified Game.Engine.Input as Input
import qualified Game.Engine.GlobState as GS
import Game.Engine.Data ( MainCoroutine, Drawable, mainCoroutineToIO )

-- consts
---------

secPerTick :: Fractional a => a
secPerTick = 0.05

maxFrameTime :: Fractional a => a
maxFrameTime = 0.05

orthoW, orthoH :: GLdouble
(orthoW, orthoH) = (1000, 1000)

initWindowSize = Size 800 600

-- entry point
--------------

gameLoop :: Drawable d => MainCoroutine d -> IO ()
gameLoop coroutine = do
  getArgsAndInitialize >> createWindow "Game"
  state <- GS.initGlobState (mainCoroutineToIO coroutine)

  let inpRef = GS.getKB state
      redraw = renderViewport (swapBuffers >> postRedisplay Nothing) state
      kbmouseCallback k ks mods pos =
        Input.updateKeyboardMouse inpRef k ks mods pos >> redraw
      mouseMotionCallback pos = Input.updatePos inpRef pos >> redraw
      mouseCrossing crossing = Input.updateCrossing inpRef crossing >> redraw
      shapeCallback size = Input.updateShape inpRef size >> redraw

  -- callbacks
  keyboardMouseCallback  $= Just kbmouseCallback
  motionCallback         $= Just mouseMotionCallback
  passiveMotionCallback  $= Just mouseMotionCallback
  crossingCallback       $= Just mouseCrossing
  reshapeCallback        $= Just shapeCallback
  displayCallback        $= redraw

  -- Set up an orthogonal projection for 2D rendering
  matrixMode $= Projection
  loadIdentity
  ortho 0 orthoW orthoH 0 (-1) 1
  matrixMode $= Modelview 0
  loadIdentity

  initialWindowSize  $= initWindowSize
  initialDisplayMode $= [DoubleBuffered] -- now display callback will be called more often
  mainLoop

-- rendering
------------

renderViewport redisplayFn state = do
  (kb, prev, c) <- GS.unpack state
  current <- getPOSIXTime

  let delta  = (current - prev)

  if delta < secPerTick
      then return ()
      else do
        let (r', c') = runC c (kb, current)
        case c' of
            Just c' -> do
                writeIORef (GS.getCoroutine state) c'
                writeIORef (GS.getPrevCall state) current
                clear [ColorBuffer]
                sequence r'
                flush
            Nothing -> do
                exitWith ExitSuccess

  redisplayFn
