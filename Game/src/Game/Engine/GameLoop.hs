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
  , orthoH
  , orthoW
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

gameLoop :: Drawable d => (GS.GlobConst -> MainCoroutine d) -> IO ()
gameLoop coroutine = do
  getArgsAndInitialize >> createWindow "Game"
  consts <- GS.initGlobConst
  state <- GS.initGlobState (mainCoroutineToIO (coroutine consts))

  let inpRef = GS.getKB state
      redraw = renderViewport (swapBuffers >> postRedisplay Nothing) state
      kbmouseCallback k ks mods pos =
        Input.updateKeyboardMouse inpRef k ks mods pos >> redraw
      mouseMotionCallback pos = Input.updatePos inpRef pos >> redraw
      mouseCrossing crossing = Input.updateCrossing inpRef crossing >> redraw
      shapeCallback size = reshape size >> Input.updateShape inpRef size >> redraw

  -- callbacks
  keyboardMouseCallback  $= Just kbmouseCallback
  motionCallback         $= Just mouseMotionCallback
  passiveMotionCallback  $= Just mouseMotionCallback
  crossingCallback       $= Just mouseCrossing
  reshapeCallback        $= Just shapeCallback
  displayCallback        $= redraw

  ortho 0 orthoW orthoH 0 (-1) 1
  reshape initWindowSize
  

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

-- reshape
----------

-- entry point callback
reshape = simpleReshape

simpleReshape :: Size -> IO ()
simpleReshape size = viewport $= ((Position 0 0), size)

squareReshape :: Size -> IO ()
squareReshape (Size w h) = setViewport >> setProjectionMatrix
  where
    setViewport = viewport $= ((Position 0 0), (Size w h))
    setProjectionMatrix = do
      let aspect = fromIntegral w / fromIntegral h
          (l, r, b, t, n, f) = (0, orthoW, orthoH, 0, (-1), 1)

      -- Set up an orthogonal projection for 2D rendering
      matrixMode $= Projection
      loadIdentity
      if aspect < 1.0
        then ortho l r (b * (1 / aspect)) t n f -- the last two don't matter?
        else ortho l (r * aspect) b t n f
      matrixMode $= Modelview 0
      loadIdentity


