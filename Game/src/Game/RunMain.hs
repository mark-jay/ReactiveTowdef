-----------------------------------------------------------------------------
--
-- Module      :  Game.RunMain
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Entry point for the game application
--
-----------------------------------------------------------------------------

module Game.RunMain (
    main
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GLUT
import Data.IORef
import qualified Data.Maybe as Maybe
import Control.Arrow
import System.Exit as System
import Data.Time.Clock.POSIX
import Prelude hiding ( log )

import Control.Coroutine

import Game.Engine.GameLoop ( gameLoop )
import qualified Game.Engine.Input as Input
import qualified Game.Engine.GlobState as GS

-- logging
import System.IO

log :: String -> IO ()
log = hPutStrLn stderr

-- tests
--------

main = main1

main2 = gameLoop $ Coroutine c
  where
    c (input, time) = (action input, Just $ Coroutine c)
    action input = do
      clear [ColorBuffer]
      let a = 400
      drawLines $ [((a, 100), (a, 500), Color3 1 0 0)]
      flush

main1 :: IO ()
main1 = gameLoop $ Coroutine c
  where
    c (input, time) = (action input, Just $ Coroutine c)
    lowPos (GLUT.Position x y) = y > 500
    a = 400
    action input | Input.isKeyDown input (Input.SpecialKey KeyF1) = do
      clear [ColorBuffer]
      drawLines $ [((a, 100), (a, 500), Color3 1 0 0)]
      flush
    action input | lowPos (Input.getMousePos input) = do
      clear [ColorBuffer]
      drawLines $ [((a, 200), (a, 400), Color3 1 0 0)]
      flush
    action _ = do
      clear [ColorBuffer]
      drawLines $ [((a, 250), (a, 350), Color3 1 0 0)]
      flush


------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- main = simpleMain

-- main stuff
-------------

-- a vector: (shiftx, shifty)
type Shift = (Coord, Coord)
type Coord = GLfloat
type ColorT = Color3 GLfloat
type Point = (Coord, Coord)
type Line = (Point, Point, ColorT)

-- plotting to the window
-------------------------
drawLines :: [Line] -> IO ()
drawLines = mapM_ drawLine

drawLine :: Line -> IO ()
drawLine (p1, p2, aColor) =  renderPrimitive Lines actions
  where actions = color aColor >> drawVertex p1 >> drawVertex p2

drawVertex :: Point -> IO ()
drawVertex (x, y) = vertex $ Vertex3 x y 0

-- entry points
---------------

-- adding value dependence
--------------------------

drawDescrLabel = do
  preservingMatrix $ do
    loadIdentity -- from scratch
    translate $ Vector3 (-1.0::GLfloat) 0.9 0
    let a = 0.0003 :: GLfloat
    scale a a a
    color $ Color3 0.01 0.8 (0.01::GLfloat)
    renderString Roman descrLabel

descrLabel = "Use arrows, +, -, F1-F4 to control and ALT+F4 to exit. Fonts are shitty, I know=)."

