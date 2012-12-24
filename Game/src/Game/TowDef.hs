-----------------------------------------------------------------------------
--
-- Module      :  Game.TowDef
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

module Game.TowDef (
  mainCor
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

import qualified Game.Engine as Engine
import qualified Game.TowDef.Drawings as Drawings

-- logging
import System.IO

log :: String -> IO ()
log = hPutStrLn stderr

-- tests
--------

mainCor :: Engine.MainCoroutine
mainCor = main1

main2 :: Engine.MainCoroutine
main2 = Coroutine c
  where
    c (input, time) = (action input, Just $ Coroutine c)
    action input = do
      clear [ColorBuffer]
      let a = 400
      drawLines $ [((a, 100), (a, 500), Color3 1 0 0)]
      flush

main1 :: Engine.MainCoroutine
main1 = Coroutine c
  where
    c (input, time) = (action input, Just $ Coroutine c)
    lowPos (GLUT.Position x y) = y > 500
    a = 400
    action input | Engine.isKeyDown input (Engine.SpecialKey KeyF1) = do
      clear [ColorBuffer]
      drawLines $ [((a, 100), (a, 500), Color3 1 0 0)]
      flush
    action input | lowPos (Engine.getMousePos input) = do
      clear [ColorBuffer]
      drawLines $ [((a, 200), (a, 400), Color3 1 0 0)]
      flush
    action _ = do
      clear [ColorBuffer]
      drawLines $ [((a, 250), (a, 350), Color3 1 0 0)]
      flush

-- plotting to the window
-------------------------
type Line = (Point, Point, ColorT)
type ColorT = Color3 GLfloat
type Point = (Coord, Coord)
type Coord = GLfloat

drawLines :: [Line] -> IO ()
drawLines = mapM_ drawLine

drawLine :: Line -> IO ()
drawLine (p1, p2, aColor) =  renderPrimitive Lines actions
  where actions = color aColor >> drawVertex p1 >> drawVertex p2

drawVertex :: Point -> IO ()
drawVertex (x, y) = vertex $ Vertex3 x y 0

