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
import qualified Control.Coroutine.FRP as FRP
import Control.Coroutine.FRP ((<++>))

import qualified Game.Engine as E
import qualified Game.TowDef.Drawings as Drawings

-- logging
import System.IO

log :: String -> IO ()
log = hPutStrLn stderr

-- tests
--------

mainCor :: E.MainCoroutine
mainCor = main1 <++> bindings

bindings :: E.MainCoroutine
bindings = E.setHotkey (SpecialKey KeyF4) [E.Alt] (exitWith ExitSuccess)

main1 :: E.MainCoroutine
main1 = arr $ const [do
      clear [ColorBuffer]
      let a = 500
      drawLines $ [((a, 100), (a, 900), Color3 1 0 0)]
      flush]

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
