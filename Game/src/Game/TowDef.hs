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

main2 :: E.MainCoroutine
main2 = Coroutine c
  where
    c (input, time) = ([action input], Just $ Coroutine c)
    action input = do
      clear [ColorBuffer]
      let a = 400
      drawLines $ [((a, 100), (a, 500), Color3 1 0 0)]
      flush

main1 :: E.MainCoroutine
main1 = Coroutine c
  where
    c (input, time) = ([action input], Just $ Coroutine c)
    lowPos (GLUT.Position x y) = y > 500
    a = 400
    action input | E.isKeyDown input (E.SpecialKey KeyF1) = do
      clear [ColorBuffer]
      drawLines $ [((a, 100), (a, 500), Color3 1 0 0)]
      flush
    action input | lowPos (E.getMousePos input) = do
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

-- key bindings utils
---------------------

setHotkey :: E.Key -> [E.Mod] -> IO ()
          -> Coroutine (E.Input, POSIXTime) [IO ()]
setHotkey key mods action = arr fst >>>
  FRP.withPrevious' >>> FRP.watch pressed >>> FRP.constE action
    where
      pressed :: ( E.Input, E.Input ) -> Bool
      pressed ( new, old ) =
         E.isKeyDown new key &&
         not ( E.isKeyDown old key ) && E.checkMods new mods

bindings = setHotkey (SpecialKey KeyF4) [E.Alt] (exitWith ExitSuccess)
