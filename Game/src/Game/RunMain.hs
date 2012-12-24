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

main :: IO ()
main = gameLoop $ Coroutine c
  where
    c (input, time) = (action input, Just $ Coroutine c)
    lowPos (GLUT.Position x y) = y > 500
    action input | Input.isKeyDown input (Input.SpecialKey KeyF1) = do
      clear [ColorBuffer]
      drawLines $ hypercubeToLines $ mkHypercube 4 6
      flush
    action input | lowPos (Input.getMousePos input) = do
      clear [ColorBuffer]
      drawLines $ hypercubeToLines $ mkHypercube 6 8
      flush
    action _ = do
      clear [ColorBuffer]
      drawLines $ hypercubeToLines $ mkHypercube 8 10
      log "Something Bad is about to happen."
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
type Hypercube = (Point, [(Shift, ColorT)])
type Line = (Point, Point, ColorT)

shiftPoint :: Shift -> Point -> Point
shiftPoint (shiftX, shiftY) (x, y) = (x + shiftX, y + shiftY)

shiftLine :: Shift -> Line -> Line
shiftLine shift line@(p1, p2, color) =
     (shiftPoint shift p1, shiftPoint shift p2, color)

hypercubeToLines :: Hypercube -> [Line]
hypercubeToLines (startingPoint, shifts) = snd $
  foldl f ([startingPoint], []) shifts
    where
      f :: ([Point], [Line]) -> (Shift, Color3 GLfloat) -> ([Point], [Line])
      f (points, lines) (shift, color) = (points ++ newPoints,
                                          lines ++ connectorLines ++ newLines)
        where newPoints = map (shiftPoint shift) points
              connectorLines = zipWith (\p1 p2 -> (p1, p2, color)) points newPoints
              newLines = map (shiftLine shift) lines

colors = let range = [0.2, 0.5, 0.7] in
         concat $ repeat [ Color3 r g b | r <- range, g <- range, b <- range ]

-- constructors
---------------

mkHypercube :: GLfloat -> GLfloat -> Hypercube
mkHypercube n heelFactor = (startingPoint, zip shifts colors)
  where startingPoint = (0,0)
        shifts = map (\k -> (sin(2*pi*k/heelFactor)/n,
                             cos(2*pi*k/heelFactor)/n))
                     [1..n]

-- plotting to the window
-------------------------

drawHypercube :: Hypercube -> IO ()
drawHypercube = drawLines . hypercubeToLines

drawLines :: [Line] -> IO ()
drawLines = mapM_ drawLine

drawLine :: Line -> IO ()
drawLine (p1, p2, aColor) =  renderPrimitive Lines actions
  where actions = color aColor >> drawVertex p1 >> drawVertex p2

drawVertex :: Point -> IO ()
drawVertex (x, y) = vertex $ Vertex3 x y 0

drawnHypercube = do
   clear [ColorBuffer]
   drawLines $ hypercubeToLines $ mkHypercube 4 6
   flush

-- entry points
---------------

genMain ioActions =
  getArgsAndInitialize >> createWindow "Hypercubes" >>
  ioActions >> mainLoop

simpleMain = genMain $ displayCallback $= drawnHypercube

dynamicMain = genMain $ do
  ref <- newIORef (4,10)
  displayCallback $= dynamicHypercube ref
  fullScreen
  keyboardMouseCallback $= Just (keyboardMouse ref)

-- adding value dependence
--------------------------

dynamicHypercube :: IORef (GLfloat, GLfloat) -> IO ()
dynamicHypercube ref = do
  clear [ColorBuffer]
  drawDescrLabel
  (n, heelFactor) <- readIORef ref
  drawHypercube $ mkHypercube n heelFactor
  flush

drawDescrLabel = do
  preservingMatrix $ do
    loadIdentity -- from scratch
    translate $ Vector3 (-1.0::GLfloat) 0.9 0
    let a = 0.0003 :: GLfloat
    scale a a a
    color $ Color3 0.01 0.8 (0.01::GLfloat)
    renderString Roman descrLabel

descrLabel = "Use arrows, +, -, F1-F4 to control and ALT+F4 to exit. Fonts are shitty, I know=)."

-- keyboard processing
----------------------

-- hypercube changing
onSpecialKey ref KeyF1  = modifyIORef ref $ first $ subtract 1
onSpecialKey ref KeyF2  = modifyIORef ref $ first (+1)
onSpecialKey ref KeyF3  = modifyIORef ref $ second $ subtract 1
onSpecialKey ref KeyF4  = modifyIORef ref $ second (+1)
-- moving
onSpecialKey ref KeyLeft  = translate $ Vector3 ( 0.1::GLfloat) 0 0
onSpecialKey ref KeyRight = translate $ Vector3 (-0.1::GLfloat) 0 0
onSpecialKey ref KeyUp    = translate $ Vector3 0 (-0.1::GLfloat) 0
onSpecialKey ref KeyDown  = translate $ Vector3 0 ( 0.1::GLfloat) 0
-- match all
onSpecialKey _ _ = return ()

-- scaling
onCharKey ref '-' = scale 0.8 0.8 (0.8 :: GLfloat)
onCharKey ref '+' = scale 1.25 1.25 (1.25 :: GLfloat)
-- match all
onCharKey _ _ = return ()

-- alt + F4
keyboardMouse ref (SpecialKey KeyF4) Down (Modifiers Up Up Down) _ =
  System.exitSuccess
-- special keys
keyboardMouse ref (SpecialKey sk) Down (Modifiers _ _ _) _ =
  onSpecialKey ref sk >> postRedisplay Nothing
-- char keys
keyboardMouse ref (Char char) Down (Modifiers _ _ _) _ =
  onCharKey ref char >> postRedisplay Nothing
-- match all
keyboardMouse _ _ _ _ _ = return ()



