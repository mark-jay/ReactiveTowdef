-----------------------------------------------------------------------------
--
-- Module      :  Game.TowDef.Drawings
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

module Game.TowDef.Drawings (
    drawRect
  , RectD(..)
) where

import Graphics.Rendering.OpenGL

import Game.Engine.Data

-- plotting to the window
-------------------------
type Line = (Point, Point, ColorT)
type ColorT = Color3 GLfloat
type Point = (Coord, Coord)
type Coord = GLfloat

drawLines :: [Line] -> IO ()
drawLines = mapM_ drawLine

drawLine :: Line -> IO ()
drawLine (p1, p2, aColor) = renderPrimitive Lines actions
  where actions = color aColor >> drawVertex p1 >> drawVertex p2

drawVertex :: Point -> IO ()
drawVertex (x, y) = vertex $ Vertex3 x y 0

-- rect
-------

data RectD = RectD Point Point ColorT

instance Drawable RectD where
  draw (RectD (x1, y1) (x2, y2) aColor) = renderPrimitive Quads actions
    where actions = color aColor >>
                    drawVertex (x1, y1) >> drawVertex (x2, y1) >>
                    drawVertex (x2, y2) >> drawVertex (x1, y2)


drawRect :: Point -> Point -> ColorT -> IO ()
drawRect (x1, y1) (x2, y2) aColor = renderPrimitive Quads actions
  where actions = color aColor >>
                  drawVertex (x1, y1) >> drawVertex (x2, y1) >>
                  drawVertex (x2, y2) >> drawVertex (x1, y2)
