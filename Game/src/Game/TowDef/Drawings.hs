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

import qualified Game.Engine as E

-- plotting to the window
-------------------------
type Line = (Point, Point, ColorT)
type ColorT = Color3 GLfloat
type Point = (Coord, Coord)
type Coord = GLdouble

drawLines :: [Line] -> IO ()
drawLines = mapM_ drawLine

drawLine :: Line -> IO ()
drawLine (p1, p2, aColor) = renderPrimitive Lines actions
  where actions = color aColor >> drawVertex p1 >> drawVertex p2

drawVertex :: Point -> IO ()
drawVertex (x, y) = vertex $ Vertex3 x y 0

-- rect
-------

-- D for drawable
data RectD = RectC Point Point ColorT
           | RectT Point Point (Maybe TextureObject)
           deriving ( Show )

instance E.Drawable RectD where
  draw (RectC (x1, y1) (x2, y2) aColor) = renderPrimitive Quads actions
    where actions = color aColor >>
                    drawVertex (x1, y1) >> drawVertex (x2, y1) >>
                    drawVertex (x2, y2) >> drawVertex (x1, y2)
  draw (RectT (x1, y1) (x2, y2) texObj) = E.withTexture2d texObj $
                                          renderPrimitive Quads actions
    where actions = do
             let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
             texCoord2f (TexCoord2 0 0) >> drawVertex (x1, y1)
             texCoord2f (TexCoord2 0 1) >> drawVertex (x1, y2)
             texCoord2f (TexCoord2 1 1) >> drawVertex (x2, y2)
             texCoord2f (TexCoord2 1 0) >> drawVertex (x2, y1)


drawRect :: Point -> Point -> ColorT -> IO ()
drawRect p1 p2 c = E.draw $ RectC p1 p2 c
