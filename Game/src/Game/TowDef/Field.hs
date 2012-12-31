-----------------------------------------------------------------------------
--
-- Module      :  Game.TowDef.Field
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

module Game.TowDef.Field (
    mkSimpleField
  , mkField
  , mousePosToIdx
  , mapTextureAt
  , Field(..)
) where

import Graphics.UI.GLUT ( TextureObject )
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT ( GLdouble )
import Data.Array ( listArray, Array(..), bounds, range, (!), indices, (//) )
import qualified Game.Engine as E
import Game.TowDef.Drawings ( RectD(..) )

-- datatype, constructors and mappers
-------------------------------------

data Field = Field  {
  getArr :: (Array (Int, Int) TextureObject)
  } deriving ( Show )

mapArr f (Field arr) = Field (f arr)

mkSimpleField :: Int -> Int -> TextureObject -> Field
mkSimpleField n m to = mkField n m $ repeat to

mkField :: Int -> Int -> [TextureObject] -> Field
mkField n m tos = Field $ listArray (( 0, 0 ), ( n-1, m-1 )) tos

-- instances and base API
-------------------------

-- getSize :: Array (Int, Int) e -> (Int, Int)
getSize = f . bounds
  where f ( (x0, y0), (x1, y1) ) = ( x1-x0+1, y1-y0+1 )

-- idxToSquare :: Array (GLdouble, GLdouble) e -> (GLdouble, GLdouble) -> ((GLdouble, GLdouble), (GLdouble, GLdouble))
idxToSquare arr (x, y) = ( ( fromIntegral x * w, fromIntegral y * h),
                         ( (fromIntegral x + 1) * w, (fromIntegral y + 1) * h ) )
  where
    ( w, h )        = ( E.orthoW / fromIntegral n , E.orthoH / fromIntegral m )
    ( n, m )        = ( getSize arr )

instance E.Drawable Field where
    draw field = mapM_ (E.draw . toRect) $ allIdx arr
      where arr             = getArr field
            allIdx          = range . bounds
            getTex (x, y)   = Just $ arr ! ( fromIntegral x, fromIntegral y )
            toRect ( x, y ) = RectT p1 p2 ( getTex (x, y) )
               where (p1, p2) = idxToSquare arr (x, y)

mousePosToIdx :: Field -> GLUT.Position -> (Int, Int)
mousePosToIdx field (GLUT.Position x y) = head [(x', y') | (x', y') <- indices (getArr field),
                                            (x, y) `inside` getBox (x', y')]
  where (x, y) `inside` ((x1, y1), (x2, y2)) = (x `inside'` (x1,x2)) && (y `inside'` (y1,y2))
        x `inside'` (x1, x2) = x1 <= fromIntegral x  && fromIntegral x <= x2
        getBox = idxToSquare $ getArr field

-- changing properties of the datatype
--------------------------------------

mapTextureAt :: (Int, Int) -> (TextureObject -> TextureObject)
              -> Field -> Field
mapTextureAt place f = mapArr g
  where g arr = arr // [(place, f (arr ! place))]
