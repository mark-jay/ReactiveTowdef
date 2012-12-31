-----------------------------------------------------------------------------
--
-- Module      :  Game.Engine.Data
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

module Game.Engine.Data
  ( Drawable(..)
  , MainCoroutine
  , MainCoroutineIO
  , MainCoroutineID
  , mainCoroutineToIO
  ) where

import Graphics.UI.GLUT ( TextureObject )

import Control.Monad ( mapM_ )

import Control.Coroutine
import qualified Game.Engine.Input as Input
import Data.Time.Clock.POSIX

import Game.Engine.Textures ( Textures )

-- Drawable
-----------

class Drawable d where
    draw :: d -> IO ()

instance Drawable (IO a) where
    draw = (>> return ())

instance Drawable a => Drawable [a] where
    draw = mapM_ draw

-- MainCoroutine
----------------

type MainCoroutine d = Coroutine (Input.Input, POSIXTime) [d]
type MainCoroutineIO = MainCoroutine (IO ())
type MainCoroutineID = Coroutine (Input.Input, POSIXTime)
                                 (Input.Input, POSIXTime)

mainCoroutineToIO :: Drawable d => MainCoroutine d -> MainCoroutineIO
mainCoroutineToIO = fmap (map draw)

