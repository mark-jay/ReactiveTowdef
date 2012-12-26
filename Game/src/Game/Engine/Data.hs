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

module Game.Engine.Data (
    Drawable(..)
  , MainCoroutine
  , MainCoroutineIO
  , mainCoroutineToIO
) where

import Control.Coroutine
import qualified Game.Engine.Input as Input
import Data.Time.Clock.POSIX

-- Drawable
-----------

class Drawable d where
    draw :: d -> IO ()

instance Drawable (IO a) where
    draw = (>> return ())

-- MainCoroutine
----------------

type MainCoroutine d = Coroutine (Input.Input, POSIXTime) [d]
type MainCoroutineIO = MainCoroutine (IO ())

mainCoroutineToIO :: Drawable d => MainCoroutine d -> MainCoroutineIO
mainCoroutineToIO = fmap (map draw)
