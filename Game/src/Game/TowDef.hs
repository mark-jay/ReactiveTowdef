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
import Data.Maybe ( fromMaybe )
import qualified Data.Maybe as Maybe
import Control.Arrow
import Control.Monad
import System.Exit as System
import Data.Time.Clock.POSIX
import Prelude hiding ( log )

import Control.Coroutine
import qualified Control.Coroutine.FRP as FRP
import Control.Coroutine.FRP ((<++>))

import qualified Game.Engine as E

import Data.Array.Storable
import Codec.Image.PNG

import Game.TowDef.Drawings ( drawRect, RectD(..) )

-- tests
--------

mainCor :: E.MainCoroutineIO
mainCor = E.mainCoroutineToIO main2 <++> bindings

bindings :: E.MainCoroutineIO
bindings = E.setHotkey (SpecialKey KeyF4) [E.Alt] (exitWith ExitSuccess)
      <++> E.setHotkey (Char '\27') [] (exitWith ExitSuccess)

main1 :: E.MainCoroutine RectD
main1 = arr $ (\(inp, _, _) -> [
      let (Position x y) = fromMaybe (Position 0 0) $ E.getMousePos' inp
          v = fromIntegral (x `rem` 1000) / 1000.0
      in RectC ( 100, 100 ) ( 500, 500 ) ( Color3 v 0 0 )
      ])

main2 :: E.MainCoroutineIO
main2 = arr $ (\(inp, _, texts) -> [ do
      let (Position x y) = fromMaybe (Position 0 0) $ E.getMousePos' inp
          v = fromIntegral (x `rem` 1000) / 1000.0

      E.withTexture2d (Just $ snd $ texts !! 0) $ do
         E.draw $ RectT ( 500, 500 ) ( 700, 700 ) (Just $ snd $ texts !! 0)

      E.draw $ RectC ( 100, 100 ) ( 500, 500 ) ( Color3 v 0 0 )
      ])

-- drawing textures
-------------------

reminder = loadPNGFile "file" >>= f
  where f (Left error)     = return ()
        f (Right pngImage) = g $ imageData pngImage
        g storArr = withStorableArray storArr (\ptr -> return ())

