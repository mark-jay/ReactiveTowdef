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

import Data.Array.Storable
import Codec.Image.PNG

import Game.TowDef.Drawings ( drawRect )
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
      <++> E.setHotkey (Char '\27') [] (exitWith ExitSuccess)

main1 :: E.MainCoroutine
main1 = arr $ (\(inp, _) -> [do
      let (Position x y) = E.getMousePos inp
          v = fromIntegral (x `rem` 255) / 255.0
      drawRect ( 100, 100 ) ( 500, 500 ) ( Color3 v 0 0 )
      ])

-- drawing textures
-------------------

reminder = loadPNGFile "file" >>= f
  where f (Left error)     = return ()
        f (Right pngImage) = g $ imageData pngImage
        g storArr = withStorableArray storArr (\ptr -> return ())

