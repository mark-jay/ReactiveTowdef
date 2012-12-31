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
{-# LANGUAGE Arrows #-}
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
import qualified Game.TowDef.Field as Field
import Game.TowDef.Field ( Field )

-- tests
--------

mainCor :: E.GlobConst -> E.MainCoroutineIO
mainCor consts = E.mainCoroutineToIO (main3 (E.getTextures consts)) <++> bindings

bindings :: E.MainCoroutineIO
bindings = E.setHotkey (SpecialKey KeyF4) [E.Alt] (exitWith ExitSuccess)
      <++> E.setHotkey (Char '\27') [] (exitWith ExitSuccess)

main1 :: E.Textures -> E.MainCoroutine RectD
main1 _ = arr $ (\(inp, _) -> [
      let (Position x y) = fromMaybe (Position 0 0) $ E.getMousePos' inp
          v = fromIntegral (x `rem` 1000) / 1000.0
      in RectC ( 100, 100 ) ( 500, 500 ) ( Color3 v 0 0 )
      ])

main2 :: E.Textures -> E.MainCoroutineIO
main2 texts = arr $ (\(inp, _) -> [ do
      let (Position x y) = fromMaybe (Position 0 0) $ E.getMousePos' inp
          v = fromIntegral (x `rem` 1000) / 1000.0

      E.withTexture2d (Just $ snd $ texts !! 0) $ do
         E.draw $ RectT ( 500, 500 ) ( 700, 700 ) (Just $ snd $ texts !! 0)

      E.draw $ RectC ( 0, 0 ) ( 500, 500 ) ( Color3 v 0 0 )
      E.draw $ RectC ( 500, 500 ) ( 1000, 1000 ) ( Color3 v 0 0 )
      ])

fieldCor :: E.Textures -> E.MainCoroutineIO
fieldCor texts = arr (const (Field.mkField 10 10 (cycle $ map snd texts)))
        >>> arr (\f -> [E.draw f])

fieldCor' :: E.Textures -> E.MainCoroutine Field
fieldCor' texs = proc arg@(inp, _) -> do
    posE      <- FRP.mapE E.getMousePos' <<<
                 E.hotKeyEvents (MouseButton LeftButton) [] -< arg
    idxsE     <- FRP.mapE (uncurry Field.mousePosToIdx) <<<
                 arr (uncurry zip . second Maybe.catMaybes) -< ([field], posE)
    field     <- FRP.scanE (flip (flip Field.mapTextureAt (E.nextTexture texs)))
                           field -< idxsE :: FRP.Event (Int, Int)
    returnA -< [field]
  where field = Field.mkField 10 10 (cycle $ map snd texs)

main3 texts = fieldCor' texts

-- drawing textures
-------------------
