-----------------------------------------------------------------------------
--
-- Module      :  Game.Engine.GlobState
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

module Game.Engine.GlobState
  ( GlobState(..)
  , initGlobState
  , unpack
  , MainCoroutineIO
  , GlobConst(getTextures)
  , initGlobConst
  ) where

import Data.IORef
import Data.Time.Clock.POSIX

import Control.Coroutine
import qualified Game.Engine.Input as Input
import Game.Engine.Data ( MainCoroutineIO )

import qualified Graphics.UI.GLUT as GLUT
import Game.Engine.Textures ( getAndCreateTexturesAll, Textures )

data GlobState = GlobState {
    getKB        :: IORef Input.Input
  , getPrevCall  :: IORef POSIXTime
  , getCoroutine :: IORef MainCoroutineIO
  }

unpack :: GlobState -> IO (Input.Input, POSIXTime, MainCoroutineIO)
unpack state = do
  kb   <- readIORef $ getKB state
  pc   <- readIORef $ getPrevCall state
  cor  <- readIORef $ getCoroutine state
  return (kb, pc, cor)

initGlobState :: MainCoroutineIO -> IO GlobState
initGlobState coroutine = do
  kbRef        <- newIORef Input.initInput
  prevCallRef  <- getPOSIXTime >>= newIORef
  coroutineRef <- newIORef coroutine
  return $ GlobState kbRef prevCallRef coroutineRef

-- global consts - global immutable state
-----------------------------------------

data GlobConst = GlobConst {
    getTextures :: Textures
    } deriving ( Show )

initGlobConst :: IO GlobConst
initGlobConst = do
  textures <- getAndCreateTexturesAll "png"
  return $ GlobConst textures
