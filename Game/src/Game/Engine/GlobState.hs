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

module Game.Engine.GlobState (
  GlobState(..),
  initGlobState,
  unpack,
  MainCoroutineIO
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
  , getTextures  :: Textures
  }

unpack :: GlobState -> IO (Input.Input, POSIXTime, MainCoroutineIO, Textures)
unpack state = do
  kb   <- readIORef $ getKB state
  pc   <- readIORef $ getPrevCall state
  cor  <- readIORef $ getCoroutine state
  let texs = getTextures state
  return (kb, pc, cor, texs)

initGlobState :: MainCoroutineIO -> IO GlobState
initGlobState coroutine = do
  kbRef        <- newIORef Input.initInput
  prevCallRef  <- getPOSIXTime >>= newIORef
  coroutineRef <- newIORef coroutine
  textures     <- getAndCreateTexturesAll "png"
  return $ GlobState kbRef prevCallRef coroutineRef textures
