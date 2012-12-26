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

data GlobState = GlobState {
    getKB        :: IORef Input.Input
  , getPrevCall  :: IORef POSIXTime
  , getCoroutine :: IORef MainCoroutineIO
  }

unpack :: GlobState -> IO (Input.Input, POSIXTime, MainCoroutineIO)
unpack state = do
  kb  <- readIORef (getKB state)
  pc  <- readIORef (getPrevCall state)
  cor <- readIORef (getCoroutine state)
  return (kb, pc, cor)

initGlobState :: MainCoroutineIO -> IO GlobState
initGlobState coroutine = do
  kbRef        <- newIORef Input.initInput
  prevCallRef  <- getPOSIXTime >>= newIORef
  coroutineRef <- newIORef coroutine
  return $ GlobState kbRef prevCallRef coroutineRef
