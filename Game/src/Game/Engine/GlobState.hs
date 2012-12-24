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
  MainCoroutine
) where

import Data.IORef
import Data.Time.Clock.POSIX

import Control.Coroutine
import qualified Game.Engine.Input as Input

type MainCoroutine = Coroutine (Input.Input, POSIXTime) [IO ()]

data GlobState = GlobState {
    getKB        :: IORef Input.Input
  , getPrevCall  :: IORef POSIXTime
  , getCoroutine :: IORef MainCoroutine
  }

unpack :: GlobState -> IO (Input.Input, POSIXTime, MainCoroutine)
unpack state = do
  kb  <- readIORef (getKB state)
  pc  <- readIORef (getPrevCall state)
  cor <- readIORef (getCoroutine state)
  return (kb, pc, cor)

initGlobState :: MainCoroutine -> IO GlobState
initGlobState coroutine = do
  kbRef        <- newIORef Input.initInput
  prevCallRef  <- getPOSIXTime >>= newIORef
  coroutineRef <- newIORef coroutine
  return $ GlobState kbRef prevCallRef coroutineRef
