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
import qualified Game.Engine.Input as KB

type MainCoroutine = Coroutine (KB.Keyboard, POSIXTime) (IO ())

data GlobState = GlobState {
    getKB        :: IORef KB.Keyboard
  , getPrevCall  :: IORef POSIXTime
  , getCoroutine :: IORef MainCoroutine}


unpack :: GlobState -> IO (KB.Keyboard, POSIXTime, MainCoroutine)
unpack state = do
  kb  <- readIORef (getKB state)
  pc  <- readIORef (getPrevCall state)
  cor <- readIORef (getCoroutine state)
  return (kb, pc, cor)

initGlobState :: MainCoroutine -> IO GlobState
initGlobState coroutine = do
  kbRef        <- newIORef KB.initKeyboard
  prevCallRef  <- getPOSIXTime >>= newIORef
  coroutineRef <- newIORef coroutine
  return $ GlobState kbRef prevCallRef coroutineRef
