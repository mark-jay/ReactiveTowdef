-----------------------------------------------------------------------------
--
-- Module      :  Game.Engine.Keyboard
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

module Game.Engine.Keyboard
    ( Keyboard
    , initKeyboard
    , isKeyDown
    , updateKeyboard
    , Key(..)
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Graphics.UI.GLUT (Key(..), KeyState(..))
import qualified Graphics.UI.GLUT as GLUT
import Data.IORef

-- | Set of all keys that are currently held down
newtype Keyboard = Keyboard (Set Key)

-- | Create a new Keyboard
initKeyboard :: Keyboard
initKeyboard = Keyboard Set.empty

-- | Record a key state change in the given Keyboard
handleKeyEvent :: Key -> KeyState -> Keyboard -> Keyboard
handleKeyEvent k Down = addKey k
handleKeyEvent k Up   = removeKey k

addKey :: Key -> Keyboard -> Keyboard
addKey k (Keyboard s) = Keyboard $ Set.insert k s

removeKey :: Key -> Keyboard -> Keyboard
removeKey k (Keyboard s) = Keyboard $ Set.delete k s

-- | Test if a key is currently held down in the given Keyboard
isKeyDown :: Keyboard -> Key -> Bool
isKeyDown (Keyboard s) k = Set.member k s

-- IO stuff
-----------

-- | Update the Keyboard state according to the event
updateKeyboard :: IORef Keyboard -> GLUT.KeyboardMouseCallback
updateKeyboard kb key keyState _ _ =
   modifyIORef kb (handleKeyEvent key keyState)

