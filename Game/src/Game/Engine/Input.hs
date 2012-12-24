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

module Game.Engine.Input
    ( Keyboard
    , initKeyboard
    , isKeyDown
    , getMousePos
    , updateKeyboard
    , updatePos
    , Key(..)
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Graphics.UI.GLUT (Key(..), KeyState(..))
import qualified Graphics.UI.GLUT as GLUT
import Data.IORef

-- datatype and basic api
-------------------------

-- | Set of all keys that are currently held down
data Keyboard = Keyboard {
    getKeys  :: (Set Key)
  , getMods  :: GLUT.Modifiers
  , getPos   :: GLUT.Position
  }

-- | Create a new Keyboard
initKeyboard :: Keyboard
initKeyboard = Keyboard Set.empty mods (GLUT.Position 0 0)
  where mods = GLUT.Modifiers Down Down Down

mapKeys f (Keyboard keys mods pos) = (Keyboard (f keys) mods pos)
mapMods f (Keyboard keys mods pos) = (Keyboard keys (f mods) pos)
mapPos  f (Keyboard keys mods pos) = (Keyboard keys mods (f pos))

-- keyboard processing
----------------------

-- | Record a key state change in the given Keyboard
handleKeyEvent :: Key -> KeyState -> GLUT.Modifiers -> Keyboard -> Keyboard
handleKeyEvent k Down mods = setMods mods . addKey k
handleKeyEvent k Up   mods = setMods mods . removeKey k

setMods :: GLUT.Modifiers -> Keyboard -> Keyboard
setMods newMods = mapMods (const newMods)

addKey :: Key -> Keyboard -> Keyboard
addKey k = mapKeys (Set.insert k)

removeKey :: Key -> Keyboard -> Keyboard
removeKey k = mapKeys (Set.delete k)

-- | Test if a key is currently held down in the given Keyboard
isKeyDown :: Keyboard -> Key -> Bool
isKeyDown kb k = Set.member k $ getKeys kb

-- | Returns position of the mouse
getMousePos :: Keyboard -> GLUT.Position
getMousePos = getPos

-- IO stuff
-----------

-- | Update the Keyboard state according to the event
updateKeyboard :: IORef Keyboard -> GLUT.KeyboardMouseCallback
updateKeyboard kb key keyState mods _ =
   modifyIORef kb (handleKeyEvent key keyState mods)

updatePos :: IORef Keyboard -> GLUT.MotionCallback
updatePos kbRef pos = modifyIORef kbRef (mapPos (const pos))
