-----------------------------------------------------------------------------
--
-- Module      :  Game.Engine.Input
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
    ( Input
    , initInput
    , isKeyDown
    , getMousePos
    , updateKeyboardMouse
    , updatePos
    , Key(..)
    , Mod(..)
    , checkMods
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Graphics.UI.GLUT (Key(..), KeyState(..))
import qualified Graphics.UI.GLUT as GLUT
import Data.IORef

-- datatype and basic api
-------------------------

-- | Set of all keys that are currently held down
data Input = Input {
    getKeys  :: (Set Key)
  , getMods  :: GLUT.Modifiers
  , getPos   :: GLUT.Position
  }

-- | Create a new Input
initInput :: Input
initInput = Input Set.empty mods (GLUT.Position 0 0)
  where mods = GLUT.Modifiers Down Down Down

mapKeys f (Input keys mods pos) = (Input (f keys) mods pos)
mapMods f (Input keys mods pos) = (Input keys (f mods) pos)
mapPos  f (Input keys mods pos) = (Input keys mods (f pos))

-- Input processing
----------------------

-- | Record a key state change in the given Input
handleKeyEvent :: Key -> KeyState -> GLUT.Modifiers -> Input -> Input
handleKeyEvent k Down mods = setMods mods . addKey k
handleKeyEvent k Up   mods = setMods mods . removeKey k

addKey :: Key -> Input -> Input
addKey k = mapKeys (Set.insert k)

removeKey :: Key -> Input -> Input
removeKey k = mapKeys (Set.delete k)

-- | Test if a key is currently held down in the given Input
isKeyDown :: Input -> Key -> Bool
isKeyDown kb k = Set.member k $ getKeys kb

-- | Returns position of the mouse
getMousePos :: Input -> GLUT.Position
getMousePos = getPos

-- modifiers
------------

setMods :: GLUT.Modifiers -> Input -> Input
setMods newMods = mapMods (const newMods)

data Mod = Alt | Shift | Ctrl

checkMod :: Mod -> Input -> Bool
checkMod Alt   = (== Down) . GLUT.alt   . getMods
checkMod Shift = (== Down) . GLUT.shift . getMods
checkMod Ctrl  = (== Down) . GLUT.ctrl  . getMods

checkMods :: Input -> [Mod] -> Bool
checkMods input = all $ flip checkMod input

-- IO stuff
-----------

-- | Update the Input state according to the event
updateKeyboardMouse :: IORef Input -> GLUT.KeyboardMouseCallback
updateKeyboardMouse kb key keyState mods _ =
   modifyIORef kb (handleKeyEvent key keyState mods)

updatePos :: IORef Input -> GLUT.MotionCallback
updatePos kbRef pos = modifyIORef kbRef (mapPos (const pos))
