-----------------------------------------------------------------------------
--
-- Module      :  Game.RunMain
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Entry point for the game application
--
-----------------------------------------------------------------------------

module Game.RunMain (
    main
) where

import Game.TowDef as TowDef
import Game.Engine as Engine

main = Engine.gameLoop TowDef.mainCor
