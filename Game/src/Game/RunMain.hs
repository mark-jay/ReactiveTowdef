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

module Game.RunMain
  ( main
  ) where

import qualified Game.TowDef as TowDef
import qualified Game.Engine as E

main = do
  E.gameLoop TowDef.mainCor
