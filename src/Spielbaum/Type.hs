module Spielbaum.Type where

-- definition of game-positions:
-- G = won  in this game-position
-- V = lost in this game-position
--
data GV = G | V  deriving (Show, Ord, Eq) 

-- definition of label in game-tree
-- label = name of the game-position
-- gv     = win/lose value of position
-- grundy = grundy-value of position
--
data SpielbaumLabel = SpielbaumLabel 
                    { label  :: String       
                    , gv     :: GV          
                    , grundy :: Int         
                    } deriving (Show, Ord, Eq)

