module Spielbaum.Type where

import ToDoc

-- definition of game-positions:
-- G = won  in this game-position
--     player has a winning strategy
-- V = lost in this game-position
--     previous player has a winning strategy
data GV = G | V  deriving (Show, Eq, Ord) -- excluding Show because of ToDoc

-- definition of label in game-tree
-- label = name of the game-position
-- gv     = win/lose value of position
-- grundy = grundy-value of position
--
data SpielbaumLabel = SpielbaumLabel 
                    { label  :: String       
                    , gv     :: GV          
                    , grundy :: Int         
                    } deriving (Show, Eq, Ord) -- excluding Show because of ToDoc
{-
instance ToDoc (SpielbaumLabel) => ToDoc (SpielbaumLabel) where
    toDoc sL = text "Label " <+> toDoc (label sL) 
                             <+> toDoc (gv sL) 
                             <+> toDoc (grundy sL)

instance ToDoc (SpielbaumLabel) => Show (SpielbaumLabel) where
    show = render.toDoc

instance ToDoc (GV) => ToDoc (GV) where
    toDoc gv = gv

instance ToDoc (GV) => Show (GV) where
    show = render.toDoc
-}
{-
data Try a = Try { a1 :: String
               , b1 :: Int
               , c1 :: a 
               } deriving (Eq, Ord)

instance ToDoc a => ToDoc (Try a) where
    toDoc t = 

instance ToDoc (Try a) => ToDoc (Try a) where
    toDoc t = text "Try " <+> toDoc (a1 t) <+> toDoc (b1 t) <+> toDoc (c1 t)

instance (ToDoc a) => Show (Try a) where
    show = render . toDoc
-}
