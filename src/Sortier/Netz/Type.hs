module Sortier.Netz.Type where

-- -- $Id$

import ToDoc
import Reader
import Size


type Comp = (Int, Int)
type Comps = [ Comp ]

type State = [ Int ]
type States = [ State ]

data Netz = Netz
	  { low :: Int
	  , high :: Int
	  , comps :: Comps
	  }
    deriving ( Eq, Ord )

mkNetz :: [(Int, Int)] -> Netz
mkNetz xys = 
    let all = do (x,y) <- xys ; [x,y]
    in Netz { low = minimum all, high = maximum all
	    , comps = xys
	    }

instance Size Netz where 
    size = length . comps

instance Reader Netz where
    reader = do
        my_reserved "mkNetz"
        xys <-  reader
        return $ mkNetz xys

instance Read Netz where
    readsPrec = parsec_readsPrec

instance ToDoc Netz where
    toDoc n = text "mkNetz" <+> toDoc (comps n)

instance Show Netz where
    show = render . toDoc





