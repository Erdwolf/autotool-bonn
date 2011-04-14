{-# LANGUAGE DeriveDataTypeable #-}
module Sortier.Netz.Type

( Netz
, mkNetz, comps, low, high
, State, States
, Comp, Comps
)

where

import Sortier.Netz.Xml

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable

type Comp = (Int, Int)
type Comps = [ Comp ]

type State = [ Int ]
type States = [ State ]

data Netz = Netz
	  { low :: Int
	  , high :: Int
	  , comps :: Comps
	  }
    deriving ( Eq, Ord, Typeable )

mkNetz :: [(Int, Int)] -> Netz
mkNetz xys = 
    let all = do (x,y) <- xys ; [x,y]
    in Netz { low = minimum all, high = maximum all
	    , comps = xys
	    }

instance Size Netz where 
    size = length . comps

instance Reader Netz where
    atomic_readerPrec p = do
        guard $ p < 9
        my_reserved "mkNetz"
        xys <-  reader
        return $ mkNetz xys
instance ToDoc Netz where
    toDoc n = text "mkNetz" <+> toDoc (comps n)
