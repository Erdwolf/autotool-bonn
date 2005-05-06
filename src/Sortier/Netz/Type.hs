module Sortier.Netz.Type where

--   $Id$

import Sortier.Netz.Xml

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

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

instance Haskell2Xml Netz where
    toContents s = toContents $ XmlNetz $ comps s
    fromContents cs =
        let ( XmlNetz x, rest ) = fromContents cs
        in  ( mkNetz x, rest )
    toHType (_ :: Netz) = toHType (undefined :: XmlNetz) -- ??




