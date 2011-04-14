{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Graph.Weighted.Data where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set
import Autolib.Hash

import Data.Typeable

data ( Ord v ) => Kante v w = 
     Kante { von :: v, nach :: v, gewicht :: w }
     deriving ( Typeable )

instance ( Ord v, Hash v, Hash w ) => Hash ( Kante v w ) where
     hash k = hash ( von k, nach k, gewicht k )

-- | das Gewicht wird beim Vergleich der Kanten ignoriert.
-- Das ist eventuell keine gute Idee. 
-- Damit kann man z. B. keine Automaten darstellen,
-- dort kann es mehrere Kanten mit verschiedenem Gewicht (Labels)
-- zwischen zwei Knoten geben.
-- außerdem versteckt sich hier die Entscheidung,
-- ob Kanten gerichtet oder ungerichtet sind.
instance Ord v => Eq ( Kante v w ) where
    k == l = von k == von l && nach k == nach l    
instance Ord v => Ord ( Kante v w ) where
    compare k l = compare ( von k, nach k ) ( von l, nach l )

data ( Ord v ) => Graph v w = 
     Graph { knoten :: Set v
           , kanten :: Set ( Kante v w )
           }
    deriving ( Typeable, Eq )

instance (Ord v, Hash v, Hash w) => Hash ( Graph v w ) where
    hash g = hash ( knoten g, kanten g )



$(derives [makeReader, makeToDoc] [''Kante, ''Graph])