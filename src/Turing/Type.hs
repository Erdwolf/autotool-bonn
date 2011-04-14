{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, TypeSynonymInstances, ScopedTypeVariables #-}

module Turing.Type 

( module Turing.Type
, module Autolib.Set
, module Autolib.FiniteMap
)

where

--   $Id$

import Autolib.Set
import Autolib.Size
import Autolib.FiniteMap

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
import Autolib.Xml


data Bewegung = L | O | R
     deriving (Eq, Ord, Typeable)

$(derives [makeReader, makeToDoc] [''Bewegung])

data TM = TM -- for challenger instances
     deriving (Eq, Ord, Typeable)
     
$(derives [makeReader, makeToDoc] [''TM])

-- ohne methoden, soll nur die constraints aufsammeln
class ( Ord y
      , Show y, Show [y]
      , ToDoc y, ToDoc [y]
      , Reader y, Reader [y]  
      , Typeable y
      ) 
    => UM y
instance ( Ord y, Show y
	 , ToDoc y, ToDoc [y]
	 , Reader y, Reader [y]  
	 , Typeable y
	 )
    => UM y

class ( UM y, UM z ) => TuringC y z 
instance ( UM y, UM z ) => TuringC y z 


data TuringC y z => Turing y z = 
     Turing { eingabealphabet  :: Set y
	    , arbeitsalphabet  :: Set y
	    , leerzeichen      :: y
	    , zustandsmenge    :: Set z 
	    , tafel	       :: FiniteMap (y, z) (Set (y, z, Bewegung))
	    , startzustand     :: z
	    , endzustandsmenge :: Set z
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Turing])

instance TuringC y z => Size (Turing y z) where 
    size = length . unCollect' . tafel

instance Container (x, y, z) (x, (y, z)) where
    label _ = "Triple"
    pack (x,y,z) = (x,(y,z))
    unpack (x,(y,z)) = (x,y,z)

-- | specialized instances used for finite automata (testing)

instance  ( TuringC y z )
      => ToDoc (FiniteMap (y,z ) (Set (y,z,Bewegung))) where
    toDocPrec p fm = docParen (p >= fcp)
                   $ text "collect" <+> toDocPrec fcp (unCollect' fm)

instance  ( TuringC y z )
        => Reader (FiniteMap  (y,z ) (Set (y,z,Bewegung))) where
    atomic_readerPrec p = default_readerPrec p <|> do
        guard $ p < 9
        my_reserved "collect"
        xys <- reader :: Parser  [ (y, z, y, z, Bewegung) ]
        return $ collect' xys

-- | collect transition function from list of quintuples
collect' :: ( TuringC y z )
        =>  [ (y, z, y, z, Bewegung) ]
        -> FiniteMap (y,z ) (Set (y,z,Bewegung))
collect' pxqs = addListToFM_C union emptyFM $ do
    ( y, z, y', z', b ) <- pxqs
    return ( (y, z), unitSet (y', z', b) )

-- | represent transition function as list of quintuples
unCollect' :: TuringC y z
           => FiniteMap  (y,z ) (Set (y,z,Bewegung))
           ->  [ (y, z, y, z, Bewegung) ]
unCollect' fm = do
    ((y,z), yzbs ) <- fmToList fm
    ( y', z', b ) <- setToList yzbs
    return ( y, z, y', z', b ) 

-- local variables:
-- mode: haskell
-- end:


