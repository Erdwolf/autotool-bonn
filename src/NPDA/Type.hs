-- {-# LANGUAGE OverlappingInstances, DeriveDataTypeable, UndecidableInstances, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-} 
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, UndecidableInstances, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-} 
-- {-# LANGUAGE IncoherentInstances, DeriveDataTypeable, UndecidableInstances, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-} 

-- -*- mode: haskell -*-

module NPDA.Type

-- nicht-deterministischer Kellerautomat
--   $Id$

( module NPDA.Type
, module Autolib.Set
, module Autolib.FiniteMap
)

where

import Autolib.Set
import Autolib.FiniteMap
import Autolib.Schichten
import Autolib.Size

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Hash
import Data.Typeable

import Data.Maybe
import Autolib.Reporter

class ( Read z, Reader z , Reader [z], ToDoc z, ToDoc [z], Ord z 
      , Hash z
      )
      
    => RTO z
instance ( Read z, Reader z , Reader [z], ToDoc z, ToDoc [z], Ord z 
        , Hash z
	 )
    => RTO z

data RTO z => Modus z = Leerer_Keller | Zustand (Set z) 
    deriving ( Eq, Typeable )

instance RTO z => Hash ( Modus z ) where
    hash Leerer_Keller = 67
    hash ( Zustand xs ) = hash xs

$(derives [makeReader, makeToDoc] [''Modus])

class ( RTO x, RTO y, RTO z )
    => NPDAC x y z
instance ( RTO x, RTO y, RTO z )
    => NPDAC x y z

data NPDAC x y z => NPDA x y z = 
     NPDA { eingabealphabet  :: Set x 
	  , kelleralphabet   :: Set y 
	  , zustandsmenge    :: Set z 
	  , startzustand     :: z
	  , startsymbol	     :: y
	  , akzeptiert	     :: Modus z
	  , transitionen     :: FiniteMap (Maybe x, z, y) (Set (z, [y]))
	  }
     deriving ( Eq, Typeable )

instance ( NPDAC x y z ) => Hash ( NPDA x y z ) where
    hash a = hash [ hash $ startzustand a
                  , hash $ startsymbol a
                  , hash $ transitionen a
                  ]

instance Container (x, z, y) (x, (z, y)) where
    label _ = "Triple"
    pack (x, y, z) = (x, (y, z))
    unpack (x, (y, z)) = (x, y, z)

$(derives [makeReader, makeToDoc] [''NPDA])

instance NPDAC x y z => Size (NPDA x y z) where
    size a = length $ unCollect' $ transitionen a

------------------------------------------------------------------------

-- | specialized instances used for finite automata (testing)
instance  ( NPDAC x y z )
      => ToDoc (FiniteMap (Maybe x, z, y) (Set (z, [y]))) where
    toDocPrec p fm = docParen (p >= fcp)
                   $ text "collect" <+> toDocPrec fcp (unCollect' fm)

instance  ( NPDAC x y z )
        => Reader (FiniteMap (Maybe x, z, y) (Set (z, [y]))) where
    atomic_readerPrec p = default_readerPrec p <|> do
        guard $ p < 9
        my_reserved "collect"
        xys <- reader -- :: Parser  [ (Maybe x, z, y, z, [y]) ]
        return $ collect' xys

-- | collect transition function from list of quintuples
collect' :: ( NPDAC x y z )
        => [ (Maybe x, z, y, z, [y]) ] 
	-> FiniteMap (Maybe x, z, y) (Set (z, [y])) 
collect' pxqs = addListToFM_C union emptyFM $ do
    ( mx, z, y, z', ys ) <- pxqs
    return ( (mx, z, y), unitSet (z', ys) )

-- | represent transition function as list of quintuples
unCollect' :: NPDAC x y z
	   => FiniteMap (Maybe x, z, y) (Set (z, [y])) 
	   -> [ (Maybe x, z, y, z, [y]) ] 
unCollect' fm = do
    ( (mx, z, y), qs ) <- fmToList fm
    ( z', ys ) <- setToList qs
    return ( mx, z, y, z', ys )

------------------------------------------------------------------------

-- | mit absturzgefahr
the :: Maybe a -> a
the = fromMaybe ( error "the" )








