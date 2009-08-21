{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-incoherent-instances  #-}
{-# LANGUAGE TemplateHaskell #-}

module PL.Struktur where

import PL.Signatur
import PL.Util

import Autolib.TES.Identifier
import Autolib.FiniteMap
import Autolib.Size
import Autolib.Set
import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter
import Data.Typeable

data Predicate u = Predicate { unPredicate :: Set [u] }
     deriving ( Eq, Ord, Typeable )

instance Size ( Predicate u ) where
    size ( Predicate s ) = cardinality s

instance ToDoc u => ToDoc ( Predicate u ) where
    toDoc ( Predicate ts ) = as_set ts

data Tuple a = Tuple [a] 
instance ToDoc a => ToDoc ( Tuple a ) where
    toDoc  ( Tuple xs ) = parens $ Autolib.ToDoc.sepBy comma $ map toDoc xs

as_set ts = braces $ Autolib.ToDoc.sepBy comma $ do
        t <- setToList $ ts
	return $ toDoc $ Tuple t

instance ( Ord u, Reader u ) => Reader ( Predicate u ) where
    reader = fmap Predicate from_set

data Function  u = Function { unFunction :: FiniteMap [u] u }
     deriving ( Eq, Ord, Typeable )

instance Size ( Function u ) where
    size ( Function f ) = sizeFM f

instance ( Ord u, ToDoc u ) => ToDoc ( Function u ) where
    toDoc ( Function fm ) = as_set $ mkSet $ do
        ( xs, y ) <- fmToList fm
	return $ xs ++ [y]

instance ( Ord u, Reader u ) => Reader ( Function u ) where
    reader = do
       tuples <- from_set
       return $ Function $ listToFM $ do
           tu <- setToList $ tuples
	   return ( init tu, last tu )

from_set :: ( Ord u, Reader u ) => Parser ( Set [u] )
from_set = fmap mkSet
	 $ my_braces $ ( `Autolib.Reader.sepBy` my_comma ) 
	 $ tuple

tuple :: Reader u => Parser [u]
tuple =  do my_parens $ ( `Autolib.Reader.sepBy` my_comma ) $ reader
     <|> do x <- reader ; return [x]

instance Reader u => Reader ( Tuple u ) where
    reader = fmap Tuple tuple


data Ord u => Struktur u = 
     Struktur { universum  :: Set u
	      , predicates :: FiniteMap Identifier ( Predicate u ) 
	      , functions  :: FiniteMap Identifier ( Function  u )
	      }
     deriving ( Eq, Ord, Typeable )

instance Ord u => Size ( Struktur u ) where
    size s = cardinality ( universum s )
	   + sum ( do ( r, rr ) <- fmToList $ predicates s ; return $ size rr )
	   + sum ( do ( f, ff ) <- fmToList $ functions  s ; return $ size ff )


instance ( ToDoc u, Ord u ) => Signed ( Struktur u ) where
     check sig s = do
          let dom = universum s
	  checkit "Funktionssymbol" 
		  ( keysFM . unFunction ) ( funktionen sig ) ( functions s ) dom
	  checkit "Relationssymbol" 
		  ( setToList . unPredicate ) ( relationen sig ) ( predicates s ) dom

checkit tag get_args arities values dom = do
	required_symbols_are_present tag get_args arities values
	no_additional_symbols tag arities values
	check_domain_for tag dom values        

check_domain_for tag dom values = sequence_ $ do
    ( k, v ) <- fmToList values
    let msg = vcat
	    [ text "Interpretation für" <+> text tag <+> toDoc k
	    ,  text "verwendet Elemente außerhalb des Universums:" 
	    ] 
    return $ check_domain msg dom v

class Check_Domain con where 
    check_domain :: ( Ord u, ToDoc u )
		  => Doc -> Set u -> con u 
		  -> Reporter ()

instance Check_Domain Function where
    check_domain tag dom fun = sequence_ $ do
        ( ks, v ) <- fmToList $ unFunction fun
	let ok = all ( `elementOf` dom ) $ v : ks
	return $ when ( not ok ) $ reject $ vcat
	     [ tag
	     , nest 4 $ toDoc ( Tuple $ ks ++ [v] )	       
	     ]

instance Check_Domain Predicate where
    check_domain tag dom pred = sequence_ $ do
        ks <- setToList $ unPredicate pred
	let ok = all ( `elementOf` dom ) ks
	return $ when ( not ok ) $ reject $ vcat
	     [ tag
	     , nest 4 $ toDoc ( Tuple ks )
	     ]



no_additional_symbols tag arities values = sequence_ $ do
    ( f, _ ) <- fmToList values
    return $ case lookupFM arities f of
        Just _ -> return ()
	Nothing -> reject $ hsep [ text tag, toDoc f, text "ist nicht in Signatur" ]
    

required_symbols_are_present tag get_args arities values = sequence_ $ do
    ( f, arity ) <- fmToList $ arities
    return $ do
        this <- find_or_complain tag values f
        sequence_ $ do
            arg <- get_args this
            return $ do
                when ( length arg /= arity ) $ reject $ vcat
                     [ text "Interpretation für" <+> text tag <+> toDoc f
                     , text "Falsche Stelligkeit für" <+> toDoc arg
                     ]
	      
empty :: Ord u
      => Signatur 
      -> Set u 
      -> Struktur u
empty sig uni = 
    Struktur { universum = uni
	     , predicates = listToFM $ do
	           ( p, a ) <- fmToList $ relationen sig
		   return ( p, Predicate emptySet )
	     , functions = listToFM $ do
	           ( f, a ) <- fmToList $ funktionen sig
		   return ( f, Function emptyFM )
	     }

	    
$(derives [makeReader, makeToDoc] [''Struktur])

-- local variables:
-- mode: haskell
-- end:

