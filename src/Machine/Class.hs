module Machine.Class where

-- $Id$

import Set
import Schichten
import ToDoc
import Size

import Machine.History

class ( ToDoc m, Size m
      , ToDoc dat, Ord dat
      , ToDoc conf, Ord conf 
      , InOut m dat conf
      , Compute m conf
      , History conf
      ) => Machine m dat conf

instance ( ToDoc m, Size m
      , ToDoc dat, Ord dat
      , ToDoc conf, Ord conf 
      , InOut m dat conf
      , Compute m conf
      , History conf
      ) => Machine m dat conf

class Numerical dat where
    -- berechnet eine mehrstellige zahlfunktion
    encode :: [ Integer ] -> dat
    decode :: dat -> Integer

class InOut m dat conf | m -> dat, m -> conf  where
    -- startkonf. herstellen (tupel von args)
    input  :: m -> dat -> conf
    -- endkonf. lesen (ein einziges arg)
    output :: m -> conf -> dat

class Ord conf => Compute m conf where
    -- alle direkten nachfolger ( nichtdeterministisch )
    next   :: m -> conf -> Set conf 
    -- 
    accepting  :: m -> conf -> Bool

nachfolger :: Compute m conf
           => m -> conf -> [ conf ]
-- unendliche liste
nachfolger a k = concat $ map setToList $
    schichten (next  a) k

