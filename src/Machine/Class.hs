module Machine.Class where

-- $Id$

import Set
import Schichten
import ToDoc
import Size

class ( ToDoc m, Size m
      , ToDoc dat, Ord dat
      , ToDoc conf, Ord conf 
      , InOut m dat conf
      , Compute m conf
      ) => Machine m dat conf

class InOut m dat conf | m dat -> conf  where
    -- startkonf. herstellen
    input  :: m -> dat -> conf
    -- endkonf. lesen
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

