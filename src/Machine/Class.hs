module Machine.Class where

--   $Id$

import Autolib.Set
import Autolib.Schichten
import Autolib.Reporter hiding ( output )
import Autolib.ToDoc
import Autolib.Size

import Machine.History

class ( ToDoc m, Size m
      , ToDoc dat, Ord dat
      , ToDoc conf, Ord conf 
      , InOut m dat conf
      , Compute m conf
      , History conf
      ) => Machine m dat conf | m -> dat,  m -> conf

instance ( ToDoc m, Size m
      , ToDoc dat, Ord dat
      , ToDoc conf, Ord conf 
      , InOut m dat conf
      , Compute m conf
      , History conf
      ) => Machine m dat conf 

--------------------------------------------------------------------

class In m dat conf | m -> dat, m -> conf where -- strong dependencies ??
    -- | startkonf. herstellen (tupel von args)
    input_reporter  :: m -> dat -> Reporter conf
    input_reporter m dat = return $ input m dat
    -- | obsolete
    input  :: m -> dat -> conf
    input = error "Machine.Class.input method is obsolete"

class Ord conf => Compute m conf where
    -- | alle direkten nachfolger ( nichtdeterministisch )
    next   :: m -> conf -> Set conf 
    accepting  :: m -> conf -> Bool
    depth :: m -> conf -> Int

nachfolger :: Compute m conf
           => m -> conf -> [ conf ]
-- unendliche liste
nachfolger a k = concat $ map setToList $
    schichten (next  a) k


class Out m dat conf  | m -> dat, m -> conf where
    -- | endkonf. lesen (ein einziges arg)
    output :: m -> conf -> dat
    output = error "Machine.Class.output method is obsolete"
    output_reporter :: m -> conf -> Reporter dat
    output_reporter m conf = return $ output m conf

class ( In m dat conf, Out m dat conf )
      => InOut m dat conf   | m -> dat, m -> conf

instance ( In m dat conf, Out m dat conf )
      => InOut m dat conf

--------------------------------------------------------------------

class Encode dat where
    encode :: [ Integer ] -> dat

class Decode dat where
    decode :: dat -> Integer

class ( Encode dat, Decode dat ) => Numerical dat 

instance ( Encode dat, Decode dat ) => Numerical dat 


