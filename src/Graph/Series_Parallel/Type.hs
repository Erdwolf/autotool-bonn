{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell #-}

module Graph.Series_Parallel.Type where

import Autolib.Graph.Type
import Autolib.Graph.Basic
import Autolib.Reader
import Autolib.ToDoc
import Data.Autolib.Transport
import Autolib.Hash
import Data.Typeable

data GraphC a => STGraph a = STGraph
               { source :: a
               , target :: a
               , contents :: Graph a
               }
    deriving ( Typeable 
	     -- , Ord, Eq 
	     )

$(derives [makeReader, makeToDoc] [''STGraph])

example :: GraphC Int => STGraph Int
example = STGraph { source = 1, target = 3
                  , contents = circle [ 1 .. 5 ]
                  }

data Threeway a b = This a | That b | Both Int
   deriving ( Typeable , Ord, Eq )

$(derives [makeReader, makeToDoc, makeToTransport] [''Threeway])

instance ( Hash a, Hash b ) => Hash ( Threeway a b ) where
    hash ( This x ) = hash ( 15 :: Int , x )
    hash ( That y ) = hash ( 11 :: Int , y )
    hash ( Both i ) = hash ( 22 :: Int , i )

-- local variables:
-- mode: haskell
-- end:

