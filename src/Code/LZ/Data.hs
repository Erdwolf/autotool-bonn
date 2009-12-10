{-# LANGUAGE TemplateHaskell #-}

module Code.LZ.Data where

import Code.Type ( BitSize (..), bits )

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size
import Autolib.Set
import Autolib.FiniteMap

import Data.Typeable

data Lempel_Ziv_Welch  = Lempel_Ziv_Welch  deriving ( Eq, Ord, Typeable )
data Lempel_Ziv_77 = Lempel_Ziv_77 deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Lempel_Ziv_Welch])
$(derives [makeReader, makeToDoc] [''Lempel_Ziv_77])

data Code_Letter a = Letter a
                   | Entry Int -- ^ num in dict
                   | Block { width :: Int, dist :: Int } 
                      -- ^ relative position in stream
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Code_Letter])

instance Size ( Code_Letter a ) where
    size _ = 1 -- not used

instance Ord a => BitSize [ Code_Letter a ] where
    bitSize xs = 
        let alpha = mkSet $ do Letter x <- xs ; return x
            weight ( Letter _ ) = 1 + bits ( cardinality alpha )
            weight ( Entry i )  = 1 + bits i
            weight ( Block { dist = d, width = w }) 
                                = 1 + bits d + bits w
        in  sum $ map weight xs

data ( ToDoc [a], Ord a, Reader [a] ) 
    => Book a = Book
          { short  :: Set a 
          , long :: FiniteMap [a] Int
          }
    deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Book])

leer :: ( ToDoc [a], Reader [a], Ord a ) => Book a
leer = Book { short = emptySet , long = emptyFM }

data ( ToDoc [a], ToDoc [b], Ord a, Reader [a], Reader [b] ) 
    => Cache a b = Cache
          { book :: Book a
          , output  :: [ b ] 
          }

$(derives [makeReader, makeToDoc] [''Cache])

blank ::  ( ToDoc [a], ToDoc [b], Ord a, Reader [a], Reader [b] ) 
      => Cache a b
blank = Cache { book = leer , output = [] }


-- Local variables:
-- mode: haskell
-- End:
