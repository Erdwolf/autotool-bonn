{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts #-}

module Code.LZ.Book where

import Code.LZ.Data

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size

import Autolib.Set
import Autolib.FiniteMap

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

lookup :: ( ToDoc [a], Reader [a], Ord a ) 
       => Book a -> [a] -> Maybe ( Code_Letter a )
lookup b [x] = do
    guard $ x `elementOf`  short b
    return $ Letter x
lookup b w   = do
    i <- lookupFM ( long b ) w
    return $ Entry i

insert :: ( ToDoc [a], Reader [a], Ord a ) 
       => Book a -> [a] -> Book a
insert b [x] = b { short = short b `union` unitSet x }
insert b w   = b { long  = addToFM (long b) w (sizeFM $ long b) }
       
-- Local variables:
-- mode: haskell
-- End:
