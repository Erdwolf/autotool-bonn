-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, UndecidableInstances #-}

module Fun.Cache 

--   $Id$

( Cache -- abstrakt
, empty
, find
, insert
)

where

import Prelude hiding ( lookup )

import Autolib.FiniteMap

import Autolib.ToDoc hiding ( empty )
import Autolib.Reader

class (  Ord k
     , Reader (FiniteMap k e)
     , ToDoc (FiniteMap k e)
     ) => CacheC k e

instance (  Ord k
     , Reader (FiniteMap k e)
     , ToDoc (FiniteMap k e)
     ) => CacheC k e

data CacheC k e
	=> Cache k e = Cache ( FiniteMap k e )

$(derives [makeReader, makeToDoc] [''Cache])

empty :: CacheC k e => Cache k e
empty = Cache emptyFM

find :: CacheC k e => Cache k e -> k -> Maybe e
find (Cache c) k = lookupFM c k

insert :: CacheC k e => Cache k e -> k -> e -> Cache k e
insert (Cache c) k e = Cache (addToFM c k e)

