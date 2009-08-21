-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module PCProblem.Pick where

import Autolib.ToDoc
import Autolib.Reader

data Pick =  
     Pick { pair :: Int
	  , top  :: Bool
	  , letter :: Int
	  }
     deriving ( Eq, Ord )

$(derives [makeReader, makeToDoc] [''Pick])

instance Show Pick where show = render . toDoc
instance Read Pick where readsPrec = parsec_readsPrec
