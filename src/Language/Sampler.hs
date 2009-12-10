{-# LANGUAGE TemplateHaskell #-}

module Language.Sampler where

import Language.Syntax
import Language.Type
import Language.Inter

import Autolib.Util.Zufall
import Autolib.Util.Wort ( alle )

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Set

import Data.Typeable
import Data.List

data Sampler = 
     Sampler { language :: Language.Syntax.Type
             , num_samples :: Int -- ^ anzahl der samples
             , min_sample_length :: Int -- ^ minimale länge der samples
             , max_sample_length :: Int -- ^ maximal länge der samples
             }
    deriving ( Eq, Typeable )

$(derives [makeReader, makeToDoc] [''Sampler])

example = Sampler 
    { language = Lukas
	, num_samples = 50
	, min_sample_length = 4
	, max_sample_length = 40
    }

create :: Integral s
       => Sampler
       -> s -- ^ random seed
       -> Maybe Int -- ^ if present, create some words longer than this
       -> ( [String], [String] ) -- ^ (yeah, noh)
create i seed large = randomly ( fromIntegral seed ) $ do
     let l = inter $ language i
         w = min_sample_length i
         n = num_samples i
     -- die kleinen sollten ja auch schnell zu testen sein
     let klein = take 40 $ do 
            n <- [0 .. ]
            alle ( setToList $ alphabet l ) n
     -- watch argument ordering! -- TODO: use records instead
     here   <- samples      l n w
     there  <- anti_samples l n w
     let top = case large of
            Nothing -> max_sample_length i
            Just lrg -> 5 + max lrg ( max_sample_length i )
     farout <-      samples l 10 top
     return $ partition (contains l) 
               $ nub 
               $ filter ( \ w -> length w <= top )
               $ klein ++ here ++ there ++ farout



-- local variables:
-- mode: haskell
-- end:
