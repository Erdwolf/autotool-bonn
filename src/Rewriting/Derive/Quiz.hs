{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-incoherent-instances #-}

module Rewriting.Derive.Quiz where

import Rewriting.TRS
import Rewriting.Step
import Rewriting.Steps 
import Rewriting.Derive.Instance
import qualified Rewriting.Roller as R
import Rewriting.Derive.Config

import Autolib.TES.OC
import Autolib.TES.Apply

import Autolib.Size
import Autolib.Reader
import Autolib.Set
import Autolib.Reader
import Autolib.FiniteMap
import Autolib.Util.Zufall

import System.Random
import Control.Monad ( guard ) 
import Data.Maybe


roll :: ( Symbol c, Symbol v, Reader ( TRS v c ) )
     => Config v c 
     -> IO ( Instance v c )
roll conf = do
    Just (trs, l, r) <- ( do
        ts <- sequence $ do
                 v <- [ 0 .. 20 ]
                 return $ do
                     t <- R.roll_ground_term 
                          ( R.mksig $ R.signature $ system_roller conf ) 
                          2
                     return ( v, t )
        let sub = listToFM ts
        trs <- R.roll_trs $ system_roller conf
        let pairs = take 100 
                  $ takeWhile ( \ p -> size ( rhs p ) < max_size conf )
                  $ Autolib.TES.OC.ocs $ Rewriting.TRS.pack trs
        let ls = map ( apply sub ) $ map lhs pairs
        let inds d = mkSet $ map rule_number $ actions d
        let rss = take 20
                       $ filter ( (1 <) . cardinality . inds ) 
                       $ filter ( ( < max_size conf) . size . start )
                       $ filter ( ( < max_size conf) . size . goal )
                       $ concat
                       $ map setToList 
                       $ take ( max_steps conf )
                       $ drop ( min_steps conf )
                       $ reachables trs ls
        case rss of
                   [] -> return Nothing
                   _ -> do
                       d <- eins rss
                       return $ Just (trs, start d, goal d) 
      ) `repeat_until` \ x -> isJust x 

    return $ Instance 
           { system = trs
           , from =  l
           , to   =  r
           }
