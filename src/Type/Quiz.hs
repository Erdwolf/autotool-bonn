{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances #-}
module Type.Quiz where

--  $Id$

import Type.Data
import Type.Check

import Autolib.NFTA
import Autolib.NFTA.Shortest ( shortest, shortest0 )
import Autolib.Size
import Autolib.ToDoc
import Autolib.NFTA.Trim
import Autolib.NFTA.Normalize
import qualified Autolib.Relation as Relation
import Autolib.Informed

import Autolib.TES.Identifier
import Autolib.Util.Zufall
import Control.Monad ( guard )
import Data.Monoid ( mconcat )

import Inter.Quiz
import Inter.Types

import Debug
import Data.Array


blank :: NFTAC c s => Set c -> Set s -> s -> NFTA c s
blank cs stats fin = NFTA
         { nfta_info = funni "blank" [ info cs, info stats ]
         , alphabet = cs
         , states = stats
         , finals = unitSet fin 
         , trans  = Relation.make []
         , eps     = Relation.empty $ stats
         }


-- | add transitions until final state is reachable
roller :: NFTAC (Int) s 
       => Conf 
       -> Set (Int) -> Set s -> s 
       -> IO ( NFTA (Int) s )
roller conf cs stats fin = extend (blank cs stats fin) 

extend :: NFTAC Int s
       => NFTA Int s 
       -> IO (NFTA Int s)
extend au = do
    let tau = trim au
    if  not $ isEmptySet $ finals tau
        then return tau
        else do
            q <- eins $ lstates au
            a <- eins $ setToList $ alphabet au
            ps <- sequence $ replicate a $ eins $ lstates au
            extend $ au { trans = trans au `Relation.plus`
                             Relation.make [ (q, (a, ps)) ]
                        }

roll :: Conf -> IO ( NFTA Int Type, TI Identifier )
roll conf = do
    let ts = types conf
    fin <- eins ts -- final state
    au <- roller conf ( mkSet [ 0 .. max_arity conf ] ) (mkSet  ts) fin
    let names = do c <- [ 'a' .. ] ; return $ mknullary [c]
    let (fs,vs) = mconcat $ do
          (n, (p, (c, qs))) <- zip names $ Relation.pairs $ trans au
          if null qs 
             then return $ ([],[Variable { vname = n, vtype = p }])
             else return $ ([Function { fname = n
                                      , arguments = qs
                                      , result = p
                                      , static = True
                                      }],[])
    return ( au
           , TI { target = fin
                , signature = Signature fs vs
                }
           )

type InstanceConf = ( NFTA Int Type, TI Identifier )

instance Generator TypeCheck Conf InstanceConf where
    generator p conf key = 
        roll conf `repeat_until` \ ( au, ti ) -> 
              min_symbols conf <= size (signature ti)
           && size (signature ti) <= max_symbols conf
           && let s = size $ head $ shortest au
              in  min_size conf <= s && s <= max_size conf 

instance Project TypeCheck InstanceConf (TI Identifier) where
    project p ( au, ti ) = ti

make :: Make
make = quiz TypeCheck conf



