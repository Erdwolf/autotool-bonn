module Type.Quiz where

--  $Id$

import Type.Data
import Type.Check

import Autolib.NFTA
import Autolib.Size
import Autolib.NFTA.Trim
import qualified Autolib.Relation as Relation
import Autolib.Informed

import Autolib.TES.Identifier
import Autolib.Util.Zufall
import Control.Monad ( guard )

import Inter.Quiz
import Inter.Types


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
       => Set (Int) -> Set s -> s 
       -> IO ( NFTA (Int) s )
roller cs stats fin = extend (blank cs stats fin)

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

roll :: Conf -> IO TI
roll conf = do
    let ts = types conf
    fin <- eins ts -- final state
    au <- roller ( mkSet [ 0 .. max_arity conf ] ) (mkSet  ts) fin
    let fs = do c <- [ 'a' .. ] ; return $ mknullary [c]
    let vfs = do 
          (f, (p, (c, qs))) <- zip fs $ Relation.pairs $ trans au
          if null qs 
	     then return $ Left  $ Variable { vname = f, vtype = p }
	     else return $ Right $ Function { fname = f
					    , arguments = qs
					    , result = p
					    }
    return $ TI { target = fin
		, signature = sammel vfs
		}

instance Generator TypeCheck Conf TI where
    generator p conf key = 
        roll conf `repeat_until` \ ti -> 
              min_symbols conf <= size (signature ti)
	   && size (signature ti) <= max_symbols conf

instance Project TypeCheck TI TI where
    project p ti = ti

make :: Make
make = quiz TypeCheck conf
