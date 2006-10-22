module Grammatik.CF.Create where

--  $Id$

import Grammatik.Type ( Grammatik )
import Grammatik.CF.Chomsky
import Grammatik.CF.Nullable

import Data.FiniteMap
import Sets
import Util.Uniq
import Fix

type Generation x = FiniteMap x ( Set String )

base :: Ord x =>  Chomsky x -> Generation x
base ch = addListToFM_C union emptyFM $ do
     ( l , Left c) <- rules ch
     return ( l, mkSet [ [c] ] )

next :: Ord x => Chomsky x -> Int -> Generation x -> Generation x
next ch limit g = addListToFM_C union g $ do
     ( l, Right ( x, y ) ) <- rules ch
     wx <- setToList $ lookupset g x
     wy <- setToList $ lookupset g y
     let w = wx ++ wy
     return ( l, mkSet [ w | length w < limit ] )

-- | erzeuge alle wörter bis zu gegebener länge
create :: Grammatik -> Int -> [ String ]
create g limit = uniqs $ do
       let ch = make g
       n <- [ 0 .. limit ]
       create_ch ch n

-- | erzeuge alle wörter bis zu gegebener länge
create_ch :: Ord x 
       => Chomsky x
       -> Int
       -> [ String ]
create_ch ch limit =  
    let g = fix ( next ch limit ) ( base ch )
    in [ "" | eps ch ] ++ setToList ( lookupset g $ start ch )


