module Hanoi.Solve where

--   $Id$

import Hanoi.Type

import Autolib.Util.Sort

import Data.Array

nviers 0 = return [0]
nviers 1 = return [0,0]
nviers n | n > 1 = do
    prev <- nviers ( n-1 )
    let checks @ ((lbest, kbest) : _ ) = sort $ do
           k <- [ last prev .. last prev + 1 ]
           let ms = nvierAD ( prev ++ [k] ) n
           return ( length ms, k )
    print checks
    let res = prev ++ [ kbest ] 
    print (n, res)
    return $ res
    
   

nvierAD fun n = 
   if n > 0 
   then let k = fun !! n
        in     nvierAD fun k
            ++ ndreilong (n - k) A B C
            ++ nvierDA fun k 
            ++ ndreishort(n - k) C B D
            ++ nvierAD fun k
   else []   

nvierDA fun n = map flipped $ nvierAD fun n

flipped (s,t) = (mirror s, mirror t )
mirror A = D; mirror B = C; mirror C = B; mirror D = A

-- | von v über h nach n
-- | v und n sind benachbart, h ist auf der Seite von v
ndreishort s v h n = 
    if s > 0
    then    ndreishort (s-1) v n h
         ++ [ (v,n) ]
         ++ ndreilong (s-1) h v n
    else []

-- | von v über h nach n
ndreilong s v h n = 
    if s > 0
    then    ndreilong (s-1) v h n
         ++ [ (v,h) ]
         ++ ndreilong (s-1) n h v
         ++ [ (h,n) ]
         ++ ndreilong (s-1) v h n
    else []


-------------------------------------------------------------

drei n von nach hilf = 
    if n > 0 
       then    drei (n-1) von hilf nach
	    ++ [ ( von, nach ) ]
	    ++ drei (n-1) hilf nach von
       else []


vier n von nach h1 h2 =
    if n > 0
       then let k = kah n
	    in     vier k     von h1   nach h2
		++ drei (n-k) von nach h2
		++ vier k     h1  nach von  h2
       else []

vau n = fst $ best ! n
kah n = snd $ best ! n
dee n = 2 ^ n - 1

best :: Array Integer (Integer, Integer)
best = array (1, 100) $ do 
--     n <- range $ bounds best
     n <- range (1, 100)
     let ( v, k ) = minimum $ ( dee n, 0 ) : do
	   k <- [ 1 .. n - 1 ]
	   let v = 2 * vau k + dee (n-k)
	   return ( v, k )
     return (n, ( v, k ))

