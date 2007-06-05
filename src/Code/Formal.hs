{-# OPTIONS -fglasgow-exts #-}

module Code.Formal where

import Autolib.Set
import Autolib.ToDoc
import Autolib.NFA
import Autolib.NFA.Ops
import Autolib.NFA.Normalize
import Autolib.NFA.Shortest

import Autolib.Reporter

import Control.Monad ( guard )
import Data.List 

prefix_free tag ws = do
    inform $ vcat 
	   [ text "Ist die Menge" <+> tag
	   , nest 4 $ toDoc ws
	   , text "präfix-frei?"
	   ]
    let wrong = do
            ( u : vs ) <- tails $ setToList ws
            v <- vs
            (x,y) <- [(u,v),(v,u)]
            guard ( x `isPrefixOf` y) 
	    return (x,y)
    case wrong of
       [] -> inform $ text "Ja."
       (x,y) : _ -> 
           reject $ vcat 
               [ text "Nein,"
	       , nest 4 $ hsep [ toDoc x, text "ist Präfix von", toDoc y ]
	       ]

self_cross :: ( NFAC c s, NFAC c (s,s) )
    => NFA c s -> NFA c (s,s)
self_cross a = 
    let  c0 = Autolib.NFA.Ops.cross a a 
	 c = c0 { finals = Autolib.Set.cross (finals a) (finals a) }
	 t = trim c
    in   t

ambiguous_words :: NFAC c s => NFA c s -> [[c]]
ambiguous_words a = do
    let c = self_cross a
    pq @ (p,q) <- lstates c
    guard $ p /= q -- off diagonal
    u <- some_shortest $ c { finals = mkSet [ pq ] }
    v <- some_shortest $ c { starts = mkSet [ pq ] }
    return $ u ++ v

code_counter_examples ws = 
    take 1 $ ambiguous_words $ normalize $ loops ws

-- | the obvious automaton accepting {w_1, .. w_n}^*
loops :: NFAC c (Int,Int)
      => [[c]] -> NFA c (Int,Int)
loops ws =
    let ts = do
	    ( i, w ) <- zip [0..] ws
	    ( p, x ) <- zip [0..] w
	    let q = succ p `mod` length w
                jam (i,p) = ( if p == 0 then 0 else i, p )
	    return ( jam (i,p), x, jam (i,q))
    in  NFA { nfa_info = funni "loop" $ map info ws
	    , alphabet = mkSet $ concat ws
	    , states   = mkSet $ do (from,x,to) <- ts ; [ from, to ]
	    , starts   = mkSet [ (0,0) ]
	    , finals   = mkSet [ (0,0) ]
	    , trans    = collect ts
	    }
