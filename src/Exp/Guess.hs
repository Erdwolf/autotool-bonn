{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

-- module Exp.Guess where


import Autolib.Exp
import Autolib.Exp.Some
import Autolib.Exp.Inter
import Autolib.NFA.Subseteq
import Autolib.NFA.Minus
import Autolib.NFA.Shortest ( some_shortest )
import Autolib.Util.Zufall
import Autolib.Util.Sort
import Autolib.Set
import Autolib.Size

import Tree.Like 
import qualified Tree.Guess as G

import Control.Monad ( guard )
import System.IO

main = G.run $ make "ab" 
	     $ read "All - All a b a b All"

make sigma target = 
    let a = inter_det (std_sigma sigma) target
    in G.Config
         { G.goal = a
         , G.eval = inter_det (std_sigma sigma) 
         , G.distance = \ a b -> 
              let ab = some_shortest ( minus a b )
		  ba = some_shortest ( minus b a )
              in  case ba of
			[] -> case ab of
			   [] -> 0
			   w : _ ->
			       4 ^^ negate ( length w  ) 
                        _ -> 100
         , G.weight = fromIntegral . size
         , G.generate = do 
             ( y, b ) <- some ( mkSet sigma ) 50
	     return y
         , G.mutate = implant
         , G.population = 100
         }

-------------------------------------------------------------------

simplified sigma x = 
    let a = inter ( std_sigma sigma ) x
	f x = 
	    let ys = do
	            y <- smaller x
		    guard $ equivalent sigma a y
		    return y
	    in case ys of
	        y : _ -> y -- f y -- not recursively
		[]    -> x
    in  f x

simpf sigma x = do
    -- hPutStrLn stderr $ "simpf " ++ show x
    let y = simplified sigma x
    -- hPutStrLn stderr $ "  ==> " ++ show y
    return y

equivalent sigma a y = 
    let 
	b = inter (std_sigma sigma) y
    in     null ( some_shortest $ minus a b )
	&& null ( some_shortest $ minus b a )

-----------------------------------------------------------------------------

implant x = do
    p <- eins $ positions x
    r <- eins [ "Eps", "Sigma" ]
    return $ poke x p $ Ref r

-----------------------------------------------------------------------------

instance Tree.Like.Class Exp String where

    children t = case t of
        Union l r -> [l, r]
        Dot   l r -> [l, r]
        PowerStar a -> [a]
        _ -> []

    label t = case t of
        Union {} -> "+"
        Dot {} -> "*"
        PowerStar {} -> "^*"
        _ -> show t

    build f xs = case ( f, xs ) of
        ( "+", [l,r] ) -> Union l r
        ( "*", [l,r] ) -> Dot l r
        ( "^*", [a] ) -> PowerStar a
        ( _,    [] ) -> read f
