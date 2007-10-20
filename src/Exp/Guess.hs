-- module Exp.Guess where

import Autolib.Genetic
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

import Control.Monad ( guard )
import System.IO

main = evolve $ make "ab" 
	      $ read "All - All a b a b All"

large = 1000

make :: [ Char ] -> Exp -> Config Exp ( Double, Double, [String ] )
make sigma target = 
    let a = inter_det (std_sigma sigma) target
    in Config
        { fitness = \ y -> 
              let b = inter_det (std_sigma sigma) y
                  ab = some_shortest ( minus a b )
		  ba = some_shortest ( minus b a )
		  diff = ab ++ ba
                  delta :: Double
		  delta = case ba of
			[] -> case ab of
			   [] -> 0
			   w : _ ->
			       2 ^^ negate ( length w ) 
                        _ -> large
                  s :: Double
                  s =  fromIntegral $ max 20 $ Autolib.Size.size y 
                  sq = truncate . sqrt . fromIntegral 
                  lg = truncate . log . fromIntegral 
	      in  ( negate (log s) - 1000 *  delta 
		  , negate s 
		  , diff
		  )

        , threshold = ( 0 , 0, [] )
        , present = score
        , trace   = score
        , Autolib.Genetic.size    = 1000
        , generate = do
             ( y, b ) <- some ( mkSet sigma ) 50
	     return y
        , combine = combination
        , num_combine = 100
        , mutate  = often 10 $ mutation sigma
        , num_mutate = 100
        , num_compact = 1
        }

score vas = mapM_ printf $ take 5 $ do
    (v, x) <- vas
    return ( Autolib.Size.size x, v, x ) 

printf x = do
    print x
    hFlush stdout

-----------------------------------------------------------------------------

smaller :: Exp -> [ Exp ]
smaller x = do
    p <- positions x
    case peek x p of
        Union l r -> [ poke x p l, poke x p r ]
        Dot   l r -> [ poke x p l, poke x p r ]
	PowerStar a -> [ poke x p a ]
	_ -> []

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

often 0 action x = return x
often k action x = do y <- action x ; often ( k - 1 ) action y

mutation :: [Char ] -> Exp -> IO Exp
mutation sigma x = do
    action <- eins [ combination x x , compress x, turn x, swap x
		   -- , simpf sigma x  
		   , eins $ x : smaller x
		   ]
    action

compress x = do
    p <- eins $ positions x
    let y = peek x p
    q <- eins $ positions y
    return $ poke x p $ peek y q

turn x = do
    p <- eins $ positions x
    return $ poke x p $ subturn $ peek x p

subturn x = case x of
    Dot l r -> Union l r
    Union l r -> Dot l r
    PowerStar a -> PowerStar $ subturn a
    _ -> x

swap x = do
    p <- eins $ positions x
    return $ poke x p $ subswap $ peek x p

subswap x = case x of
    Dot l r -> Dot r l
    Union l r -> Union r l
    PowerStar a -> PowerStar $ subswap a
    _ -> x

combination :: Exp -> Exp -> IO Exp
combination x y = do
    p <- eins $ positions x
    q <- eins $ positions y
    return $ poke x p $ peek y q

-----------------------------------------------------------------------------

type Position = [Int]

positions :: Exp -> [Position]
positions x = [] : case x of
    Dot   l r -> map (0:) ( positions l ) ++ map (1:) ( positions r )
    Union l r -> map (0:) ( positions l ) ++ map (1:) ( positions r )
    PowerStar a -> map (0:) ( positions a ) 
    _ -> []

peek :: Exp -> Position -> Exp
peek x [] = x
peek x (p : ps) = case x of
    Dot   l r -> peek (case p of 0 -> l ; 1 -> r ) ps 
    Union l r -> peek (case p of 0 -> l ; 1 -> r ) ps 
    PowerStar a -> peek ( case p of 0 -> a ) ps

poke :: Exp -> Position -> Exp -> Exp
poke x [] y = y
poke x (p : ps) y = case x of
    Dot   l r -> case p of
        0 -> Dot ( poke l ps y ) r
	1 -> Dot l ( poke r ps y )
    Union l r -> case p of
        0 -> Union ( poke l ps y ) r
	1 -> Union l ( poke r ps y )
    PowerStar a -> case p of
        0 -> PowerStar ( poke a ps y ) 

-----------------------------------------------------------------------------

