{-# LANGUAGE NoMonomorphismRestriction #-}

-- module Hanoi.Fast where

import Hanoi.QSearch
import qualified Hanoi.Type as T

import Autolib.ToDoc

import Data.List
import Control.Monad ( guard )
import System.IO

type Turm = Int -- ^ argh
type State = [ Turm ]

type Zug = (Turm, Turm)

<<<<<<< Fast.hs
main = sequence_ $ do
    ( tag, fun ) <- [ ( "neighbours", neighbours )
                    , ( "cyclic", cyclic )
                    ]
    return $ do
        putStrLn tag
        print $ tabulate fun 10 10 1000000
        hFlush stdout

tabulate targets p s depth = besides $ do
=======

tabulate targets p s = besides $ do
>>>>>>> 1.3
    k <- [ 0 .. s ]
    return $ vcat $ do
        q <- [ 3 .. p ]
        return $ case solve depth ( targets q ) q k of
            Nothing -> text "*"
            Just n  -> toDoc n

<<<<<<< Fast.hs
solve depth targets pegs pieces = 
=======
present :: [(Int,Int)] -> [T.Zug]
present = map ( \ (x,y) -> ( toEnum x, toEnum y))


solve targets pegs pieces = 
    fmap length $ solve_path targets pegs pieces

solve_path targets pegs pieces = 
>>>>>>> 1.3
    let start_peg = 0
        start = replicate pieces start_peg
        goal_peg  = pegs - 1
        goal = replicate pieces goal_peg
        badness zs this = 
                if this == goal
                then 0
                else 1 + fromIntegral ( length zs )
        
    in  case   filter ( \ (b,c,zs) -> c == goal )
             $ take depth
             $ search ( moves targets )  badness $ start of
            ( b, c, zs ) : _ -> Just zs 
            _ -> Nothing

central top t = 
    if t == 0 then [ 1 .. top - 1 ] else [ 0 ]

cyclic top t = [ succ t `mod` top ]

neighbours top t = 
        [ pred t | t > 0 ]
     ++ [ succ t | t < top - 1 ]
    
free top t = filter ( /= t ) [ 0 .. top - 1 ]

moves :: ( Turm -> [Turm] ) -> State -> [ ( Zug, State ) ]
moves targets this = do
    ( pre, t : post ) <- splits this
    guard $ not $ t `elem` pre
    s <- targets t
    guard $ not $ s `elem` pre
    let next = pre ++ s : post 
    return ( (t,s), next )

splits xs = zip ( inits xs ) ( tails xs )

