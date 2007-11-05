{-# language NoMonomorphismRestriction #-}

module Hanoi.Fast where

import Hanoi.QSearch
import Hanoi.Type

import Autolib.ToDoc

import Data.List
import Control.Monad ( guard )

type State = [ Turm ]

handle k = do
    let targets = -- neighbours
                  cyclic
    let start = replicate k minBound
    let final = replicate k maxBound
    let badness zs this = 
            let ( ok, unok ) = span ( == maxBound ) $ reverse this
            in  if this == final
                then 0
                else 1 + fromIntegral ( length zs )
    let cs = decreasing
                $ search ( moves targets )  badness $ start 
    let obfuscate zs = zs ++ concat ( replicate 20 [ (D,C),(C,D) ] )
    sequence_ $ do
        ( v, s, zs ) <- cs
        return $ print $ vcat
            [ empty
            , toDoc -- $ obfuscate
                    $ zs 
            , toDoc s
            , toDoc ( length zs )
            ]

cyclic t = 
    [ if t < maxBound then succ t else minBound ]

neighbours t = 
        [ pred t | t > minBound ]
     ++ [ succ t | t < maxBound ]
    
moves :: ( Turm -> [Turm] ) -> State -> [ ( Zug, State ) ]
moves targets this = do
    ( pre, t : post ) <- splits this
    guard $ not $ t `elem` pre
    s <- targets t
    guard $ not $ s `elem` pre
    let next = pre ++ s : post 
    return ( (t,s), next )

splits xs = zip ( inits xs ) ( tails xs )

