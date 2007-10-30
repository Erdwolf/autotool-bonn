{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}

-- module Algebraic.Set.Guess where

import Algebraic.Set

import Algebraic.Nested.Type as Nested
import Algebraic.Nested.Op

import qualified Algebraic2.Instance as AI
import qualified Algebraic2.Quiz as Q
import qualified Algebraic2.Class

import Autolib.TES.Type
import Autolib.TES.Binu 
import Autolib.Util.Zufall

import Autolib.Size
import Autolib.Depth
import Autolib.FiniteMap

import Autolib.Reporter ( result )

import Data.List ( nub ) 

import Tree.Like 
import qualified Tree.Guess as G

import Control.Monad ( guard )
import System.IO

conf = AI.Make 
     { AI.context = ( ) 
     , AI.target = read "{ 2, {}, {3, {4}}}"
     , AI.description = Nothing
     , AI.operators = Binu { binary = read "[ + , - , & ]" 
                        , unary = read "[ pow ]"
                 , nullary = read "[ 2 , 3 , 4 ]"
                 }
     ,  AI.predefined = listToFM 
            $ read " [ ( A , {1, 3, 5, 6} ) , ( B , {2, 3, 6, 7} ) ] "
     , AI.max_size = 7
     }

main = G.run $ make conf

make conf = G.Config
         { G.goal = AI.target conf
         , G.eval = \ x ->
              let Just a = result 
                         $ Algebraic2.Class.evaluate 
                               Algebraic_Set ( AI.predefined conf ) x
              in  if dangerous x
                  then read "{}"
                  else a
         , G.distance = \ a b -> 
                 ( ( 1 / log ( fromIntegral ( full_size b) )   ) * )
                 $ fromIntegral
                $  100 * abs ( full_size a - full_size b ) 
                + 300 * (abs $ 3 - top_length b )
                + 5 * (abs $ 3 - length ( flatten b ) )
                + 70 * (abs $ 3 - length ( nub $ flatten b ) )
                + 100 * (abs $ 3 - depth b )
                + 20 *  top_length ( difference a b ) 
                + 20 * top_length ( difference b a ) 
         , G.weight = \ t -> ((1/10) *) -- log $ ( 1 + ) -- $ log $ ( 1 + ) 
                      $ fromIntegral $ ( size t )
         , G.generate = Q.roll ( AI.operators conf ) 10 ( AI.predefined conf )
         , G.mutate = patch conf
         , G.population = 1000
         }

powdep ( Node f xs ) =
    let this = if ( show f == "pow" ) then 1 else 0
    in  this + maximum ( 0 : map powdep xs )

dangerous t = powdep t > 2

dangerous' t = or $ do
    p <- positions t
    return $ case peek t p of
        Node f [x] | show f == "pow" -> size x > 3
        _ -> False

patch conf t = do
    p <- eins $ positions t
    let s = peek t p
    f <- case arity s of
        0 -> eins $ nullary $ AI.operators conf
        1 -> eins $ unary $ AI.operators conf
        2 -> eins $ binary $ AI.operators conf
    return $ poke t p $ build f $ Tree.Like.children s

-----------------------------------------------------------------------------

instance Tree.Like.Class ( Term v c )  c where
    children ( Node f args ) = args
    label ( Node f args ) = f
    build f xs = Node f xs
