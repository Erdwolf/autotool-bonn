module Petri.Step where

import Petri.Type
import Petri.Dot
import Autolib.Dot.Dotty ( peng )

import Autolib.FiniteMap  
import Data.Map ( Map )  
import qualified Data.Map as M
import Autolib.Set
import qualified Data.Set as S
import Control.Monad ( guard )

import Autolib.Reporter hiding ( initial )
import Autolib.ToDoc

deadlocks n = do
    zs <- levels n
    return $ filter ( null . successors n ) zs

levels :: ( Ord s, Ord t )
           => Net s t 
           -> [ [ State s ] ]   
levels n = 
    let f done [] = []
        f done xs = 
            let done' = S.union done $ S.fromList xs
                next = S.fromList $ do 
                   x <- xs ; ( t, y ) <- successors n x
                   return y
            in  xs : f done' 
                     ( S.toList $ S.difference next done' ) 
    in  f S.empty [ start n ]


executes n ts = foldM ( \ z (k, t) -> do
                          inform $ text "Schritt" <+> toDoc k
                          Petri.Step.execute n t z ) 
                     ( start n ) ( zip [ 1 :: Int .. ] ts )

execute n t z0 = do
    inform $ text "Transition" <+> toDoc t
    let cs = do 
            c @ (vor, t', nach) <- connections n 
            guard $ t' == t
            return c
    case cs of
        [] -> reject $ text "existiert nicht"
        [(vor, _, nach)] -> do
            let z1 = change pred vor z0
            inform $ text "Zwischenzustand (nach Einziehen der Marken im Vorbereich)"    
                </> toDoc z1
            when ( not $ all_non_negative z1 ) $ reject $ text
                "enthält negative Markierungen (Transition war nicht aktiviert)"
            let z2 = change succ nach z1    
            inform $ text "Endzustand (nach Austeilen der Marken im Nachbereich)"    
                </> toDoc z2
            when ( not $ conforms ( capacity n ) z2 ) $ reject $ text    
                "enthält Markierungen, die die Kapazität überschreiten"
            peng $ n { start = z2 }
            return z2       

successors :: ( Ord s, Ord t )
           => Net s t -> State s 
           -> [ ( t, State s ) ]   
successors n z0 = do           
    c @ ( vor, t, nach ) <- connections n
    let z1 = change pred vor z0
    guard $ all_non_negative z1    
    let z2 = change succ nach z1
    guard $ conforms ( capacity n ) z2
    return ( t, z2 )
    
change :: Ord s
       => ( Int -> Int ) -> [s] -> State s -> State s
change f ps (State z) = State $ foldl 
   ( \ z p -> M.insert p ( f $ M.findWithDefault 0 p z ) z )
   z ps
    
        