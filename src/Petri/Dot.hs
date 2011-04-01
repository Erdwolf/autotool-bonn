module Petri.Dot

( module Autolib.Dot.Dot )

where

import Petri.Type

import Autolib.Dot.Dot
import qualified Autolib.Dot.Graph as G
import qualified Autolib.Dot.Node  as N
import qualified Autolib.Dot.Edge  as E

import qualified Data.Map as M
import qualified Data.Set as S

instance ( Show s, Show t, Ord s, Ord t ) => ToDot ( Net s t ) where
    toDot n = helper n
    toDotProgram a = Dot
    toDotOptions a = "-Grankdir=LR"

helper n = 
    let placemap = M.fromList 
            $ zip ( S.toList $ places n ) $ map show [ Place 0 .. ]
        transmap = M.fromList 
            $ zip ( S.toList $ transitions n ) $ map show [ Transition 0 .. ]
        oneline = unwords . words
        placenodes = do
            ( p, q ) <- M.toList placemap       
            return $ N.blank
                   { N.ident = q
                   , N.label = Just 
                          $ unlines [ oneline $ show p , show ( mark (start n) p ) ]
                   , N.shape = Just "ellipse"
                   }
        transnodes = do
            ( p, q ) <- M.toList transmap
            return $ N.blank
                   { N.ident = q
                   , N.label = Just $ oneline $ show p
                   , N.shape = Just "box"
                   }
        inputs = do
            ( vor, t, nach ) <- connections n
            v <- vor
            return $ E.blank
                   { E.from = placemap M.! v
                   , E.to = transmap M.! t
                   }
        outputs = do
            ( vor, t, nach ) <- connections n
            n <- nach
            return $ E.blank
                   { E.from = transmap M.! t
                   , E.to = placemap M.! n
                   }
    in  G.Type
        { G.directed = True
        , G.name = "Petri"
        , G.nodes = placenodes ++ transnodes
        , G.edges = inputs ++ outputs
        , G.attributes = []
        }