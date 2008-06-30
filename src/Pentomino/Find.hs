module Pentomino.Find where

import qualified Pentomino.Position as P

import Pentomino.Cover
import qualified Autolib.Genetic as G

import Autolib.ToDoc

import Data.Map ( Map )
import qualified Data.Map as M

import Data.Set ( Set )
import qualified Data.Set as S

import System.Random
import Data.Ix
import System.Environment
import System.IO

main = do
    argv <- getArgs
    let z = case argv of
	      [ zz ] -> read zz
	      [] -> 10
    G.evolve $ conf z

conf z = G.Config 
    { G.fitness = \ f -> 
          ( unreach f - overlaps f - returning f 
	  , unreach f
          , negate $ overlaps f
          )
    , G.threshold = ( 130 + 5 * 12 + 10 * 12, 0, 0 )
    , G.present = mapM_ ( \ (v,f) -> printf (toDoc v <+> form f ) )
                . reverse . take 3
    , G.trace = printf . map fst . take 5
    , G.size  = 5 * z
    , G.generate = roll
    , G.combine = glue
    , G.num_combine = 2 * z
    , G.mutate = \ f -> fmap figure_delta $ sequence $ do
          p <- pieces f
          return $ do
              m <- randomRIO ( 0, length ( pieces f )) 
              if ( m > 1 ) 
                 then return p
                 else modify p
    , G.num_mutate = 2 * z
    , G.num_compact = 10
    , G.num_steps = Nothing
    , G.num_parallel = 1
    }

printf x = do print x ; hFlush stdout

diversity f = S.size $ S.fromList $ map orig $ pieces f

glue f g = do
    let gs = do
	( p, o ) <- zip ( pieces g ) $ map orig $ pieces f
	return $ p { orig = o }
    k <- randomRIO ( 0, length ( pieces f ) )
    let ps = reverse ( drop k gs ) ++ take k ( pieces f ) 
    return $ figure_delta ps

merge f g = fmap figure_delta $ sequence $ do
          k <- [ 0 .. length ( pieces f ) - 1 ]
          return $ do
              s <- randomRIO ( False, True )
              return $ pieces ( if s then f else g ) !! k

returning :: Figure -> Int
returning f = minimum $ do
    p <- S.toList $ halo $ last $ covers f
    return $ maximum $ map abs [ P.x p, P.y p ]


-- | penalty for overlapping figures
overlaps :: Figure -> Int
overlaps f = 
    let m = M.fromListWith S.union $ do
              (k, c) <- zip [ 0 .. ] $ covers f
              x <- S.toList c
              return ( x, S.singleton k )
    in  sum $ filter ( > 1 ) 
            $ map S.size
            $ M.elems m

