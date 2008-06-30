module Pentomino.Find where

import qualified Pentomino.Position as P

import Pentomino.Cover ( roll_shift, pieces, covers, reach, unreach, form, orig, Figure
       , halo, area, modify, figure_shift, Piece(..), container )
import Pentomino.Force ( step, total )
import qualified Autolib.Genetic as G

import Autolib.ToDoc
import Autolib.Util.Zufall
import Autolib.Util.Splits

import Data.Map ( Map )
import qualified Data.Map as M

import Data.Set ( Set )
import qualified Data.Set as S

import Data.Ix
import System.Environment
import System.IO

main = main2

main2 = run

main1 = do
    argv <- getArgs
    let z = case argv of
	      [ zz ] -> read zz
	      [] -> 10
    G.evolve $ conf z

conf z = G.Config 
    { G.fitness = \ f -> 
          ( reach f
	  , unreach f
          , negate $ overlaps f
          )
    , G.threshold = ( 130 + 5 * 12 + 10 * 12, 0, 0 )
    , G.present = mapM_ ( \ (v,f) -> printf (toDoc v <+> form f ) )
                . reverse . take 3
    , G.trace = printf . map fst . take 5
    , G.size  = 1 * z
    , G.generate = roll_shift
    , G.combine = \ f g ->  return f
    , G.num_combine = 0 * z
    , G.mutate = \ p -> do
          -- action <- eins [ Pentomino.Force.step, change ]
          let action = change
	  action p
    , G.num_mutate = 1 * z
    , G.num_compact = 10
    , G.num_steps = Nothing
    , G.num_parallel = 1
    }

printf x = do print x ; hFlush stdout

diversity f = S.size $ S.fromList $ map orig $ pieces f


best_for f = do
    let bnd = container f
    ( pre, this : post ) <- splits $ pieces f
    t <- [ 0 .. 3 ]   
    m <- [ 0 .. 1 ]
    let w = 3
    let bnd0 = ((negate w, negate w),(w,w))
    (sx,sy) <- range bnd0
    let that = this
               { turns = t
               , mirrors = m
               , shift = shift this + P.Position sx sy
               }
    let g = figure_shift $ pre ++ that : post
    return ( unreach g
	   , g	     
	   )


run = do 
    f <- roll_shift
    let runner f = do
	    let ( v, g ) = maximum $ best_for f
	    print $ toDoc v <+> form g
	    runner g
    runner f




{-
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
-}

change :: Figure -> IO Figure
change f = fmap figure_shift $ sequence $ do
    p <- pieces f
    return $ do
        k <- randomRIO ( 0, length $ pieces f )
	if k == 0 then modify' p else return p

modify' p = do
    dx <- eins [ -1 .. 1 ]
    dy <- eins [ -1 .. 1 ]
    return $ p { shift = shift p + P.Position dx dy }

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

