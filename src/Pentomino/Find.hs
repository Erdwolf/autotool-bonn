{-# language PatternSignatures #-}

module Pentomino.Find where

import qualified Pentomino.Position as P

import Pentomino.Cover ( 
       roll_shift, pieces, covers, reach, unreach, form, orig, Figure
       , halo, area, modify, figure_shift, Piece(..), container
       , points, spoints 
       )
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
import Data.List ( partition )

import Control.Monad ( guard )

main = run2

main2 = run

main1 = do
    argv <- getArgs
    let z = case argv of
	      [ zz ] -> read zz
	      [] -> 10
    G.evolve $ conf z



conf z = G.Config 
    { G.fitness = \ f -> let ( v, _ ) = evaluate f in v
    , G.threshold = ( 130, 0 )
    , G.present = mapM_ ( \ (v,f) -> printf (toDoc v <+> form f ) )
                . reverse . take 3
    , G.trace = printf . map fst . take 5
    , G.size  = 1 * z
    , G.generate = roll_shift
    , G.combine = undefined
    , G.num_combine = 0 * z
    , G.mutate = improve_straight 3
    , G.num_mutate = 2 * z
    , G.num_compact = z
    , G.num_steps = Nothing
    , G.num_parallel = 1
    }

printf x = do print x ; hFlush stdout

diversity f = S.size $ S.fromList $ map orig $ pieces f

---------------------------------------------------------------------------

complete_changes w f = do
    k <- [ 0 .. length ( pieces f ) - 1 ]
    changes_for w f k

changes_for w f k = do
    let ( pre, this : post ) = splitAt k $ pieces f
        ( xs, _ : ys ) = splitAt k $ covers f
        rest = xs ++ ys
    that <- changes w rest this
    return $ figure_shift $ pre ++ that : post

several_complete_changes width f n = 
    let fun pre 0 ps = return $ reverse pre ++ ps
        fun pre n (p : ps) = 
               do guard $ n <= length ps 
                  guard $ S.null $ S.intersection ( spoints p ) $ S.unions ( map spoints pre )
                  fun (p : pre ) n ps
            ++ do q <- changes width ( map spoints $ pre ) p
                  fun (q : pre) (n-1) ps 
    in  map figure_shift $ fun [] n $ pieces f
    
-- | with swap
changes2_for w f i j = do
    let ps = pieces f
        x0 = ps !! i
        y0 = ps !! j
        rest = covers f `without` [ i, j ]
    x <- changes w rest $ x0 { orig = orig y0 }
    y <- changes w rest $ y0 { orig = orig x0 }
    return $ figure_shift $ ps // [(i,x),(j,y)]

---------------------------------------------------------------------

xs // [] = xs
xs // ((i,x) : rest ) = 
    let ( pre, _ : post ) = splitAt i xs
        ys = pre ++ x : post
    in  ys // rest

xs `without` [] = xs
xs `without` (i : rest) = 
    let ( pre, _ : post ) = splitAt i xs        
        ys = pre ++ post
    in  ys `without` rest

---------------------------------------------------------------------

changes w rest this = do
    let others = S.unions rest
    t <- [ 0 .. 3 ]   
    m <- [ 0 .. 1 ]
    let bnd0 = ((negate w, negate w),(w,w))
    (sx,sy) <- range bnd0
    let p = this
               { turns = t
               , mirrors = m
               , shift = shift this + P.Position sx sy
               }
    guard $ S.null $ S.intersection ( spoints p ) others
    return p

some_best fs = do
    let m = M.fromListWith S.union $ do
            f <- fs
            let ( v, _) = evaluate f
            return ( v, S.singleton f )
    let ( v, gs ) = last $ M.toAscList m
    g <- eins $ S.toList gs
    return ( v, g )


run = do
    let width = 3
    f <- roll_shift
    let runner ( v, f ) = do
            printf $ toDoc v <+> form f
            r <- randomRIO ( 0, 10 :: Int )
            let action = if 0 ==  r 
                         then improve_double_repeat width 
                         else \ ( v,f) -> improve_simple width f
            (w, g) <- action (v, f)
	    runner ( w, g )
    runner $ evaluate f

first_best v fs = 
    let ( yeah, hmnoh ) = partition ( \(w,_) -> w > v ) $ map evaluate fs
        ( hm, noh ) = partition ( \(w,_) -> w == v ) $ map evaluate fs
    in  case yeah of
          [] -> eins $ hm
          p : _ -> return p

run2 = do
    f <- roll_shift
    let strat0 = [(1,1),(2,1),(1,2)]
    let runner strat ( v @ ( u,_ ) , f ) = do
            printf $ toDoc v <+> form f
            let (width,num) = head strat
            print $ toDoc ( width, num )
            ( w @ (u',_), g ) <- first_best v -- some_best 
                        $ several_complete_changes width f num
            let strat' = if w > v then strat0 
                         else if null ( tail strat ) then strat0
                         else tail strat
            runner strat' ( w, g )
    runner strat0 $ evaluate f

improve_straight width f = do
    ( w, g ) <- some_best $ complete_changes width f 
    return g



improve w f = do
    r <- randomRIO ( 0, 10 :: Int )
    let action = if 0 ==  r then improve_double else improve_simple
    (w, g) <- action w f
    return g

improve_simple w f = do
    let n = length $ pieces f
    k <- eins [ 0 .. n-1 ]
     -- print $ text "simple/select:" <+> toDoc k <+> toDoc ( ['a' .. ] !! k )
    some_best $ changes_for w f k

improve_double_repeat width (v, f) = do
    (w,g) <- improve_double width f
    if w >= v then return( w, g) 
              else improve_double_repeat width (v,f)

improve_double w f = do
    let n = length $ pieces f
        ks = [ 0 .. n - 1 ]
    i <- eins ks
    j <- eins ( ks `without` [i] )
    -- print $ text "double/select:" <+> toDoc (i,j)
    let x = orig $ get f i ; y = orig $ get f j    
    ( _ , g ) <- some_best $ changes_for w ( reshape f  i y  ) i
    some_best $ changes_for w ( reshape g j x ) j


reshape f i s =
    replace f i $ ( get f i ) { orig = s }

replace f i p =
    f { pieces = pieces f // [ (i, p) ] }

get f i = pieces f !! i



evaluate f = ( (unreach f - 5 * 12, negate $ max_extension f ) , f )



max_extension f = 
    let ((l,u),(r,o)) = container  f
    in  max (r-l) (o-u)


{-
swapper ( v, f ) = do
	          print $ text "stagnation"
		  i <- randomRIO ( 0, length ( pieces f ) - 2 )
		  j0 <- randomRIO ( 0, length ( pieces f ) - 2 )
		  let j = if j0 >= i then j0 + 1 else j0
		  printf $ text "select:" <+> toDoc (i,j)
      	          case  filter ( \ (w,g) -> w > v ) 
		         $ best2_for f i j of
                      (w,g) : _ -> do
	                 printf $ toDoc v <+> form g
                  	 runner ( w, g )
                      [] -> do
		         runner ( v, f)
-}

---------------------------------------------------------------------


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

