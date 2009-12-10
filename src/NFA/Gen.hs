module NFA.Gen where

--  $Id$

import NFA.Genetic
import NFA.Synchronize

import Autolib.NFA.Dot
import Autolib.NFA.Type
import Autolib.Util.Zufall
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Output

import Autolib.Set
import Autolib.Schichten

import Random
import Data.List
import Control.Monad
import Text.XHtml (Html)

type Gene = [(Int,Int)]

test :: IO ()
test = do
    this <- evolve (conf 4) 
    print this

fixed xs conf = 
    let fix xys = zip xs $ map snd xys
    in conf { generate = fmap fix $ generate conf
            , combine  = \ a b -> fmap fix $ combine conf a b
            , mutate   = \ a   -> fmap fix $ mutate  conf a
	    }

conf :: Int -> Conf Gene Int
conf n = Conf 
       { fitness = \ g ->  case shosyn $ mach g of
	                     w : _ | nohc g -> length w
	                     _     -> 0
       , threshold = (n-1) ^ 2
       , generate = sequence $ replicate n $ do
            let qs = [0 .. n-1]
	    x <- eins qs ; y <- eins qs
            return (x, y)
       , combine = \ a b -> entweders 
            [ combine_vertically a b
	    , combine_horizontally a b
	    ]
       , mutate = \ a -> entweders
            [ often  1 reverse_subsequence a
	    , often  1 change_target       a
	    , shift a 
	    ]
       , size = 100
       , num_mutate = 50
       , num_combine = 50
       , num_compact = 5
       , trace = hush
       , present = schreib n
       }

-----------------------------------------------------------------------------

hush pool = do
     print $ toDoc $ take 3 $ popul pool
     let vs = map fst $ popul pool
     putStrLn $ "histogram: " ++ show (reverse $ histo vs)
     putStrLn $ "average  : " 
              ++ show (  fromIntegral (sum vs) 
		      / fromIntegral (length vs)
		      )

combine_horizontally a b = do
    return $ zip ( map fst a ) ( map snd b )

combine_vertically a b = do
    k <- randomRIO (0, length a - 1)
    return $ take k a ++ drop k b

-- | rotate (cyclically) by one
shift g = do
    let xs = map fst g ; ys = map snd g
    f <- randomRIO (False, True)
    case f of
	 False -> return $ zip (rotate xs) ys
	 True  -> return $ zip xs (rotate ys)

-- | flip two letters (from one state)
flip_letters a = do
    let n = length a
    i <- randomRIO (0, n-1)
    return $ update a (i, \ (x,y) -> (y,x) )

-- | change target of one arrow
change_target a = do
    let n = length a
    i <- randomRIO (0, n-1)
    f <- randomRIO (False, True)
    z <- randomRIO (0, n-1)
    return $ update a (i, \ (x,y) -> if f then (z,y) else (x,z))

-- | swap two states
swap_states a = do
    let n = length a
    [i, j] <- einige 2 [0 .. n-1]
    return $ pokes a [(i, a!!j), (j, a!!i)]

-- | reverse subsequence (brutally)
reverse_subsequence a = do 
    let n = length a
    [i, j] <- einige 2 [0 .. n-1]
    let (lo, hi) = (min i j, max i j)
        (pre, midpost) = splitAt lo a
        (mid, post) = splitAt (hi - lo) midpost
    return $ pre ++ reverse mid ++ post

-----------------------------------------------------------------------

histo :: Ord a => [a] -> [(a, Int)]
histo xs = fmToList $ addListToFM_C (+) emptyFM $ do
    x <- xs
    return (x, 1)

-----------------------------------------------------------------------

schreib :: Int
	-> Pool Gene Int
	-> IO ()
schreib n pool = tofile ("pool-" ++ show n ++ "-" ++ show (num pool)) $ do
    inform $ fsep [ text "generation", toDoc $ num pool ]
    sequence_ $ do
        (v, g) <- take 3 $ popul pool
        return $ handle $ mach g

tofile :: FilePath 
     -> Reporter a
     -> IO ()
tofile fname r = do
        ( _ , out :: Text.XHtml.Html ) <- Autolib.Reporter.run r
	writeFile ( fname ++ ".html" ) $ show out

------------------------------------------------------------------------------

both_kein_perm :: Gene -> Bool
both_kein_perm g =
    kein_perm ( map fst g ) && kein_perm ( map snd g )

kein_perm :: [Int] -> Bool
kein_perm xs = 
    cardinality (mkSet xs) < length xs

-- | for at least one letter, 
-- number of states that are changed is larger than 1
fast_id :: [Int] -> Bool
fast_id xs = (1 >=) $  length $ do
              (k, x) <- zip [0..] xs
	      guard $ k /= x
              return ()

kreis :: [Int] -> Bool
kreis xs =
    let orbit xs = bfs ( \ k -> unitSet (xs !! k) ) ( head xs )
        full orb = length orb == length xs
    in  full $ orbit xs

nohc :: Gene -> Bool
nohc g = not (kreis $ map fst g)
      && not (kreis $ map snd g)

standard :: Gene -> Bool
standard g = 
    let xs = map fst g
        ys = map snd g
    in     ( fast_id xs && kreis ys )
        || ( fast_id ys && kreis xs )

----------------------------------------------------------------------------

mach g = make $ umform g 
umform g = [map fst g, map snd g]

often :: Monad m 
      => Int 
      -> (a -> m a)
      -> (a -> m a)
often k this = \ a -> foldM ( \ a () -> this a ) a ( replicate k () )

