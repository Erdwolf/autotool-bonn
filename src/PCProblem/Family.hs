--  $Id$

module PCProblem.Family where

import PCProblem.Type
import PCProblem.Solver
import PCProblem.Generator
import PCProblem.Param
import PCProblem.Pick

import Autolib.Util.Zufall
import Autolib.ToDoc
import Control.Monad

import IO

par :: Param
par = Param { alpha = "01"
	      , paare = 4
	      , breite = 6
	      , nah = 1
	      , fern = 20
	      , viel = 1000
	      }


-- | randomly pick one position (of one letter in one word of one pair)
pick :: PCP -> IO Pick
pick (PCP ps) = do
    i <- eins [ 0 .. length ps - 1 ]
    let (x, y) = ps !! i
    f <- eins [ False, True ]
    let w = if f then x else y
    j <- eins [ 0 .. length w - 1 ]
    return $ Pick { pair = i , top = f, letter = j }

picks :: PCP -> Int -> IO [Pick]
picks p k = sequence $ replicate k $ pick p

-- | expand instance (repeat letter at chosen position)
expand :: Int -> PCP -> Pick  -> PCP
expand k (PCP ps) ick =
    let (pre, (x, y) : post) = splitAt (pair ick) ps
        w = if top ick then x else y
        (we, c : wost) = splitAt (letter ick) w
        w' = we ++ replicate k c ++ wost
        xy' = if top ick then (w', y) else (x, w')
    in  PCP $ pre ++ [ xy' ] ++ post

-- | multiple expand 
-- warning: one after another, not in parallel
expands :: Int -> PCP -> [Pick] -> PCP
expands k p icks = foldl (expand k) p icks

-- | (infinite) list of shortest solutions (if any)
sols :: Param -> PCP -> [Pick] -> [ Folge ]
sols conf p icks = map head
                 $ takeWhile (not . null) 
                 $ do
    k <- [ 1 .. ]
    let p' = expands k p icks 
    return $ take 1 $ solutions (viel conf) (fern conf) p'

picker :: Param -> PCP -> IO ()
picker conf p = do
    n <- eins [1 .. 3]
    icks <- picks p n
    let (ss, ts) = splitAt 10 $ sols conf p icks
        xs = map length ss
    if null ts
       then putStr "-"
       else do
           let j = judge $ drop 5 xs
               l = length j
           putStr $ show l
           when (l > 3) $ do
                     putStrLn $ "\nlevel " ++ show l
                     print $ toDoc (p, icks, ss, xs)
    hFlush stdout

judge :: [Int] -> String
judge [] = "0"
judge xs = 
    if all (>0) xs
    then "P" ++ judge (diff xs)
    else "X"

-- | difference sequence
diff xs = zipWith (-) (tail xs) xs


thrower :: Param -> IO ()
thrower conf = do
    (p, _) <- generator conf
    sequence_ $ replicate 10 $ picker conf p
 
runit :: Param -> IO ()
runit conf = sequence_ 
      $ repeat 
      $ thrower conf
