module NFA.Compress.Roll where

import NFA.Compress.Config
import NFA.Compress.Instance
import NFA.Compress.Compressed
import NFA.Compress.Look

import Autolib.Util.Zufall
import Autolib.Reporter

import Data.Maybe
import Data.List ( tails )


-- forall x . default[x] < x (damit es terminiert)
-- aber später die Zeilen zufällig permutieren

-- forall x in states . exists y in letters . check[base[x]+y]==x

roll :: RandomC m 
     => Config 
     -> m Instance
roll conf = do
    o <- do 
        c <- roller conf
        let Just o = result
               $ NFA.Compress.Look.table c ( states conf ) ( letters conf )
        return o
      `repeat_until` all_different
    oo <- permutation o
    return $ Instance
           { original = oo
           , max_size = compressed_size conf + allow_slack conf
           }

all_different xss = and $ do
    xs : yss <- tails xss
    ys <- yss
    return $ xs /= ys

roller conf = do
    nx <- sequence $ replicate ( compressed_size conf ) 
                   $ randomRIO ( 0, states conf - 1 )
    let c = Compressed
          { dflt = replicate ( states conf ) 0
          , base = replicate ( states conf ) 0
          , chck = replicate ( compressed_size conf ) 0
          , next = nx
          }
    handle conf ( states conf - 1 ) c
  `repeat_until` \ c -> 
      isJust $ result  
               $ NFA.Compress.Look.table c ( states conf ) ( letters conf )

handle conf p c | p <= 0 = return c
handle conf p c = do
    q <- randomRIO ( max 0 $ p - 1 , p - 1 )
    b <- randomRIO ( 0, compressed_size conf - letters conf )
    picks <- permutation [ b .. b + letters conf - 1 ]
    w <- randomRIO ( 1, letters conf - 1 )
    let ks = take w picks
    handle conf (p - 1) $ c
           { dflt = patch ( dflt c ) (p, q )
           , base = patch ( base c ) (p, b) 
           , chck = patches ( chck c ) $ zip ks $ repeat p
           }

patch xs (i, y) = 
    let ( pre, _ : post ) = splitAt i xs
    in  pre ++ y : post

patches = foldl patch




