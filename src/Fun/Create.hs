module Fun.Create where

-- $Id$

-- erzeuge eine k-stellige funktion bestimmter größe

import Fun.Type
import Fun.Table

import Util.Zufall

create :: Int -> Int -> IO Fun
create k s | s < 3 = entweders 
    $  [ return $ Zero k | False ]
    ++ [ do i <- eins [ 1 .. k ] ; return $ Proj k i ]
    ++ [ return $ Succ 1 | 1 == k ]

create k s = entweders $
    [ create_sub k s ]
    ++ [ create_pr  k s | k > 0 ]

-- hier ist s >= 3
create_sub k s = do 
    print ("create_sub", k, s)
    -- stelligkeit der kopf-funktion (g) raten
    i  <- eins [ 1 .. s-2 ]
    -- größen aller funktionen (g und hs) raten
    j : js <- summe (i+1) (s-1)
    print (j, js)
    g  <- create i j
    hs <- sequence $ do j <- js ; return $ create k j
    return $ Sub k ( g : hs )

-- hier ist s >= 3
create_pr  k s = do
    print ("create_pr", k, s)
    [ i, j ] <- summe 2 (s-1)
    print (i, j)
    g <- create (k-1) i
    h <- create (k+1) j
    return $ PR k [ g, h ]
