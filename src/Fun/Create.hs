module Fun.Create where

--   $Id$

-- erzeuge eine k-stellige funktion bestimmter größe

import Fun.Type
import Fun.Table

import Util.Zufall
import Array
import Data.Set

create :: Int -> Int -> IO Fun
create k s | s < 3 = do
  -- print ("create (klein)", k, s)
  entweders 
    $  [ return $ Zero k | k == 0 ]
    ++ [ do i <- eins [ 1 .. k ]
	    return $ Proj k i 
       | k >= 1 ]
    ++ [ return $ Succ 1 | 1 == k ]

create k s = do
    -- print ("create", k, s)
    entweders $  [ create_sub k s ]
              ++ [ create_pr  k s | k > 0 ]

-- hier ist s >= 3
create_sub k s = do 
    -- print ("create_sub", k, s)
    -- stelligkeit der kopf-funktion (g) raten
    i  <- eins [ 1 .. min 3 $ s-2 ]
    -- größen aller funktionen (g und hs) raten
    j : js <- summe (i+1) (s-1)
    g  <- create i j
    hs <- sequence $ do j <- js ; return $ create k j
    return $ Sub k ( g : hs )

-- hier ist s >= 3
create_pr  k s = do
    -- print ("create_pr", k, s)
    [ i, j ] <- summe 2 (s-1)
    g <- create (k-1) i
    h <- create (k+1) j
    return $ PR k [ g, h ]

-------------------------------------------------------------------

-- erzeugt eine nichttriviale zweistellige primitiv-rekursive funktion
nontrivial :: Int -- größe (des funktions-ausdrucks)
	   -> Int -- größe der funktionstafel
	   -> IO ( Fun, Tafel )
nontrivial s t =
    repeat_until ( do 
        f <- create 2 s
	return ( f, tabulate f (fromIntegral t, fromIntegral t) )
    ) $ \ ( f, tab ) -> 
            let ((0,0), (h,w)) = bounds tab
		-- nur bis zur hälfte testen (aber mehr ausgeben)
		hh = h `div` 2 ; ww = w `div` 2
		rows = do x <- [ 0 .. hh ]
			  return $ do y <- [ 0 .. ww ] ; return $ tab ! (x,y)
		cols = do y <- [ 0 .. ww ]
			  return $ do x <- [ 0 .. hh ] ; return $ tab ! (x,y)
		hdiag = do d <- [ negate hh .. ww ] -- d == x - y
			   return $ do x <- [ max d 0 .. min hh ( ww + d ) ]
				       return $ tab ! ( x, x - d )
		ndiag = do d <- [ 0 .. hh + ww ] -- d == x + y
			   return $ do x <- [ max 0 (d - ww) .. min d hh  ]
				       return $ tab ! ( x, d - x )
		-- nur wenige verschiedene werte
		is_lame xs = length xs > 3 && 3 > cardinality ( mkSet xs )
		lames xss = filter is_lame xss
	    in     3 > length ( lames rows )
		&& 3 > length ( lames cols )
		&& 4 > length ( lames hdiag )
		&& 4 > length ( lames ndiag )
	        && 3 < cardinality ( mkSet rows )
		&& 3 < cardinality ( mkSet cols )

-------------------------------------------------------------------

summe' k n = do
    putStr $ "summe " ++ show (k, n)
    xs <- summe k n
    putStrLn $ " -> " ++ show xs
    return xs

eins' xs = do
    putStr $ "eins " ++ show xs
    x <- eins xs
    putStrLn $ " -> " ++ show x
    return x
