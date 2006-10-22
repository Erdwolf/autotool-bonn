module Uni.SS04.Serie1 where

--  $Id$

import Turing
import Turing.Example
import Turing.Bin

-- import Turing_Fun

import Wort
import ToDoc
import Random
import Data.List (inits)

import qualified Machine.Fun.Type as F
import qualified Machine.Fun.Inter as FI

import qualified Machine.Clock.Type as C
import qualified Machine.Clock.Inter as CI

import qualified Inter.Types as T
import Inter.Wrapper

-- --------------------
tmpred,tmexpo :: IO ( T.Var F.Computer 
					  ( F.Type ( Turing Char Int ) String )
					  ( Turing Char Int )
					)

tmstep1,tmstep2,tmstep3 :: IO ( T.Var C.Clock
								( C.Type ( Turing Char Int ) )
								( Turing Char Int )
							  )

-- Forderung an alle Maschinen
checkforall m = 
	do
	check m 			-- Standardcheck +
	deterministisch m 	-- determ.check
	return ()
-- ---------------------------------------
-- Maschinen mit Schrittbeschränkungen
-- ---------------------------------------
tmstep1 = do
    let it = C.Make
	   { C.fun = \ n -> n + 1
	   , C.fun_info = text "\\ n -> n + 1"
	   , C.args = [ 0 .. 7 ] ++ [ 13 ]
	   , C.cut = 1000
	   , C.check = checkforall
	   , C.start = stepexample
	   }
    return $ CI.clock "TM" "STEP1" it

tmstep2 = do
    let it = C.Make
	   { C.fun = \ n -> (2*n) + 3
	   , C.fun_info = text "\\ n -> 2n + 3"
	   , C.args = [ 0 .. 7 ] ++ [ 13 ]
	   , C.cut = 1000
	   , C.check = checkforall
       , C.start = stepexample
       }
    return $ CI.clock "TM" "STEP2" it

tmstep3 = do
    let it = C.Make
		{ C.fun = \ n -> (n + 1)^2
		, C.fun_info = text "\\ n -> (n+1)^2"
		, C.args = [ 0 .. 7 ] ++ [ 13 ]
		, C.cut = 1000
		, C.check = checkforall
		, C.start = stepexample
		}
    return $ CI.clock "TM" "STEP3" it

-- Beispiel für diese Maschinen
stepexample = 
    Turing { eingabealphabet = mkSet "1."
           , arbeitsalphabet = mkSet "1.", leerzeichen = '#'
           , zustandsmenge = mkSet [ 0, 1, 2, 3, 4, 7 ]
           , tafel = listToFM [ ( ( '1', 0 ), mkSet [ ( '1', 1, L ) ] )
                              , ( ( '.', 1 ), mkSet [ ( '1', 2, R ) ] ) 
                              , ( ( '.', 2 ), mkSet [ ( '1', 2, O ) ] ) 
                              ]
           , startzustand = 0
           , endzustandsmenge = mkSet [ 2 ] 
           }
-- ----------------------------------------
-- Standard TMs
-- ---------------------------------------
tmpred = do
    let tmpredtest = [ (binstr n,binstr (n-1)) | n <- [1..5] ++ [7..9] ++ [13] ]
    let it = F.Make
           { F.fun_info = text "Vorgängerfunktion für Binärzahlen > 0, \\ n -> n - 1"
           , F.pairs = tmpredtest
           , F.cut = 10000
           , F.check = checkforall
           , F.start = 
             Turing { eingabealphabet = mkSet "01"
                    , arbeitsalphabet = mkSet "01", leerzeichen = '#'
                    , zustandsmenge = mkSet [ 0, 1, 2, 3, 4, 7 ]
                    , tafel = listToFM [ ( ( '0', 0 ), mkSet [ ( '1', 0, L ) ] )
                                       , ( ( '1', 0 ), mkSet [ ( '1', 0, R ) ] ) 
                                       , ( ( '2', 0 ), mkSet [ ( '1', 0, O ) ] ) 
                                       ]
                    , startzustand = 0
                    , endzustandsmenge = mkSet [ 1 ] 
                    }
           }
    return $ FI.computer "TM" "PRED" it

tmexpo = do 
    let tmexpotest = [ (unstr n,unstr (2*n)) | n <- ( [1..7] ++ [11,13] )]
    let it = F.Make
           { F.fun_info = text "expo('1'^n) = '1'^(2n) für unäre Zahlen"
           , F.pairs = tmexpotest
           , F.cut = 10000
           , F.check = checkforall
           , F.start =
             Turing { eingabealphabet = mkSet "1"
                    , arbeitsalphabet = mkSet "12", leerzeichen = '#'
                    , zustandsmenge = mkSet [ 0, 1, 2, 3, 4, 7 ]
                    , tafel = listToFM [ ( ( '1', 0 ), mkSet [ ( '1', 1, L ) ] )
                                       , ( ( '1', 1 ), mkSet [ ( '1', 2, R ) ] ) 
                                       , ( ( '1', 2 ), mkSet [ ( '1', 3, O ) ] ) 
                                       ]
                    , startzustand = 0
                    , endzustandsmenge = mkSet [ 3 ] 
                    }
           }
    return $ FI.computer "TM" "EXPO" it

-- --------------------

generate :: [ IO T.Variant ]
generate = 
    [ do i <- tmpred   ; return $ T.Variant i
    , do i <- tmexpo   ; return $ T.Variant i
    , do i <- tmstep1  ; return $ T.Variant i
    , do i <- tmstep2  ; return $ T.Variant i
    , do i <- tmstep3  ; return $ T.Variant i
    ]