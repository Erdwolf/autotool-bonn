module Uni.SS04.Serie6 where

--  $Id$

import Turing
import Turing.Example
import Turing.Bin

-- import Turing_Fun

import Wort
import ToDoc
import Random
import Data.List (inits)

import Array
import Prime
import Sets (mkSet,union)

import qualified Machine.Fun.Type as F
import qualified Machine.Fun.Inter as FI

-- Nummerical
import Fun.Type
import Fun.Machine
import Fun.Check
import qualified Fun.Examples
import qualified RAM.Builtin

import Fun.Quiz
import Fun.Quiz.Type
import Fun.Create
import Fun.Table

import RAM
import RAM.Example
import RAM.Check
import RAM.Builtin

-- Helper
import qualified Uni.SS04.Testliste as TL
-- Machine Parts
import Machine.Class
import qualified Machine.Numerical.Type as N
import qualified Machine.Numerical.Inter as I

-- Challenger

import Challenger.Partial

-- Inter.Face
import qualified Inter.Types as T
import Inter.Wrapper


-- mindea
import NFA.Determine 
import NFA

-- --------------------
tmadd :: IO ( T.Var F.Computer 
               ( F.Type ( Turing Char Int ) String )
               ( Turing Char Int )
             )

-- Forderung an alle Maschinen
checkforall m = 
    do
    check m             -- Standardcheck +
--  deterministisch m   -- determ.check
    return ()
-- ---------------------------------------

-- ----------------------------------------
-- Standard TMs
-- ---------------------------------------
tmadd = do
    let tmaddtest = [ (binstr n ++ "." ++binstr m , binstr (n+m)) | n <- [1..5] , m <- [1..5] ]
    let it = F.Make
           { F.fun_info = text "Addition für Binärzahlen > 0, \\ n,m -> n + m "
           , F.pairs = tmaddtest
           , F.cut = 10000
           , F.check = checkforall
           , F.start = 
             Turing { eingabealphabet = mkSet "01."
                    , arbeitsalphabet = mkSet "01.", leerzeichen = '#'
                    , zustandsmenge = mkSet [ 0, 1, 2, 3, 4, 7 ]
                    , tafel = listToFM [ ( ( '0', 0 ), mkSet [ ( '1', 0, L ) ] )
                                       , ( ( '1', 0 ), mkSet [ ( '1', 0, R ) ] ) 
                                       , ( ( '2', 0 ), mkSet [ ( '1', 0, O ) ] ) 
                                       ]
                    , startzustand = 0
                    , endzustandsmenge = mkSet [ 1 ] 
                    }
           }
    return $ FI.computer "TM" "ADD" it

-- --------------------

-- --------------------------------------------------
-- FUN NOTPRIM
-- --------------------------------------------------

funPrim :: IO ( T.Var N.Computer ( N.Type Fun ) Fun )
funPrim = do
    args <- TL.genTestliste 
                TL.Make 
                  { TL.arity = 1 
                  , TL.len = 10 
                  , TL.fixs = (map return [0,1,2,7,23,51])
                  , TL.randVonBis = (10,100) 
                  }
    let it = N.Make
           { N.fun = \ [ x ] -> if not (prime x) then 1 else 0
           , N.fun_info = text "\\ x -> if istNichtPrimzahl( x ) then 1 else 0"
           , N.args = args
           , N.cut = 20000
           , N.check = \ f -> do
                 Fun.Check.check_builtins RAM.Builtin.every f
                 Fun.Check.check_arity 1 f
                 anzeig $ tabelle1 f 9
           , N.start = Fun.Examples.plus
           }
    return $ I.computer "FUN" "NOTPRIM" it

-- --------------------------------------------------------------------------------
-- WHILE NOTPRIM
-- --------------------------------------------------------------------------------

stdbuiltins = mkSet [ RAM.Builtin.Copy , RAM.Builtin.Plus , RAM.Builtin.Minus ]

conformwhile bs prog = 
    do
    builtins bs prog

whileexample = [ While "x1" [ Loop "x2" [ Inc "x0" ],Dec "x1"]]

nums :: IO [Integer]
nums = do 
    let fixed = [ 0 .. 10] ++ [ 23 , 101 ]
    rnds <- sequence $ replicate 10 $ randomRIO ( 10, 100 )
    return $ fixed ++ rnds


iowhiletests :: IO [[Integer]]
iowhiletests = do
    xs <- nums 
    return $ map return xs

whilenotprim = do 
    tests <- iowhiletests
    let it = N.Make
             { N.fun_info = text "\\ x -> if istNichtPrimzahl( x ) then 1 else 0"
             , N.fun      = \ [ x ] -> if not (prime x) then 1 else 0
             , N.args     = tests 
             , N.cut      = 10000
             , N.check    = conformwhile $ union stdbuiltins $ mkSet [ RAM.Builtin.Mod ]
             , N.start    = whileexample
             }
    return $ I.computer "WHILE" "NOTPRIM" it



-- --------------------------------------------------------------------------------
-- NEA -> min DEA
-- --------------------------------------------------------------------------------

generateMinDea :: IO T.Variant
generateMinDea = return $ T.Variant 
           $ determine "DEA" "MIN" $
             Conf { alpha = mkSet "ab"
                  , nea_size = 4
                  , min_dea_size = 6
                  , max_dea_size = 10
                  , mustmini = True -- soll auch minimiert werden
                  , randomizer = 1
                  }

-- --------------------------------------------------------------------------------
-- Generate
-- --------------------------------------------------------------------------------
generate :: [ IO T.Variant ]
generate = 
    [ do i <- tmadd   ; return $ T.Variant i
    , do i <- funPrim ; return $ T.Variant i
    , do i <- whilenotprim ; return $ T.Variant i
    , generateMinDea
    ]