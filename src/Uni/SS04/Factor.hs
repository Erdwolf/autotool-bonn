module Uni.SS04.Factor where

--   $Id$

import Prime

import Machine.Class
import Machine.Akzeptieren

import Turing
import Turing.Machine
import Turing.Konfiguration
import qualified Turing.Example as E

import qualified Machine.Acceptor.Type as A
import qualified Machine.Acceptor.Inter as I

import qualified Inter.Types as T
import Inter.Wrapper

import Reporter hiding ( output )
import ToDoc
import Size
import Data.List (partition)

import Reporter.Checker


type Dat = Int

cut = 2000 :: Int

ok :: Dat-> Bool
ok x = x >= 4 && not ( prime ( toInteger x ) )


( yeah, noh ) = partition ok $ [ 0 .. 5 ] ++ [ 10 .. 13 ] ++ [ 15, 17 ] :: ([Dat],[Dat])

unibranch :: Turing Char Dat-> Reporter ()
unibranch m = do
    let nondets = do 
        thing @ ( (x, y), yzs ) <- fmToList $ tafel m
        guard $ 1 < cardinality yzs
        return thing
    inform $ text "Die Maschine enth�lt diese nichtdeterministischen Verzweigungen:"
    inform $ nest 4 $ toDoc nondets
    when ( 1 /= length nondets ) 
     $ reject $ text "Die Maschine soll genau eine nichtdeterministische Verzweigung enthalten."

bewerte :: String -> Turing Char Dat-> IO ( Maybe Int)
bewerte mat student = do
    v <- generate
    wrapper v mat student

generate :: IO ( T.Var A.Acceptor 
                 ( A.Type  ( Turing Char Dat) String ) 
                 ( Turing Char Dat)
               )
generate = do
    let make n = replicate ( fromIntegral n ) '1'
    let it = 
            A.Make { A.data_info = text "{ 1 ^ (x*y) | 2 <= x && 2 <= y }"
                   , A.machine_info = text "NDTM"
                   , A.yeah = map make yeah
                   , A.noh  = map make noh
                   , A.cut = cut
--                   , A.check = Reporter.Checker.wahr
                    , A.check = Reporter.Checker.make 
		               "unibranch"
		               ( text "genau eine Verzweigung" )
		               $ \ prog -> do
                         check prog
                         unibranch prog
                   , A.start = E.student
                   }
    return $ I.acceptor "TM" "FACTOR" it





