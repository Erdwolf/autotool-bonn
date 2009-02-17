{-# language MultiParamTypeClasses, FlexibleInstances #-}

module Brainfuck.Machine where

--   $Id$

import Machine.Class
import Machine.Akzeptieren

import qualified Challenger as C
import qualified Machine.Acceptor.Type as A


import Brainfuck.Syntax.Data
import qualified Brainfuck.Env as E
--import Brainfuck.Nachfolger ( folgekonfigurationen )


import Autolib.Reporter
import Autolib.Reporter.Set
import qualified Autolib.Reporter.Checker
-- import qualified Autolib.Subset
import Autolib.Set
import Autolib.Size
import Autolib.ToDoc


instance Compute Statement E.Env where
    next st env = mkSet [ E.next env | not $ E.accepting env ] -- Nachf-Env
    accepting st env = E.accepting env -- fertig?
    depth st env = fromIntegral $ E.depth env -- Schrittzahl

instance  In Statement [ Integer ] E.Env where
    input_reporter st input = do
        return $ E.newEnv input [st]

instance Out Statement [ Integer ] E.Env where
    output_reporter st env = return $ E.output env
    
instance Encode [Integer] where
    -- unär
    encode xs = xs
 
instance Decode [Integer] where
    decode xs = case xs of
      x : _ -> x
      [] -> 0
    
{-
instance BrainfuckC y z => 
        C.Partial A.Acceptor ( A.Type ( Brainfuck y z ) [y] ) 
	    ( Brainfuck y z ) 
  where
    describe p i  = vcat
        [ text "Gesucht ist eine Maschine/ein Programm,"
	, nest 4 $ A.machine_info i
	, text "das diese Sprache akzeptiert:"
	, nest 4 $ A.data_info i
	, text "diese Eingaben sollen akzeptiert werden:"
	, nest 4 $ toDoc $ A.yeah i
	, text "diese Eingaben sollen nicht akzeptiert werden:"
	, nest 4 $ toDoc $ A.noh i
	]
    initial p i   = A.start i
    partial p i b = Autolib.Reporter.Checker.run ( A.check i ) b
    total   p i b = do
        positiv_liste (A.cut i) b $ A.yeah i
        negativ_liste (A.cut i) b $ A.noh  i
        return () -- größe der maschine (hier) ignorieren

-}

