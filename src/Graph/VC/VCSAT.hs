-- $Id$

module Graph.VC.VCSAT

( make_fixed
, make_quiz
)

where

import Graph.VC.SAT ( vc )
import Graph.VC.Central ( VC ( VC ) )
import qualified Graph.VC.ParamSAT as P
import qualified Graph.VC.Input as I

import SAT.Generator
import SAT.Types

import Graph.Util
import Autolib.Graph.Ops ( normalize )

import Inter.Quiz ( Generator , generator , Project , project , quiz )
import Inter.Types ( Make , direct )
import Data.Typeable ( Typeable )

import qualified Challenger as C

-------------------------------------------------------------------------------

data VCSAT = VCSAT deriving ( Eq , Ord , Show , Read , Typeable )

get :: I.Input -> (Graph Int, Int)
get i = let (g,c) = vc (fromIntegral $ I.anzeige_groesse i) (I.formel i) 
	in (normalize g,succ c)

instance C.Partial VCSAT I.Input (Set Int) where
    report  VCSAT i    = C.report  VC (get i)
    initial VCSAT i    = C.initial VC (get i)
    partial VCSAT i ns = C.partial VC (get i) ns
    total   VCSAT i ns = C.total   VC (get i) ns

instance C.Measure VCSAT (Formel,Int) (Set Int) where
    measure VCSAT _ = fromIntegral . cardinality

make_fixed :: Make
make_fixed = direct VCSAT I.i0

instance Generator VCSAT P.Param I.Input where
    generator VCSAT conf _ = do (f,_) <- hgen2 (P.formel conf)
				return $ I.Input 
				   { I.formel = f
				   , I.anzeige_groesse = P.anzeige_groesse conf
				   }

instance Project VCSAT I.Input I.Input where
    project VCSAT fp = fp

make_quiz :: Make
make_quiz = quiz VCSAT P.p0
