-- $Id$

module Graph.VC.VCSAT

-- ( make_fixed
-- , make_quiz
-- )

where

import Graph.VC.SAT ( vc )
import Graph.VC.Central ( VC ( VC ) )

import SAT.Param
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

get :: Formel -> (Graph Int, Int)
get f = let (g,c) = vc f in (normalize g,succ c)

instance C.Partial VCSAT Formel (Set Int) where
    report  VCSAT f    = C.report  VC (get f)
    initial VCSAT f    = C.initial VC (get f)
    partial VCSAT f ns = C.partial VC (get f) ns
    total   VCSAT f ns = C.total   VC (get f) ns

instance C.Measure VCSAT Formel (Set Int) where
    measure VCSAT _ = fromIntegral . cardinality

make_fixed :: Make
make_fixed = direct VCSAT $ And [ Or [ Pos "x" , Pos "y" , Pos "z" ] 
				, Or [ Neg "x" , Pos "y" , Neg "z" ]
				]

instance Generator VCSAT Param Formel where
    generator VCSAT conf _ = fmap fst $ hgen2 conf

instance Project VCSAT Formel Formel where
    project VCSAT f = f

make_quiz :: Make
make_quiz = quiz VCSAT (p 5)
