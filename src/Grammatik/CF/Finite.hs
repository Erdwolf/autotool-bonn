
-- | stellt fest, ob geg. CF-Grammatik unendliche Sprache erzeugt

module Grammatik.CF.Finite where

import Grammatik.Type

import Grammatik.Reduziert
import Grammatik.CF.Epsfrei
import Grammatik.CF.Kettenfrei

import qualified Relation 
import Reporter
import ToDoc
import qualified Reporter.Checker as C

infinite :: C.Type Grammatik
infinite = C.make 
	 "infinite"
	 ( text "Die erzeugte Sprache soll unendlich sein." )
	 ( \ g -> when ( finite g ) $ reject $ text "ist endlich" )

finite :: Grammatik -> Bool
finite g = 
    let h = kettenfrei $ epsfrei $ reduktion g
    in  null $ loopings h

-- | Liste der Variablen V mit Ableitungen V ->> .. V ..
loopings :: Grammatik -> [ Char ]
loopings g = do
    let depends = Relation.make $ do 
            ( [l], rs ) <- setToList $ regeln g
	    r <- rs
	    guard $ elementOf r $ nichtterminale g
	    return (l, r)
    (l, r) <- Relation.pairs $ Relation.trans depends
    guard $ l == r
    return l


