module HTWK.SS04.CASE.Activity.Analyse where

--   $Id$

import Autolib.Exp
import Autolib.Exp.Inter
import Autolib.NFA
import Autolib.ToDoc 

import Autolib.Set

import qualified NFA.Synthese as S
import NFA.Analyse 

import Inter.Types

import qualified HTWK.SS04.CASE.Activity.Language as L

convert si = 
    AI {  name = S.name si
	     , automat = informed ( toDoc $ S.ausdruck si ) 
	               $ inter (std_sigma (setToList $ S.alphabet si))
	               $ S.ausdruck si
	     , NFA.Analyse.alphabet = S.alphabet si
	     }

make ai = return 
	$ Variant 
	$ analyse "A" ( name ai ) ai

generates :: [ IO Variant ]
generates = map ( make . convert ) L.languages




