module HTWK.SS04.CASE where

--  $Id$

import qualified HTWK.SS04.CASE.Activity.Analyse
import qualified HTWK.SS04.CASE.LCS

aufgaben = []
    ++ HTWK.SS04.CASE.Activity.Analyse.generates
    ++ HTWK.SS04.CASE.LCS.generates
