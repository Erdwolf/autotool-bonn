module Grammatik.Checker where

-- $Id$

import Grammatik.Type
import Reporter
import ToDoc


data Type =
     Make { nametag :: String
	     , condition :: Doc
	     , investigate :: Grammatik -> Reporter ()
	     }

make :: String
	-> Doc 
	-> ( Grammatik -> Reporter () ) 
	-> Type
make tag doc inv = Make { nametag = tag, condition = doc, investigate = inv }

run :: Type -> Grammatik -> Reporter ()
run c g = do
    inform $ condition c
    investigate c g





