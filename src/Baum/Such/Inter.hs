module Baum.Such.Inter where

--  $Id$

import Baum.Such.Class
import Baum.Such.Op

import Autolib.Reporter
import Autolib.ToDoc


step :: ( Such baum, OpC a )
      => baum a 
      -> Op a
      -> Reporter ( baum a )
step b op = do
    inform $ text "Operation:" <+> toDoc op
    c <- case op of
	 Insert a -> return $ insert b a
	 Delete a -> return $ delete b a
	 _        -> reject $ text "ist unbekannt"
    inform $ text "Resultat:" <+> form c
    return c

steps :: ( Such baum, OpC a )
      => baum a 
      -> [ Op a ] -- plan (mit Any)
      -> [ Op a ] -- einsendung (ohne Any)
      -> Reporter ( baum a )
steps b [] [] = return b
steps b [] send = reject $ vcat
         [ text "Sie wollen noch diese Operationen ausführen:"
	 , nest 4 $ toDoc send
	 , text "es sind aber keine mehr zugelassen."
	 ]
steps b plan [] = reject $ vcat
         [ text "Es müssen noch diese Operationen ausgeführt werden:"
	 , nest 4 $ toDoc plan
	 ]
steps b (p : plan) (s : send) = do
    conforms p s
    c <- step b s
    steps c plan send

