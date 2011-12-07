module Baum.Such.Inter where

--  $Id$

import Baum.Such.Class
import Baum.Such.Op

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Dot
import Autolib.Hash

step :: ( Such baum, OpC a, ToDot (baum a), Show (baum a), Hash (baum a) )
      => baum a 
      -> Op a
      -> Reporter ( baum a )
step b op = do
    --inform $ text "Operation:" <+> toDoc op
    c <- case op of
	 Insert a -> return $ insert b a
	 Delete a -> return $ delete b a
	 _        -> reject $ text "Operation ist unbekannt"
    --inform $ text "Resultat:"
    return c

steps :: ( Such baum, OpC a, ToDot (baum a), Show (baum a), Hash (baum a) )
      => baum a 
      -> [ Op a ] -- plan (mit Any)
      -> [ Op a ] -- einsendung (ohne Any)
      -> Reporter ( baum a )
steps b [] [] = return b
steps b [] send = do peng b
                     reject $ vcat
                       [ text "Sie wollen noch diese Operationen ausführen:"
	                   , nest 4 $ toDoc send
	                   , text "es sind aber keine mehr zugelassen."
	                   ]
steps b plan [] = do peng b
                     reject $ vcat
                       [ text "Es müssen noch diese Operationen ausgeführt werden:"
	                   , nest 4 $ toDoc plan
	                   ]
steps b (p : plan) (s : send) = do
    conforms p s
    c <- step b s
    steps c plan send
  where
    conforms _ Any = do
        peng b
        reject $ text "Sie sollen Any durch eine Operation ersetzen."
    conforms Any _ = return ()
    conforms x y | x == y = return ()
    conforms x y | x /= y = do
        peng b
        reject $ text "Die Operation" <+> toDoc x <+> text "soll nicht geändert werden." 
