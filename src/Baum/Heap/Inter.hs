-- |  (c) F. Pilz, M. Siegburg, J. Waldmann, 2010, License: GPL

module Baum.Heap.Inter where

--  $Id$

import Baum.Heap.Class
import Baum.Heap.Op

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Dot
import Autolib.Hash

step :: ( Heap baum, OpC a, ToDot (baum a), Show (baum a), Hash (baum a) )
      => baum a 
      -> Op a
      -> Reporter ( baum a )
step b op = do
    inform $ text "Operation:" <+> toDoc op
    c <- case op of
	 Insert a -> return $ insert b a
	 DeleteMin -> 
             if isEmpty b
             then reject $ text "der Baum ist leer"
             else return $ deleteMin b
	 DecreaseTo p a -> 
             if ( contains b p ) 
             then return $ decreaseTo b p a
             else reject $ text "diese Position ist nicht im Baum"
	 _        -> 
             reject $ text "diese Operation ist unbekannt"
    inform $ text "Resultat:"
    peng c
    return c

steps :: ( Heap baum, OpC a, ToDot (baum a), Show (baum a), Hash (baum a) )
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

