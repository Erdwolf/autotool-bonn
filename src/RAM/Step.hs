module RAM.Step where

-- $Id$

import RAM.Type
import RAM.Memory
import RAM.State

import qualified RAM.Builtin as B

step :: State -> [ State ]
-- liste aller direkten nachfolge-zustände
-- for deterministische maschinen hier immer länge <= 1
step s = case todo s of
    [] -> [] -- fertig
    x : xs -> case x of
        Builtin { name = n, res = r, args = xs } -> do
            let ( ar, fun ) = B.get n
	    let inputs = map ( get (memory s) ) xs
	    let output = fun inputs
	    stepped $ update ( const output ) s r
	Inc v -> stepped $ update succ s v
	Dec v -> stepped $ update ( \ n -> max 0 (pred n) ) s v
	Loop v p -> do
	    let n = fromIntegral $ get ( memory s ) v
		-- so oft wird der schleifenkörper ausgeführt
	    stepped $  s { todo = concat ( replicate n p ) ++ xs }
	While v p -> stepped $  
	    if 0 == get ( memory s ) v
	    then -- fertig
		 s { todo = xs } 
	    else -- einmal ausführen, dann nochmal testen
		 s { todo = p ++ todo s } 
	    
stepped :: State -> [ State ]     
stepped s = return $ s { schritt = succ $ schritt s
		       , past    = s : past s
		       }

update :: (Integer -> Integer) -> State -> Var -> State
-- wert einer variablen ändern
-- nächsten befehl anwählen
update fun s v = 
    let n = get (memory s) v
    in  s { memory = set ( memory s ) ( v, fun n )
          , todo   = tail $ todo s
	  }

