module Fun.Step where

--   $Id$ 

-- auswertung durch (top-)rewriting (mit cache)

-- status besteht aus:
-- input-stack = Liste von auszuwertenden ausdrücken
-- output-stack = Liste von resultaten (Zahlen)

-- eine rechnung verläuft
-- von todo = App fun args : rest , stack = accu
-- zu  todo = rest                , stack = fun args : rest

import Fun.Type
import Fun.State
import qualified Fun.Cache

import qualified RAM.Builtin

step :: State -> [ State ]
step s = case todo s of
    [] -> [] -- fertig
    x : xs -> return $ stepped $ cached s $ case x of

        -- wert von ausdruck e aus top(stack) in cache schreiben
        M (Mark e) -> s { todo = xs
		    , cache = Fun.Cache.insert ( cache s ) e 
		            $ head $ stack s
		    }

        -- auf ergebnis-stack kopieren (kommt das überhaupt vor?)
        Zahl i -> s { todo = xs
		    , stack = i : stack s
		    }

	-- Konstante: einfach in output-stack schreiben
	App (Zero i) args -> 
	    s { todo = xs 
	      , stack = 0 : stack s
	      }

        -- strikte Funktionen (builtins)
	-- argument-ausdrücke zur auswertung in input-stack schreiben
	-- danach builtin-markierung
	App (Succ 1) [ arg ] ->
            s { todo = arg : Builtin_ 1 Suc : marked x : xs }
	App (Decr 1) [ arg ] ->
            s { todo = arg : Builtin_ 1 Pre : marked x : xs }
	App (Builtin i b) args ->
	    s { todo = args ++ Builtin_ i b : marked x : xs }

	-- in output-stack landen dann die werte der argumente
	-- beim verarbeiten von Builtin_ werden diese gelesen
        -- (poppen, rechnen, pushen)
	Builtin_ i b -> 
	    let ( pre, post ) = splitAt i $ stack s
		( arity, f ) = RAM.Builtin.get b
	    in  s { todo = xs
		  , stack = f (reverse pre) : post
		  }

        -- non-strikte funktionen:
	-- einen top-rewrite-schritt ausführen
	-- d. h. top des input-stacks ersetzen
        App ( Proj oben unten ) args ->
            s { todo = args !! (unten - 1) : xs
	      }

        App ( Sub a ( g : fs ) ) args -> 
	    s { todo = App g ( map ( \ f -> App f args ) fs ) : marked x : xs
	      }

        -- Primitive Rekursion:
	-- letzes argument (y) auswerten
	-- If-verzweigung auf input-stack
        App f @ ( PR a [ g, h ] ) args | a == length args -> 
            let (ys , [y]) = splitAt (a - 1) args
            in  s { todo = y : App_ f args : marked x : xs
		  }
        -- wenn y ausgewertet ist, steht ergebnis in output-stack
	App_ f @ ( PR a [ g, h ] ) args ->
	    let z : zs = stack s
		(ys , [y]) = splitAt (a - 1) args
		zero = App g ys
		prev = ys ++ [ Zahl $ pred z ]
		nonzero = App h $ prev ++ [ App f prev ]
            in  s { todo  = ( if 0 == z then zero else nonzero ) : xs
		  , stack = zs
		  }
	
        _ -> error $ "step\n" ++ show s


stepped :: State -> State
stepped s = s { schritt = succ $ schritt s
	      , past = s : past s
	      }

marked x = M $ Mark x

cached :: State -- start
       -> State -- würde bei expliziter rechnung rauskommen
       -> State -- ergebnis
cached s it =
    let x : xs = todo s
    in  case Fun.Cache.find (cache s) x of 
            Nothing -> it
	    Just v  -> s  { todo = xs , stack = v : stack s }

