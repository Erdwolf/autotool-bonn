module Finite.Run where

import Finite.Type

import Array
import ToDoc
import Set
import FiniteMap
import Maybe

class ( ToDoc state, Show question, Show action ) =>
      World state question action | state -> action question where
    ask :: state -> question -> String
    act :: state -> action -> state

run :: World s q a 
    => Program q a -> s -> ( Bool, Doc )
run p s = execute (runtime p s)

data Runtime q a s =
     Runtime { program :: Array Int ( Statement q a )
	     , state   :: s   -- external state
	     , pc      :: Int -- next instruction
	     , step    :: Int -- number of instruction so far
	     , labels  :: FiniteMap String Int
	     , values  :: FiniteMap String String
	     , traced_labels :: Set String
	     , watched_values :: Set String
	     }

runtime :: Program q a -> s -> Runtime q a s
runtime p s = r 
  where r = Runtime 
	    { program = listArray (1, length $ statements p) $ statements p
	    , state = s
	    , pc = fromMaybe 1 $ lookupFM (labels r) "start"
	    , step = 0
	    , labels = listToFM $ do
	          ( i, Label l ) <- assocs $ program r
		  return (l, i)
	    , values = emptyFM
	    , traced_labels = trace p
	    , watched_values = watch p
	    }

inform :: World s q a => Runtime q a s -> Doc
inform r =
    fsep $ punctuate comma
	 [ text "step:" <+> toDoc ( step r )
	 , text "state:" <+> toDoc ( state r )
	 , text "mem:" <+> toDoc ( values r )
	 , text "pc:" <+> toDoc ( pc r )
	 , text "next:" <+> toDoc ( program r ! pc r )
	 ]

execute :: World s q a => Runtime q a s -> (Bool, Doc)
execute r =
    let 
	inst =  program r ! pc r
	i = inform r
	informed ( f, d ) t = ( f, t <+> i $$ d )
    in 
       case inst of
         Label l -> 
	     let
		 track x = if l `elementOf` ( traced_labels r )
			   then informed x (text "TRACE") else x
	     in	 track $ next r

	 Accept -> informed ( True, empty ) $ text "done"
	 Reject -> informed ( False, empty ) $ text "done"

	 Goto lab -> goto r lab
	 If (x, y) lab -> 
	    if evaluate r x == evaluate r y
	    then goto r lab else next r

	 Assign loc exp -> 
	     let val = evaluate r exp
		 look x = if loc `elementOf` watched_values r 
			  then informed x (text "WATCH") else x
	     in	 look $ next $ r { values = addToFM (values r) loc val }

	 Do action -> next $ r { state = act (state r) action }


evaluate :: World s q a => Runtime q a s -> Value q -> String
evaluate r ( Const s ) = s
evaluate r ( Ref loc ) = fromMaybe "" $ lookupFM (values r) $ loc
evaluate r ( Ask q   ) = ask ( state r ) q 


goto :: World s q a => Runtime q a s -> String -> ( Bool, Doc )
goto r lab = 
    execute $ r { pc = fromMaybe 0 $ lookupFM ( labels r ) lab
		, step = succ $ step r
		}

next :: World s q a => Runtime q a s -> ( Bool, Doc )
next r = 
    execute $ r { pc = succ $ pc r
		, step = succ $ step r
		}
