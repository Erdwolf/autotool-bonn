module Algebraic.Set where


import qualified Algebraic.Nested.Type as Nested
import Algebraic.Nested.Op 
import Algebraic.Nested.Restriction

import Algebraic2.Class
import Algebraic2.Instance as AI
import Condition

import Autolib.ToDoc
import Autolib.Choose
import Autolib.Reader
import Autolib.Size
import Autolib.FiniteMap

import qualified Autolib.Reporter.Set

import Data.Typeable

data Algebraic_Set = Algebraic_Set deriving ( Read, Show, Typeable )

instance Algebraic Algebraic_Set () ( Nested.Type Integer ) where
    -- evaluate         :: tag -> Exp a -> Reporter a
    evaluate tag bel exp = do
        v <- tfoldB bel inter exp
	inform $ vcat [ text "Der Wert Ihres Terms ist"
		      , nest 4 $ toDoc v
		      ]
	return v
        
    -- equivalent       :: tag -> a -> a -> Reporter Bool
    equivalent tag a b = do
        inform $ text "stimmen die Werte überein?"
	let ab = difference a b
	    ba = difference b a
	    err = union ab ba
        let ok = is_empty err 
	when ( not ok ) $ reject $ vcat 
             [ text "Nein, diese Elemente kommen nur in jeweils einer der Mengen vor:"
	     , nest 4 $ toDoc err
	     ]
	return ok


    -- some_formula     :: tag -> Algebraic.Instance.Type a -> Exp a
    some_formula tag i = read "pow (1 + pow (2)) "

    default_context tag = ()

    -- default_instance :: tag -> Algebraic.Instance.Type a
    default_instance tag = AI.Make
        { target = Nested.example
	, context = ()
          , description = Nothing
	  , operators = default_operators tag
	  , predefined = listToFM 
	      [ (read "A", read "{1,3,5,6}" )
	      , (read "B", read "{2,3,6,7}" )
	      ]		  
          , max_size = 7
	}

    default_operators tag = bops

