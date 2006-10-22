module Baum.Bin where

--  $Id$

import Baum.Type
import Baum.Label

import Control.Monad ( when )
import Autolib.Reporter.Type
import Autolib.ToDoc

-- | check whether tree is binary
-- i. e. each node has 0 or 2 children exactly
checkbin :: ( Symbol c, ToDoc v, Show v )
	 => Term v c -> Reporter ()
checkbin = mapM_ check . subterms where
    check t @ ( Node f args ) 
	= when ( not $ length args `elem` [0, 2] )
               $ reject $ vcat 
		    [ text "Das ist kein binärer Baum,"
		    , text "denn die Wurzel diese Teilbaums"
		    , text "hat" <+> toDoc (length args) <+> text "Kinder:"
		    , nest 4 $ present t
		    ]

-- | construct balanced binary tree of (at most) given size
balanced :: Int -> Term a ()
balanced s | s <= 2 = Node () []
balanced s = 
    let sl = (s - 1) `div` 2
	l = balanced sl
	sr = s - 1 - sl
	r = balanced sr
    in	Node () [ l, r ]

