module Baum.Bin where

--  $Id$

import Baum.Type
import Control.Monad ( when )
import Reporter.Type
import ToDoc
import Random
import Util.Zufall

checkbin :: Baum -> Reporter ()
checkbin = mapM_ check . subterms where
    check t @ ( Node f args ) 
	= when ( not $ length args `elem` [0, 2] )
               $ reject $ vcat 
		    [ text "Das ist kein binärer Baum,"
		    , text "denn die Wurzel diese Teilbaums"
		    , text "hat" <+> toDoc (length args) <+> text "Kinder:"
		    , nest 4 $ present t
		    ]

-- | make "random" binary tree of given size (must be odd)
-- contains no variables
rollbin :: Int -> IO ( Term a () )
rollbin s | s < 2 = return $ Node () []
rollbin s = do
    x <- randomRIO (1, s-2)
    let sl = if even x then pred x else x
    let sr = s - 1 - sl
    l <- rollbin sl
    r <- rollbin sr
    return $ Node () [ l, r ]


