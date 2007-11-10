module Specify.Check where

import Specify.Definition
import Specify.Constraint
import Specify.Eval

import Autolib.Reporter
import Autolib.ToDoc

full :: System
       -> Program
       -> Int
       -> Reporter ()
full ( System cs ) p num =
    mapM_ ( \ c -> single c p num ) cs

single :: Constraint
       -> Program
       -> Int
       -> Reporter ()
single con @ ( Constraint vars body ) p num = do
    inform $ text "Prüfe Constraint:" <+> toDoc con
    sequence_ $ do
        values <- take num $ candidates $ length vars
        return $ do
	    let pairs = zip vars values
	    x <- eval ( extend p pairs ) body
            when ( not x ) $ reject $ vcat
	        [ if null pairs then empty else text "Für Belegung" <+> toDoc pairs <+> text ":"
		, text "Constraint ist nicht erfüllt."	  
		]

-- | all tuples of naturals of given length,
-- listed in order of increasing sum.
-- produces infinite list.
candidates 0 = return []
candidates l = do
    let handle s 1 = return [s]
	handle s l = do
	    x <- [ 0 .. s ]
	    xs <- handle (s - x) (l-1)
	    return $ x : xs
    s <- [ 0 .. ]
    handle s l

