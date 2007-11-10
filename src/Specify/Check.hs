module Specify.Check where

import Specify.Definition
import Specify.Constraint
import Specify.Eval

import Autolib.Reporter
import Autolib.ToDoc

full :: System
       -> Program
       -> Int
       -> Reporter [ ( Maybe Bool, Doc ) ]
full ( System cs ) p num = do
    results <- mapM ( \ c -> single c p num ) cs
    return $ concat results

single :: Constraint
       -> Program
       -> Int
       -> Reporter [ ( Maybe Bool, Doc ) ]
single con @ ( Constraint vars body ) p num = do
    sequence $ do
        values <- take num $ candidates $ length vars
        return $ do
	    let pairs = zip vars values
	    let ( mmx, doc ) = export $ do
                     inform $ text "Constraint:" <+> toDoc con
                     when ( not $ null pairs )
                          $ inform $ text "Belegung:" <+> bform pairs
                     nested 4 $ do
                         inform $ text "dabei berechnete Funktionswerte:"
                         eval ( extend p pairs ) body
            case mmx of
                Nothing -> reject $ doc
                Just mx -> return ( mx, doc )

bform pairs = hsep $ do
    ( i, v ) <- pairs
    return $ hsep [ toDoc i, text "=", toDoc v, semi ]

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

