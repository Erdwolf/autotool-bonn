module Language.Mutate where

-- -- $Id$

import Language.Type

import Autolib.Util.Zufall
import Autolib.Util.Edit

import Data.List ( nub, partition )

mutate :: Language -> Language
mutate l = 
    l { sample = \ c n -> do 
	    ( yeah, noh ) <- mutabor l c n
	    einige c yeah
      , anti_sample = \ c n -> do
	    ( yeah, noh ) <- mutabor l c n
	    einige c noh
      }
	    
mutabor l w n = do
    ws <- samples      l w n
    vs <- anti_samples l w n
    let vws = ws ++ vs
    ms <- mapM edits vws
    let large = maximum [ length w | w <- vws ]
    let mutants = filter ( \ w -> length w <= large ) ms
    return $ partition (contains l) $ nub $ vws ++ mutants
