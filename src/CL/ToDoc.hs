module CL.ToDoc where

import CL.Data

import Autolib.ToDoc

instance ToDoc Identifier where
    toDoc ( Identifier i ) = text i

instance ToDoc Term where
    toDoc t = case t of
        App {} -> 
            let x : xs = spine t 
            in  parens $ toDoc x 
                         <+> fsep ( map toDoc xs )
        Sym s -> toDoc s
