{-# LINE 1 "Hilbert/Actions.hs.drift" #-}
{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts #-}

module Hilbert.Actions

( Action 
, value, informed_value
)

where

import Boolean.Op
import Expression.Op

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader
import Autolib.TES.Identifier
import Autolib.TES

import Autolib.FiniteMap

import Hilbert.Env

data Action = Sub Identifier ( Env ( Exp Bool ) )
	    | Mopo Identifier Identifier

instance ToDoc Action where
    toDoc ( Sub id env ) = 
        hsep [ text "sub", toDoc id, toDoc env ]
    toDoc ( Mopo left right ) =
        hsep [ text "mopo", toDoc left, toDoc right ]

instance Reader Action where
    reader = 
          do my_reserved "sub" ; i <- reader ; e <- reader ; return $ Sub i e
      <|> do my_reserved "mopo" ; l <- reader ; r <- reader ; return $ Mopo l r


----------------------------------------------------------------

informed_value env act = do
   inform $ toDoc act
   val <- value env act
   inform $ nest 4 $ text "resultat" <+> toDoc val
   return val

value env act @ ( Sub orig sub ) = do
   val <- look env orig
   fm <- mkfm sub
   rapply fm val

value env act @ ( Mopo left right ) = do
   l <- look env left
   r <- look env right
   let whine tag = reject $ vcat 
	     [ text tag
	     , text "links" <+> toDoc left <+> equals <+> toDoc l
	     , text "rechts" <+> toDoc right <+> equals <+> toDoc r
	     ]
   case r of
       Node fun [ pre, post ] | show fun == "->" -> 
           if l == pre 
	   then return post
	   else whine "links passt nicht" 
       _ -> whine "rechts steht keine Implikation"


rapply :: ( ToDoc v, Ord v )
       => FiniteMap v ( Term w c ) -> Term v c 
       -> Reporter ( Term w c )
rapply fm ( Var t ) = 
    case lookupFM fm t of
        Nothing -> reject $ hsep
	    [ text "variable" , toDoc t , text "nicht gebunden"
	    ]
	Just v -> return v
rapply fm ( Node f args ) = do
    xs <- mapM ( rapply fm ) args
    return $ Node f xs
