module Lambda.Step where

import Lambda.Type
import Autolib.Set
import Autolib.TES.Identifier
import Autolib.ToDoc
import Control.Monad ( guard )

is_redex :: Lambda -> Bool
is_redex ( Apply ( Abstract _ _ ) _ ) = True
is_redex _ = False

redex_positions :: Lambda -> [ Position ]
redex_positions t = do
    p <- positions t
    s <- peek t p 
    guard $ is_redex s
    return p

-- | apply beta reduction at root (if possible)
step :: Monad m => Lambda -> m Lambda
step t = case t of
    Apply (Abstract var body) arg -> 
         return $ free_sub var arg body 
    _ -> fail $ show $ text "ist kein Redex:" <+> toDoc t


-- | from left to right
successors :: Lambda -> [ Lambda ]
successors t = 
    step t ++ case t of
        Apply f a -> 
              do f' <- successors f ; return $ Apply f' a
           ++ do a' <- successors a ; return $ Apply f a'
        Abstract v b -> 
              do b' <- successors b ; return $ Abstract v b'
        _ -> []

-- | replace each free occurence of v by a 
-- rename bound variables in b when necessary.
-- implementation is not efficient (will compute FV(b) repeatedly)
free_sub :: Identifier -> Lambda -> Lambda -> Lambda
free_sub v a t = case t of
    Variable w -> if v == w then a else t
    Apply fun arg -> Apply ( free_sub v a fun ) ( free_sub v a arg )
    Abstract w b -> 
        let ( w', b' ) = if w `elementOf` free_variables t
                         then let w' = next_free ( free_variables t )
                              in  ( w', free_rename w w' b )
                         else ( w, b )
        in  Abstract w' $ free_sub v a b'


free_rename :: Identifier -> Identifier -> Lambda -> Lambda
free_rename v w t = case t of
    Variable u -> if v == u then Variable w else t
    Apply fun arg -> Apply (free_rename v w fun) ( free_rename v w arg)
    Abstract u b -> 
        if v == u then t else Abstract u $ free_rename v w b
