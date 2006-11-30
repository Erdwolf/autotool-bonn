module Lambda.Step where

import Lambda.Type
import Lambda.Alpha
import Lambda.Tree ( peng )

import Autolib.Set
import Autolib.TES.Identifier
import Autolib.ToDoc
import Autolib.Reporter

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

derive :: Lambda -> [ Int ] -> Reporter Lambda
derive start xs = do
    ts <- derivation start xs
    return $ last ts

derivation :: Lambda -> [ Int ] -> Reporter [ Lambda ]
derivation t xxs = do
    inform $ vcat 
        [ text "*****************************************************"
        , text "aktueller Term ist" 
        , nest 4 $ toDoc t 
        ]
    peng t

    inform $ text "Liste der Redex-Positionen ist ..."
    let ps = redex_positions t
    
    inform $ nest 4 $ case ps of
        [] -> text "leer."
        ps -> vcat $ map toDoc $ do
                     ( n, p ) <-  zip [ 0 :: Int .. ] ps
                     redex <- peek t p 
                     return ( n, p, redex )
    case xxs of
        [] -> return [ t ]
        x : xs -> do
            inform $ text "Sie wählen den Redex Nummer" <+> toDoc x
            silent $ assert ( 0 <= x && x < length ps )
                   $ text "Nummer ist zulässig?"
            let p = ps !! x
            redex <- peek t p
            inform $ vcat [ text "Redex ist", nest 4 $ toDoc redex ]
            redukt <- step redex
            inform $ vcat [ text "Redukt ist", nest 4 $ toDoc redukt ]
            result <- poke t ( p, redukt )
            ts <- derivation result xs
	    return $ t : ts


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

