module Type.Poly.Infer where

import Type.Poly.Data
import Type.Poly.Tree

import Autolib.Reporter.Type hiding ( result )
import Autolib.ToDoc

import Autolib.TES.Identifier

import Control.Monad ( guard, when, forM )

import qualified Data.Map as M
import Autolib.FiniteMap () -- instances

infer :: Signature -> Expression -> Reporter Type
infer sig exp = do
    inform $ text "berechne Typ für Ausdruck:" <+> toDoc exp
    t <- nested 4 $ case exp of
        Apply targs n args -> 
	    case do f <- functions sig ; guard $ fname f == n ; return f
	    of  [ f ] -> do
		    inform $ text "Funktion hat Deklaration:" <+> toDoc f
                    silent $ assert ( length targs == length ( tyvars f ) ) 
                        $ text "Anzahl der Typ-Argumente stimmt mit Deklaration überein?"
                    
                    let sub = M.fromList $ zip ( tyvars f ) targs 
                    argtypes <- forM ( arguments f ) $ apply sub
                    rtype <- apply sub $ result f
                    let finst = Function { fname = fname f
                                      , tyvars = []
                                      , arguments = argtypes
                                      , result = rtype
                                      }
                    when ( not $ null targs ) $ inform 
                         $ text "die instantiierte Deklaration der Funktion ist"
                         </> toDoc finst

		    silent $ assert ( length args == length ( arguments finst ) )
			   $ text "Anzahl der Argumente stimmt mit Deklaration überein?" 
		    sequence_ $ do
		        ( k, arg ) <- zip [1..] args
                        return $ do
                            inform $ text "prüfe Argument Nr." <+> toDoc k
			    t <- nested 4 $ infer sig arg
			    assert ( t == arguments finst !! (k-1) )
				   $ text "Argument-Typ stimmt mit instantiierter Deklaration überein?"
		    return $ result finst
                [   ] -> reject $ text "ist nicht deklarierte Funktion."
		fs    -> reject $ vcat
		         [ text "ist mehrfach deklarierte Funktion:"
			 , toDoc fs
			 ]
    inform $ text "hat Typ:" <+> toDoc t
    return t

apply :: M.Map Identifier Type
      -> Type
      -> Reporter Type
apply sub t = case t of
    TyVar v -> case M.lookup v sub of
          Nothing -> reject $ vcat [ text "Variable" <+> toDoc v
                                   , text "nicht gebunden in" <+> toDoc sub
                                   ]
    TyCon f args -> do
        bargs <- forM args $ apply sub
        return $ TyCon f bargs
