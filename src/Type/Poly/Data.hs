{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, MultiParamTypeClasses #-}
{-# language DeriveDataTypeable #-}

module Type.Poly.Data where

import Autolib.FiniteMap
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size 

import Autolib.TES.Identifier
import Autolib.Xml
import Autolib.Hash
import Autolib.Size

import Data.Typeable
import Control.Monad ( when, forM )
import Data.List ( nub )

instance Container Identifier String where
     label _ = "Identifier"
     pack = show 
     unpack = mknullary

data Type = TyCon Identifier [ Type ]
          | TyVar Identifier
     deriving ( Eq, Ord, Typeable )

instance Hash Type where 
    hash t = case t of
        TyVar v -> hash (0 :: Int, v)
        TyCon f args -> hash (1 :: Int, (f, args))

-- | subexpressions
subtypes t = t : case t of
    TyCon f args -> args >>= subtypes
    _ -> []

angles :: Doc -> Doc
angles p = Autolib.ToDoc.char '<'
     <> p 
     <> Autolib.ToDoc.char '>'

anglist :: [ Doc ] -> Doc
anglist ps = angles ( hsep $ punctuate comma  ps )

protect :: Doc -> Doc
protect d = vcat 
        $ map text $ lines 
        $ concat 
        $ map ( \ c -> case c of
                    '<' -> "&lt;"
                    '>' -> "&gt;"
                    _ -> [c] )
        $ render d

instance ToDoc Type where 
    toDoc t = case t of
        TyVar v -> toDoc v
        TyCon f [] -> toDoc f 
        TyCon f args -> toDoc f <> anglist (  map toDoc args ) 

instance Reader Type where 
    reader = do 
         t <- reader
         args <- option [] $ my_angles $ my_commaSep reader 
         return $ TyCon t args

data Variable = 
     Variable { vname :: Identifier
	      , vtype :: Type
	      }
     deriving ( Eq, Ord, Typeable )

instance ToDoc Variable where
    toDoc v = toDoc (vtype v) <+> toDoc (vname v)
instance Reader Variable where
    reader = do
        t <- reader -- type
        n <- reader -- name
        return $ Variable { vname = n, vtype = t }


data Expression = Apply [ Type ] Identifier  [ Expression ]
    deriving ( Eq, Ord, Typeable )

instance Size Expression where
    size x = case x of
        Apply vs f args -> succ $ sum $ map size args

instance ToDoc Expression where
    toDoc x = case x of
        Apply vs f args -> hsep
            [ if null vs then empty else text "M." <> anglist ( map toDoc vs )
            , toDoc f
            , parens $ fsep $ punctuate comma $ map toDoc args
            ]

instance Reader Expression where
    reader = do
        vs <- option [] $ do
            my_reserved "M"
            my_reserved "."
            my_angles $ my_commaSep reader            
        f <- reader
        args <- my_parens $ my_commaSep reader
        return $ Apply vs f args

data Function = 
     Function { fname :: Identifier
              , tyvars :: [ Identifier ]
	      , arguments :: [ Type ] 
	      , result :: Type
	      }
     deriving ( Eq, Ord, Typeable )

supply :: [ Identifier ]
supply = do
    v <- "xyzpqrst" ++ error "too many parameters in function"
    return $ mknullary [v]

instance ToDoc Function where
    -- vorsicht: alte syntax ist im cache -- na und?
    toDoc f = hsep [ text "static"
                   , if null ( tyvars f ) then empty
                     else anglist ( map toDoc $ tyvars f )
		   , toDoc ( result f )
		   , toDoc ( fname f )
		   , dutch_tuple $ do
		         ( t, z ) <- zip ( arguments f ) supply
		         return $ toDoc t <+> toDoc z
		   ]

instance Reader Function where
    reader = do
        my_reserved "static"
        vs <- option [] $ my_angles $ my_commaSep reader
        when ( vs /= nub vs ) $ fail $ show $ 
            ( text "Typvariablen sind nicht paarweise verschieden:" </> toDoc vs )
        r <- reader -- result type
	n <- reader -- function name
        let repair t = case t of
                TyCon f args -> 
                    if f `elem` vs
                    then if null args then return $ TyVar f 
                         else fail $ show $ 
                            ( text "Typvariable mit Argument:" </> toDoc t )
                    else do
                         args <- forM args repair
                         return $ TyCon f args
        ps <- my_parens $ my_commaSep reader 
        args <- forM ( map vtype ps )  repair
        return $ Function { fname = n
                          , tyvars = vs
			  , arguments = args
			  , result = r 
			  }

data Signature =
     Signature { functions :: [ Function ]
	       }
  deriving ( Typeable )

instance Size Signature where
     size s = length (functions s) 

instance ToDoc Signature where
    toDoc sig = vcat 
       $ do f <- functions sig ; return $ toDoc f <> semi

instance Reader Signature where
    reader = do
        vfs <- many $ do x <- reader ; my_semi ; return x
        return $ Signature { functions = vfs }



data TI = TI { target :: Type
	     , signature :: Signature
	     }
    deriving  ( Typeable )

$(derives [makeReader, makeToDoc] [''TI])

data Conf = Conf { types_with_arities :: [ (Identifier, Int) ]
                 , type_variables :: [ Identifier ]
                 , function_names :: [ Identifier ]
                 , type_expression_size_range :: (Int,Int)  
                 , arity_range :: (Int, Int) -- ^ min arity should be zero
                 , solution_size_range :: (Int,Int)  
		 }
    deriving ( Typeable )

conf :: Conf
conf = Conf { types_with_arities = 
                 read "[ (int, 0), (boolean,0), (List, 1), (Map, 2) ]"
                 , type_variables = read "[ hund, maus, elefant ]"
                 , function_names = read "[goethe, schiller, herder]" 
                 , type_expression_size_range = (1, 5)
                 , arity_range = (0, 2)
                 , solution_size_range = (5, 10 )
	    }

$(derives [makeReader, makeToDoc] [''Conf])
