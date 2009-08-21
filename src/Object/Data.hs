-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module Object.Data where

--  $Id$


import Autolib.ToDoc
import Autolib.Reader

import Autolib.TES.Identifier
import Autolib.Xml
import Autolib.Hash
import Autolib.Size

import Data.Typeable

----------------------------------------------------------------------------

data Name = Name Identifier 
     deriving ( Eq, Ord, Typeable )

instance Hash Name where hash (Name t) = hash t

instance ToDoc Name where toDoc (Name t) = toDoc t
instance Reader Name where 
     reader = do t <- reader; return $ Name t

instance Container Identifier String where
     label _ = "Identifier"
     pack = show 
     unpack = mknullary

----------------------------------------------------------------------------

data Expression = Reference Name
		| Dot  Expression Name
		| Call Expression [ Expression ]
    deriving ( Eq, Ord, Typeable )

instance ToDoc Expression where
    toDoc ( Reference i ) = toDoc i
    toDoc ( Dot x i ) = toDoc x <> text "." <> toDoc i
    toDoc ( Call f args ) = toDoc f <+> dutch_tuple ( map toDoc args )

instance Reader Expression where
    reader = do
       i <- reader 
       dcs <- many dc
       return $ foldr (.) id ( reverse dcs ) $ Reference i

dc =   do my_dot ; i <- reader ; return $ \ x -> Dot x i
   <|> my_parens ( do xs <- my_commaSep reader ; return $ \ f -> Call f xs )

----------------------------------------------------------------------------

data Variable = 
     Variable { vtype :: Name
	      , vname :: Name
	      }
     deriving ( Eq, Ord, Typeable )

instance ToDoc Variable where
    toDoc v = toDoc (vtype v) <+> toDoc (vname v)
instance Reader Variable where
    reader = do
        t <- reader -- type
        n <- reader -- name
        return $ Variable { vname = n, vtype = t }

----------------------------------------------------------------------------

data Method = 
     Method { fname :: Name
	      , arguments :: [ Name ] 
	      , result :: Name
	      }
     deriving ( Eq, Ord, Typeable )

supply :: [ Identifier ]
supply = do
    v <- "xyzpqrst" ++ error "too many parameters in method"
    return $ mknullary [v]

instance ToDoc Method where
    -- vorsicht: alte syntax ist im cache -- na und?
    toDoc f = hsep [ toDoc ( result f )
		   , toDoc ( fname f )
		   , dutch_tuple $ do
		         ( t, z ) <- zip ( arguments f ) supply
		         return $ toDoc t <+> toDoc z
		   ]

instance Reader Method where
    reader = do
        r <- reader -- result type
	n <- reader -- method name
        ps <- my_parens $ reader `Autolib.Reader.sepBy` my_comma  -- parameters
        return $ Method { fname = n
			  , arguments = map vtype ps
			  , result = r 
			  }

----------------------------------------------------------------------------

data Class =
     Class { name :: Name
           , extends :: Maybe Name
	   , declarations :: [ Declaration ]
	   }
  deriving ( Typeable )

instance Size Class where
     size s = length (declarations s) 


instance ToDoc Class where
    toDoc c = vcat 
      [	    text "class" 
	<+> toDoc ( name c )
        <+> case extends c of 
	       Nothing -> empty 
	       Just i  -> text "extends" <+> toDoc i
        <+> text "{"
      , nest 4 $ vcat $ map toDoc $ declarations c
      , text "}"
      ]
 
instance Reader Class where
    reader = do
        my_reserved "class"
        n <- reader -- name
        e <- option Nothing $ do
               my_reserved "extends"
	       i <- reader
	       return $ Just i
        ds <- my_braces $ many reader
        return $  Class
	       { name      = n
	       , extends   = e
	       , declarations = ds
	       }

---------------------------------------------------------------------------

data Declaration =
     Declaration { access :: Access
		 , static :: Static
		 , member :: Member
		 }
     deriving ( Typeable )

instance ToDoc Declaration where
    toDoc d =  toDoc ( access d )
	   <+> toDoc ( static d )
	   <+> toDoc ( member d )

instance Reader Declaration where
    reader = do
        a <- reader
	s <- reader
	m <- reader
	return $ Declaration { access = a, static = s, member = m }

---------------------------------------------------------------------------

data Static = Static | Dynamic
   deriving ( Eq, Ord, Typeable )

instance ToDoc Static where
    toDoc a = case a of
       Static -> text "static"
       Dynamic -> empty

instance Reader Static where
    reader = do my_reserved "static" ; return Static
	 <|> do                        return Dynamic

---------------------------------------------------------------------------

data Access = Public | Private | Protected | Default
   deriving ( Eq, Ord, Typeable )

instance ToDoc Access where
    toDoc a = case a of
       Public -> text "public"
       Private -> text "private"
       Protected -> text "protected"
       Default -> empty

instance Reader Access where
    reader = do my_reserved "public" ; return Public
         <|> do my_reserved "private" ; return Private
         <|> do my_reserved "protected" ; return Protected
	 <|> do                           return Default

---------------------------------------------------------------------------

data Member = V Variable
	 | M Method 
	 | C Class
    deriving ( Typeable )


instance ToDoc Member where
    toDoc ( C c ) =  toDoc c
    toDoc ( V v ) =  toDoc v <> semi
    toDoc ( M m ) =  toDoc m <> semi

instance Reader Member where
    reader =  do c <-     reader ;          return $ C c
          <|> do m <- try reader ; my_semi; return $ M m
          <|> do v <-     reader ; my_semi; return $ V v
