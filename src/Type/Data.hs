{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-} 

module Type.Data where

--  $Id$

import Autolib.FiniteMap
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size 

import Autolib.TES.Identifier
import Autolib.Xml
import Autolib.Hash
import Autolib.Size

import Data.Typeable

instance Container Identifier String where
     label _ = "Identifier"
     pack = show
     unpack = mknullary

data Type = Type Identifier
          | PointerTo Type
     deriving ( Eq, Ord, Typeable )

instance Hash Type where hash (Type t) = hash t

instance ToDoc Type where
    toDoc (Type t) = toDoc t
    toDoc (PointerTo t) = toDoc t <> text "*"
instance Reader Type where
     reader = do
        t <- reader
        p <- option id $ do
               my_symbol "*"
               return PointerTo
        return $ p $ Type t

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

data Function =
     Function { fname :: Identifier
              , arguments :: [ Type ]
              , result :: Type
              , static :: Bool
              }
     deriving ( Eq, Ord, Typeable )

supply :: [ Identifier ]
supply = do
    v <- "xyzpqrst" ++ error "too many parameters in function"
    return $ mknullary [v]

instance ToDoc Function where
    -- vorsicht: alte syntax ist im cache -- na und?
    toDoc f = (if static f then text "static" else text "") <>
              hsep [ toDoc ( result f )
                   , toDoc ( fname f )
                   , dutch_tuple $ do
                         ( t, z ) <- zip ( arguments f ) supply
                         return $ toDoc t <+> toDoc z
                   ]

instance Reader Function where
    reader = do
        s <- option False $ do
               my_reserved "static"
               return True
        r <- reader -- result type
        n <- reader -- function name
        ps <- my_parens $ reader `Autolib.Reader.sepBy` my_comma  -- parameters
        return $ Function { fname = n
                          , arguments = map vtype ps
                          , result = r
                          , static = s
                          }

data Signature =
     Signature { functions :: [ Function ]
               , variables :: [ Variable ]
               }
  deriving ( Typeable )

instance Size Signature where
     size s = length (functions s) + length (variables s)

instance ToDoc Signature where
    toDoc sig = vcat 
       $  do v <- variables sig ; return $ toDoc v <> semi
       ++ do f <- functions sig ; return $ toDoc f <> semi

instance Reader Signature where
    reader = do
        let vf =  
                 do f <- try reader ; return $ Right f -- function
             <|> do v <-     reader ; return $ Left  v -- variable
        vfs <- many $ do x <- vf ; my_semi ; return x
        return $ sammel vfs

sammel vfs = Signature
           { functions = do Right f <- vfs ; return f
           , variables = do Left  v <- vfs ; return v
           }


data TI = TI { target :: Type
             , signature :: Signature
             }
    deriving  ( Typeable )

$(derives [makeReader, makeToDoc] [''TI])

data Conf = Conf { max_arity :: Int
                 , types :: [ Type ]
                 , min_symbols :: Int
                 , max_symbols :: Int
                 , min_size :: Int
                 , max_size :: Int
                 }
    deriving ( Typeable )

conf :: Conf
conf = Conf { max_arity = 3
            , types = read "[ int, boolean, char, String, Foo, Bar ]"
            , min_symbols = 4
            , max_symbols = 10
            , min_size = 5
            , max_size = 10
            }

$(derives [makeReader, makeToDoc] [''Conf])
