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
import Data.Monoid (mconcat)

instance Container Identifier String where
     label _ = "Identifier"
     pack = show
     unpack = mknullary

data Type = Type Identifier
          | PointerTo Type
     deriving ( Eq, Ord, Typeable )

isPointerType (PointerTo _) = True
isPointerType _             = False

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

data Variable a =
     Variable { vname :: a
              , vtype :: Type
              }
     deriving ( Eq, Ord, Typeable )

instance ToDoc a => ToDoc (Variable a) where
    toDoc v = toDoc (vtype v) <+> toDoc (vname v)
instance Reader a => Reader (Variable a) where
    reader = do
        t <- reader -- type
        n <- reader -- name
        return $ Variable { vname = n, vtype = t }

data Function a =
     Function { fname :: a
              , arguments :: [ Type ]
              , result :: Type
              , static :: Bool
              }
     deriving ( Eq, Ord, Typeable )


instance ToDoc a => ToDoc (Function a) where
    -- vorsicht: alte syntax ist im cache -- na und?
    toDoc f = (if static f then text "static " else text "") <>
              toDoc ( result f ) <+> toDoc ( fname f ) <> parameterList
        where parameterList =
                 parens $ hsep $ punctuate (text ",") [ toDoc t <+> toDoc z | ( t, z ) <- zip ( arguments f ) supply ]

              supply :: [ Identifier ]
              supply = [ mknullary [v] | v <- "xyzpqrst" ++ error "too many parameters in function" ]


instance Reader a => Reader (Function a) where
    reader = do
        s <- option False $ do
               my_reserved "static"
               return True
        r <- reader -- result type
        n <- reader -- function name
        ps <- my_parens $ reader `Autolib.Reader.sepBy` my_comma :: Reader a => Parser [Variable a]  -- parameters
        return $ Function { fname = n
                          , arguments = map vtype ps
                          , result = r
                          , static = s
                          }

data Signature a =
     Signature { functions :: [ Function a ]
               , variables :: [ Variable a ]
               }
  deriving ( Typeable )

instance Size (Signature a) where
     size s = length (functions s) + length (variables s)

instance ToDoc a => ToDoc (Signature a) where
    toDoc sig = vcat 
       $  do v <- variables sig ; return $ toDoc v <> semi
       ++ do f <- functions sig ; return $ toDoc f <> semi

instance Reader a => Reader (Signature a) where
    reader = do
        let vf =  
                 do f <- try reader ; return ([f],[]) -- function
             <|> do v <-     reader ; return ([],[v]) -- variable
        vfs <- many $ do x <- vf ; my_semi ; return x
        let (fs,vs) = mconcat vfs
        return $ Signature
           { functions = fs
           , variables = vs
           }


data TI a = TI { target :: Type
               , signature :: Signature a
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
