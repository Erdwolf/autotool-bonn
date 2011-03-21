{-# language DeriveDataTypeable #-}

module Program.List.Value where

import qualified Program.General.Value as G

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable


data Value = Collect { typeof :: Type , contents :: [ Value ] }
           | Scalar  Integer
           | Void
    deriving ( Eq, Typeable )

instance ToDoc Value where
    toDoc v = case v of
        Collect {} -> dutch_record $ map toDoc $ contents v
        Scalar s -> toDoc s
        Void -> text "void"

instance G.Value Value where 
    typeform = toDoc . typeof
    typeread = do
        ty <- reader
        return $ typed_reader ty

typed_reader t = case t of
    TScalar -> do i <- reader ; return $ Scalar i
    TCollect {} -> do xs <- my_braces $ my_commaSep $ typed_reader ( arg t )
                      return $ Collect t xs

data Type = TCollect { name :: Name, arg :: Type }
          | TScalar
          | TVoid
    deriving ( Eq, Typeable )

data Name = List | Stack | Queue | Array
    deriving ( Eq, Typeable, Show, Enum )

instance ToDoc Type where
    toDoc t = case t of
        TScalar -> text "int"
        TCollect {} -> case name t of
            Array -> toDoc ( arg t ) <+> text "[]"
            n -> text ( show n ) <> angles ( toDoc $ arg t )

angles t = hsep [ text "<", t, text ">" ]

instance Reader Type where
    reader = do
         a <- atom
         is <- many $ my_brackets $ return ()
         return $ foldl ( \ t () -> TCollect Array t ) a is

atom  = foldr1 (<|>) $ do my_reserved "int" ; return TScalar
      : do n <- [ List .. Queue ]
           return $ do my_reserved ( show n ) 
                       t <- my_angles reader
                       return $ TCollect n t

