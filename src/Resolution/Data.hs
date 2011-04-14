{-# LANGUAGE DeriveDataTypeable #-}

module Resolution.Data where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set
import Autolib.TES.Identifier

import Data.Typeable

data Literal = Literal { parity :: Bool , name :: Identifier }
    deriving ( Eq, Ord, Typeable )

turn :: Literal -> Literal
turn l = l { parity = not $ parity l }

data Clause = Clause ( Set Literal )
    deriving ( Eq, Ord, Typeable )

instance ToDoc Literal where
    toDoc l = ( case parity l of True -> empty ; False -> text "!" )
           <+> toDoc ( name l )

instance Reader Literal where
    reader = do
        p <- option True $ do my_reserved "!" ; return False
        n <- my_identifier
        return $ Literal { parity = p, name = mkunary n }

instance ToDoc Clause where
    toDoc ( Clause xs ) = 
        if cardinality xs == 0 
        then text "False"
        else fsep $ punctuate ( text " ||" ) 
                        $ map toDoc $ setToList xs

instance Reader Clause where
    reader = do my_reserved "False" ; return $ Clause emptySet
         <|> do xs <- Autolib.Reader.sepBy reader ( my_reserved "||" ) 
                return $ Clause $ mkSet xs

