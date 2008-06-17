{-# language DeriveDataTypeable #-}

module CNF.Form where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size

import Data.Typeable

import Data.Set ( Set ) 
import qualified Data.Set as S

import Data.List ( intersperse, sort )

------------------------------------------------------------------------

data CNF = CNF [ Clause ]
    deriving ( Eq, Ord, Typeable )

instance ToDoc CNF where
    toDoc ( CNF cs ) = fsep $ intersperse ( text "*" ) $ map toDoc cs

instance Reader CNF where
    reader = do
        cs <- Autolib.Reader.sepBy reader ( my_reserved "*" ) 
        return $ CNF cs

instance Size CNF where size ( CNF cs ) = length cs

--------------------------------------------------------------------------

data Clause = Clause [ Literal ]
    deriving ( Eq, Ord, Typeable )

instance ToDoc Clause where
    toDoc ( Clause ls ) = parens $ fsep $ intersperse ( text "+" ) $ map toDoc ls

instance Reader Clause where
    reader = my_parens $ do
        ls <- Autolib.Reader.sepBy reader ( my_reserved "+" ) 
        return $ Clause ls

instance Size Clause where size ( Clause ls ) = length ls

--------------------------------------------------------------------------

data Literal = Literal { variable :: Variable , parity :: Bool }
    deriving ( Eq, Ord, Typeable )

instance ToDoc Literal where
    toDoc l =
        let pre = case parity l of True -> empty ; False -> text "-"
        in  pre <> toDoc ( variable l )

instance Reader Literal where
    reader = do
        par <- option True ( do my_symbol "-" ; return False ) 
        var <- reader
        return $ Literal { variable = var, parity = par }

opposite :: Literal -> Literal
opposite l = l { parity = not $ parity l }

----------------------------------------------------------------------

data Variable = Variable { number :: Int }
    deriving ( Eq, Ord, Typeable )

instance ToDoc Variable where 
    toDoc v = text $ "x" ++ show ( number v )

instance Reader Variable where
    reader = do
        my_symbol "x"
        n <- my_integer
        return $ Variable { number = fromIntegral n }

--------------------------------------------------------------------

class Variables a  where variables :: a -> Set Variable

instance Variables Variable where variables v = S.singleton v
instance Variables Literal  where variables l = variables $ variable l
instance Variables Clause   where variables ( Clause ls ) = S.unions $ map variables ls
instance Variables CNF      where variables ( CNF    cs ) = S.unions $ map variables cs

--------------------------------------------------------------------

class Normalize a where normalize :: a -> a

instance Normalize Clause  where
    normalize ( Clause xs ) = Clause $ uniq  xs

instance Normalize CNF  where
    normalize ( CNF xs ) = CNF $ uniq $ map normalize xs

uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList

