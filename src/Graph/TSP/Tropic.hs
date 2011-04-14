{-# LANGUAGE DeriveDataTypeable #-}

-- | numbers, extended with  Plus Infinity

module Graph.TSP.Tropic where

import Data.Typeable
import Autolib.Reader
import Autolib.ToDoc

data Tropic a = Finite a | Infinite
    deriving (Eq, Ord, Typeable)

instance ( ToDoc a, Num a ) => Num (Tropic a) where
    -- note: fromInteger 0 is called by the implementation of sum
    fromInteger i = Finite $ fromInteger i

    Finite x + Finite y = Finite (x+y)
    _ + _ = Infinite

instance ToDoc a => ToDoc ( Tropic a ) where
    toDoc t = case t of
        Finite f -> toDoc f
        Infinite -> text "+"

instance Reader a => Reader ( Tropic a ) where
    reader = do my_reserved "+" ; return Infinite
         <|> do x <- reader ; return $ Finite x

