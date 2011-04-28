{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- | like Maybe, but Nothing is shown as question mark

module String_Matching.Option where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set

import Data.Typeable

data Option a = Yes a | No
    deriving ( Eq, Ord )

instance ToDoc a => ToDoc ( Option a ) where
    toDoc ( Yes a ) = toDoc a
    toDoc No = text "?"

instance Reader a => Reader ( Option a ) where
    reader = do my_symbol "?" ; return No
         <|> do x <- reader ; return $ Yes x

yes :: [ Option a ] -> Int
yes xs = sum $ do Yes x <- xs ; return 1


class Sub a b where 
    sub :: a -> b -> Bool
    inject :: b -> a

instance Eq a => Sub ( Option a ) a where
    sub No _  = True
    sub ( Yes x ) y = x == y
    inject = Yes

instance Eq a => Sub ( Option a ) ( Option a ) where
    sub No _  = True
    sub ( Yes x ) ( Yes y ) = x == y
    sub _ _  = False
    inject = id

instance Sub a b => Sub [a] [b] where
    sub [] [] = True
    sub (x:xs) (y:ys) = sub x y && sub xs ys
    sub _ _ = False
    inject = fmap inject

instance (Ord a, Ord b, Sub a b) => Sub (Set a) (Set b) where
    sub xs ys = sub ( setToList xs ) ( setToList ys )
    inject = smap inject





