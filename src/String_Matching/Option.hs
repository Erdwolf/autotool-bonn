-- | like Maybe, but Nothing is shown as question mark

module String_Matching.Option where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Option a = Yes a | No
    deriving ( Eq )

instance ToDoc a => ToDoc ( Option a ) where
    toDoc ( Yes a ) = toDoc a
    toDoc No = text "?"

instance Reader a => Reader ( Option a ) where
    reader = do my_symbol "?" ; return No
         <|> do x <- reader ; return $ Yes x

yes :: [ Option a ] -> Int
yes xs = sum $ do Yes x <- xs ; return 1

class Sub a where sub :: a -> a -> Bool

instance Eq a => Sub ( Option a ) where
    sub No _  = True
    sub ( Yes x ) ( Yes y ) = x == y
    sub _ _ = False

instance Sub a => Sub [a] where
    sub [] [] = True
    sub (x:xs)(y:ys) = sub x y && sub xs ys
    sub _ _ = False


