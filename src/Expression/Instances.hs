module Expression.Instances where

import Expression.Type
import Expression.Class

----------------------------------------------------------------------------

instance Expressive Int where
    toExp i = Num (fromIntegral i)
    fromExp (Num e) = fromIntegral e

instance Expressive Integer where
    toExp i = Num (fromIntegral i)
    fromExp (Num e) = fromIntegral e

instance Expressive String where
    toExp s = Strg s
    fromExp (Strg s) = s
	     
instance Expressive a => Expressive [ a ] where
    toExp = List . map toExp 
    fromExp ( List es ) = map fromExp es

instance Expressive () where 
    toExp () = Tuple []
    fromExp ( Tuple [] ) = ()

instance ( Expressive a, Expressive b ) => Expressive ( a, b ) where
    toExp ( x, y ) = Tuple [ toExp x, toExp y ]
    fromExp ( Tuple [ x, y ] ) = ( fromExp x, fromExp y )

instance ( Expressive a, Expressive b, Expressive c ) 
	 => Expressive ( a, b, c ) where
    toExp ( x, y, z ) = Tuple [ toExp x, toExp y, toExp z ]
    fromExp ( Tuple [ x, y, z ] ) = ( fromExp x, fromExp y, fromExp z )

instance Expressive a => Expressive (Maybe a) where
    toExp Nothing = Id "Nothing"
    toExp (Just x) = Positional "Just" [ toExp x ]
    fromExp (Id "Nothing") = Nothing
    fromExp ( Positional "Just" [ x ] ) = Just (fromExp x)
